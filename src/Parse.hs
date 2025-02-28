module Parse (run, Game (..), Move (..)) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Lazy qualified as Lazy
import Data.Attoparsec.Text qualified as TextP
import Data.ByteString.Lazy qualified as BSL
import Data.List (foldl', intercalate)
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeASCII)
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import Skill (getSkill)

-- https://stackoverflow.com/questions/27236539/what-about-data-attoparsec-bytestring-lazy-char8

-- compared to lichess-pgn-parser we significantly cut the number of parsed fields
data Move = Move {san :: !Text, clk :: !Int} deriving stock (Show)

type Moves = [Move]

data Game = Game
  { whiteSkill :: !(Maybe Int),
    blackSkill :: !(Maybe Int),
    time :: !Int,
    inc :: !Int,
    moves :: !Moves
  }
  deriving stock (Show)

-- run the parser from stdin
run :: (Int, Int) -> IO [Game]
run tc = catMaybes . goRec . Lazy.parse game' <$> BSL.getContents
  where
    game' = game tc
    goRec :: Lazy.Result (Maybe Game) -> [Maybe Game]
    goRec (Lazy.Fail _ ctxs e) =
      error $
        "pgn parser failed: "
          <> e
          <> "\n"
          <> intercalate "\n" ctxs
    goRec (Lazy.Done bs pgn)
      | BSL.null bs = [pgn]
      | otherwise = pgn : goRec (Lazy.parse game' bs)

-- i borrow a lot from https://hackage.haskell.org/package/chesshs here
-- in terms of parsing the tags and the whole PGN type

-- main parser for the whole game
game (mint, maxt) = do
  tags <- Map.fromList <$> many1 tag
  skipSpace
  -- time control is a Maybe (Int, Int) which is time and increment
  let timeControl = parseTc (decodeASCII $ tags ! "TimeControl")
      wSkill = getSkill . parseElo $ tags ! "WhiteElo"
      bSkill = getSkill . parseElo $ tags ! "BlackElo"
      -- skip parsing the moves if:
      shouldSkip =
        isNothing timeControl -- time control is not parseable
          || (fst (fromJust timeControl) > maxt) -- time control too long
          || (fst (fromJust timeControl) < mint) -- time control too short
          || (isNothing wSkill && isNothing bSkill) -- out of relevant skill range
          || isInvalidTermination tags -- game "unterminated"
          || isBotGame tags -- bot game
  if shouldSkip
    then do
      -- if we are skipping, just consume the whole line with moves
      -- without worrying about its contents
      skipMoveLine
      skipSpace
      return Nothing
    else do
      moves' <- moveLine
      skipSpace
      let time = fst $ fromJust timeControl
          inc = snd $ fromJust timeControl
      return . Just $
        Game
          wSkill
          bSkill
          time
          inc
          moves'
  where
    -- check whether white or black player is a BOT
    isBotGame t = isBot "WhiteTitle" t || isBot "BlackTitle" t
    isBot v t = case Map.lookup v t of
      Just x -> decodeASCII x == "BOT"
      Nothing -> False
    -- check whether game termination was "weird"
    isInvalidTermination t = case decodeASCII (t ! "Termination") of
      "Unterminated" -> True
      "Rules infraction" -> True
      _ -> False
    -- this is now parsing Text, so use appropriate functions
    -- this works weird as "partial" result is always returned
    -- (i assume this is because more digits could technically be parsed),
    -- hence the couple `case of`
    parseTc tc' =
      case TextP.parse
        ( do
            tc <- TextP.decimal
            TextP.char '+'
            inc <- TextP.decimal
            return (tc, inc)
        )
        tc' of
        (TextP.Done _ r) -> Just r
        (TextP.Partial rest) -> case rest "" of
          (TextP.Done _ r) -> Just r
          _ -> Nothing
        (TextP.Fail {}) -> Nothing
    parseElo t = case T.decimal (decodeASCII t) of
      Left _ -> 0
      Right (v, _) -> v

tag = do
  skipSpace
  char '['
  tagName <- takeTill isSpace
  skipSpace
  tagValue <- stringLiteral
  char ']'
  return (tagName, tagValue)

stringLiteral = do
  char '"'
  value <- takeTill ('"' ==)
  char '"'
  return value

-- MOVE LINE PARSING
--  we don't want to waste time parsing beyond the opening
--  so let's try to parse 16 moves fully, and then skip the rest until EOL

moveLimit = 16

-- special combinator that tries to consume the next move
upto :: Int -> Parser a -> Parser [a]
upto n p | n > 0 = (:) <$> try p <*> upto (n - 1) p <|> return []
upto _ _ = return []

moveLine = do
  -- try to consume the first 16 moves
  --   or (hopefully) fail on hitting result symbol
  moves <- upto moveLimit move
  -- consume until end of line
  skipWhile ('\n' /=)
  char '\n'
  return moves

-- this parses both the move and the clock comment
move = do
  moveNumber
  san <- takeTill isSpace
  space
  clockValue <- comment
  space
  return $ Move (cleanSan $ decodeASCII san) clockValue

-- strip the evaluation suffixes that are present for some games
cleanSan san = foldl' (\x s -> fromMaybe x (T.stripSuffix s x)) san ["??", "!!", "!?", "?!", "!", "?"]

comment = do
  char '{'
  space
  -- skip eval block that is present in some games
  --   (examples always show it coming before %clk)
  option () evalBlock
  clockValue <- clkBlock
  -- skip until end of comment just in case there is something else
  skipWhile ('}' /=)
  char '}'
  return clockValue
  where
    -- parse "[%eval 0.32]"
    evalBlock = do
      string "[%eval"
      skipWhile (']' /=)
      string "] "
      return ()
    -- parse "[%clk 0:03:44]"
    clkBlock = do
      string "[%clk "
      vals <- decimal `sepBy1` char ':'
      char ']'
      -- assume here that we always have time as h:mm:ss
      -- also we won't ever deal with games with > hour on the clock (hence the `tail`)
      return . sum $ zipWith (*) [60, 1] (drop 1 vals)

moveNumber = do
  many1 digit
  many' $ char '.'
  space

skipMoveLine =
  skipWhile ('\n' /=)
    *> endOfLine
