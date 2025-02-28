module Ponder where

import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Parse
import Skill

-- Clock Time DEFINITIONS:

data PTime = PTime
  { meanTime :: !Double,
    itemCnt :: !Int
  }
  deriving stock (Show, Eq)

newPTime :: [PTime]
newPTime = replicate (length skillGroups) (PTime 0 0)

-- online mean calculation
increment :: PTime -> Double -> PTime
increment (PTime cur cnt) val = PTime new cnt'
  where
    cnt' = cnt + 1
    new = cur + (val - cur) / fromIntegral cnt'
{-# INLINE increment #-}

-- ended up coming up with what is available as
-- modifyAt from ilist without extra checks
incrementBySkill :: Int -> Double -> [PTime] -> [PTime]
incrementBySkill _ _ [] = []
incrementBySkill 0 val (x : xs) = increment x val : xs
incrementBySkill i val (x : xs) = x : incrementBySkill (i - 1) val xs
{-# INLINE incrementBySkill #-}

-- yet another wrapper that takes care of the Maybe
incrementTime :: Maybe Int -> Double -> [PTime] -> [PTime]
incrementTime Nothing _ t = t
incrementTime (Just i) val t = incrementBySkill i val t
{-# INLINE incrementTime #-}

-- MapNode DEFINITIONS:

data MapNode = MapNode
  { move :: !Text,
    time :: ![PTime],
    leaves :: ![MapNode]
  }
  deriving stock (Show)

-- we don't start thinking until after the first move, so the root is a list
type MoveMap = [MapNode]

-- BUILDING THE MAP:

-- building up a new MoveMap
--   here the positions are the ground truth
--   and we have to insert anything we have not seen before.
--   we will solve this elegrantly with a fold

makeMoveMap ::
  [Text] -> -- list of positions in "pgn"ish notation
  MoveMap
makeMoveMap = foldl' buildInsert []

buildInsert ::
  MoveMap -> -- map to insert into
  Text -> -- position, sequence of algebraic notation moves
  MoveMap
buildInsert mm pos = go mm moves
  where
    moves = T.words pos

    -- recursively traverse m
    --   stop when no more moves
    go m [] = m
    --   make new node if empty
    go [] (x : xs) = [MapNode x newPTime (go [] xs)]
    --   otherwise, look at each leaf to check if move is present
    go (node@(MapNode c t leaves) : ns) list@(san : xs)
      -- recurse on move list if match found
      | c == san = MapNode c t (go leaves xs) : ns
      -- recurse on children if not
      | otherwise = node : go ns list

-- UPDATING THE MAP:

-- so now here is the code for counting thinking time
--   here the MoveMap is the ground truth and any node that
--   does not already exist in the map doesn't matter (is discarded)

-- helper function to convert Game to an insertable list
--   (skill, san, clock)
type GameList = [(Maybe Int, Text, Double)]

-- key function in which we convert clock values for both players
-- to time spent on each move
gameToList :: Game -> GameList
gameToList (Game ws bs time inc ms) = go time time True ms
  where
    go ::
      Int -> -- last white time
      Int -> -- last black time
      Bool -> -- is current entry for white or black?
      [Move] ->
      GameList
    go _ _ _ [] = []
    go wclk bclk white ((Move san clk) : xs)
      | white = (ws, san, fromIntegral (wclk - clk + inc)) : go clk bclk (not white) xs
      | otherwise = (bs, san, fromIntegral (bclk - clk + inc)) : go wclk clk (not white) xs

process ::
  MoveMap ->
  [Game] ->
  MoveMap
process mm = foldl' insert mm . map gameToList

insert ::
  MoveMap ->
  GameList ->
  MoveMap
-- inserting empty move sequence does nothing
insert mm [] = mm
-- inserting into an empty movemap does nothing!
insert [] _ = []
-- inserting otherwise looks over all nodes
insert (node@(MapNode c t leaves) : ns) gameList@((sk, san, clk) : xs)
  | c == san = MapNode c (incrementTime sk clk t) (insert leaves xs) : ns
  | otherwise = node : insert ns gameList

-- OUTPUT:
formatMap ::
  MoveMap ->
  [(Text, Int, Double, Int)] -- (position, skill, mean, cnt)
formatMap = formatWithPrefix ""
  where
    formatWithPrefix _ [] = []
    formatWithPrefix pfix ((MapNode c t leaves) : ns) =
      let pfix' = pfix <> " " <> c
          curFormat = zipWith (\i (PTime cur cnt) -> (pfix', i, cur, cnt)) [0 ..] t
          childFormat = formatWithPrefix pfix' leaves
       in curFormat ++ formatWithPrefix pfix ns ++ childFormat
