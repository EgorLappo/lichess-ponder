module Skill where

-- this contains general config variables
-- related to the algorithm
-- these are unlikely to change as the main
-- framework of the paper is already settled

skillGroups :: [(Int, Int)]
skillGroups =
  [ (1000, 1400),
    (1400, 1800),
    (1800, 2200),
    (2200, 3600)
  ]

-- get skillGroupIndex
getSkill :: Int -> Maybe Int
getSkill = go 0 skillGroups
  where
    go :: Int -> [(Int, Int)] -> Int -> Maybe Int
    go _ [] _ = Nothing
    go i ((mi, ma) : gs) val
      | val < mi = Nothing -- filter out irrelevant games with Maybe
      | (val >= mi) && (val < ma) = Just i
      | otherwise = go (i + 1) gs val
