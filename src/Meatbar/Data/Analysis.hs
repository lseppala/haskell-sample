module Meatbar.Data.Analysis where

import           Data.Function      (on)
import           Data.List
import           Data.Time.Calendar
import           Data.Time.Clock


-- | Data type to contain a group of elements, 'groupElems', which all share
-- some key, 'groupKey'
data GroupedBy a b = GroupedBy
    { groupKey   :: a
    , groupElems :: [b]
    } deriving (Show, Eq)

-- | Wrapper for year and month
newtype YearMonth =
    YearMonth (Integer, Int) deriving (Eq, Ord, Show)


-- | Modified version of 'groupBy', which collects elements into the
-- 'GroupedBy' data type to associate elements with the key they were grouped by
groupedBy :: Ord b => (a -> b) -> [a] -> [GroupedBy b a]
groupedBy _ [] = []
groupedBy f (x:xs) = GroupedBy (f x) (x:ys): groupedBy f zs
    where
        (ys,zs) = span (\y -> f x == f y) xs


-- | Number of elements in a 'GroupBy'
groupCount :: GroupedBy a b -> Int
groupCount = length . groupElems


-- | Comparison by the keys of a group
compareGroupKey :: Ord a => GroupedBy a b -> GroupedBy a b -> Ordering
compareGroupKey x y = compare (groupKey x) (groupKey y)


-- | Find the largest group by number of elements in the group. Gives the last
-- group if all equal sizes
largestGroup :: Foldable f => f (GroupedBy b a) -> GroupedBy b a
largestGroup =
    maximumBy (compare `on` groupCount)


-- | Group a list of elements by 'Day' derived from a function to get 'UTCTime'
groupedByDay :: (a -> UTCTime) -> [a]
             -> [GroupedBy Day a]
groupedByDay f = groupedBy (utctDay . f)


-- | For a list of elements already grouped by 'Day', group them by 'YearMonth'
groupedByDayOfMonth :: [GroupedBy Day a]
                    -> [GroupedBy YearMonth (GroupedBy Day a)]
groupedByDayOfMonth = groupedBy yearMonth
    where
        yearMonth (GroupedBy day _) =
            let (year, month, _) = toGregorian day
            in YearMonth (year, month)


-- | Find the largest daily group for each month
largestDayEachMonth :: (a -> UTCTime)
                    -> [a]
                    -> [(Day, Int)]
largestDayEachMonth f as = do
    let dayGroups = groupedByDay f as
        monthGroups = groupedByDayOfMonth dayGroups
    g <- monthGroups
    let bigDay = largestGroup (groupElems g)
    return (groupKey bigDay, groupCount bigDay)


-- | For a list of elements, find all streaks where the next element in the
-- list matches some predicate when compared to the current element
-- n.b. The 'reverse' functions are used to keep elements in the same order as
-- the input.
allStreaks :: [a] -> (a -> a -> Bool) -> [[a]]
allStreaks as p = reverse $ go as [] []
    where
        -- Base case for empty list input
        go [] [] acc =
            acc
        -- Base case, return the accumulation
        go [] current acc =
            reverse current : acc
        -- A single remaining element is not a streak
        go [_] [] acc =
            acc
        -- A remaining element element tops off the streak
        go [x] current acc =
            reverse (x:current) : acc
        -- Case when there is no streak started
        go (x:y:xs) [] acc
          -- The the next element meets the predicate, start a streak
          | p x y =
              go (y:xs) [x] acc
          -- The predicate does not match, do not start a streak
          | otherwise =
              go (y:xs) [] acc
        -- Case when a streak is started
        go (x:y:xs) current acc
          -- The next element meets the predicate, continue the streak
          | p x y =
              go (y:xs) (x:current) acc
          -- The next element does not meet the predicate. This is the end
          -- of the current streak
          | otherwise =
              go (y:xs) [] $ reverse (x:current) : acc


-- | Find all streaks where the group of the current day has more elements than
-- the previous day, ignoring days without a count
allHigherCountDailyStreaks :: (a -> UTCTime) -> [a] -> [[(Day, Int)]]
allHigherCountDailyStreaks f as =
    let groups = groupedByDay f as
        sortedGroups = sortBy compareGroupKey groups
        increasingCount x y = groupCount x < groupCount y
        streaks = allStreaks sortedGroups increasingCount
        dayAndCount g = (groupKey g, groupCount g)
     in fmap dayAndCount <$> streaks
