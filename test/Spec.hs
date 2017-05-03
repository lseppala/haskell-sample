import           Data.Time.Clock
import           Test.Hspec

import           Meatbar.Data.Analysis

-- Used to make dis-ambiguate forf type inference
asInt :: Int -> Int
asInt = id

increasing :: Int -> Int -> Bool
increasing = (<)

secondsInDay :: Num a => a
secondsInDay = 86400

-- Roughly. Depending on the lenght of the month, this might be more than the
-- the actual number of seconds, but it's a workable approximation for these
-- tests
secondsInMonth :: Num a => a
secondsInMonth = 31 * secondsInDay

main :: IO ()
main = do
    now <- getCurrentTime
    let yesterday = (-secondsInDay) `addUTCTime` now
        lastMonth = (-secondsInMonth) `addUTCTime` now
    hspec $
        describe "Meatbar.Data.Analysis" $ do
            describe "groupedBy" $ do
                it "has no groups with empty list" $
                    groupedBy asInt [] `shouldBe` []

                it "has 1 group with singleton list" $
                    groupedBy asInt [1] `shouldBe` [GroupedBy 1 [1]]

                it "groups items with same key" $
                    groupedBy asInt [1, 1] `shouldBe` [GroupedBy 1 [1,1]]

                it "doesn't group items with different key" $
                    groupedBy asInt [1, 2] `shouldBe`
                        [GroupedBy 1 [1], GroupedBy 2 [2]]

            describe "largestGroup" $ do
                it "finds largest group" $
                    let groups = groupedBy asInt [1,1,2]
                     in largestGroup groups
                        `shouldBe` GroupedBy 1 [1,1]
                it "gives last group if all equal sizes" $
                    let groups = groupedBy asInt [1,2]
                     in largestGroup groups
                        `shouldBe` GroupedBy 2 [2]

            describe "groupedByDay" $ do
                it "groups single day" $
                    groupedByDay id [now] `shouldBe`
                        [GroupedBy (utctDay now) [now]]
                it "groups multiple on single day" $
                    groupedByDay id [now, now] `shouldBe`
                        [GroupedBy (utctDay now) [now, now]]
                it "groups each day" $
                    groupedByDay id [now, now, yesterday] `shouldBe`
                        [ GroupedBy (utctDay now) [now, now]
                        , GroupedBy (utctDay yesterday) [yesterday] ]

            describe "groupedByDayOfMonth" $ do
                let dayGroups = groupedByDay id [now, now, lastMonth]
                it "has two groups" $
                    length (groupedByDayOfMonth dayGroups) `shouldBe` 2
                it "groups each month" $
                    groupedByDayOfMonth dayGroups `shouldBe`
                        [ GroupedBy (timeToYearMonth now)
                            [ GroupedBy (utctDay now) [now, now] ]
                        , GroupedBy (timeToYearMonth lastMonth)
                            [ GroupedBy (utctDay lastMonth) [lastMonth]]
                        ]

            describe "largestDayEachMonth" $ do
                let testData = [now, now, lastMonth]
                it "has each largest day each month " $
                    largestDayEachMonth id testData `shouldBe`
                        [ (utctDay now, 2)
                        , (utctDay lastMonth, 1)
                        ]

            describe "allStreaks" $ do
                it "has no streak with empty list" $
                    allStreaks [] increasing `shouldBe` []
                it "has no streak with singleton list" $
                    allStreaks [1] increasing `shouldBe` []
                it "has streak when increasing" $
                    allStreaks [1,2] increasing `shouldBe` [[1,2]]
                it "has no streak when not increasing" $
                    allStreaks [2, 1] increasing `shouldBe` []
                it "has two streaks when broken up" $
                    allStreaks [1, 2, 1, 3] increasing `shouldBe` [[1,2], [1,3]]
                it "has one streak when broken up with dangling element" $
                    allStreaks [1, 2, 1] increasing `shouldBe` [[1,2]]
                it "has one streak when started with dangling element" $
                    allStreaks [3, 1, 2] increasing `shouldBe` [[1,2]]

            describe "allHigherCountDailyStreaks" $ do
                it "has streak with increasing count" $
                    let singleStreak =
                            [lastMonth, yesterday, yesterday, now, now, now]
                     in allHigherCountDailyStreaks id singleStreak `shouldBe`
                        [ [ (utctDay lastMonth, 1)
                          , (utctDay yesterday, 2)
                          , (utctDay now, 3)
                          ]
                        ]
                it "has no streak with decreasing count" $
                    let noStreak =
                            [lastMonth, lastMonth, lastMonth, yesterday, yesterday, now]
                     in allHigherCountDailyStreaks id noStreak `shouldBe` []

                it "has no streak with equal count" $
                    let noStreak =
                            [lastMonth, lastMonth, yesterday, yesterday, now, now]
                     in allHigherCountDailyStreaks id noStreak `shouldBe` []

                it "has multiple streaks when broken up" $
                    let lastMonthPlus1 = secondsInDay `addUTCTime` lastMonth
                        dayBeforeYesterday = (-secondsInDay) `addUTCTime` yesterday
                        multipleStreaks =
                            [
                              -- first streak
                              lastMonth, lastMonthPlus1, lastMonthPlus1
                              -- break
                            , dayBeforeYesterday
                              -- second streak
                            , yesterday, now, now
                            ]
                     in allHigherCountDailyStreaks id multipleStreaks `shouldBe`
                        [ [ (utctDay lastMonth, 1)
                          , (utctDay lastMonthPlus1, 2)
                          ] ,
                          [ (utctDay yesterday, 1)
                          , (utctDay now, 2)
                          ]
                        ]
