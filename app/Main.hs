module Main where

import Meatbar

main :: IO ()
main = runWithInMemorySqlite $ do
    runMigrations
    loadCsvData "data.csv"
    startServer 8080
