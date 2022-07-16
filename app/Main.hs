{-# LANGUAGE OverloadedStrings #-}
module Main where

import Softcode
import MySoftcode
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Foldable (foldMap')
import Data.Monoid

main :: IO ()
main = T.putStrLn (T.intercalate "\n" $ map (\v -> "\\" <> toText v) mySoftcode)
