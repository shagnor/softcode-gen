{-# LANGUAGE OverloadedStrings #-}
module Main where

import Softcode
import Pregnancy (pregnancyCode)
import WarnLock (warnlockCode)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Foldable (foldMap')
import Data.Monoid
import System.Environment

softcodeFor :: String -> Maybe [Softcode]
softcodeFor s | s == "pregnancy" = Just pregnancyCode
              | s == "warnlock" = Just warnlockCode
              | otherwise = Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    n:[] ->
      case softcodeFor n of
        Nothing -> T.putStrLn "Sorry, that softcode isn't registered in Main.hs"
        Just code ->
          T.putStrLn (T.intercalate "\n" $ map (\v -> "\\" <> toText v) code)
    _ -> T.putStrLn "Usage: softcode-gen <program>"
