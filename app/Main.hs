{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main
  ( main
  ) where

import Language.Language
import Options
import Parse
import Run

import Control.Monad
import Data.Maybe
import System.Exit

main :: IO ()
main = do
  (Options {size, language}, files) <- handleArgs
  runner <-
    case language of
      Nothing -> return (const run)
      Just lang ->
        case lang `lookup` transpilers of
          Just transpiler -> return transpiler
          Nothing -> putStrLn ("rror: unknown language " ++ lang) >> exitFailure
  srcs <-
    case files of
      [] ->
        (: []) . (, "<stdin>") <$> maybe getLine (const getContents) language
      _ -> mapM (\file -> (, file) <$> readFile file) files
  forM_ srcs $ \(src, name) ->
    either
      ((putStr (name ++ ":") >>) . (>> exitFailure) . putStrLn)
      (runner (fromJust size))
      (parse src)
