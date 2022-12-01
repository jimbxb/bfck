module Options
  ( Options(..)
  , handleArgs
  ) where

import Language.Language
import System.Console.GetOpt
import System.Environment
import Text.Read

data Options =
  Options
    { size :: Maybe Int
    , language :: Maybe String
    }

defaultOptions :: Options
defaultOptions = Options {size = Just 30000, language = Nothing}

handleArgs :: IO (Options, [String])
handleArgs = do
  argv <- getArgs
  case getOpt Permute options argv of
    (opts, srcs, []) -> do
      let opts' = foldl (flip id) defaultOptions opts
      return (opts', srcs)
    (_, _, errs) -> ioError $ userError $ concat errs

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['s']
      ["size"]
      (ReqArg (\s opts -> opts {size = readMaybe s}) "SIZE")
      ("set the size of the tape (default: " ++
       show (size defaultOptions) ++ ")")
  , Option
      ['l']
      ["language"]
      (ReqArg (\l opts -> opts {language = Just l}) "LANGUAGE")
      ("set the language to traspile to (one of: " ++
       concatMap fst transpilers ++ ")")
  ]
