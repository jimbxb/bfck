module Language.Language
  ( transpilers
  ) where

import BrainFuck
import qualified Language.C as C
import qualified Language.Haskell as Haskell
import qualified Language.Python as Python
import qualified Language.Wybe as Wybe

transpilers :: [(String, Int -> Program -> IO ())]
transpilers =
  [ ("c", C.transpile)
  , ("haskell", Haskell.transpile)
  , ("python", Python.transpile)
  , ("wybe", Wybe.transpile)
  ]
