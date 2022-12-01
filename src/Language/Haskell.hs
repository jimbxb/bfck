module Language.Haskell
  ( transpile
  ) where

import BrainFuck

transpile :: Int -> Program -> IO ()
transpile _ program = do
  putStr $
    unlines $
    [ "import Data.Char"
    , ""
    , "data Tape = Tape { left :: [Int], current :: Int, right :: [Int] }"
    , ""
    , "iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a"
    , "iterateUntilM p f v"
    , "  | p v       = return v"
    , "  | otherwise = f v >>= iterateUntilM p f"
    , ""
    , "fix :: Int -> Int"
    , "fix x = mod (mod x 256 + 256) 256"
    , ""
    , "main :: IO ()"
    , "main = run $ Tape zeroes 0 zeroes"
    , "  where zeroes = 0:zeroes"
    , ""
    , "run :: Tape -> IO ()"
    , "run t = return t"
    ] ++
    concatMap (map (uncurry indent) . go 1) program ++ ["  >> return ()"]
  where
    indent i s = replicate (2 * i) ' ' ++ ">>= " ++ s
    go i Incr = [(i, "\\(Tape l c r) -> return (Tape l (c + 1) r)")]
    go i Decr = [(i, "\\(Tape l c r) -> return (Tape l (c - 1) r)")]
    go i Left' = [(i, "\\(Tape l c (r:r')) -> return (Tape (c:l) r r')")]
    go i Right' = [(i, "\\(Tape (l:l') c r) -> return (Tape l' l (c:r))")]
    go i Input =
      [(i, "\\(Tape l _ r) -> getChar >>= \\c -> return (Tape l (ord c) r)")]
    go i Output = [(i, "\\t@(Tape _ c _) -> putStr [chr (fix c)] >> return t")]
    go i (Loop loop) =
      [(i, "iterateUntilM (\\(Tape _ c _) -> fix c == 0) (\\t -> return t")] ++
      concatMap (go $ i + 1) loop ++ [(i + 1, "return)")]
