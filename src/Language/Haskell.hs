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
    , "data Tape = Tape { left' :: [Int], current :: Int, right' :: [Int] }"
    , ""
    , "incr :: Tape -> IO Tape"
    , "incr (Tape l c r) = return (Tape l (c + 1) r)"
    , ""
    , "decr :: Tape -> IO Tape"
    , "decr (Tape l c r) = return (Tape l (c - 1) r)"
    , ""
    , "left :: Tape -> IO Tape"
    , "left (Tape l c (r:r')) = return (Tape (c:l) r r')"
    , ""
    , "right :: Tape -> IO Tape"
    , "right (Tape (l:l') c r) = return (Tape l' l (c:r))"
    , ""
    , "input :: Tape -> IO Tape"
    , "input (Tape l _ r) = getChar >>= \\c -> return (Tape l (ord c) r)"
    , ""
    , "output :: Tape -> IO Tape"
    , "output t@(Tape _ c _) = putStr [chr (fix c)] >> return t"
    , ""
    , "loop :: (Tape -> IO Tape) -> Tape -> IO Tape"
    , "loop _ t@(Tape _ c _) | fix c == 0 = return t"
    , "loop f t = f t >>= loop f"
    , "{-# INLINE loop #-}"
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
    go i Incr = [(i, "incr")]
    go i Decr = [(i, "decr")]
    go i Left' = [(i, "left")]
    go i Right' = [(i, "right")]
    go i Input = [(i, "input")]
    go i Output = [(i, "output")]
    go i (Loop loop) =
      [(i, "loop (\\t -> return t")] ++ concatMap (go $ i + 1) loop ++ [(i + 1, "return)")]
