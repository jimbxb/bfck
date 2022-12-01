module Language.Python
  ( transpile
  ) where

import BrainFuck

transpile :: Int -> Program -> IO ()
transpile size program = do
  putStr $
    unlines $
    [ "import sys"
    , ""
    , "def main():"
    , "    array = [0] * " ++ show size
    , "    idx = 0"
    ] ++
    concatMap (map (uncurry indent) . go 1) program ++
    ["", "if __name__ == \"__main__\":", "    main()"]
  where
    indent i s = replicate (4 * i) ' ' ++ s
    go i Incr = [(i, "array[idx] += 1")]
    go i Decr = [(i, "array[idx] -= 1")]
    go i Left' = [(i, "idx -= 1")]
    go i Right' = [(i, "idx += 1")]
    go i Input = [(i, "sys.stdin.read(1)")]
    go i Output =
      [(i, "print(chr(((array[idx] % 256) + 256) % 256), end=\"\")")]
    go i (Loop loop) =
      [(i, "while (array[idx] % 256 + 256) % 256:")] ++
      if null loop
        then [(i + 1, "pass")]
        else concatMap (go $ i + 1) loop
