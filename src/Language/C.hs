module Language.C
  ( transpile
  ) where

import BrainFuck

transpile :: Int -> Program -> IO ()
transpile size program = do
  putStr $
    unlines $
    [ "#include <stdio.h>"
    , ""
    , "int main(char *argv[], int argc) {"
    , "    char array[" ++ show size ++ "] = {0};"
    , "    char *ptr = array;"
    , ""
    ] ++
    concatMap (map (uncurry indent) . go 1) program ++
    ["", "    return 0;", "}"]
  where
    indent i s = replicate (4 * i) ' ' ++ s
    go i Incr = [(i, "++*ptr;")]
    go i Decr = [(i, "--*ptr;")]
    go i Left' = [(i, "--ptr;")]
    go i Right' = [(i, "++ptr;")]
    go i Input = [(i, "*ptr = getchar();")]
    go i Output = [(i, "putchar(*ptr);")]
    go i (Loop loop) =
      [(i, "while (*ptr) {")] ++ concatMap (go $ i + 1) loop ++ [(i, "}")]
