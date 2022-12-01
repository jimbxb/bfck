module Language.Wybe
  ( transpile
  ) where

import BrainFuck

transpile :: Int -> Program -> IO ()
transpile _ program = do
  putStr $
    unlines $
    [ "type tape {"
    , "    pub tape(left:list(char), current:char, right:list(char))"
    , ""
    , "    pub def {test} (x:_ = y:_) { fail }"
    , ""
    , "    pub def left(!tape:_) {"
    , "        tape(?left, ?current, ?right) = tape"
    , "        if { right = [?r | ?right] :: "
    , "            ?tape = tape([current | left], r, right)"
    , "           | else :: "
    , "            ?tape = tape([current | left], '\\0', [])"
    , "        }"
    , "    }"
    , ""
    , "    pub def right(!tape:_) {"
    , "        tape(?left, ?current, ?right) = tape"
    , "        if { left = [?l | ?left] :: "
    , "            ?tape = tape(left, l, [current | right])"
    , "           | else :: "
    , "            ?tape = tape([], '\\0', [current | right])"
    , "        }"
    , "    }"
    , ""
    , "    pub def {inline} incr(!tape:_) {"
    , "        !tape^current = foreign llvm add(tape^current, 1:char)"
    , "    }"
    , ""
    , "    pub def {inline} decr(!tape:_) {"
    , "        !tape^current = foreign llvm sub(tape^current, 1:char)"
    , "    }"
    , "}"
    , ""
    , "?t = tape([], '\\0', [])"
    , ""
    ]++
    concatMap (map (uncurry indent) . go 0) program
  where
    indent i s = replicate (4 * i) ' ' ++ s
    go i Incr = [(i, "incr(!t)")]
    go i Decr = [(i, "decr(!t)")]
    go i Left' = [(i, "left(!t)")]
    go i Right' = [(i, "right(!t)")]
    go i Input = [(i, "!read(?t^current)")]
    go i Output = [(i, "!print(t^current)")]
    go i (Loop loop) =
      [(i, "do {"), (i + 1, "until t^current = '\\0'")] ++
      concatMap (go $ i + 1) loop ++ [(i, "}")]
