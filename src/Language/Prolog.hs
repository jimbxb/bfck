module Language.Prolog
  ( transpile
  ) where

import BrainFuck

import Data.List (intercalate)

transpile :: Int -> Program -> IO ()
transpile _ program = do
  putStr $
    unlines $
    [ "#!/usr/bin/env swipl"
    , ""
    , ":- initialization(main, main)."
    , ""
    , "incr(state(Ls, X0, Rs), state(Ls, X, Rs)) :-"
    , "    X is ((X0 + 1) mod 256 + 256) mod 256."
    , ""
    , "decr(state(Ls, X0, Rs), state(Ls, X, Rs)) :-"
    , "    X is ((X0 - 1) mod 256 + 256) mod 256."
    , ""
    , "left(state([], X, Rs), state([], 0, [X | Rs]))."
    , "left(state([L | Ls], X, Rs), state(Ls, L, [X | Rs]))."
    , ""
    , "right(state(Ls, X, []), state([X | Ls], 0, []))."
    , "right(state(Ls, X, [R | Rs]), state([X | Ls], R, Rs))."
    , ""
    , "output(state(Ls, _, Rs), state(Ls, X, Rs)) :-"
    , "    prompt1(''),"
    , "    get_char(X0),"
    , "    ( X0 = end_of_file ->"
    , "        X = 0"
    , "    ;"
    , "        X = X0"
    , "    )."
    , ""
    , "input(state(Ls, X, Rs), state(Ls, _, Rs)) :-"
    , "    put_char(X)."
    , ""
    , "main(_) :-"
    , "    body(state([], 0, []), _)."
    , ""
    , "body(S0, S" ++ show (length program) ++ ") :-"
    ] ++
    commas body ++ [""] ++ concat predicates
  where
    (body, predicates) = build [] program

build :: [Int] -> [Instruction] -> ([String], [[String]])
build _ [] = ([], [])
build is instrs = unzip $ zipWith (instr is) [0 ..] instrs

predicate :: [Int] -> [Instruction] -> [[String]]
predicate is instrs =
  let (instrs', preds) = build is instrs
      lastS = "S" ++ show (length instrs + 1)
   in ((name is ++ "(state(Ls, 0, Rs), state(Ls, 0, Rs)) :- !.") :
       (name is ++ "(S0, " ++ lastS ++ ") :-") :
       commas (instrs' ++ [call (name is) (length instrs)]) ++ [""]) :
      preds

instr :: [Int] -> Int -> Instruction -> (String, [String])
instr _ i Incr = (call "incr" i, [])
instr _ i Decr = (call "decr" i, [])
instr _ i Left' = (call "left" i, [])
instr _ i Right' = (call "right" i, [])
instr _ i Output = (call "output" i, [])
instr _ i Input = (call "input" i, [])
instr is i (Loop instrs) =
  let is' = i : is
   in (call (name is') i, concat (predicate is' instrs))

name :: [Int] -> String
name is = "body" ++ intercalate "_" (map show is)

call :: String -> Int -> String
call n i =
  n ++ "(" ++ intercalate ", " (map (("S" ++) . show) [i, i + 1]) ++ ")"

commas :: [String] -> [String]
commas [] = ["    true."]
commas xs = map ("    " ++) $ map (++ ",") (init xs) ++ [last xs ++ "."]
