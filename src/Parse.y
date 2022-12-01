{
module Parse
     ( parse 
     ) where

import BrainFuck
}

%name parse_
%tokentype { ((Int, Int), Char) }
%error { parseError }
%monad { Either String }

%token
    '+'   { (_, '+') }
    '-'   { (_, '-') }
    '<'   { (_, '<') }
    '>'   { (_, '>') }
    '.'   { (_, '.') }
    ','   { (_, ',') }
    '['   { (_, '[') }
    ']'   { (_, ']') }

%% 

prog : prog_   { reverse $1 }

prog_ : {- empty -} { [] }
      | prog_ symb  { $2:$1 }

symb : '+'          { Incr }
     | '-'          { Decr }
     | '<'          { Left' }
     | '>'          { Right' }
     | '.'          { Output }
     | ','          { Input }
     | '[' prog ']' { Loop $2 }

{
parseError :: [((Int, Int), Char)] -> Either String a
parseError [] = Left "parse error at eof"
parseError (((lNo, cNo), x):xs) =
  Left $ show lNo ++ ":" ++ show cNo ++ ": parse error at '" ++ [x] ++ "'"

parse :: String -> Either String Program
parse = parse_ . lex_

lex_ :: String -> [((Int, Int), Char)]
lex_ =
  concat .
  zipWith
    ((filter ((`elem` "+-><.,[]") . snd) .) . (`zipWith` [1 ..]) . ((,) .) . (,))
    [1 ..] .
  lines
}
