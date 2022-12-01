{-# LANGUAGE NamedFieldPuns #-}

module BrainFuck
  ( Program
  , Instruction(..)
  , Cells(..)
  , initCells
  , moveRight
  , moveLeft
  , incr
  , decr
  , setCurrent
  , fix
  ) where

type Program = [Instruction]

data Instruction
  = Incr
  | Decr
  | Left'
  | Right'
  | Output
  | Input
  | Loop [Instruction]
  deriving (Eq, Ord, Show)

data Cells =
  Cells
    { left :: [Int]
    , current :: Int
    , right :: [Int]
    }
  deriving (Show)

initCells :: Maybe Int -> Cells
initCells (Just len) = Cells [] 0 (replicate (len - 1) 0)
initCells Nothing = Cells zeroes 0 zeroes
  where
    zeroes = 0 : zeroes

moveRight :: Cells -> Cells
moveRight cells@(Cells _ _ []) = cells
moveRight (Cells ls c (r:rs)) = Cells (c : ls) r rs

moveLeft :: Cells -> Cells
moveLeft cells@(Cells [] _ _) = cells
moveLeft (Cells (l:ls) c rs) = Cells ls l (c : rs)

incr :: Cells -> Cells
incr cells@Cells {current} = cells {current = current + 1}

decr :: Cells -> Cells
decr cells@Cells {current} = cells {current = current - 1}

setCurrent :: Int -> Cells -> Cells
setCurrent c cells = cells {current = fix c}

fix :: Int -> Int
fix x = ((x `mod` 256) + 256) `mod` 256
