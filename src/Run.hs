{-# LANGUAGE LambdaCase #-}

module Run
  ( run
  ) where

import BrainFuck

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char

run :: Program -> IO ()
run = (`evalStateT` initCells Nothing) . go
  where
    cont f xs = f >> go xs
    go [] = return ()
    go (Incr:rest) = modify incr `cont` rest
    go (Decr:rest) = modify decr `cont` rest
    go (Left':rest) = modify moveLeft `cont` rest
    go (Right':rest) = modify moveRight `cont` rest
    go (Input:rest) = (modify . setCurrent . ord =<< liftIO getChar) `cont` rest
    go (Output:rest) =
      (liftIO . putStr . (:[]) . chr . fix =<< gets current) `cont` rest
    go loop@(Loop body:rest) = do
      gets (fix . current) >>=
        (\case
           0 -> go rest
           _ -> go (body ++ loop))
