{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module Game where

type Player = Bool
type Value = Double

class (Show (Move g), Show (GameState g)) => Game g where
  type Move g
  type GameState g

  startState :: g -> GameState g
  value :: g -> Player -> GameState g -> Value
  moves :: g -> Player -> GameState g -> [Move g]
  move :: g -> Player -> GameState g -> Move g -> GameState g
  showGame :: g -> GameState g -> String
