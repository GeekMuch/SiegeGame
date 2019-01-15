{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module SiegeGameImpl where
import Game
import SiegeGame
import SiegeGameScore1

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List (tails, partition)

data ScoreAlg = Score0 | Score1 deriving (Show,Read)
getGame :: ScoreAlg -> SiegeGame
getGame Score0 = defaultGame
getGame Score1 = case defaultGame of SiegeGame g _ -> SiegeGame g (score1 g)


-- Task 1.1)
placeDefenders :: [V] -> Map V Player -> Map V Player
placeDefenders [] s = s
placeDefenders vs s = M.insert (head vs) False 
  (placeDefenders (tail vs) s)

-- Task 1.2)
simpleMove     :: V -> V -> Map V Player -> Map V Player
simpleMove v v' s = if (fromJust $ M.lookup v s) == False 
  then M.insert v' (fromJust $ M.lookup v s)
  (M.delete v s)
  else if  (fromJust $ M.lookup v s) == True 
  then M.insert v' (fromJust $ M.lookup v s)
    (M.delete v s)   
    else s

-- Task 1.3)

captureMove   :: V -> [V] -> Map V Player -> Map V Player
captureMove v [] s = s
captureMove v (x:xs) s =
   case M.lookup x s of
   Nothing -> s
   Just q -> 
     case M.lookup v s of
     Nothing -> s
     Just w | not(w) -> 
       case M.lookup (hapsMove v x) s of
       Nothing -> captureMove (hapsMove v x) xs (M.delete x (simpleMove v (hapsMove v x) s))
       Just e -> s
          | otherwise -> s

--This function is good for reducing the task at hand, this handles the position moving over the attacker.
hapsMove :: V -> V -> V
hapsMove (x,y) (x',y') =  (2 * x' - x, 2 * y' - y)

-- Task 1.4)
moveImpl :: SiegeMove -> Map V Player -> Map V Player
moveImpl (PlaceDefenders vs) s = placeDefenders vs s
moveImpl (SimpleMove v v') s = simpleMove v v' s
moveImpl (CaptureMove v v') s = captureMove v v' s



-- Task 1.5)

startStateImpl :: SiegeGame -> Map V Player
startStateImpl (SiegeGame g _) =  unity g (M.fromList[])

--This functions helps placing the defenders, recursively.
unity :: Map V VertexInfo -> Map V Player -> Map V Player 
unity v q = 
  case M.toList v of
    []-> q
    _ ->  case initialPiece(snd (M.elemAt 0 v)) of
       Nothing -> unity ( M.fromList( tail( M.toList((v))))) q
       Just player -> unity ( M.fromList( tail( M.toList(v)))) ( M.insert ( fst( M.elemAt 0 v)) player q)


-- Task 1.6)

placeDefenderMoves :: SiegeGame -> [SiegeMove]
placeDefenderMoves (SiegeGame g _) = pairs(goalPosts g [])

--This functions helps placing the defenders, recursively.
pairs :: [V] -> [SiegeMove]
pairs xs = [(PlaceDefenders[x1, x2]) | (x1:xs1) <- tails xs, x2 <- xs1]

--This functions is an aid for the determination of placements for placeDefenderMoves.
goalPosts:: Map V VertexInfo -> [V] -> [V]
goalPosts v q =
  case M.toList v of
    []-> q
    _ ->  case initialPiece(snd (M.elemAt 0 v)) of
       Nothing -> goalPosts (M.fromList( tail( M.toList(v)))) (q ++ [(fst(M.elemAt 0 v))])
       Just player -> goalPosts (M.fromList(tail(M.toList((v))))) q 


-- Task 1.7)

simpleMoves :: SiegeGame -> Player -> Map V Player -> [SiegeMove]
simpleMoves (SiegeGame g _) p s = legalMoves p g s (hostedVertice p s)  


-- This function will get all the hosted veracities  
hostedVertice :: Player -> Map V Player -> [V]
hostedVertice p s = [v | (v, x) <- M.toList s, x == p]

-- This function will check for legals moves by using SimpleMove and getNeighbor as
-- instructed.
legalMoves:: Player -> Map V VertexInfo -> Map V Player -> [V] -> [SiegeMove]
legalMoves p g s vs = case (p) of
    False -> [ SimpleMove v (getNeighbor x)
            | v <- vs, x <- neighbors (fromJust (M.lookup v g)), M.notMember (getNeighbor x) s]
    True -> [ SimpleMove v (getNeighbor x)
            | v <- vs, x <- neighbors (fromJust (M.lookup v g)), M.notMember (getNeighbor x) s, isAttackerAllowed x]


-- Task 1.8)

defenderCaptureMoves :: SiegeGame -> Map V Player -> [SiegeMove]
defenderCaptureMoves (SiegeGame g _) s = [ CaptureMove v l
                                        | v <- getDefenders, l <- eatATT v s]
  where
    getDefenders  =  [x | (x, False) <- M.toList s]

    -- Handling empty list 
    xList :: [[V]] -> [[V]]
    xList [] = [[]]
    xList l = l

    eatATT :: V -> Map V Player -> [[V]]
    eatATT v s' = [ x:xs
                      | x <- findNeighbor v, let v' = hapsMove v x, validFood v x s', xs <- xList (eatATT v' (captureMove v [x] s'))]

    findNeighbor :: V -> [V]
    findNeighbor v = [ getNeighbor x | x <- neighbors (fromJust (M.lookup v g))]

    validFood :: V -> V -> Map V Player -> Bool
    validFood v v' s' = M.notMember (hapsMove v v') s' && M.member v' s' && s' M.! v' && inV (hapsMove v v') 


-- Task 1.9)

showGameImpl :: SiegeGame -> Map V Player -> String
showGameImpl (SiegeGame g _) m = undefined

---

defenderMoves :: SiegeGame -> Map V Player -> [SiegeMove]
defenderMoves sg s = case [v | (v, False) <- M.toList s] of
  [] -> placeDefenderMoves sg
  _  -> case defenderCaptureMoves sg s of
    [] -> simpleMoves sg False s
    xs -> xs

movesImpl :: SiegeGame -> Player -> Map V Player -> [SiegeMove]
movesImpl sg True s    = simpleMoves sg True s
movesImpl sg False s   = defenderMoves sg s

valueImpl :: SiegeGame -> Player -> Map V Player -> Double
valueImpl sg p m | null $ movesImpl sg p m = if p then -infinity else infinity
valueImpl (SiegeGame g _) _ m | and [ M.lookup v m == Just True | (v,vi) <- M.toList g, isGoal vi ] = infinity
valueImpl (SiegeGame g s) p m = s m


instance Game SiegeGame where
  type GameState SiegeGame  = Map V Player
  type Move SiegeGame       = SiegeMove

  startState    = startStateImpl
  showGame      = showGameImpl
  move _ _ s m  = moveImpl m s
  moves         = movesImpl
  value         = valueImpl
