{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module SiegeGame where
import Game
import Data.Map (Map)
import qualified Data.Map as M

type V = (Int,Int)

data VertexInfo = VertexInfo {
  isGoal        :: Bool,
  initialPiece  :: Maybe Player,
  neighbors     :: [NeighborInfo]
} deriving Show

data NeighborInfo = NeighborInfo {
  getNeighbor       :: V,
  isAttackerAllowed :: Bool
} deriving Show

data SiegeGame = SiegeGame {
  getBoard :: Map V VertexInfo,
  getScore :: Map V Player -> Double
}

data SiegeMove = PlaceDefenders [V]
               | SimpleMove V V
               | CaptureMove V [V]
                 deriving (Show,Read,Eq,Ord)

inV :: V -> Bool
inV (x,y) = (abs x <= 3 && abs y <= 1) || (abs x <= 1 && abs y <= 3)

inG :: V -> Bool
inG (x,y) = y >= 1 && abs x <= 1

adjacent :: V -> [V]
adjacent (x,y) = [ (x+i,y+j) | i <- is, j <- is, (i,j) /= (0,0) ] where is = [-1..1]

edgeTo :: V -> V -> Bool
(x,y) `edgeTo` (x',y') = x==x' || y==y' || even (x+y)

attackerEdgeTo :: V -> V -> Bool
(x,y) `attackerEdgeTo` (x',y') = (y == y' && x > 1  && y == 1 && (x'-x) == -1) ||
                                 (y == y' && x < -1 && y == 1 && (x'-x) == 1) ||
                                 (y-y') == -1

getVertexInfo :: V -> VertexInfo
getVertexInfo v = VertexInfo {
    isGoal        = inG v,
    initialPiece  = if inG v then Nothing else Just True,
    neighbors     = [ NeighborInfo v' (v `attackerEdgeTo` v') | v' <- adjacent v, inV v', v `edgeTo` v' ]
  }

defaultGame :: SiegeGame
defaultGame = SiegeGame g (const 0)
  where
    vs = [ (v, getVertexInfo v) | x <- is, y <- is, let v = (x,y), inV v]
    is = [-3..3]
    g = M.fromList vs
