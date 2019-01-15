module SiegeGameAI where
import Data.Map (Map)
import SiegeGame
import SiegeGameImpl
import Game
import GameStrategies
import Text.Read
import System.Random

data TreeAlg = Minimax | AlphaBeta deriving (Show,Read)
type Alg = (TreeAlg, ScoreAlg, Int)

uniformly res gen = case length res of
  0 -> Nothing
  1 -> Just $ head res
  _ -> Just $ res !! i where (i,_) = randomR (0, length res - 1) gen

fromBoard = const id

getNextMove :: String -> Map V Player -> Player -> IO (Maybe SiegeMove)
getNextMove conf s p = case (readMaybe conf :: Maybe Alg) of
  Just (Minimax, sa, d)    -> return $ fst $ minimax g $ takeTree d $ tree g (p,fromBoard g s)
    where g = getGame sa :: SiegeGame
  Just (AlphaBeta, sa, d)  -> return $ fst $ minimaxAlphaBeta g (-1.0/0.0, 1.0/0.0) $ takeTree d $ tree g (p,fromBoard g s)
    where g = getGame sa :: SiegeGame
  _ -> do
    print $ "Could not parse " ++ conf
    return Nothing
