module Main where

import Agent
import Grid
import Types
import Draw

import Data.Maybe (catMaybes)

-- | The next round
next :: Grid Cell -- ^ old grid
     -> Grid Cell -- ^ new grid
next g = step unsatisfieds emptys g
   where ns = neighbourhoods g
         unsatisfieds = fmap nRoot $ filter unsatisfiedA agents
         agents = catMaybes $ fmap neighbourhoodC2A ns
         emptys = catMaybes $ fmap neighbourhoodC2I ns

-- | Recursively moves unsatisfied agents to empty cells.
step :: [Agent]             -- ^ unsatisfieds
      -> [NeighbourhoodI]   -- ^ empty lots
      -> Grid Cell          -- ^ old grid
      -> Grid Cell          -- ^ new grid
step u e g = case u of 
        (a:as) ->
           case options a of
               (o:_os) -> -- move to the first desirable empty cell
                  step as (filter (/= o) e) (replace a (nRoot o) g)
               [] -> -- do not move
                  step as e g
        [] -> g
    where options a = filter (not . unsatisfied (agentGroup a) (agentThreshold a) . nNeighbours) e

agentX :: Int -> Agent
agentX i = Agent i 1 0.90

agentY :: Int -> Agent
agentY i = Agent i 2 0.10

charToAgent :: Char -> State Int (Maybe Agent)
charToAgent 'X' = State (\i -> (Just (agentX i), i+1))
charToAgent 'O' = State (\i -> (Just (agentY i), i+1))
charToAgent ' ' = pure Nothing
charToAgent c = error (c : " is not a valid character")

setupGrid :: Grid (Maybe Agent) -> Grid Cell
setupGrid g = evalState (traverse f g) 0
    where f :: Maybe Agent -> State Int Cell
          f (Just agent) = pure (Full agent)
          f Nothing = State (\i -> (Empty i, i+1))

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p = foldr (\x xs -> if p x then x:xs else [x]) []

main :: IO ()
main = do
    m <- readFile("data/data.txt")
    let g = setupGrid $ Grid $ evalState (traverse (traverse (charToAgent)) (lines m)) 0
        filenames = fmap (\x -> "test" <> show x) [1..]
        grids = takeWhile1 (\x -> score x < 1) (iterate next g)
    sequence_ (zipWith save filenames grids)
