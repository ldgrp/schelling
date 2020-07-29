module Grid where

import Agent

import Data.List (sort, transpose, groupBy)
import Data.Tuple (swap)
import Data.Maybe (catMaybes)


data Grid a = Grid 
   { unGrid :: [[a]]  -- ^ a 2-dimensional grid
   }

instance Show a => Show (Grid a) where
    show (Grid g) = unlines (fmap show g)

instance Functor Grid where
    fmap f (Grid g) = Grid (fmap (fmap f) g)

instance Foldable Grid where
    foldMap f (Grid g) = foldMap (foldMap f) g

instance Traversable Grid where
    traverse f (Grid g) = Grid <$> traverse (traverse f) g

-- | An n x m grid of a's
grid :: Int -> Int -> a -> Grid a
grid n m = Grid . replicate n . replicate m

-- | An n x m grid of Nothings
emptyGrid :: Int -> Int -> Grid (Maybe a)
emptyGrid n m = grid n m Nothing


-- | A Cell in a Grid.
data Cell
    = Empty Int  -- ^ An empty cell with a unique integer
    | Full Agent -- ^ A cell occupied by an agent
    deriving (Eq, Ord)

data Neighbourhood a b = Neighbourhood 
   { nRoot :: a             -- ^ root
   , nNeighbours :: [b]     -- ^ neighbours
   } deriving (Eq, Ord, Show)

-- | A neighbourhood of cells
type NeighbourhoodC = Neighbourhood Cell Cell

-- | An agent and its neighbouring agents
type NeighbourhoodA = Neighbourhood Agent Agent

-- | An empty space and its neighbouring agents
type NeighbourhoodI = Neighbourhood Int Agent

-- | Cast from NeighbourhoodC to NeighbourhoodA
neighbourhoodC2A :: NeighbourhoodC -> Maybe (NeighbourhoodA)
neighbourhoodC2A (Neighbourhood (Full agent) cs) = Just $ Neighbourhood agent [asi | Full asi <- cs]
neighbourhoodC2A _ = Nothing

-- | Cast from NeighbourhoodC to NeighbourhoodI
neighbourhoodC2I :: NeighbourhoodC -> Maybe (NeighbourhoodI)
neighbourhoodC2I (Neighbourhood (Empty i) cs) = Just $ Neighbourhood i [asi | Full asi <- cs]
neighbourhoodC2I _ = Nothing

-- | List of right neighbours of a 1-dimensional list
right1D :: [a] -> [(a, a)]
right1D (x1:x2:xs) = (x1, x2) : right1D (x2:xs)
right1D _ = []

-- | List of left neighbours of a 1-dimensional list
left1D :: [a] -> [(a, a)]
left1D = fmap swap . right1D

-- | List of right neighbours of a 2-dimensional list
right :: [[a]] -> [(a, a)]
right = (>>= right1D)

-- | List of left neighbours of a 2-dimensional list
left :: [[a]] -> [(a, a)]
left = (>>= left1D)

-- | List of down neighbours of a 2-dimensional list
down :: [[a]] -> [(a, a)]
down = right . transpose

-- | List of up neighbours of a 2-dimensional list
up :: [[a]] -> [(a, a)]
up = fmap swap . down

-- | List of (diagonal) downRight neighbours of a 2-dimensional list
downRight :: [[a]] -> [(a, a)]
downRight (x:y:rest) = zip x (drop 1 y) ++ downRight (y:rest)
downRight _ = []

-- | List of (diagonal) upLeft neighbours of a 2-dimensional list
upLeft :: [[a]] -> [(a, a)]
upLeft = fmap swap . downRight

-- | List of (diagonal) upRight neighbours of a 2-dimensional list
upRight :: [[a]] -> [(a, a)]
upRight = downRight . reverse

-- | List of (diagonal) downLeft neighbours of a 2-dimensional list
downLeft :: [[a]] -> [(a, a)]
downLeft = fmap swap . upRight

-- | List of neighbours of an 8-connected grid
neighbours :: Ord a => [[a]] -> [(a, [a])]
neighbours = catMaybes . ((f <$>) unzip <$>) . groupFirst . sort . tupled
    where tupled = concat . ([up, down, left, right, downRight, upLeft, downLeft, upRight] <*>) . pure
          groupFirst = groupBy (\x y -> fst x == fst y)
          f (x:xs, y) = Just (x, y)
          f ([], y) = Nothing

-- | Neighbourhoods in an 8-connected grid
neighbourhoods :: Grid Cell -> [NeighbourhoodC]
neighbourhoods = fmap f . neighbours . unGrid
    where f (x, y) = Neighbourhood x y

-- | Swap the location of an agent and an empty cell
replace :: Agent     -- ^ from
        -> Int       -- ^ to
        -> Grid Cell -- ^ old grid
        -> Grid Cell -- ^ new grid
replace a i g = fmap replace' g
    where replace' (Full f)
            | f == a = Empty i
          replace' (Empty e) 
            | e == i = Full a
          replace' o  = o

-- | Check if an agent in a neighbourhood is unsatisfied
unsatisfiedA :: NeighbourhoodA -> Bool
unsatisfiedA (Neighbourhood (Agent _id group threshold) as) =
    unsatisfied group threshold as


-- | Percent of satisfied agents
score :: Grid Cell -> Float
score g = 1 - listRatio unsatisfieds agents
   where ns = neighbourhoods g
         unsatisfieds = filter unsatisfiedA agents
         agents = catMaybes $ fmap neighbourhoodC2A ns

listRatio :: [a] -> [b] -> Float
listRatio a b = fromIntegral (length a) / fromIntegral (length b)

