module Agent where

type AgentGroup = Int

data Agent = Agent 
    { agentId :: Int            -- ^ unique identifier
    , agentGroup :: AgentGroup  -- ^ group
    , agentThreshold :: Float   -- ^ similiarity threshold
    } deriving (Eq, Ord)

instance Show Agent where
    show = show . agentGroup

-- | An agent is satisfied if the ratio of neighbours and neighbours
--   matching its group is greater than or equal to the threshold.
unsatisfied :: AgentGroup  -- ^ group
            -> Float       -- ^ threshold
            -> [Agent]     -- ^ neighbours
            -> Bool
unsatisfied group threshold as = ratio < threshold
    where num = length $ filter ((group ==) . agentGroup) as
          den = length as
          ratio = fromIntegral num / fromIntegral den
