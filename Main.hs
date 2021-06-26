{-# LANGUAGE ConstraintKinds #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Map (Map(), toList, fromListWith)
import Data.List (groupBy)
import Data.Sort (sortOn)
import Data.Tuple (fst)
import Data.Maybe (fromMaybe)


-- Model State
type StateVariableKey = String
type StateVariableValue = Float
type StateVariable = (StateVariableKey, StateVariableValue)
type State = Map StateVariableKey StateVariableValue
type StateHistory = [State]

-- Model Policies
type PolicySignalKey = String
type PolicySignalValue = Float
type PolicySignal = (PolicySignalKey, PolicySignalValue)

-- Model Functions
type StateUpdateFunction = State -> [PolicySignal] -> StateVariable
type PolicyFunction = State -> [PolicySignal]

-- Model Structure
data PartialStateUpdateBlock = PartialStateUpdateBlock { policies :: [PolicyFunction], variables :: [StateUpdateFunction] }
type Model = [PartialStateUpdateBlock]

-- Model Execution
partialStateUpdate :: State -> PartialStateUpdateBlock -> State
partialStateUpdate s psub = updateState s $ applyPartialStateUpdate s
    where
        applyPartialStateUpdate :: State -> StateVariable
        applyPartialStateUpdate s = stateUpdateFunction s $ policyFunction s
            where
                policyFunction = head $ policies psub
                stateUpdateFunction = head $ variables psub
        updateState :: State -> StateVariable -> State
        updateState s v = Map.insert (fst v) (snd v) s

policySignalAggregation :: Num b => [(PolicySignalKey, b)] -> [(PolicySignalKey, b)]
policySignalAggregation = map sumGroup . groupBy fstEq . sortOn fst
  where
    sumGroup (x:xs) = (fst x, sum $ map snd (x:xs))
    sumGroup _ = error "This can never happen - groupBy cannot return empty groups"
    fstEq (a, _) (b, _) = a == b

simulation :: Model -> State -> StateHistory
simulation m initialState = take 10 (iterate (\s -> partialStateUpdate s (head m)) initialState)

-- Model Configuration
stateUpdateFunction :: StateUpdateFunction
stateUpdateFunction s _ = (\v -> ("a", v)) $ (+) 1 $ fromMaybe 0 (Map.lookup "a" s)

policyFunction :: PolicyFunction
policyFunction _ = [("signal", 1)]

partialStateUpdateBlock :: PartialStateUpdateBlock
partialStateUpdateBlock = PartialStateUpdateBlock [policyFunction] [stateUpdateFunction]

model = [partialStateUpdateBlock, partialStateUpdateBlock]

initialState :: State
initialState = Map.fromList [("a", 0), ("b", 0)]

-- Simulation
results = simulation model initialState

main :: IO ()
main = print $ results
