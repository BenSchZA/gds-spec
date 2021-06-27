# Generalized Dynamical Systems Spec

This is an (informal) technical specification for Generalized Dynamical Systems.

See [Generalized Dynamical Systems](https://hackmd.io/sHhp-CoUTf2SeZy6vJ8EcA?view) and [Semantic Disambiguation and Term Standardization for Generalized Dynamical Systems (GDS)](https://hackmd.io/4gxtBCKWRKSUHPUNa9PVJA) for inspiration.

The goals of this codebase are to:
* develop a foundation for building GDS reference implementations
* standardize the terminology used to refer to the structure of a GDS model

This README.md file is the spec, and is testable and executable!

```bash
make run
make test
```

## Imports
```haskell
module Main where

import qualified Data.Map.Strict as Map
import Data.Map (Map(), toList, fromListWith)
import Data.List (groupBy)
import Data.Sort (sortOn)
import Data.Tuple (fst)
import Data.Maybe (fromMaybe)
```

## Types

### Model State
```haskell
type StateVariableKey = String
type StateVariableValue = Float
type StateVariable = (StateVariableKey, StateVariableValue)
type State = Map StateVariableKey StateVariableValue
type StateHistory = [State]
```

### Model Policies
```haskell
type PolicySignalKey = String
type PolicySignalValue = Float
type PolicySignal = (PolicySignalKey, PolicySignalValue)
```

### Model Functions
```haskell
type PolicyFunction = State -> [PolicySignal]
type StateUpdateFunction = State -> [PolicySignal] -> StateVariable
```

### Model Structure
```haskell
data PartialStateUpdateBlock = PartialStateUpdateBlock { policies :: [PolicyFunction], variables :: [StateUpdateFunction] }
type Model = [PartialStateUpdateBlock]
```

## Simulation Logic
```haskell
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
```

## Model Configuration
```haskell
stateUpdateFunction :: StateUpdateFunction
stateUpdateFunction s _ = (\v -> ("a", v)) $ (+) 1 $ fromMaybe 0 (Map.lookup "a" s)

policyFunction :: PolicyFunction
policyFunction _ = [("signal", 1)]

partialStateUpdateBlock :: PartialStateUpdateBlock
partialStateUpdateBlock = PartialStateUpdateBlock [policyFunction] [stateUpdateFunction]

model = [partialStateUpdateBlock, partialStateUpdateBlock]

initialState :: State
initialState = Map.fromList [("a", 0), ("b", 0)]
```

## Simulation Execution
```haskell
results = simulation model initialState

main :: IO ()
main = print $ results
```

## Simulation Results
```haskell ignore
[fromList [("a",0.0),("b",0.0)],fromList [("a",1.0),("b",0.0)],fromList [("a",2.0),("b",0.0)],fromList [("a",3.0),("b",0.0)],fromList [("a",4.0),("b",0.0)],fromList [("a",5.0),("b",0.0)],fromList [("a",6.0),("b",0.0)],fromList [("a",7.0),("b",0.0)],fromList [("a",8.0),("b",0.0)],fromList [("a",9.0),("b",0.0)]]
```
