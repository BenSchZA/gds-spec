# Generalized Dynamical Systems Spec

This is an (informal) technical specification for Generalized Dynamical Systems.

See [Generalized Dynamical Systems](https://hackmd.io/sHhp-CoUTf2SeZy6vJ8EcA?view) and [Semantic Disambiguation and Term Standardization for Generalized Dynamical Systems (GDS)](https://hackmd.io/4gxtBCKWRKSUHPUNa9PVJA) for inspiration.

The goals of this codebase are to:
* develop a foundation for building GDS reference implementations
* standardize the terminology used to refer to the structure of a GDS model

An extract from [Main.hs](Main.hs) using Type Driven Development:
```Haskell
type StateVariableKey = String
type StateVariableValue = Float
type StateVariable = (StateVariableKey, StateVariableValue)
type State = Map StateVariableKey StateVariableValue
type StateHistory = [State]

type StateUpdateFunction = State -> [PolicySignal] -> StateVariable
type PolicyFunction = State -> [PolicySignal]

data PartialStateUpdateBlock = PartialStateUpdateBlock { policies :: [PolicyFunction], variables :: [StateUpdateFunction] }
type Model = [PartialStateUpdateBlock]
```
