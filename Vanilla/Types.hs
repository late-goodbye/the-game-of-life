module Types where

  type Node = (Int, Int)
  type State = (Bool, Bool)
  type Cell = (Node, State)
  type Universe = [Cell]
  type Multiverse = [Universe]
