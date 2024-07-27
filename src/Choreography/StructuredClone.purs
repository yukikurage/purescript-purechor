module Choreography.Structured where

import Prelude

import Prim.Row (class Cons)
import Prim.RowList (RowList)
import Prim.RowList as RL

-- | 構造化複製ができる型を表す制約
class Structured :: Type -> Constraint
class Structured a

instance Structured Int
instance Structured String
instance Structured Number
instance Structured Boolean
instance Structured Unit
instance Structured a => Structured (Array a)
instance (RL.RowToList r rl, StructuredRecord rl r) => Structured (Record r)

class StructuredRecord :: RowList Type -> Row Type -> Constraint
class StructuredRecord rl r | rl -> r

instance StructuredRecord RL.Nil ()

instance (Structured a, StructuredRecord rl r, Cons l a r r1) => StructuredRecord (RL.Cons l a rl) r1
