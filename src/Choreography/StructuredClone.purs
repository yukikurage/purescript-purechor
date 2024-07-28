module Choreography.Structured where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Foreign (Foreign, readArray, readBoolean, readInt, readNumber, readString)
import Foreign.Index (readProp)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (RowList)
import Prim.RowList as RL
import Record as R
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | 構造化複製ができる型を表す制約
class Structured :: Type -> Constraint
class Structured a where
  fromForeign :: Foreign -> Maybe a

instance Structured Foreign where
  fromForeign = Just

instance Structured Int where
  fromForeign = readInt >>> runExcept >>> hush

instance Structured String where
  fromForeign = readString >>> runExcept >>> hush

instance Structured Number where
  fromForeign = readNumber >>> runExcept >>> hush

instance Structured Boolean where
  fromForeign = readBoolean >>> runExcept >>> hush

instance Structured Unit where
  fromForeign _ = Just unit

instance Structured a => Structured (Array a) where
  fromForeign xs = do
    fs <- (readArray >>> runExcept >>> hush) xs
    traverse fromForeign fs

instance (RL.RowToList r rl, StructuredRecord rl r) => Structured (Record r) where
  fromForeign f = fromForeignRecord @rl f

class StructuredRecord :: RowList Type -> Row Type -> Constraint
class StructuredRecord rl r | rl -> r where
  fromForeignRecord :: Foreign -> Maybe (Record r)

instance StructuredRecord RL.Nil () where
  fromForeignRecord _ = Just {}

instance
  ( Structured a
  , IsSymbol l
  , StructuredRecord rl r
  , Cons l a r r1
  , Lacks l r
  ) =>
  StructuredRecord (RL.Cons l a rl) r1 where
  fromForeignRecord f = do
    recRest <- fromForeignRecord @rl f
    af <- hush $ runExcept $ readProp (reflectSymbol @l Proxy) f
    a <- fromForeign af

    pure $ R.insert (Proxy :: Proxy l) a recRest

toForeign :: forall @a. Structured a => a -> Foreign
toForeign = unsafeCoerce
