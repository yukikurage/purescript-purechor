module Choreography.Location where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

type LocTy = Symbol

type LocTm = String

toLocTm :: forall (@l :: LocTy). IsSymbol l => LocTm
toLocTm = reflectSymbol @l Proxy

data Located a (l :: LocTy)
  = Wrap a
  | Empty

infix 4 type Located as #

wrap :: forall a @l. a -> a # l
wrap = Wrap

unsafeUnwrap :: forall a @l. a # l -> a
unsafeUnwrap (Wrap a) = a
unsafeUnwrap Empty = unsafeCrashWith "this should never happen for a well-typed choreography"
