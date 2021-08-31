module Halogen.IHooks
  ( IndexedHookM
  , hookCons
  , component
  , getHooksM
  , lift
  , HookAction
  , HookM
  , HookArg
  , asHooks
  , setHookMCons
  , setHookMUnion
  , getHookCons
  , setHookCons
  , setHookUnion
  , Hooks
  , defaultOptions
  , Options
  , ReadOnly(..)
  , class NotReadOnly
  , class NotReadOnlyRL
  , HookHTML
  ) where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, iapply, ipure)
import Control.Apply (applySecond)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Indexed (class IxMonad, iap)
import Data.Functor.Indexed (class IxFunctor)
import Data.Lens (over, set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type HookState hooks input slots output m
  =
  { hooks :: Hooks hooks
  , input :: input
  , html :: HookHTML hooks input slots output m
  }

type HookM hooks input slots output m a
  = H.HalogenM
  (HookState hooks input slots output m)
  (HookAction hooks input slots output m)
  slots
  output
  m
  a

p_ :: { hooks :: Proxy "hooks", input :: Proxy "input", html :: Proxy "html" }
p_ = { hooks: Proxy, input: Proxy, html: Proxy }

newtype ReadOnly a
  = ReadOnly a

derive instance newtypeReadOnly :: Newtype (ReadOnly a) _

derive instance functorReadOnly :: Functor ReadOnly

data Hooks (r :: Row Type)

foreign import getHookConsFFI :: forall a r. Maybe a -> (a -> Maybe a) -> String -> Hooks r -> Maybe a

getHookCons
  :: forall proxy sym a r1 hooks
   . IsSymbol sym
  => Cons sym a r1 hooks
  => proxy sym
  -> Hooks hooks
  -> Maybe a
getHookCons = getHookConsFFI Nothing Just <<< reflectSymbol

foreign import setHookConsFFI :: forall a r. String -> a -> Hooks r -> Hooks r

setHookCons
  :: forall proxy sym a r1 hooks
   . IsSymbol sym
  => Cons sym a r1 hooks
  => proxy sym
  -> a
  -> Hooks hooks
  -> Hooks hooks
setHookCons = setHookConsFFI <<< reflectSymbol

data SetHookUnion

foreign import setHookUnionFFI :: forall r. SetHookUnion -> Hooks r -> Hooks r

setHookUnion
  :: forall r1 r2 hooks
   . Union r1 r2 hooks
  => { | r1 }
  -> Hooks hooks
  -> Hooks hooks
setHookUnion = setHookUnionFFI <<< unsafeCoerce

setHookMCons
  :: forall proxy output input slots m sym a r1 hooks
   . NotReadOnly a
  => Cons sym a r1 hooks
  => IsSymbol sym
  => proxy sym
  -> a
  -> HookM hooks input slots output m Unit
setHookMCons px = H.modify_ <<< over (prop p_.hooks) <<< setHookCons px

class NotReadOnlyRL (rl :: RL.RowList Type)

instance notReadOnlyRLNil :: NotReadOnlyRL RL.Nil

instance notReadOnlyRLCons :: (NotReadOnly b, NotReadOnlyRL c) => NotReadOnlyRL (RL.Cons a b c)

setHookMUnion
  :: forall output input slots m r1 rl r2 hooks
   . RL.RowToList r1 rl
  => NotReadOnlyRL rl
  => Union r1 r2 hooks
  => { | r1 }
  -> HookM hooks input slots output m Unit
setHookMUnion = H.modify_ <<< over (prop p_.hooks) <<< setHookUnion

asHooks :: forall r. { | r } -> Hooks r
asHooks = unsafeCoerce

getHooksM
  :: forall hooks input slots output m
   . HookM hooks input slots output m (Hooks hooks)
getHooksM = H.gets _.hooks

class NotReadOnly (a :: Type)

instance readOnlyFail :: Fail (Text "This value is read only") => NotReadOnly (ReadOnly a)
else instance readOnlySucceed :: NotReadOnly a

type HookHTML hooks input slots output m
  = HC.HTML (H.ComponentSlot slots m (HookAction hooks input slots output m)) (HookAction hooks input slots output m)

type HookArg hooks input slots output m
  =
  input
  -> IndexedHookM hooks input slots output m () hooks (HookHTML hooks input slots output m)

handleHookAction
  :: forall hooks input slots output m
   . HookArg hooks input slots output m
  -> HookAction hooks input slots output m
  -> H.HalogenM
       (HookState hooks input slots output m)
       (HookAction hooks input slots output m)
       slots
       output
       m
       Unit
handleHookAction f = flip applySecond H.get
    >=> unIx <<< f <<< _.input
    >=> H.modify_ <<< set (prop p_.html)

type Options query hooks input slots output m
  =
  { receiveInput :: Boolean
  , handleQuery :: forall a. query a -> HookM hooks input slots output m (Maybe a)
  , finalize :: HookM hooks input slots output m Unit
  , initialHTML :: HookHTML hooks input slots output m
  }

defaultOptions
  :: forall query hooks input slots output m
   . Options query hooks input slots output m
defaultOptions =
  { receiveInput: false
  , handleQuery: const (pure Nothing)
  , finalize: pure unit
  , initialHTML: HH.div [] []
  }

component
  :: forall slots hooks query input output m
   . Options query hooks input slots output m
  -> HookArg hooks input slots output m
  -> H.Component query input output m
component options f =
  H.mkComponent
    { initialState: { input: _, hooks: unsafeCoerce {}, html: options.initialHTML }
    , render: _.html
    , eval:
        H.mkEval
          { initialize: Just mempty
          , finalize: Just options.finalize
          , receive: if options.receiveInput then Just <<< H.modify_ <<< set (prop p_.input) else const Nothing
          , handleAction: handleHookAction f
          , handleQuery: options.handleQuery
          }
    }

newtype IndexedHookM hooks input slot output m (i :: Row Type) (o :: Row Type) a
  = IndexedHookM (HookM hooks input slots output m a)

unIx :: forall hooks input slots output m i o a. IndexedHookM hooks input slots output m i o a -> HookM hooks input slots output m a
unIx (IndexedHookM m) = m

derive instance indexedHookMFunctor :: Functor (IndexedHookM hooks input slots output m i i)

derive newtype instance indexedHookMSemigroup :: Semigroup a => Semigroup (IndexedHookM hooks input slots output m i i a)
derive newtype instance indexedHookMMonoid :: Monoid a => Monoid (IndexedHookM hooks input slots output m i i a)

instance indexedHookMApply :: Apply (IndexedHookM hooks input slots output m i i) where
  apply = iapply

instance indexedHookMBind :: Bind (IndexedHookM hooks input slots output m i i) where
  bind = ibind

instance indexedHookMApplicative :: Applicative (IndexedHookM hooks input slots output m i i) where
  pure = ipure

instance indexedHookMMonad :: Monad (IndexedHookM hooks input slots output m i i)

instance indexedHookMIxFunctor :: IxFunctor (IndexedHookM hooks input slots output m) where
  imap f = IndexedHookM <<< map f <<< unIx

instance indexedHookMIxApply :: IxApply (IndexedHookM hooks input slots output m) where
  iapply = iap

instance indexedHookMIxApplicative :: IxApplicative (IndexedHookM hooks input slots output m) where
  ipure = IndexedHookM <<< pure

instance indexedHookMIxBind :: IxBind (IndexedHookM hooks input slots output m) where
  ibind (IndexedHookM fmonad) = IndexedHookM <<< bind fmonad <<< compose unIx

instance indexedHookMIxMonad :: IxMonad (IndexedHookM hooks input slots output m)

type HookAction hooks input slots output m
  = HookM hooks input slots output m Unit

lift
  :: forall hooks input slots output m v i
   . HookM hooks input slots output m v
  -> IndexedHookM hooks input slots output m i i v
lift = IndexedHookM

hookCons
  :: forall hooks' hooks input slots output proxy sym m v i o
   . IsSymbol sym
  => Lacks sym i
  => Cons sym v i o
  => Lacks sym hooks'
  => Cons sym v hooks' hooks
  => proxy sym
  -> HookM hooks input slots output m v
  -> IndexedHookM hooks input slots output m i o v
hookCons px m = IndexedHookM (map (getHookCons px) getHooksM >>= maybe (m >>= (applySecond <$> setHookMCons px <*> pure)) pure)
