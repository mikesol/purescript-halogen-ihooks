module Halogen.IHooks.Sugar where

import Prelude

import Control.Applicative.Indexed (ivoid, iwhen)
import Control.Apply (applySecond)
import Control.Bind.Indexed (ibind)
import Data.Maybe (fromMaybe, maybe)
import Data.Symbol (class IsSymbol)
import Halogen.IHooks (class NotReadOnly, HookAction, HookM, IndexedHookM, doThis, getHookCons, getHooksM, hookCons, lift, setHookMCons)
import Prim.Row as Row

withFinalize
  :: forall hooks' hooks input slots output proxy sym m i o
   . IsSymbol sym
  => Row.Lacks sym i
  => Row.Cons sym (HookM hooks input slots output m Unit) i o
  => Row.Lacks sym hooks'
  => Row.Cons sym (HookM hooks input slots output m Unit) hooks' hooks
  => proxy sym
  -> IndexedHookM hooks input slots output m i o Unit
withFinalize px = ivoid $ hookCons px (pure (pure unit))

addToFinalize
  :: forall proxy hooks' hooks input slots output m sym i o
   . IsSymbol sym
  => Row.Lacks sym i
  => Row.Cons sym (HookM hooks input slots output m Unit) i o
  => Row.Lacks sym hooks'
  => Row.Cons sym (HookM hooks input slots output m Unit) hooks' hooks
  => proxy sym
  -> HookM hooks input slots output m Unit
  -> HookM hooks input slots output m Unit
addToFinalize px m = map (getHookCons px) getHooksM >>= void <<< setHookMCons px <<< applySecond m <<< fromMaybe (pure unit)

runFinalize
  :: forall proxy hooks' hooks input slots output m sym i o
   . IsSymbol sym
  => Row.Lacks sym i
  => Row.Cons sym (HookM hooks input slots output m Unit) i o
  => Row.Lacks sym hooks'
  => Row.Cons sym (HookM hooks input slots output m Unit) hooks' hooks
  => proxy sym
  -> HookM hooks input slots output m Unit
runFinalize px = map (getHookCons px) getHooksM >>= fromMaybe (pure unit)

capture
  :: forall proxy hooks' hooks input slots output m sym v i o
   . IsSymbol sym
  => Row.Lacks sym i
  => Row.Cons sym v i o
  => Row.Lacks sym hooks'
  => Row.Cons sym v hooks' hooks
  => Eq v
  => proxy sym
  -> v
  -> HookM hooks input slots output m Unit
  -> IndexedHookM hooks input slots output m i o Unit
capture px v m = ibind (hookCons px (pure v)) (flip iwhen (lift (setHookMCons px v *> m)) <<< notEq v)

hookConsPure
  :: forall hooks' hooks input slots output proxy sym m v i o
   . IsSymbol sym
  => Row.Lacks sym i
  => Row.Cons sym v i o
  => Row.Lacks sym hooks'
  => Row.Cons sym v hooks' hooks
  => proxy sym
  -> v
  -> IndexedHookM hooks input slots output m i o v
hookConsPure px = hookCons px <<< pure

setM
  :: forall proxy output input slots m sym a r1 hooks
   . NotReadOnly a
  => Row.Cons sym a r1 hooks
  => IsSymbol sym
  => proxy sym
  -> HookM hooks input slots output m a
  -> HookAction hooks input slots output m
setM px v = doThis $ v >>= (void <<< setHookMCons px)

set
  :: forall proxy output input slots m sym a r1 hooks
   . NotReadOnly a
  => Row.Cons sym a r1 hooks
  => IsSymbol sym
  => proxy sym
  -> a
  -> HookAction hooks input slots output m
set px = setM px <<< pure

modify_
  :: forall proxy output input slots m sym a r1 hooks
   . NotReadOnly a
  => Row.Cons sym a r1 hooks
  => IsSymbol sym
  => proxy sym
  -> (a -> a)
  -> HookAction hooks input slots output m
modify_ px f = doThis $ map (getHookCons px) getHooksM >>= maybe (pure unit) (setHookMCons px <<< f)
