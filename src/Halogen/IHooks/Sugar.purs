module Halogen.IHooks.Sugar where

import Prelude

import Control.Applicative.Indexed (iapplySecond, ivoid, iwhen)
import Control.Apply (applySecond)
import Control.Bind.Indexed (ibind)
import Data.Foldable (fold, for_)
import Data.Symbol (class IsSymbol)
import Halogen as H
import Halogen.IHooks (class NotReadOnly, HookAction, HookHTML, HookM, IndexedHookM, Options, component, doThis, getHookCons, getHooksM, hookCons, lift, setHookMCons)
import Prim.Row as Row
import Type.Proxy (Proxy(..))

newtype F hooks' input slots output m
  = F
  ( HookM
      ( "_finalize" :: F hooks' input slots output m
      | hooks'
      )
      input
      slots
      output
      m
      Unit
  )

unF
  :: forall hooks' input slots output m
   . F hooks' input slots output m
  -> HookM
       ( "_finalize" :: F hooks' input slots output m
       | hooks'
       )
       input
       slots
       output
       m
       Unit

unF (F q) = q

finalize :: Proxy "_finalize"
finalize = Proxy

withFinalize
  :: forall hooks' input slots output m i
   . Row.Lacks "_finalize" i
  => Row.Lacks "_finalize" hooks'
  => IndexedHookM ("_finalize" :: F hooks' input slots output m | hooks') input slots output m i ("_finalize" :: F hooks' input slots output m | i) Unit
withFinalize = ivoid $ hookCons (Proxy :: _ "_finalize") (pure $ F $ pure unit)

addToFinalize
  :: forall hooks' input slots output m
   . Row.Lacks "_finalize" hooks'
  => HookM ("_finalize" :: F hooks' input slots output m | hooks') input slots output m Unit
  -> HookM ("_finalize" :: F hooks' input slots output m | hooks') input slots output m Unit
addToFinalize m = bind (map (getHookCons finalize) getHooksM) (setHookMCons finalize <<< F <<< applySecond m <<< fold <<< map unF)

runFinalize
  :: forall hooks' input slots output m
   . Row.Lacks "_finalize" hooks'
  => HookM ("_finalize" :: F hooks' input slots output m | hooks') input slots output m Unit
runFinalize = bind (map (getHookCons finalize) getHooksM) (fold <<< map unF)

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
setM px v = doThis $ bind v (setHookMCons px)

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
modify_ px f = doThis $ bind (map (getHookCons px) getHooksM) (flip for_ (setHookMCons px <<< f))

withHook
  :: forall proxy hooks' hooks input slots output m sym v
   . IsSymbol sym
  => Row.Lacks sym hooks'
  => Row.Cons sym v hooks' hooks
  => proxy sym
  -> (v -> HookM hooks input slots output m Unit)
  -> HookM hooks input slots output m Unit
withHook px = bind (map (getHookCons px) getHooksM) <<< flip for_

-- | A component with a finalizer bolted on
-- | The same as manually calling `withFinalizer` and `runFinalizer`
componentF
  :: forall slots hooks' query output m input
   . Row.Lacks "_finalize" hooks'
  => Options query ("_finalize" :: F hooks' input slots output m | hooks') input slots output m
  -> ( input
       -> IndexedHookM ("_finalize" :: F hooks' input slots output m | hooks') input slots output m ("_finalize" :: F hooks' input slots output m) ("_finalize" :: F hooks' input slots output m | hooks') (HookHTML ("_finalize" :: F hooks' input slots output m | hooks') input slots output m)
     )
  -> H.Component query input output m
componentF options f = component (options { finalize = options.finalize *> runFinalize }) (iapplySecond (withFinalize) <<< f)