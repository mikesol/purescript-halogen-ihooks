module App.KitchenSink where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Effect.Aff (Aff)
import Effect.Class.Console as Log
import FRP.Event (subscribe)
import FRP.Event.Time (interval)
import Halogen (lift, unsubscribe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.IHooks as Hooks
import Halogen.IHooks.Sugar as Sugar
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))

aff0 :: Aff Int
aff0 = pure 0

affAdd1 :: Int -> Aff Int
affAdd1 = pure <<< add 1

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

component :: forall q i o. H.Component q i o Aff
component =
  Sugar.componentF Hooks.defaultOptions \_ -> Ix.do
    -- a pure hook
    foo <- Sugar.hookConsPure (Proxy :: _ "foo") 0
    -- an effectful hook
    bar <- Hooks.hookCons (Proxy :: _ "bar") (lift aff0)
    -- captures the value of a hook, running an effect only when it changes
    Sugar.capture (Proxy :: _ "deltaFoo") foo (Log.info $ "Foo changed")
    -- adds an effect to the finalizer
    -- reading the "bar" hook during finalization
    Hooks.lift
      $ Sugar.addToFinalize
      $ Sugar.withHook (Proxy :: _ "bar")
        \bar' -> Log.info $ "I finalized with a bar of " <> show bar'
    -- garden-varienty applicative logic
    when (bar `mod` 3 == 0) (Hooks.lift (Log.info $ "Bar at " <> show bar <> " is mod 3!"))
    -- adds an emitter that increments foo even if we don't!
    -- should respond to both the passage of time and clicks
    Hooks.hookCons (Proxy :: _ "emitter") do
      { emitter, listener } <- H.liftEffect HS.create
      Sugar.addToFinalize <<< unsubscribe =<< H.subscribe emitter
      Sugar.addToFinalize <<< H.liftEffect =<<
        ( H.liftEffect
            $ subscribe (interval 1000)
              (const $ HS.notify listener (Sugar.modify_ (Proxy :: _ "foo") (add 1)))
        )
    -- html
    ipure
      ( HH.div [ classes [ "w-screen", "h-screen" ] ]
          [ HH.div [ classes [ "flex", "flex-col", "w-full", "h-full" ] ]
              [ HH.div [ classes [ "flex-grow" ] ] []
              , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
                  [ HH.div [ classes [ "flex-grow" ] ]
                      []
                  , HH.div [ classes [ "flex", "flex-col" ] ]
                      [ HH.h1 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                          [ HH.text ("Foo is: " <> show foo <> " Bar is: " <> show bar) ]
                      -- effectful setter
                      , HH.button
                          [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> Sugar.setM (Proxy :: _ "foo") (lift $ affAdd1 foo) ]
                          [ HH.text "Increment foo" ]
                      -- pure modifier
                      , HH.button
                          [ classes [ "text-2xl", "m-5", "bg-pink-500", "p-3", "rounded-lg", "text-white", "hover:bg-pink-400" ], HE.onClick \_ -> Sugar.modify_ (Proxy :: _ "bar") (add 1) ]
                          [ HH.text "Increment bar" ]
                      ]
                  , HH.div [ classes [ "flex-grow" ] ] []
                  ]
              , HH.div [ classes [ "flex-grow" ] ] []
              ]
          ]
      )
