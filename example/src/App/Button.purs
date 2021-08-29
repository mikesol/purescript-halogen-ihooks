module App.Button where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Effect.Aff (Aff)
import Effect.Class.Console as Log
import Halogen (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.IHooks as Hooks
import Halogen.IHooks.Sugar as Sugar
import Type.Proxy (Proxy(..))

aff0 :: Aff Int
aff0 = pure 0

affAdd1 :: Int -> Aff Int
affAdd1 = pure <<< add 1

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

component :: forall q i o. H.Component q i o Aff
component =
  Hooks.component Hooks.defaultOptions \_ -> Ix.do
    -- a pure hook
    foo <- Sugar.hookConsPure (Proxy :: _ "foo") 0
    -- an effectful hook
    bar <- Hooks.hookCons (Proxy :: _ "bar") (lift aff0)
    when (bar `mod` 3 == 0) (Hooks.lift (Log.info $ "Bar at " <> show bar <> " is mod 3!"))
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
