module App.Compat where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.IHooks.Compat as Hooks

data Query a = IsOn (Boolean -> a)

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

component :: forall i o. H.Component Query i o Aff
component = Hooks.component \{ queryToken } _ -> Hooks.do
  count /\ countId <- Hooks.useState 0
  Hooks.useQuery queryToken case _ of
    IsOn reply -> do
      pure (Just (reply true))

  Hooks.pure
      ( HH.div [ classes [ "w-screen", "h-screen" ] ]
          [ HH.div [ classes [ "flex", "flex-col", "w-full", "h-full" ] ]
              [ HH.div [ classes [ "flex-grow" ] ] []
              , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
                  [ HH.div [ classes [ "flex-grow" ] ]
                      []
                  , HH.div [ classes [ "flex", "flex-col" ] ]
                      [ HH.h1 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                          [ HH.text (show count) ]
                      , HH.button
                          [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ ->  Hooks.modify_ countId (_ + 1)]
                          [ HH.text "Increment foo" ]
                      ]
                  , HH.div [ classes [ "flex-grow" ] ] []
                  ]
              , HH.div [ classes [ "flex-grow" ] ] []
              ]
          ]
      )
