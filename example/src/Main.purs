module Main where

import Prelude

import App.Button as Button
import App.Compat as Compat
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "" $ proxy Button.component
  , Tuple "button" $ proxy Button.component
  , Tuple "compat" $ proxy Compat.component
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>=
    runStorybook
      { stories
      , logo: Just (HH.div [] [HH.text "halogen ihooks"])
      }
