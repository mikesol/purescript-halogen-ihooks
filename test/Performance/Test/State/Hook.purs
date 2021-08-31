module Performance.Test.State.Hook where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Array (replicate)
import Data.Foldable (sequence_)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.IHooks (doThis)
import Halogen.IHooks as Hooks
import Halogen.IHooks.Compat as Compat
import Halogen.IHooks.Sugar as Sugar
import Performance.Test.State.Shared (Output(..), stateUpdates)
import Performance.Test.Types (Test(..), startSuffix, testToString)
import Type.Proxy (Proxy(..))

_stateHook = Proxy :: Proxy "stateHook"

component :: forall q i m. MonadAff m => H.Component q i Output m
component = Hooks.component Hooks.defaultOptions \_ -> Ix.do
  n /\ nId <- Compat.useState { n: 0, n1: 0, n2: 0, n3: 0, n4: 0 }

  let
    runState = doThis do
      sequence_ $ replicate stateUpdates $ Sugar.modify_ nId \s -> s { n = s.n + 1 }
      sequence_ $ replicate stateUpdates $ Sugar.modify_ nId \s -> s { n1 = s.n1 + 1 }
      sequence_ $ replicate stateUpdates $ Sugar.modify_ nId \s -> s { n2 = s.n2 + 1 }
      sequence_ $ replicate stateUpdates $ Sugar.modify_ nId \s -> s { n3 = s.n3 + 1 }
      sequence_ $ replicate stateUpdates $ Sugar.modify_ nId \s -> s { n4 = s.n4 + 1 }
      H.raise Done

  ipure do
    HH.div_
      [ HH.button
          [ HP.id (testToString StateHook <> startSuffix)
          , HE.onClick \_ -> runState
          ]
          [ HH.text "Start Test" ]
      , HH.text $ show n
      ]
