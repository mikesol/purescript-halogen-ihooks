module Performance.Test.Todo.Hook where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.IHooks as Hooks
import Halogen.IHooks.Sugar as Sugar
import Performance.Test.Todo.Shared (CheckboxInput, CheckboxOutput(..), TodoInput, TodoOutput(..))
import Performance.Test.Todo.Shared as Shared
import Type.Proxy (Proxy(..))

_todoHook = Proxy :: Proxy "todoHook"
_containerState = Proxy :: Proxy "containerState"
_description = Proxy :: Proxy "description"

container :: forall q i o m. MonadAff m => H.Component q i o m
container = Hooks.component Hooks.defaultOptions \_ -> Ix.do
  state <- Hooks.hookCons _containerState (liftEffect $ Shared.fillContainerState Shared.initialContainerState)

  let
    handleTodo = Hooks.doThis <<< case _ of
      Save t -> do
        for_ (Shared.updateTodo t state.todos) \todos ->
          Sugar.modify_ _containerState _ { todos = todos }

      SetCompleted id complete -> do
        if complete then
          Sugar.modify_ _containerState _ { completed = Set.insert id state.completed }
        else
          Sugar.modify_ _containerState _ { completed = Set.delete id state.completed }

  ipure do
    let
      todos = state.todos <#> \t ->
        HH.slot Shared._todo t.id todo { todo: t, completed: state.completed } handleTodo

    HH.div_
      [ HH.button
          [ HP.id Shared.addNewId
          , HE.onClick \_ -> Hooks.doThis do
              newState <- liftEffect $ Shared.createTodo state
              Hooks.setHookMCons _containerState newState
          ]
          [ HH.text "Add New" ]
      , HH.div
          [ HP.id Shared.todosId ]
          todos
      ]

todo :: forall q m. MonadAff m => H.Component q TodoInput TodoOutput m
todo = Hooks.component
  ( Hooks.defaultOptions
      { receiveInput = \cur prev ->
          if prev.todo.id == cur.todo.id && prev.completed == cur.completed then Nothing
          else Just cur
      }
  )
  \input -> Ix.do
    description <- Sugar.hookConsPure _description input.todo.description

    let
      handleCheckbox (Check bool) = Hooks.doThis do
        H.raise $ SetCompleted input.todo.id bool

    ipure $
      HH.div_
        [ HH.input
            [ HP.id (Shared.editId input.todo.id)
            , HE.onValueInput (Sugar.doSet _description)
            , HP.value description
            ]
        , HH.slot Shared._checkbox unit checkbox { id: input.todo.id, completed: input.completed } handleCheckbox
        , HH.button
            [ HP.id (Shared.saveId input.todo.id)
            , HE.onClick \_ -> Hooks.doThis do
                H.raise $ Save { id: input.todo.id, description }
            ]
            [ HH.text "Save Changes" ]
        ]

checkbox :: forall q m. MonadAff m => H.Component q CheckboxInput CheckboxOutput m
checkbox = Hooks.component Hooks.defaultOptions \input -> ipure $ HH.input
  [ HP.id (Shared.checkId input.id)
  , HP.checked $ Set.member input.id input.completed
  , HP.type_ HP.InputCheckbox
  , HE.onChecked \checked -> Hooks.doThis $ H.raise (Check checked)
  ]
