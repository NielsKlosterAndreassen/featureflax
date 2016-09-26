module FeatureFlax exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import List exposing (..)
import ListExtra exposing (..)

type alias Environment = String
type alias Feature = String

type alias FeatureState =
  { name : Feature,
    isOnFor : List Environment
  }

type alias Model =
  { featureStates : List FeatureState,
    environments : List Environment,
    history : List Msg
  }
type Msg =
  AddEnvironment Environment |
  TurnFeatureOn (Feature, Environment) |
  TurnFeatureOff (Feature, Environment)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
  let initialState =
    [ 
      AddEnvironment "Development",
      AddEnvironment "Staging",
      AddEnvironment "Production",
      TurnFeatureOn("Two Factor Authentication", "Development"),
      TurnFeatureOn("Two Factor Authentication", "Staging"),
      TurnFeatureOff("Two Factor Authentication", "Development")
    ]
    |> foldr rollupUpdate { featureStates = [], environments = [], history = []}
  in
  (initialState, Cmd.none)

rollupUpdate : Msg -> Model -> Model
rollupUpdate msg model =
  let (result, _) = update msg model in
  result

except : Feature -> List FeatureState -> List FeatureState
except feature states =
  states |> filter (\ x -> x.name /= feature)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ({ environments = getEnvironments msg model.environments, 
     featureStates = getFeatureStates msg model.featureStates,
     history = msg :: model.history
  }, Cmd.none)

getEnvironments msg environments =
  case msg of
    AddEnvironment environment -> environment :: environments
    _ -> environments

getFeatureStates msg featureStates =
  case msg of
  TurnFeatureOn (feature, environment) ->
    case featureStates |> find (\ x -> x.name == feature) of
    Nothing ->
      { name = feature, isOnFor = [ environment ] } :: featureStates
    Just existing ->
      { existing | isOnFor = environment :: existing.isOnFor } :: (featureStates |> except feature)
  TurnFeatureOff (feature, environment) -> []
  _ -> featureStates

view : Model -> Html Msg
view model =
  let environments =
    model.featureStates
    |> map (\ feature -> feature.isOnFor)
    |> concat
    |> append model.environments
    |> unique
  in
  span [] [
    table [ class "toggles"] (
      drawHeader environments :: 
      (model.featureStates |> map (drawFeature environments))
    ),
    ul [ class "history"] (
      (model.history |> map (\ item -> li [] [ getHistoryText item ]))
    )
  ]

getHistoryText msg =
  case msg of
  AddEnvironment environment -> text ("Added " ++ environment)
  TurnFeatureOn (feature, environment) -> text ("Turned on " ++ feature ++ " for " ++ environment)
  TurnFeatureOff (feature, environment) -> text ("Turned off " ++ feature ++ " for " ++ environment)

drawHeader environments =
  thead [] ( "" :: environments |> map (\ header -> th [] [ text header ]) )

drawFeature environments feature =
  tr [ ] ( td [] [text feature.name] :: (environments |> map (drawSlider feature)))

drawSlider feature environment =
  let isChecked = feature.isOnFor |> any (\ x -> x == environment) in
  td [] [ label [ class "switch"] [ 
    input [ type' "checkbox", checked isChecked ] [],
    div [ class "slider" ] []
  ]]

subscriptions model =
  Sub.none
