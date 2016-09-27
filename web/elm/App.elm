module FeatureFlax exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onClick, onSubmit, onInput)
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
    history : List Msg,
    newFeatureText : String,
    newEnvironmentText : String
  }
type Msg =
  UpdateNewFeature String |
  UpdateNewEnvironment String |
  AddEnvironment Environment |
  AddFeature Feature |
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
  let
    emptyModel = { featureStates = [], environments = [], history = [], newFeatureText = "", newEnvironmentText = ""}
    initialState =
    [ 
      AddEnvironment "Production",
      AddEnvironment "Staging",
      AddEnvironment "Development",
      TurnFeatureOn("Two Factor Authentication", "Development")
    ]
    |> foldl rollupUpdate emptyModel
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
update msg modelWithoutHistory =
  let model = { modelWithoutHistory | history = msg :: modelWithoutHistory.history } in
  case msg of
    UpdateNewFeature text -> ({ model | newFeatureText = text }, Cmd.none)
    UpdateNewEnvironment text -> ({ model | newEnvironmentText = text }, Cmd.none)
    _ ->
      ({ model |
         environments = getEnvironments msg model.environments, 
         featureStates = getFeatureStates msg model.featureStates
      }, Cmd.none)

getEnvironments msg environments =
  case msg of
    AddEnvironment environment -> environment :: environments
    _ -> environments

getFeatureStates msg featureStates =
  case msg of
  AddFeature feature -> { name = feature, isOnFor = [] } :: featureStates
  TurnFeatureOn (feature, environment) ->
    case featureStates |> find (\ x -> x.name == feature) of
    Nothing ->
      { name = feature, isOnFor = [ environment ] } :: featureStates
    Just existing ->
      { existing | isOnFor = environment :: existing.isOnFor } :: (featureStates |> except feature)
  TurnFeatureOff (feature, environment) ->
    case featureStates |> find (\ x -> x.name == feature) of
    Nothing ->
      featureStates
    Just existing ->
      { existing | isOnFor = existing.isOnFor |> filter (\x -> x /= environment ) } :: (featureStates |> except feature)
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
    Html.form [ class "input-group col-lg-4", onSubmit (AddEnvironment model.newEnvironmentText)] [
      input [type' "text", class "form-control", placeholder "Add environment", onInput UpdateNewEnvironment] [],
      span [class "input-group-btn"] [ button [class "btn btn-default"] [ text "Add"]]
    ],
    Html.form [ class "input-group col-lg-4", onSubmit (AddFeature model.newFeatureText)] [
      input [type' "text", class "form-control", placeholder "Add feature", onInput UpdateNewFeature] [],
      span [class "input-group-btn"] [ button [class "btn btn-default"] [ text "Add"]]
    ],
    ul [ class "history"] (
      (model.history |> filter isInHistory |> map (\ item -> li [ ] [ getHistoryText item ]))
    )
  ]

isInHistory msg =
  case msg of 
  AddEnvironment _ -> True
  AddFeature _ -> True
  TurnFeatureOn _ -> True
  TurnFeatureOff _ -> True
  _ -> False

getHistoryText msg =
  case msg of
  AddEnvironment environment -> text ("Added " ++ environment)
  AddFeature feature -> text ("Added " ++ feature)
  TurnFeatureOn (feature, environment) -> text ("Turned on " ++ feature ++ " for " ++ environment)
  TurnFeatureOff (feature, environment) -> text ("Turned off " ++ feature ++ " for " ++ environment)
  _ -> text ""

drawHeader environments =
  thead [] ( "" :: environments |> map (\ header -> th [] [ text header ]) )

drawFeature environments feature =
  tr [ ] ( td [] [text feature.name] :: (environments |> map (drawSlider feature)))

drawSlider feature environment =
  let 
    isChecked = feature.isOnFor |> any (\ x -> x == environment)
    toggle = if isChecked then TurnFeatureOff(feature.name, environment) else TurnFeatureOn(feature.name, environment)
  in
  td [] [ label [ class "switch"] [ 
    input [ type' "checkbox", checked isChecked, onClick toggle] [],
    div [ class "slider" ] []
  ]]

subscriptions model =
  Sub.none
