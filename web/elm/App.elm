module FeatureFlax exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onClick, onSubmit, onInput)
import List exposing (..)

type alias Environment = String
type alias Feature = String

type alias Model =
  { history : List Msg,
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
    emptyModel = { history = [], newFeatureText = "", newEnvironmentText = ""}
    initialState =
    [ 
      AddEnvironment "Production",
      AddEnvironment "Staging",
      AddEnvironment "Development",
      AddFeature "Two Factor Authentication",
      AddFeature "New Layout"
    ]
    |> foldl rollupUpdate emptyModel
  in
  (initialState, Cmd.none)

rollupUpdate : Msg -> Model -> Model
rollupUpdate msg model =
  let (result, _) = update msg model in
  result

update : Msg -> Model -> ( Model, Cmd Msg )
update msg modelWithoutHistory =
  let model = { modelWithoutHistory | history = msg :: modelWithoutHistory.history } in
  case msg of
    UpdateNewFeature text -> ({ model | newFeatureText = text }, Cmd.none)
    UpdateNewEnvironment text -> ({ model | newEnvironmentText = text }, Cmd.none)
    AddEnvironment text -> ({model | newEnvironmentText = "" }, Cmd.none)
    AddFeature text -> ({model | newFeatureText = "" }, Cmd.none)
    _ -> (model, Cmd.none)

getEnvironment msg =
  case msg of
    AddEnvironment environment -> Just environment
    _ -> Nothing

getFeatureName msg =
  case msg of
    AddFeature feature -> Just feature
    _ -> Nothing

view : Model -> Html Msg
view model =
  let
    features = model.history |> filterMap getFeatureName
    environments = model.history |> filterMap getEnvironment
    newFeatureTextValid = case model.newFeatureText of
      "" -> False
      _ -> features |> filter (\ x -> x == model.newFeatureText) |> isEmpty
    newEnvironmentTextValid = case model.newEnvironmentText of
      "" -> False
      _ -> environments |> filter (\ x -> x == model.newEnvironmentText) |> isEmpty
  in
  span [] [
    table [ class "toggles"] (
      drawHeader environments :: 
      (features |> map (drawFeature environments model.history))
    ),
    Html.form [ class "input-group col-lg-4", onSubmit (AddEnvironment model.newEnvironmentText)] [
      input [
        type' "text",
        class "form-control",
        placeholder "New environment",
        onInput UpdateNewEnvironment,
       value model.newEnvironmentText
      ] [],
      span [class "input-group-btn"] [ button [class "btn btn-default", disabled (not newEnvironmentTextValid)] [ text "Add"]]
    ],
    Html.form [ class "input-group col-lg-4", onSubmit (AddFeature model.newFeatureText)] [
      input [
        type' "text",
        class "form-control",
        placeholder "New feature",
        onInput UpdateNewFeature,
        value model.newFeatureText
      ] [],
      span [class "input-group-btn"] [ button [class "btn btn-default", disabled (not newFeatureTextValid)] [ text "Add"]]
    ],
    ul [ class "history"] (
      (model.history |> filterMap getHistoryText |> map (\ item -> li [ ] [ item ]))
    )
  ]

getHistoryText msg =
  case msg of
  AddEnvironment environment -> text ("Added " ++ environment ++ " environment") |> Just
  AddFeature feature -> text ("Added feature " ++ feature) |> Just
  TurnFeatureOn (feature, environment) -> text ("Turned on " ++ feature ++ " for " ++ environment) |> Just
  TurnFeatureOff (feature, environment) -> text ("Turned off " ++ feature ++ " for " ++ environment) |> Just
  _ -> Nothing

drawHeader environments =
  thead [] ( "" :: environments |> map (\ header -> th [] [ text header ]) )

drawFeature : List Environment -> List Msg -> Feature -> Html Msg
drawFeature environments history feature =
  tr [ ] ( td [] [text feature] :: (environments |> map (drawSlider feature history)))

isCurrentToggleAction : Feature -> Environment -> Msg -> Bool
isCurrentToggleAction feature environment msg =
  case msg of
  TurnFeatureOn (currentFeature, currentEnvironment) -> feature == currentFeature && environment == currentEnvironment
  TurnFeatureOff (currentFeature, currentEnvironment) -> feature == currentFeature && environment == currentEnvironment
  _ -> False

getToggleState history feature environment =
  let lastToggle = history
  |> filter (\x -> isCurrentToggleAction feature environment x)
  |> head
  in
  case lastToggle of
    Just (TurnFeatureOn _) -> True
    _ -> False

drawSlider : Feature -> List Msg -> Environment -> Html Msg
drawSlider feature history environment =
  let 
    isChecked = getToggleState history feature environment
    toggle = if isChecked then TurnFeatureOff(feature, environment) else TurnFeatureOn(feature, environment)
  in
  td [] [ label [ class "switch"] [ 
    input [ type' "checkbox", checked isChecked, onClick toggle] [],
    div [ class "slider" ] []
  ]]

subscriptions model =
  Sub.none
