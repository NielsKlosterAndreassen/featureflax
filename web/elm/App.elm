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
    environments : List Environment
  }
type Msg =
  None |
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
      AddEnvironment "Staging"
    ]
    |> foldr rollupUpdate { featureStates = [], environments = []}
  in
  (initialState, Cmd.none)

rollupUpdate : Msg -> Model -> Model
rollupUpdate msg model =
  let (result, _) = update msg model in
  result

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
  None -> (model, Cmd.none)
  AddEnvironment environment ->
    ({model | environments = environment :: model.environments } , Cmd.none)
  TurnFeatureOn (feature, environment) -> 
    (model, Cmd.none)
  TurnFeatureOff (feature, environment) -> (model, Cmd.none)

view : Model -> Html Msg
view model =
  let environments =
    model.featureStates
    |> map (\ feature -> feature.isOnFor)
    |> concat
    |> append model.environments
    |> unique
  in
  table [ class "toggles"] (
    drawHeader environments :: 
    (model.featureStates |> map (drawFeature environments))
  )

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
