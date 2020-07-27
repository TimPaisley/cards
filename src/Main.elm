port module Main exposing (Model, Msg(..), emptyModel, init, main, setStorage, update, updateWithStorage, view, viewCard, viewHero, viewQueue, viewTeam)

import Browser
import Browser.Dom as Dom
import Cards exposing (Card, getCards)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Random
import Task


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Cards", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



-- MODEL


type alias Model =
    { queue : List Card
    , team : List Card
    }


emptyModel : Model
emptyModel =
    { queue = []
    , team = []
    }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( emptyModel
      -- Maybe.withDefault emptyModel maybeModel
    , Random.list 20 (Random.int 0 2)
        |> Random.generate UpdateQueue
    )



-- UPDATE


type Msg
    = NoOp
    | UpdateQueue (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateQueue cardIDs ->
            ( { model | queue = getCards cardIDs }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "cards-wrapper" ]
        [ div
            [ class "game" ]
            [ viewHeader
            , lazy viewQueue model.queue
            , viewHero
            , lazy viewTeam model.team
            ]
        ]


viewHeader : Html Msg
viewHeader =
    div
        [ class "header" ]
        [ div [ class "header-title" ] [ text "Cards" ]
        , div [ class "header-subtitle" ] [ text "Build Phase" ]
        ]


viewQueue : List Card -> Html Msg
viewQueue queue =
    div
        [ class "queue" ]
        (List.indexedMap viewCard <| List.take 16 queue)



-- VIEW ALL ENTRIES


viewHero : Html Msg
viewHero =
    div
        [ class "hero" ]
        [ div [ class "hero-points" ] [ text <| String.fromInt 45 ]
        , div [ class "hero-portrait" ] []
        , div [ class "hero-health" ] [ text <| String.fromInt 12 ]
        ]


viewTeam : List Card -> Html Msg
viewTeam team =
    div [ class "team" ] (List.repeat 4 viewEmpty)


viewCard : Int -> Card -> Html Msg
viewCard index card =
    div
        [ classList [ ( "card", True ), ( "active", index > 11 ) ] ]
        [ div [ class "card-name" ] [ text card.name ]
        , div [ class "card-attack" ] [ text <| String.fromInt card.attack ]
        , div [ class "card-energy" ] [ text <| String.fromInt card.energy ]
        ]


viewEmpty : Html Msg
viewEmpty =
    div [ class "empty" ] []
