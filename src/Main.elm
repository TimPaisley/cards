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
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.batch
        [ Random.list 20 (Random.int 0 2)
            |> Random.generate UpdateQueue
        , Random.list 4 (Random.int 0 2)
            |> Random.generate UpdateTeam
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | UpdateQueue (List Int)
    | UpdateTeam (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateQueue cardIDs ->
            ( { model | queue = getCards cardIDs }
            , Cmd.none
            )

        UpdateTeam cardIDs ->
            ( { model | team = getCards cardIDs }
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
        (List.map viewCard <| List.take 16 queue)



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
    div
        [ class "team" ]
        (List.map viewCard team)


viewCard : Card -> Html Msg
viewCard card =
    div
        [ class "card" ]
        [ div [ class "card-name" ] [ text card.name ]
        , div [ class "card-attack" ] [ text <| String.fromInt card.attack ]
        , div [ class "card-energy" ] [ text <| String.fromInt card.energy ]
        ]
