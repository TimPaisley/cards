port module Main exposing (Model, Msg(..), emptyModel, init, main, setStorage, update, updateWithStorage, view, viewCard, viewHero, viewQueue, viewTeam)

import Browser
import Browser.Dom as Dom
import Cards exposing (Card, getCard)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Html5.DragDrop
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Task


main : Program Encode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Cards", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Encode.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    case msg of
        ResetModel ->
            ( newModel, cmds )

        _ ->
            ( newModel
            , Cmd.batch [ setStorage (encode newModel), cmds ]
            )



-- MODEL


type alias Model =
    { queue : List Int
    , team : List Int
    , cardDragDrop : CardDragDrop
    }


emptyModel : Model
emptyModel =
    { queue = []
    , team = []
    , cardDragDrop = emptyCardDragDrop
    }


generateRandomQueue : Cmd Msg
generateRandomQueue =
    Random.list 20 (Random.int 0 2)
        |> Random.generate UpdateQueue


type alias CardDragDrop =
    { dragDrop : Html5.DragDrop.Model Card ( Int, Int )
    , hoverPos : Maybe ( Int, Int )
    }


emptyCardDragDrop : CardDragDrop
emptyCardDragDrop =
    { dragDrop = Html5.DragDrop.init
    , hoverPos = Nothing
    }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue decoder flags of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( emptyModel, generateRandomQueue )



-- UPDATE


type Msg
    = NoOp
    | ResetModel
    | UpdateQueue (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ResetModel ->
            ( emptyModel, generateRandomQueue )

        UpdateQueue cardIDs ->
            ( { model | queue = cardIDs }
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
        [ div [] [ text "Cards" ]
        , div [] [ text "Build Phase" ]
        , div [ onClick ResetModel ] [ text "Reset" ]
        ]


viewQueue : List Int -> Html Msg
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


viewTeam : List Int -> Html Msg
viewTeam team =
    div [ class "team" ] (List.repeat 4 viewEmpty)


viewCard : Int -> Int -> Html Msg
viewCard index cardID =
    let
        card =
            getCard cardID
    in
    div
        [ classList [ ( "card", True ), ( "active", index > 11 ) ] ]
        [ div [ class "card-name" ] [ text card.name ]
        , div [ class "card-attack" ] [ text <| String.fromInt card.attack ]
        , div [ class "card-energy" ] [ text <| String.fromInt card.energy ]
        ]


viewEmpty : Html Msg
viewEmpty =
    div [ class "empty" ] []



-- ENCODE/DECODE


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "queue", Encode.list Encode.int model.queue )
        , ( "team", Encode.list Encode.int model.team )
        ]


decoder : Decode.Decoder Model
decoder =
    Decode.map3 Model
        (Decode.field "queue" <| Decode.list Decode.int)
        (Decode.field "team" <| Decode.list Decode.int)
        (Decode.succeed emptyCardDragDrop)
