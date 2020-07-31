port module Main exposing (Model, Msg(..), emptyModel, init, main, setStorage, update, updateWithStorage, view, viewCard, viewHero, viewQueue, viewTeam)

import Browser
import Cards exposing (getCard)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Html5.DragDrop
import Json.Decode as Decode
import Json.Decode.Extra as DecodeX
import Json.Encode as Encode
import Json.Encode.Extra as EncodeX
import Random


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
    , team : Dict Int (Maybe Int)
    , cardDragDrop : CardDragDrop
    }


emptyModel : Model
emptyModel =
    { queue = []
    , team = Dict.fromList [ ( 1, Nothing ), ( 2, Nothing ), ( 3, Nothing ), ( 4, Nothing ) ]
    , cardDragDrop = emptyCardDragDrop
    }


generateRandomQueue : Cmd Msg
generateRandomQueue =
    Random.list 20 (Random.int 0 2)
        |> Random.generate UpdateQueue


type alias CardDragDrop =
    { dragDrop : Html5.DragDrop.Model Int Int
    , hoverPos : Maybe Int
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
    | DragDropMsg (Html5.DragDrop.Msg Int Int)


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

        DragDropMsg msg_ ->
            let
                ( newDragDrop, result ) =
                    Html5.DragDrop.update msg_ model.cardDragDrop.dragDrop

                dragId =
                    Html5.DragDrop.getDragId newDragDrop

                dropId =
                    Html5.DragDrop.getDropId newDragDrop

                newCardDragDrop =
                    case ( dragId, dropId ) of
                        ( Just _, _ ) ->
                            { dragDrop = newDragDrop, hoverPos = dropId }

                        _ ->
                            model.cardDragDrop

                ( newQueue, newTeam ) =
                    case result of
                        Just ( card, slot, _ ) ->
                            ( model.queue, Dict.insert slot (Just card) model.team )

                        Nothing ->
                            ( model.queue, model.team )
            in
            ( { model | cardDragDrop = newCardDragDrop, queue = newQueue, team = newTeam }
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


viewTeam : Dict Int (Maybe Int) -> Html Msg
viewTeam team =
    div [ class "team" ] (Dict.toList team |> List.map viewSlot)


viewCard : Int -> Int -> Html Msg
viewCard index cardID =
    let
        card =
            getCard cardID
    in
    div
        (classList [ ( "card", True ), ( "active", index > 11 ) ] :: Html5.DragDrop.draggable DragDropMsg cardID)
        [ div [ class "card-name" ] [ text card.name ]
        , div [ class "card-attack" ] [ text <| String.fromInt card.attack ]
        , div [ class "card-energy" ] [ text <| String.fromInt card.energy ]
        ]


viewSlot : ( Int, Maybe Int ) -> Html Msg
viewSlot ( slot, card ) =
    let
        cardDiv =
            case card of
                Just c ->
                    [ viewCard 12 c ]

                Nothing ->
                    []
    in
    div
        (class "slot" :: Html5.DragDrop.droppable DragDropMsg slot)
        cardDiv



-- ENCODE/DECODE


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "queue", Encode.list Encode.int model.queue )
        , ( "team", Encode.dict (\i -> String.fromInt i) (EncodeX.maybe Encode.int) model.team )
        ]


decoder : Decode.Decoder Model
decoder =
    Decode.map3 Model
        (Decode.field "queue" <| Decode.list Decode.int)
        (Decode.field "team" <| DecodeX.dict2 Decode.int <| Decode.nullable Decode.int)
        (Decode.succeed emptyCardDragDrop)
