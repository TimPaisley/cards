port module Main exposing (Model, Msg(..), emptyModel, init, main, setStorage, update, updateWithStorage, view, viewCard, viewHero, viewQueue, viewTeam)

import Browser
import Cards exposing (getCard)
import Dict exposing (Dict)
import Enemies exposing (getEnemy)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Html5.DragDrop as Drag
import Json.Decode as Decode
import Json.Decode.Extra as DecodeX
import Json.Encode as Encode
import Json.Encode.Extra as EncodeX
import List
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Random
import Time


main : Program Encode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Cards", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = subscriptions
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
    { time : Time.Posix
    , queue : List Int
    , team : Dict Int (Maybe Int)
    , stage : Stage
    , enemy : Int
    , cardDragDrop : CardDragDrop
    }


emptyModel : Model
emptyModel =
    { time = Time.millisToPosix 0
    , queue = []
    , team = Dict.fromList [ ( 1, Nothing ), ( 2, Nothing ), ( 3, Nothing ), ( 4, Nothing ) ]
    , stage = Build
    , enemy = 0
    , cardDragDrop = emptyCardDragDrop
    }


generateRandomQueue : Cmd Msg
generateRandomQueue =
    Random.list 20 (Random.int 0 2)
        |> Random.generate UpdateQueue


generateRandomEnemy : Cmd Msg
generateRandomEnemy =
    Random.int 0 2
        |> Random.generate UpdateEnemy


type Stage
    = Build
    | Battle


type alias CardDragDrop =
    { dragDrop : Drag.Model Int Int
    , highlightedSlot : Maybe Int
    }


emptyCardDragDrop : CardDragDrop
emptyCardDragDrop =
    { dragDrop = Drag.init
    , highlightedSlot = Nothing
    }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue decoder flags of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( emptyModel, Cmd.batch [ generateRandomQueue, generateRandomEnemy ] )



-- UPDATE


type Msg
    = NoOp
    | Tick Time.Posix
    | ResetModel
    | UpdateQueue (List Int)
    | UpdateEnemy Int
    | DragDropMsg (Drag.Msg Int Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        ResetModel ->
            ( emptyModel, Cmd.batch [ generateRandomQueue, generateRandomEnemy ] )

        UpdateQueue cardIDs ->
            ( { model | queue = cardIDs }
            , Cmd.none
            )

        UpdateEnemy enemyID ->
            ( { model | enemy = enemyID }
            , Cmd.none
            )

        DragDropMsg msg_ ->
            let
                ( newDragDrop, result ) =
                    Drag.update msg_ model.cardDragDrop.dragDrop

                newCardDragDrop =
                    case ( Drag.getDragId newDragDrop, Drag.getDropId newDragDrop ) of
                        ( Just _, dropId ) ->
                            case result of
                                Just _ ->
                                    { dragDrop = newDragDrop, highlightedSlot = Nothing }

                                Nothing ->
                                    { dragDrop = newDragDrop, highlightedSlot = dropId }

                        _ ->
                            model.cardDragDrop

                ( newQueue, newTeam ) =
                    case result of
                        Just ( card, slot, _ ) ->
                            ( removeLineFromQueue model.queue, Dict.insert slot (Just card) model.team )

                        Nothing ->
                            ( model.queue, model.team )

                newStage =
                    case model.stage of
                        Build ->
                            if List.length newQueue == 0 then
                                Battle

                            else
                                Build

                        Battle ->
                            Battle
            in
            ( { model | queue = newQueue, team = newTeam, stage = newStage, cardDragDrop = newCardDragDrop }
            , Cmd.none
            )


removeLineFromQueue : List Int -> List Int
removeLineFromQueue queue =
    List.drop 4 queue



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewStage =
            case model.stage of
                Build ->
                    lazy viewQueue model.queue

                Battle ->
                    lazy viewEnemy model.enemy
    in
    div
        [ class "cards-wrapper" ]
        [ div
            [ class "game" ]
            [ lazy viewHeader model.stage
            , viewStage
            , viewHero
            , lazy2 viewTeam model.team model.cardDragDrop.highlightedSlot
            ]
        ]


viewHeader : Stage -> Html Msg
viewHeader stage =
    div
        [ class "header" ]
        [ div [] [ text "Cards" ]
        , div [] [ text <| stageToString stage ++ " Phase" ]
        , div [ onClick ResetModel ] [ text "Reset" ]
        ]


viewQueue : List Int -> Html Msg
viewQueue queue =
    let
        groups =
            ListX.groupsOf 4 queue
    in
    div [ class "stage" ]
        (List.take 4 groups |> List.indexedMap viewGroup)


viewHero : Html Msg
viewHero =
    div
        [ class "hero" ]
        [ div [ class "hero-points" ] [ text <| String.fromInt 45 ]
        , div [ class "hero-portrait" ] []
        , div [ class "hero-health" ] [ text <| String.fromInt 12 ]
        ]


viewEnemy : Int -> Html Msg
viewEnemy enemyID =
    let
        enemy =
            getEnemy enemyID
    in
    div
        [ class "stage" ]
        [ div
            [ class "enemy" ]
            [ div [ class "portrait" ] [ text enemy.name ]
            , div [ class "health" ] [ text <| String.fromInt enemy.health ]
            ]
        ]


viewGroup : Int -> List Int -> Html Msg
viewGroup index group =
    div
        [ classList
            [ ( "group", True )
            , ( "active", index == 0 )
            ]
        , style "bottom" <| String.fromInt (index * 100) ++ "px"
        ]
        (List.map (viewCard <| index == 0) group)


viewTeam : Dict Int (Maybe Int) -> Maybe Int -> Html Msg
viewTeam team highlightedSlot =
    div [ class "team" ] (Dict.toList team |> List.map (viewSlot highlightedSlot))


viewCard : Bool -> Int -> Html Msg
viewCard active cardID =
    let
        card =
            getCard cardID

        draggable =
            if active then
                Drag.draggable DragDropMsg cardID

            else
                []
    in
    div
        (class "card" :: draggable)
        [ div [ class "card-name" ] [ text card.name ]
        , div [ class "card-attack" ] [ text <| String.fromInt card.attack ]
        , div [ class "card-energy" ] [ text <| String.fromInt card.energy ]
        ]


viewSlot : Maybe Int -> ( Int, Maybe Int ) -> Html Msg
viewSlot highlightedSlot ( slot, card ) =
    let
        cardDiv =
            case card of
                Just c ->
                    [ viewCard False c ]

                Nothing ->
                    []
    in
    div
        (classList
            [ ( "highlighted", highlightedSlot == Just slot )
            , ( "active", MaybeX.isJust card )
            , ( "slot", True )
            ]
            :: Drag.droppable DragDropMsg slot
        )
        cardDiv



-- ENCODE/DECODE


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "time", Encode.int <| Time.posixToMillis model.time )
        , ( "queue", Encode.list Encode.int model.queue )
        , ( "team", Encode.dict (\i -> String.fromInt i) (EncodeX.maybe Encode.int) model.team )
        , ( "stage", Encode.string <| stageToString model.stage )
        , ( "enemy", Encode.int <| model.enemy )
        ]


decoder : Decode.Decoder Model
decoder =
    Decode.map6 Model
        (Decode.field "time" <| DecodeX.datetime)
        (Decode.field "queue" <| Decode.list Decode.int)
        (Decode.field "team" <| DecodeX.dict2 Decode.int <| Decode.nullable Decode.int)
        (stageDecoder "stage")
        (Decode.field "enemy" <| Decode.int)
        (Decode.succeed emptyCardDragDrop)


stageDecoder : String -> Decode.Decoder Stage
stageDecoder stage =
    case stage of
        "Build" ->
            Decode.succeed Build

        "Battle" ->
            Decode.succeed Battle

        _ ->
            Decode.fail (stage ++ " is not a recognised Stage")



-- HELPERS


stageToString : Stage -> String
stageToString stage =
    case stage of
        Build ->
            "Build"

        Battle ->
            "Battle"
