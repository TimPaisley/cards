port module Main exposing (Model, Msg(..), emptyModel, init, main, setStorage, update, updateWithStorage, view, viewCard, viewHero, viewQueue, viewTeam)

import Browser
import Cards exposing (Card, getCard)
import Enemies exposing (Enemy, getEnemy)
import Html exposing (Html, button, div, text)
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
    , randoms : Maybe Randoms
    , team : List (Maybe Card)
    , phase : Phase
    , cardDragDrop : CardDragDrop
    }


type alias Randoms =
    { queue : List Int
    , enemy : Int
    }


type Phase
    = Introduction
    | Build (List Card)
    | Battle Enemy


type alias CardDragDrop =
    { dragDrop : Drag.Model Card Int
    , highlightedSlot : Maybe Int
    }


emptyModel : Model
emptyModel =
    { time = Time.millisToPosix 0
    , randoms = Nothing
    , team = List.repeat 4 Nothing
    , phase = Introduction
    , cardDragDrop = emptyCardDragDrop
    }


generateRandoms : Cmd Msg
generateRandoms =
    let
        createRandoms queue enemy =
            { queue = queue
            , enemy = enemy
            }
    in
    Random.map2
        createRandoms
        (Random.list 20 (Random.int 0 2))
        (Random.int 0 2)
        |> Random.generate UpdateRandoms


emptyCardDragDrop : CardDragDrop
emptyCardDragDrop =
    { dragDrop = Drag.init
    , highlightedSlot = Nothing
    }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue decode flags of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( emptyModel, generateRandoms )



-- UPDATE


type Msg
    = NoOp
    | Tick Time.Posix
    | ResetModel
    | UpdateRandoms Randoms
    | StartGame
    | DragDropMsg (Drag.Msg Card Int)


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
            ( emptyModel, generateRandoms )

        UpdateRandoms randoms ->
            ( { model | randoms = Just randoms }, Cmd.none )

        StartGame ->
            case (model.phase, model.randoms) of
                (Introduction, Just randoms) ->
                    let
                        queue =
                            List.map getCard randoms.queue
                    in
                    ( { model | phase = Build queue }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        DragDropMsg msg_ ->
            case ( model.phase, model.randoms ) of
                ( Build queue, Just randoms ) ->
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
                            case ( result, model.phase ) of
                                ( Just ( card, slot, _ ), Build q ) ->
                                    ( removeLineFromQueue q, ListX.setAt slot (Just card) model.team )

                                _ ->
                                    ( queue, model.team )

                        newPhase =
                            if List.length newQueue == 0 then
                                Battle (getEnemy randoms.enemy)

                            else
                                Build newQueue
                    in
                    ( { model | phase = newPhase, team = newTeam, cardDragDrop = newCardDragDrop }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


removeLineFromQueue : List Card -> List Card
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
            case model.phase of
                Introduction ->
                    viewIntroduction

                Build queue ->
                    lazy viewQueue queue

                Battle enemy ->
                    lazy viewEnemy enemy
    in
    div
        [ class "cards-wrapper" ]
        [ div
            [ class "game" ]
            [ lazy viewHeader model.phase
            , viewStage
            , viewHero
            , lazy2 viewTeam model.team model.cardDragDrop.highlightedSlot
            ]
        ]


viewHeader : Phase -> Html Msg
viewHeader phase =
    div
        [ class "header" ]
        [ div [] [ text "Cards" ]
        , div [] [ text <| phaseToString phase ++ " Phase" ]
        , div [ onClick ResetModel ] [ text "Reset" ]
        ]


viewIntroduction : Html Msg
viewIntroduction =
    div [ class "stage" ]
        [ text "Welcome to Cards"
        , button [ onClick StartGame ] [ text "Start Game" ]
        ]


viewQueue : List Card -> Html Msg
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


viewEnemy : Enemy -> Html Msg
viewEnemy enemy =
    div
        [ class "stage" ]
        [ div
            [ class "enemy" ]
            [ div [ class "portrait" ] [ text enemy.name ]
            , div [ class "health" ] [ text <| String.fromInt enemy.health ]
            ]
        ]


viewGroup : Int -> List Card -> Html Msg
viewGroup index group =
    div
        [ classList
            [ ( "group", True )
            , ( "active", index == 0 )
            ]
        , style "bottom" <| String.fromInt (index * 100) ++ "px"
        ]
        (List.map (viewCard <| index == 0) group)


viewTeam : List (Maybe Card) -> Maybe Int -> Html Msg
viewTeam team highlightedSlot =
    div [ class "team" ] (List.indexedMap (viewSlot highlightedSlot) team)


viewCard : Bool -> Card -> Html Msg
viewCard active card =
    let
        draggable =
            if active then
                Drag.draggable DragDropMsg card

            else
                []

        clock =
            toFloat card.clock / toFloat card.energy * 100
    in
    div
        (class "card" :: draggable)
        [ div [ class "name" ] [ text card.name ]
        , div [ class "attack" ] [ text <| String.fromInt card.attack ]
        , div [ class "energy" ] [ text <| String.fromInt card.energy ]
        , div [ class "clock", style "height" (String.fromFloat clock ++ "%") ] []
        ]


viewSlot : Maybe Int -> Int -> Maybe Card -> Html Msg
viewSlot highlightedSlot slot card =
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
        [ ( "time", Encode.int (Time.posixToMillis model.time) )
        , ( "randoms", EncodeX.maybe encodeRandoms model.randoms )
        , ( "team", Encode.list (EncodeX.maybe encodeCard) model.team )
        , ( "phase", encodePhase model.phase )
        ]


encodeRandoms : Randoms -> Encode.Value
encodeRandoms randoms =
    Encode.object
        [ ( "queue", Encode.list Encode.int randoms.queue )
        , ( "enemy", Encode.int randoms.enemy )
        ]


encodePhase : Phase -> Encode.Value
encodePhase phase =
    case phase of
        Introduction ->
            Encode.object
                [ ( "name", Encode.string "introduction" )
                ]

        Build queue ->
            Encode.object
                [ ( "name", Encode.string "build" )
                , ( "queue", Encode.list encodeCard queue )
                ]

        Battle enemy ->
            Encode.object
                [ ( "name", Encode.string "battle" )
                , ( "enemy", encodeEnemy enemy )
                ]


encodeCard : Card -> Encode.Value
encodeCard card =
    Encode.object
        [ ( "name", Encode.string card.name )
        , ( "attack", Encode.int card.attack )
        , ( "energy", Encode.int card.energy )
        , ( "clock", Encode.int card.clock )
        ]


encodeEnemy : Enemy -> Encode.Value
encodeEnemy enemy =
    Encode.object
        [ ( "name", Encode.string enemy.name )
        , ( "attack", Encode.int enemy.attack )
        , ( "energy", Encode.int enemy.energy )
        , ( "health", Encode.int enemy.health )
        ]


decode : Decode.Decoder Model
decode =
    Decode.map5 Model
        (Decode.field "time" DecodeX.datetime)
        (Decode.field "randoms" (Decode.nullable decodeRandoms))
        (Decode.field "team" decodeTeam)
        (Decode.field "phase" decodePhase)
        (Decode.succeed emptyCardDragDrop)


decodeRandoms : Decode.Decoder Randoms
decodeRandoms =
    Decode.map2 Randoms
        (Decode.field "queue" (Decode.list Decode.int))
        (Decode.field "enemy" Decode.int)


decodeTeam : Decode.Decoder (List (Maybe Card))
decodeTeam =
    Decode.nullable decodeCard
        |> Decode.list


decodeCard : Decode.Decoder Card
decodeCard =
    Decode.map4 Card
        (Decode.field "name" Decode.string)
        (Decode.field "attack" Decode.int)
        (Decode.field "energy" Decode.int)
        (Decode.field "clock" Decode.int)


decodeEnemy : Decode.Decoder Enemy
decodeEnemy =
    Decode.map4 Enemy
        (Decode.field "name" Decode.string)
        (Decode.field "attack" Decode.int)
        (Decode.field "energy" Decode.int)
        (Decode.field "health" Decode.int)


decodePhase : Decode.Decoder Phase
decodePhase =
    let
        decodePhaseAssociations phase =
            case phase of
                "introduction" ->
                    Decode.succeed Introduction

                "build" ->
                    Decode.field "queue" (Decode.list Decode.int)
                        |> Decode.map (List.map getCard)
                        |> Decode.map Build

                "battle" ->
                    Decode.field "enemy" Decode.int
                        |> Decode.map getEnemy
                        |> Decode.map Battle

                _ ->
                    Decode.fail (phase ++ " is not a recognised Phase")
    in
    Decode.field "name" Decode.string
        |> Decode.andThen decodePhaseAssociations



-- HELPERS


phaseToString : Phase -> String
phaseToString stage =
    case stage of
        Introduction ->
            "Introduction"

        Build _ ->
            "Build"

        Battle _ ->
            "Battle"
