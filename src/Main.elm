port module Main exposing (Model, Msg(..), emptyModel, init, main, setStorage, update, updateWithStorage, view, viewCard, viewHero, viewQueue, viewTeam)

import Browser
import Cards exposing (Card, getCard)
import Enemies exposing (Enemy, getEnemy)
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
    , queue : List Card
    , team : List (Maybe Card)
    , stage : Stage
    , enemy : Maybe Enemy
    , cardDragDrop : CardDragDrop
    }


emptyModel : Model
emptyModel =
    { time = Time.millisToPosix 0
    , queue = []
    , team = List.repeat 4 Nothing
    , stage = Build
    , enemy = Nothing
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
    { dragDrop : Drag.Model Card Int
    , highlightedSlot : Maybe Int
    }


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
            ( emptyModel, Cmd.batch [ generateRandomQueue, generateRandomEnemy ] )



-- UPDATE


type Msg
    = NoOp
    | Tick Time.Posix
    | ResetModel
    | UpdateQueue (List Int)
    | UpdateEnemy Int
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
            ( emptyModel, Cmd.batch [ generateRandomQueue, generateRandomEnemy ] )

        UpdateQueue cardIDs ->
            ( { model | queue = List.map getCard cardIDs }
            , Cmd.none
            )

        UpdateEnemy enemyID ->
            ( { model | enemy = Just (getEnemy enemyID) }
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
                            ( removeLineFromQueue model.queue, ListX.setAt slot (Just card) model.team )

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
            case model.stage of
                Build ->
                    lazy viewQueue model.queue

                Battle ->
                    lazy viewEnemy (Maybe.withDefault (getEnemy 0) model.enemy)
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
        , ( "queue", Encode.list encodeCard model.queue )
        , ( "team", Encode.list (EncodeX.maybe encodeCard) model.team )
        , ( "stage", Encode.string (stageToString model.stage) )
        , ( "enemy", EncodeX.maybe encodeEnemy model.enemy )
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
    Decode.map6 Model
        (Decode.field "time" DecodeX.datetime)
        (Decode.field "queue" (Decode.list decodeCard))
        (Decode.field "team" decodeTeam)
        (decodeStage "stage")
        (Decode.field "enemy" (Decode.nullable decodeEnemy))
        (Decode.succeed emptyCardDragDrop)


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


decodeStage : String -> Decode.Decoder Stage
decodeStage stage =
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
