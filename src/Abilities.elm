module Abilities exposing (Ability(..), abilityToString, decodeAbility, encodeAbility)

import Json.Decode as Decode
import Json.Encode as Encode


type Ability
    = Orchestration Int
    | Unity Int
    | Momentum Int


decodeAbility : Decode.Decoder Ability
decodeAbility =
    let
        decodeFullAbility ( name, value ) =
            case name of
                "Orchestration" ->
                    Decode.succeed (Orchestration value)

                "Unity" ->
                    Decode.succeed (Unity value)

                "Momentum" ->
                    Decode.succeed (Momentum value)

                _ ->
                    Decode.fail (name ++ " is not a recognised Ability")

        decodeName =
            Decode.field "name" Decode.string

        decodeValue =
            Decode.field "value" Decode.int
    in
    Decode.map2 Tuple.pair decodeName decodeValue
        |> Decode.andThen decodeFullAbility


encodeAbility : Ability -> Encode.Value
encodeAbility ability =
    Encode.object
        [ ( "name", Encode.string (Tuple.first <| abilityToString ability) )
        , ( "value", Encode.int (Tuple.second <| abilityToString ability) )
        ]


abilityToString : Ability -> ( String, Int )
abilityToString ability =
    case ability of
        Orchestration val ->
            ( "Orchestration", val )

        Unity val ->
            ( "Unity", val )

        Momentum val ->
            ( "Momentum", val )
