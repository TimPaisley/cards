module Cards exposing (Card, decodeCard, encodeCard, getCard)

import Abilities exposing (Ability(..), decodeAbility, encodeAbility)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra as EncodeX
import List.Extra as ListX


type alias Card =
    { name : String
    , attack : Int
    , energy : Int
    , clock : Int
    , ability : Maybe Ability
    }


decodeCard : Decode.Decoder Card
decodeCard =
    Decode.map5 Card
        (Decode.field "name" Decode.string)
        (Decode.field "attack" Decode.int)
        (Decode.field "energy" Decode.int)
        (Decode.field "clock" Decode.int)
        (Decode.field "ability" (Decode.nullable decodeAbility))


encodeCard : Card -> Encode.Value
encodeCard card =
    Encode.object
        [ ( "name", Encode.string card.name )
        , ( "attack", Encode.int card.attack )
        , ( "energy", Encode.int card.energy )
        , ( "clock", Encode.int card.clock )
        , ( "ability", EncodeX.maybe encodeAbility card.ability )
        ]


getCard : Int -> Card
getCard id =
    case ListX.getAt id deck of
        Just card ->
            card

        Nothing ->
            lion


deck : List Card
deck =
    [ lion
    , meerkat
    , rhinoceros
    ]


lion : Card
lion =
    { name = "Lion"
    , attack = 4
    , energy = 2
    , clock = 0
    , ability = Just <| Orchestration 2
    }


meerkat : Card
meerkat =
    { name = "Meerkat"
    , attack = 3
    , energy = 3
    , clock = 0
    , ability = Just <| Unity 2
    }


rhinoceros : Card
rhinoceros =
    { name = "Rhinoceros"
    , attack = 6
    , energy = 5
    , clock = 0
    , ability = Just <| Momentum 1
    }
