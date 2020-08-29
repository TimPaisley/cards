module Cards exposing (Card, getCard)

import Dict exposing (Dict)


type alias Card =
    { name : String
    , attack : Int
    , energy : Int
    , clock : Int
    }


getCard : Int -> Card
getCard id =
    case Dict.get id deck of
        Just card ->
            card

        Nothing ->
            squid


deck : Dict Int Card
deck =
    Dict.fromList
        [ ( 0, squid )
        , ( 1, zebra )
        , ( 2, eagle )
        ]


squid : Card
squid =
    { name = "Squid"
    , attack = 4
    , energy = 2
    , clock = 0
    }


zebra : Card
zebra =
    { name = "Zebra"
    , attack = 3
    , energy = 3
    , clock = 0
    }


eagle : Card
eagle =
    { name = "Eagle"
    , attack = 6
    , energy = 5
    , clock = 0
    }
