module Cards exposing (Card, getCards)

import Dict exposing (Dict)


type alias Card =
    { name : String
    , attack : Int
    , energy : Int
    }


getCards : List Int -> List Card
getCards cardIDs =
    let
        getCard i =
            case Dict.get i deck of
                Just card ->
                    card

                Nothing ->
                    squid
    in
    List.map getCard cardIDs


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
    }


zebra : Card
zebra =
    { name = "Zebra"
    , attack = 3
    , energy = 3
    }


eagle : Card
eagle =
    { name = "Eagle"
    , attack = 6
    , energy = 5
    }
