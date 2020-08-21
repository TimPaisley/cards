module Enemies exposing (Enemy, getEnemy)

import Dict exposing (Dict)


type alias Enemy =
    { name : String
    , attack : Int
    , energy : Int
    , health : Int
    }


getEnemy : Int -> Enemy
getEnemy id =
    case Dict.get id deck of
        Just card ->
            card

        Nothing ->
            ogre


deck : Dict Int Enemy
deck =
    Dict.fromList
        [ ( 0, ogre )
        , ( 1, hydra )
        , ( 2, titan )
        ]


ogre : Enemy
ogre =
    { name = "Ogre"
    , attack = 4
    , energy = 2
    , health = 300
    }


hydra : Enemy
hydra =
    { name = "Hydra"
    , attack = 6
    , energy = 3
    , health = 400
    }


titan : Enemy
titan =
    { name = "Titan"
    , attack = 3
    , energy = 3
    , health = 500
    }
