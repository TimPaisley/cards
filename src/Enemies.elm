module Enemies exposing (Enemy, getEnemy)

import List.Extra as ListX


type alias Enemy =
    { name : String
    , attack : Int
    , energy : Int
    , health : Int
    }


getEnemy : Int -> Enemy
getEnemy id =
    case ListX.getAt id deck of
        Just card ->
            card

        Nothing ->
            ogre


deck : List Enemy
deck =
    [ ogre, hydra, titan ]


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
