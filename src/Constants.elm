module Constants exposing (..)


config : { gameWidth : number, gameHeight : number, gameSpeed : number, growthStartAt : number, growthRate : number }
config =
    { gameWidth = 32
    , gameHeight = 24
    , gameSpeed = 90 -- Lower number -> faster
    , growthStartAt = 10
    , growthRate = 180
    }


pillColor : { green : String, blue : String, yellow : String, pink : String, teal : String }
pillColor =
    -- Must have corresponding CSS classes
    { green = "green"
    , blue = "blue"
    , yellow = "yellow"
    , pink = "pink"
    , teal = "teal"
    }


allPillColors : List String
allPillColors =
    [ pillColor.green, pillColor.blue, pillColor.yellow, pillColor.pink, pillColor.teal ]


pillRotations : { min : number, max : number, multiplier : number }
pillRotations =
    { min = 1
    , max = 8
    , multiplier = 10
    }
