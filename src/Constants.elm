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


pillShape : { square : String, circle : String, rectangle : String, oval : String }
pillShape =
    -- Must have corresponding CSS classes
    { square = "square"
    , circle = "circle"
    , rectangle = "rectangle"
    , oval = "oval"
    }


allPillShapes : List String
allPillShapes =
    [ pillShape.square, pillShape.circle, pillShape.rectangle, pillShape.oval ]


pillRotations : { min : number, max : number, multiplier : number }
pillRotations =
    { min = 1
    , max = 10
    , multiplier = 5
    }
