module Config exposing (config)


config : { gameWidth : number, gameHeight : number, gameSpeed : number, growthStartAt : number, growthRate : number }
config =
    { gameWidth = 32
    , gameHeight = 24
    , gameSpeed = 90 -- Lower number -> faster
    , growthStartAt = 10
    , growthRate = 180
    }
