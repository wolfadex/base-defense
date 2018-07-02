module Default exposing (init, boardSize)


import Model exposing (..)
import Mouse
import Random as Rand
import Task
import Time
import Window


boardSize : Float
boardSize =
    800


possibleEnemies : List EntityType
possibleEnemies =
    [ SlowAndSteady
    , FastAndWeak
    , HulkingBrute
    ]


init : ( Model, Cmd Msg )
init =
    ( { curorPosition = Mouse.Position 0 0
      , windowSize = Window.Size 0 0
      , aimAngle = (0, 0)
      , bullets = []
      , randSeed = Rand.initialSeed 0
      , spawnFrequency = 2000
      , enemies = []
      , gameState = NotYetStarted
      , score = 0
      }
    , Cmd.batch [ Task.perform SetWindowSize Window.size
                , Task.perform SetInitialSeed Time.now
                ]
    )
