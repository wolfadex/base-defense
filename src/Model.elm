module Model exposing (..)


import Mouse
import Random as Rand
import Time
import Window


---- MODEL ----


type EntityType
    = Bullet
    | SlowAndSteady
    | FastAndWeak
    | HulkingBrute


type alias Transform =
    { origin : (Float, Float)
    , angle : (Float, Float)
    }


type alias Entity =
    { transform : Transform
    , velocity : Float
    , eType : EntityType
    }


type GameState
    = NotYetStarted
    | Running
    | Paused
    | GameOver


type alias Model =
    { curorPosition : Mouse.Position
    , windowSize : Window.Size
    , aimAngle : (Float, Float)
    , bullets : List Entity
    , randSeed : Rand.Seed
    , spawnFrequency : Time.Time
    , enemies : List Entity
    , gameState : GameState
    , score : Int
    }


type Msg
    = NoOp
    | SetMousePosition Mouse.Position
    | SetWindowSize Window.Size
    | MouseShoot Mouse.Position
    | KeyboardShoot
    | Tick Time.Time
    | SetInitialSeed Time.Time
    | SpawnEnemy
    | StartGame
    | PauseGame
    | ResumeGame
    | ResetGame
    | LoseFocus String
    
