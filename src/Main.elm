port module Main exposing (main)


import AnimationFrame as Anim
import Default exposing (init)
import Html
import Keyboard
import Keyboard.Extra as Keys
import Model exposing (..)
import Mouse
import Time
import Update exposing (update)
import Util exposing (scaleAngle)
import View exposing (view)
import Window


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions { spawnFrequency, gameState } =
    Sub.batch
        [ Mouse.moves SetMousePosition
        , Window.resizes SetWindowSize
        , Mouse.clicks MouseShoot
        , Anim.diffs (\deltaTime -> Tick (deltaTime / 1000))
        , Keyboard.presses (handleKeyPresses gameState)
        , Time.every spawnFrequency (\_ -> SpawnEnemy)
        , windowBlur LoseFocus
        ]


handleKeyPresses : GameState -> Keyboard.KeyCode -> Msg
handleKeyPresses gameState keyCode =
    case (Keys.fromCode keyCode) of
        Keys.Space ->
            KeyboardShoot
        Keys.Escape ->
            case gameState of
                Running ->
                    PauseGame
                Paused ->
                    ResumeGame
                _ ->
                    NoOp
        _ ->
            NoOp


port windowBlur : (String -> msg) -> Sub msg
