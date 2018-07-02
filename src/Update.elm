module Update exposing (update)


import Default exposing (..)
import Model exposing (..)
import Mouse
import Random as Rand
import Util exposing (scaleAngle)
import Window


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        SetMousePosition position ->
            case model.gameState of
                Running ->
                    ( { model
                      | curorPosition = position
                      , aimAngle = calculateAimAngle model.windowSize position
                      }
                    , Cmd.none
                    )
                _->
                    ( model, Cmd.none )
        SetWindowSize windowSize ->
            ( { model
              | windowSize = windowSize
              , aimAngle = calculateAimAngle windowSize model.curorPosition
              }
            , Cmd.none
            )
        MouseShoot _ ->
            fireBullet model
        KeyboardShoot ->
            fireBullet model
        Tick deltaTime ->
            case model.gameState of
                Running ->
                    let
                        updatedBullets = List.map moveEntity model.bullets
                        filteredBullets = List.filter filterBullets updatedBullets

                        updatedEnemies = List.map moveEntity model.enemies
                    in
                        ( { model
                          | bullets = filteredBullets
                          , enemies = updatedEnemies
                          }
                        , Cmd.none
                        )
                _ ->
                    ( model, Cmd.none )
        SetInitialSeed time ->
            ( { model | randSeed = Rand.initialSeed (floor time) }
            , Cmd.none
            )
        SpawnEnemy ->
            case model.gameState of
                Running ->
                    let
                        (newEnemy, newSeed) = spawnEnemy model.randSeed
                    in
                        ( { model
                          | randSeed = newSeed
                          , enemies = newEnemy :: model.enemies
                          }
                        , Cmd.none
                        )
                _->
                    ( model, Cmd.none )
        StartGame ->
            case model.gameState of
                NotYetStarted ->
                    ( { model | gameState = Running }, Cmd.none )
                _->
                    ( model, Cmd.none )
        PauseGame ->
            case model.gameState of
                Running ->
                    ( { model | gameState = Paused }, Cmd.none )
                _->
                    ( model, Cmd.none )
        ResumeGame ->
            case model.gameState of
                Paused ->
                    ( { model | gameState = Running }, Cmd.none )
                _->
                    ( model, Cmd.none )
        ResetGame ->
            case model.gameState of
                NotYetStarted ->
                    ( { model | gameState = NotYetStarted }, Cmd.none )
                _->
                    ( model, Cmd.none )
        LoseFocus _ ->
            case model.gameState of
                Running ->
                    ( { model | gameState = Paused }, Cmd.none )
                _->
                    ( model, Cmd.none )


spawnEnemy : Rand.Seed -> (Entity, Rand.Seed)
spawnEnemy seed =
    let
        center = boardSize / 2
        ( angle, seed1 ) = Rand.step (Rand.float 0 (2 * pi)) seed
        -- 580 = sqrt ((400 ^ 2) + (400 ^ 2)), plus a little extra
        x = center + 580 * (cos angle)
        y = center + 580 * (sin angle)
    in
        ( { transform = { origin = (x, y)
                        , angle = angleToCenter x y
                        }
          , velocity = 2.5
          , eType = SlowAndSteady
          }
        , seed1
        )


angleToCenter : Float -> Float -> (Float, Float)
angleToCenter x y =
    let
        center = boardSize / 2

        dX = center - x
        dY = center - y

        scaleFactor = sqrt ((dX ^ 2) + (dY ^ 2))
    in
        (dX / scaleFactor, dY / scaleFactor)


fireBullet : Model -> ( Model, Cmd Msg )
fireBullet model =
    case model.gameState of
        Running ->
            let
                center = boardSize / 2
                newBullet =
                    { transform = { origin = (center, center)
                                  , angle = model.aimAngle
                                  }
                    , velocity = 20
                    , eType = Bullet
                    }
            in
                ( { model
                  | bullets = newBullet :: model.bullets
                  }
                , Cmd.none
                )
        _->
            ( model, Cmd.none )


moveEntity : Entity -> Entity
moveEntity entity =
    let
        { transform, velocity } = entity
        { origin, angle } = transform
        (x, y) = origin
        (additionalX, additionalY) = scaleAngle angle velocity
    in
        { entity
        | transform = { origin = (x + additionalX, y + additionalY)
                      , angle = angle
                      }
        }


filterBullets : Entity -> Bool
filterBullets { transform } =
    let
        { origin } = transform
        (x, y) = origin
    in
        x >= 0 &&
        x <= boardSize &&
        y >= 0 &&
        y <= boardSize


calculateAimAngle : Window.Size -> Mouse.Position -> (Float, Float)
calculateAimAngle { width, height } { x, y } =
    let
        centerX = (toFloat width) / 2
        centerY = (toFloat height) / 2

        offsetX = (toFloat x) - centerX
        offsetY = (toFloat y) - centerY

        scaleFactor = sqrt ((offsetX ^ 2) + (offsetY ^ 2))
    in
        (offsetX / scaleFactor, offsetY / scaleFactor)
