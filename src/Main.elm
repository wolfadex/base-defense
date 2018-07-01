module Main exposing (main)


import AnimationFrame as Anim
import Debug exposing (log)
import Html
import Keyboard
import Keyboard.Extra as Keys
import Mouse
import Svg
import Svg.Attributes as Attr
import Task
import Time
import Window
-- import Css
-- import Html.Styled as Styled
-- import Html.Styled.Attributes as Attr
-- import Html.Styled.Events as Events
-- import Json.Decode as JsonD
-- import Random as Rand


---- MODEL ----

boardSize : Float
boardSize =
    800


type alias Bullet =
    { origin : (Float, Float)
    , angle : (Float, Float)
    }


type alias Model =
    { curorPosition : Mouse.Position
    , windowSize : Window.Size
    , aimAngle : (Float, Float)
    , bullets : List Bullet
    }


init : ( Model, Cmd Msg )
init =
    ( { curorPosition = Mouse.Position 0 0
      , windowSize = Window.Size 0 0
      , aimAngle = (1.0, 0.0)
      , bullets = []
      }
    , Cmd.batch [ Task.perform SetWindowSize Window.size
                ]
    )


scaleAngle : (Float, Float) -> Float -> (Float, Float)
scaleAngle (x, y) scale =
    (x * scale, y * scale)


---- UPDATE ----


type Msg
    = NoOp
    | SetMousePosition Mouse.Position
    | SetWindowSize Window.Size
    | MouseShoot Mouse.Position
    | KeyboardShoot
    | Tick Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        SetMousePosition position ->
            ( { model
              | curorPosition = position
              , aimAngle = calculateAimAngle model.windowSize position
              }
            , Cmd.none
            )
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
            let
                updatedBullets = List.map updateBullet model.bullets
                filteredBullets = List.filter filterBullets updatedBullets
            in
                ( { model | bullets = filteredBullets }, Cmd.none )


fireBullet : Model -> ( Model, Cmd Msg )
fireBullet model =
    let
        center = boardSize / 2
        newBullet =
            { origin = (center, center)
            , angle = model.aimAngle
            }
    in
        ( { model
          | bullets = newBullet :: model.bullets
          }
        , Cmd.none
        )


updateBullet : Bullet -> Bullet
updateBullet { origin, angle } =
    let
        (x, y) = origin
        (additionalX, additionalY) = scaleAngle angle 20
    in
        { origin = (x + additionalX, y + additionalY)
        , angle = angle
        }


filterBullets : Bullet -> Bool
filterBullets { origin } =
    let
        (x, y) = origin
        carl = log "Carl" ("X: " ++ (toString x) ++ ", Y: " ++ (toString y))
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


---- VIEW ----


view : Model -> Html.Html Msg
view { aimAngle, bullets } =
    Svg.svg [ Attr.viewBox ("0 0 " ++ (toString boardSize) ++ " " ++ (toString boardSize))
            , Attr.class "svg-container"
            ]
            ( (List.map viewBullet bullets)
            ++ [ (viewTurret aimAngle) ]
            ++ [ viewBackdropMask, viewBackdrop ]
            )


viewBackdropMask : Html.Html Msg
viewBackdropMask =
    Svg.mask [ Attr.id "backdropMask" ]
             [ Svg.rect [ Attr.x <| toString -boardSize
                        , Attr.y <| toString -boardSize
                        , Attr.width <| toString (boardSize * 3)
                        , Attr.height <| toString (boardSize * 3)
                        , Attr.fill "white"
                        ]
                        []
             , Svg.rect [ Attr.x "0"
                        , Attr.y "0"
                        , Attr.width <| toString boardSize
                        , Attr.height <| toString boardSize
                        , Attr.fill "black"
                        ]
                        []
             ]


viewBackdrop : Html.Html Msg
viewBackdrop =
    Svg.rect [ Attr.x <| toString -boardSize
             , Attr.y <| toString -boardSize
             , Attr.width <| toString (boardSize * 3)
             , Attr.height <| toString (boardSize * 3)
             , Attr.fill "rgb(200, 200, 200)"
             , Attr.mask "url(#backdropMask)"
             ]
             []


viewTurret : (Float, Float) -> Html.Html Msg
viewTurret aimAngle =
    let
        (x, y) = scaleAngle aimAngle 40
        center = boardSize / 2
    in
        Svg.g []
              [ Svg.circle [ Attr.cx <| toString center
                           , Attr.cy <| toString center
                           , Attr.r "20"
                           ]
                           []
               , Svg.line [ Attr.x1 <| toString center
                          , Attr.y1 <| toString center
                          , Attr.x2 <| toString (center + x)
                          , Attr.y2 <| toString (center + y)
                          , Attr.stroke "black"
                          , Attr.strokeWidth "8"
                          ] []
               ]


viewBullet : Bullet -> Html.Html Msg
viewBullet { origin, angle } =
    let
        (x, y) = scaleAngle angle 40
        (oX, oY) = origin
    in
        Svg.line [ Attr.x1 <| toString oX
                 , Attr.y1 <| toString oY
                 , Attr.x2 <| toString (oX + x)
                 , Attr.y2 <| toString (oY + y)
                 , Attr.stroke "red"
                 , Attr.strokeWidth "6"
                 ] []


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        -- , view = view >> Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves SetMousePosition
        , Window.resizes SetWindowSize
        , Mouse.clicks MouseShoot
        , Anim.diffs (\deltaTime -> Tick (deltaTime / 1000))
        , Keyboard.presses handleKeyPresses
        ]


handleKeyPresses : Keyboard.KeyCode -> Msg
handleKeyPresses keyCode =
    case (Keys.fromCode keyCode) of
        Keys.Space ->
            KeyboardShoot
        _ ->
            NoOp
