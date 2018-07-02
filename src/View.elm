module View exposing (view)


import Default exposing (boardSize)
import Html
import Html.Events as HEvent
import Model exposing (..)
import Regex as Rgx
import Svg
import Svg.Attributes as Attr
import Svg.Events as SEvent
import Util exposing (scaleAngle)


view : Model -> Html.Html Msg
view { aimAngle, bullets, enemies, gameState, score } =
    Html.div []
             [ Svg.svg [ Attr.viewBox ("0 0 " ++ (toString boardSize) ++ " " ++ (toString boardSize))
                       , Attr.class "svg-container"
                       ]
                       ( (List.map viewBullet bullets)
                       ++ (List.map viewEnemies enemies)
                       ++ [ (viewTurret aimAngle) ]
                       ++ [ viewBackdropMask
                          , viewBackdrop
                          , toggleGameButton gameState
                          , viewScore score
                          ]
                       )
             , newGameButton gameState
             ]


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


viewBullet : Entity -> Html.Html Msg
viewBullet { transform } =
    let
        { origin, angle } = transform
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


viewEnemies : Entity -> Html.Html Msg
viewEnemies { transform } =
    let
        { origin, angle } = transform
        (x, y) = origin
    in
        Svg.circle [ Attr.cx <| toString x
                   , Attr.cy <| toString y
                   , Attr.r <| toString 30
                   , Attr.fill "green"
                   ] []


newGameButton : GameState -> Html.Html Msg
newGameButton gameState =
    case gameState of
        NotYetStarted ->
            Html.button [ Attr.class "new-game-button"
                        , HEvent.onClick StartGame
                        ]
                        [ Html.text "New Game" ]
        _->
            Html.text ""


toggleGameButton : GameState -> Html.Html Msg
toggleGameButton gameState =
    case gameState of
        Running ->
            pauseButton
        Paused ->
            resumeButton
        _->
            Svg.text ""


resumeButton : Html.Html Msg
resumeButton =
    Svg.g [ SEvent.onClick ResumeGame
          ]
          [ Svg.polygon [ Attr.points "10,10 50,30 10,50"
                        , Attr.fill "black"
                        ]
                        []
          , Svg.rect [ Attr.x "10"
                     , Attr.y "10"
                     , Attr.width "40"
                     , Attr.height "40"
                     , Attr.fill "black"
                     , Attr.opacity "0"
                     ]
                     []
          ]


pauseButton : Html.Html Msg
pauseButton =
  Svg.g [ SEvent.onClick PauseGame
        ]
        [ Svg.rect [ Attr.x "10"
                   , Attr.y "10"
                   , Attr.width "12"
                   , Attr.height "40"
                   , Attr.fill "black"
                   ]
                   []
        , Svg.rect [ Attr.x "38"
                   , Attr.y "10"
                   , Attr.width "12"
                   , Attr.height "40"
                   , Attr.fill "black"
                   ]
                   []
        , Svg.rect [ Attr.x "10"
                   , Attr.y "10"
                   , Attr.width "40"
                   , Attr.height "40"
                   , Attr.fill "black"
                   , Attr.opacity "0"
                   ]
                   []
        ]


addComas : Int -> String
addComas num =
    let
        numAsString = toString num
        rgx = Rgx.regex "(?=(?:\\d{3})+(?:\\.|$))"
    in
        String.join "," (Rgx.split Rgx.All rgx numAsString)


viewScore : Int -> Html.Html Msg
viewScore score =
    Svg.g []
          [ Svg.text_ [ Attr.textAnchor "end"
                      , Attr.x "790"
                      , Attr.y "20"
                      ]
                      [ Svg.text "Score" ]
          , Svg.text_ [ Attr.textAnchor "end"
                      , Attr.x "790"
                      , Attr.y "40"
                      ]
                      [ Svg.text (addComas score) ]
          ]
