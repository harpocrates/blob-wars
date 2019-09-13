module Main exposing (main)

import GameState exposing (..)
import BlobRender exposing (..)

-- elm/core
import Array exposing ( Array )
import Dict exposing ( get )
import String exposing  ( fromInt )

-- elm/browser
import Browser

-- elm/html
import Html exposing ( Html )
import Html.Events
import Html.Attributes

-- elm/svg
import Svg
import Svg.Attributes


main : Program () GameState Msg
main = Browser.sandbox { init = initialGameState, update = update, view = view }

{-| Update the game state -}
update : Msg -> GameState -> GameState
update msg ({ currentPlayer } as model) =
  case msg of
    NewGame          -> initialGameState

    UnSelectMe       -> model |> setAlertBlob Nothing
    SelectMe ix      -> model |> setAlertBlob (Just ix)
    PassTurn         -> model |> advancePlayer

    SlideBlob _ to   -> model
                          |> setAlertBlob Nothing
                          |> advancePlayer
                          |> mapBoard (convertCell to currentPlayer)

    LeapBlob from to -> model
                          |> setAlertBlob Nothing
                          |> advancePlayer
                          |> mapBoard (setCell from Vacant >>
                                       convertCell to currentPlayer)

{-| Render the game state into a div -}
view : GameState -> Html Msg
view { players, currentPlayer, selectedBlob, blobBoard, boardHeight, boardWidth } =
  let
    sizeHeight = fromInt (cellSize * boardHeight)
    sizeWidth = fromInt (cellSize * boardWidth)
    scores = scoreBoard blobBoard

    renderedBoard : Html Msg
    renderedBoard = Svg.svg
      [ Svg.Attributes.width sizeWidth, Svg.Attributes.height sizeHeight
      , Svg.Attributes.viewBox ("0 0 " ++ sizeWidth ++ " " ++ sizeHeight) ]
      (tickingClock
        :: List.concatMap (renderBlobSpace currentPlayer selectedBlob) blobBoard)

    renderScore : Player -> Int -> Html msg
    renderScore player score = Html.span [ Html.Attributes.style "color" player ]
                                         [ Html.text (" " ++ fromInt score) ]

    renderedScores : List (Html msg)
    renderedScores =
      Array.toList players
        |> List.filterMap (\p -> Maybe.map (renderScore p) (Dict.get p scores))
  in
  Html.div
    [ Html.Attributes.style "font-family"     "sans-serif"
    , Html.Attributes.style "position"        "fixed"
    , Html.Attributes.style "display"         "flex"
    , Html.Attributes.style "align-items"     "center"
    , Html.Attributes.style "justify-content" "center"
    , Html.Attributes.style "width"           "100%"
    , Html.Attributes.style "height"          "100%"
    ]
    [ Html.div []
               [ Html.div [ Html.Attributes.style "width" "100%" ]
                          [ Html.h3 [ Html.Attributes.style "float" "left"
                                    , Html.Attributes.style "display" "inline"
                                    , Html.Attributes.style "color" currentPlayer
                                    ]
                                    [ Html.text ("It is " ++ currentPlayer ++ "'s turn") ]
                          , Html.h3 [ Html.Attributes.style "float" "right"
                                    , Html.Attributes.style "display" "inline"
                                    ]
                                    renderedScores
                          ]
               , renderedBoard
               , Html.br [] []
               , Html.button [ Html.Events.onClick PassTurn ] [ Html.text "Skip Turn" ]
               , Html.button [ Html.Events.onClick NewGame  ] [ Html.text "New Game"  ]
               ] ]

