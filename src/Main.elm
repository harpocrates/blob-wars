module Main exposing (main)

import GameState exposing (..)
import BlobRender exposing (..)

-- elm/core
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
view { currentPlayer, selectedBlob, blobBoard, boardSize } =
  let
    size = fromInt (cellSize * boardSize)
    renderedBoard = Svg.svg
      [ Svg.Attributes.width size, Svg.Attributes.height size
      , Svg.Attributes.viewBox ("0 0 " ++ size ++ " " ++ size) ]
      (List.concatMap (renderBlobSpace currentPlayer selectedBlob) blobBoard)
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
               [ Html.h3 [ Html.Attributes.style "color" currentPlayer ]
                         [ Html.text ("It is " ++ currentPlayer ++ "'s turn") ] 
               , renderedBoard
               , Html.br [] []
               , Html.button [ Html.Events.onClick PassTurn ] [ Html.text "Skip Turn" ]
               , Html.button [ Html.Events.onClick NewGame  ] [ Html.text "New Game"  ]
               ] ]

