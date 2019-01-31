module BlobRender exposing
  ( renderBlobSpace, cellSize )

import GameState exposing (..)

-- elm/core
import Array
import String exposing  ( fromInt )

-- elm/svg
import Svg exposing ( Svg )
import Svg.Attributes
import Svg.Events 

-- Tunable
cellSize = 40
cellSpace = 4

-- Derived from tunable
halfSize = cellSize // 2
halfSpace = cellSpace // 2
rectSize = cellSize - cellSpace
rectRounding = cellSpace

{-| Render one cell on the blob board -}
renderBlobSpace : Player -> Maybe BlobIndex -> (BlobIndex, BlobSpace) -> List (Svg Msg)
renderBlobSpace theirTurn alerted ((i,j), bs) =
  let
    alertedFallback = Maybe.withDefault (-128,-128) alerted
    distToAlerted = distance (i,j) alertedFallback

    iOffset = i * cellSize
    jOffset = j * cellSize

    -- position, shape, round edges, color of a square (occupied or not)
    rectAttributes =
      [ Svg.Attributes.x (fromInt jOffset)     
      , Svg.Attributes.y (fromInt iOffset)
      , Svg.Attributes.rx (fromInt rectRounding)
      , Svg.Attributes.ry (fromInt rectRounding)
      , Svg.Attributes.width  (fromInt rectSize)
      , Svg.Attributes.height (fromInt rectSize) 
      , Svg.Attributes.fill "lightgray"
      ]

    -- position, shape, color of a blob
    blobAttributes p =
      [ Svg.Attributes.cx (fromInt (jOffset + halfSize - halfSpace))
      , Svg.Attributes.cy (fromInt (iOffset + halfSize - halfSpace))
      , Svg.Attributes.rx (fromInt (halfSize - cellSpace * 2))
      , Svg.Attributes.ry (fromInt (halfSize - cellSpace))
      , Svg.Attributes.fill p
      ]
  in
  case bs of
    -- Pretend these spots don't exist!
    Invalid ->
      []

    -- Render as a rectangle, but vary the onclick event (based on whether the
    -- alerted blob is nearby)
    Vacant ->
      let
        clickMsg = case distToAlerted of
                     1 -> SlideBlob alertedFallback (i,j)
                     2 -> LeapBlob alertedFallback (i,j)
                     _ -> UnSelectMe
      in
      [ Svg.rect (Svg.Events.onClick clickMsg :: rectAttributes) [] ]

    -- Render a rectangle under an ellipse. If the blob is the selected one,
    -- we need to support unselecting too
    Occupied p ->
      let
        extraAttrs =
          if alertedFallback == (i,j) then
             [ Svg.Attributes.stroke "black", Svg.Events.onClick UnSelectMe ]
          else if p == theirTurn then
             [ Svg.Events.onClick (SelectMe (i,j)) ]
          else 
             [ Svg.Events.onClick UnSelectMe ]
      in
      [ Svg.rect rectAttributes []
      , Svg.ellipse (extraAttrs ++ blobAttributes p) []
      ]



