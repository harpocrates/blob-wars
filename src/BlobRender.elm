module BlobRender exposing
  ( renderBlobSpace, tickingClock, cellSize )

import GameState exposing (..)

-- elm/core
import Array
import String exposing  ( fromInt, fromFloat )

-- elm/svg
import Svg exposing ( Svg )
import Svg.Attributes
import Svg.Events 

-- Tunable
cellSize = 60
cellSpace = 4

-- Derived from tunable
halfSize = cellSize // 2
halfSpace = cellSpace // 2
rectSize = cellSize - cellSpace
rectRounding = cellSpace

{-| We keep a top-level clock element that always sticks around. This gives us
a nice reliable tick event to latch on to. -}
tickingClock : Svg msg
tickingClock = Svg.circle [ Svg.Attributes.r "0" ]
  [ Svg.animate [ Svg.Attributes.id "clockTick"
                , Svg.Attributes.attributeType "XML"
                , Svg.Attributes.attributeName "r"
                , Svg.Attributes.from "0"
                , Svg.Attributes.to "0"
                , Svg.Attributes.begin ("0s; clockTick.end + 5")
                , Svg.Attributes.dur "1s"
                ] []
  ]

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
    rxSize  = halfSize - cellSpace * 2
    rxSize2 = halfSize - cellSpace * 3 // 2
    rySize  = halfSize - cellSpace
    rySize2 = halfSize - cellSpace // 2
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
        isAlerted = alertedFallback == (i,j)
        blobIdPrefix = "blob" ++ fromInt i ++ "at" ++ fromInt j
        blockBlinkingOffset = modBy 5 (i * 223 + j * 293)
        extraAttrs =
          if isAlerted then
             [ Svg.Attributes.stroke "black", Svg.Events.onClick UnSelectMe ]
          else if p == theirTurn then
             [ Svg.Events.onClick (SelectMe (i,j)) ]
          else 
             [ Svg.Events.onClick UnSelectMe ]
      in
      [ Svg.rect rectAttributes []
      , Svg.g (Svg.Attributes.transform ("translate(" ++ fromInt (jOffset + halfSize - halfSpace) ++ ","
                                                      ++ fromInt (iOffset + halfSize - halfSpace)
                                                      ++ ")" ++
                                         "scale(" ++ fromFloat (cellSize / 20) ++ ")")
                 :: extraAttrs)
              [ blob blobIdPrefix isAlerted p  blockBlinkingOffset ]
      ]


type alias IdPrefix = String
type alias Alerted  = Bool

{-| Produce a blinking blob centered between its eyes -}
blob : IdPrefix -> Alerted -> Player -> Int -> Svg msg
blob idPrefix alerted colour startBlinkingAt =
  let
    -- eyebrows and smirk
    alertedFeatures =
      if alerted then
        [ Svg.path [ Svg.Attributes.d "M 0.2,-2 L 2,-4" ] []
        , Svg.path [ Svg.Attributes.d "M-0.2,-2 L-2,-4" ] []
        , Svg.path [ Svg.Attributes.d "M-2,4 Q2,4 2,3z" ] []
        ]
      else
        []

    -- blinking eye
    eye idPref xPos = Svg.ellipse
      [ Svg.Attributes.cx xPos
      , Svg.Attributes.cy "0"
      , Svg.Attributes.rx "1.5"
      , Svg.Attributes.ry "2"
      ]
      [ Svg.animate [ Svg.Attributes.id (idPref ++ "Close")
                    , Svg.Attributes.attributeType "XML"
                    , Svg.Attributes.attributeName "ry"
                    , Svg.Attributes.from "2"
                    , Svg.Attributes.to "0.2"
                    , Svg.Attributes.begin ("clockTick.end + " ++ fromInt startBlinkingAt)
                    , Svg.Attributes.dur "0.15s"
                    ] []
      , Svg.animate [ Svg.Attributes.id (idPref ++ "Open")
                    , Svg.Attributes.attributeType "XML"
                    , Svg.Attributes.attributeName "ry"
                    , Svg.Attributes.from "0.2"
                    , Svg.Attributes.to "2"
                    , Svg.Attributes.begin (idPref ++ "Close.end")
                    , Svg.Attributes.dur "0.2s"
                    ] []
      ]

  in
  Svg.g []
    [ -- The actual blob
      Svg.path
        [ Svg.Attributes.d "M0,9 Q8,9 4,-2 Q0,-10 -4,-2 Q-8,9 0,9"
        , Svg.Attributes.fill colour
        ] []
    , -- Eyes
      Svg.g
        [ Svg.Attributes.fill "white"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "0.4"
        ]
        (alertedFeatures ++
          [ eye (idPrefix ++ "Left") "-2"
          , eye (idPrefix ++ "Right") "2"
          ])
    , -- Pupils
      Svg.g
        [ Svg.Attributes.fill "black"
        , Svg.Attributes.strokeWidth "0"
        ]
        [ Svg.circle [ Svg.Attributes.cx "-2", Svg.Attributes.cy "0", Svg.Attributes.r "0.6" ] []
        , Svg.circle [ Svg.Attributes.cx  "2", Svg.Attributes.cy "0", Svg.Attributes.r "0.6" ] []
        ]
    ]


