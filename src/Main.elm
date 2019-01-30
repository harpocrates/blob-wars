-- elm/core
import Array exposing ( Array )
import String exposing  ( fromInt )

-- elm/browser
import Browser

-- elm/html
import Html exposing ( Html )
import Html.Events
import Html.Attributes

-- elm/svg
import Svg exposing ( Svg )
import Svg.Attributes
import Svg.Events 


main : Program () GameState Msg
main = Browser.sandbox { init = initialGameState, update = update, view = view }


-- * The game state
---------------------

{-| State kept by the game -}
type alias GameState =
     { players       : Array Player
     , turnNumber    : Int
     , currentPlayer : Player          {- must be in 'players'!   -}
     , selectedBlob  : Maybe BlobIndex {- must be in 'blobBoard'! -}
     , blobBoard     : BlobBoard
     , boardSize     : Int             {- sidelength of board     -} 
     }

{-| A player is a color name (which must be recognized in HTML) -}
type alias Player = String

{-| A list of all the cells in the board (where blob indices are distinct) -}
type alias BlobBoard = List (BlobIndex, BlobSpace)

{-| A position in the blob board -}
type alias BlobIndex = (Int, Int)

{-| Represents a square in the blob board -}
type BlobSpace
     = Invalid
     | Vacant
     | Occupied Player


-- * Sample initial game state
------------------

{-| Helper to convert from nested lists into 'BlobBoard' -} 
boardCells : List (List BlobSpace) -> List (BlobIndex, BlobSpace)
boardCells = List.indexedMap (\i -> List.indexedMap (\j cell -> ((i,j), cell)))
          >> List.concat

-- We could easily accomodate more than 2 players...
blue = Occupied "blue"
red  = Occupied "red"

-- We could get much more creative with boards...
board =
  boardCells [ [ blue,    Vacant, Vacant, Vacant,  Vacant, Vacant, Invalid ]
             , [ Vacant,  Vacant, Vacant, Vacant,  Vacant, Vacant, Vacant  ]
             , [ Vacant,  Vacant, Vacant, Vacant,  Vacant, Vacant, Vacant  ]
             , [ Vacant,  Vacant, Vacant, Invalid, Vacant, Vacant, Vacant  ]
             , [ Vacant,  Vacant, Vacant, Vacant,  Vacant, Vacant, Vacant  ]
             , [ Vacant,  Vacant, Vacant, Vacant,  Vacant, Vacant, Vacant  ]
             , [ Invalid, Vacant, Vacant, Vacant,  Vacant, Vacant, red     ] ]


initialGameState = { players = Array.fromList ["blue", "red"]
                   , turnNumber = 0
                   , currentPlayer = "blue"
                   , selectedBlob = Nothing
                   , blobBoard = board
                   , boardSize = 7 }


-- * Mutating game state
------------------------

{-| Maximum of the vertical and horizontal differences -}
distance : BlobIndex -> BlobIndex -> Int
distance (x1,y1) (x2,y2) = Basics.max (abs (x1 - x2)) (abs (y1 - y2))

{-| Advance to the next player and to the next turn -}
advancePlayer : GameState -> GameState
advancePlayer ({ turnNumber, players } as gameState) =
  let
    newTurnNumber = turnNumber + 1
    n = Array.length players
    newPlayer = Array.get (modBy n newTurnNumber) players 
  in
    { gameState | turnNumber = newTurnNumber
                , currentPlayer = Maybe.withDefault "" newPlayer }

{-| Set the "alert" (selected) blob -}
setAlertBlob : Maybe BlobIndex -> GameState -> GameState
setAlertBlob newAlert gameState = { gameState | selectedBlob = newAlert }

{-| Mutate the board -}
mapBoard : (BlobBoard -> BlobBoard) -> GameState -> GameState
mapBoard f gameState = { gameState | blobBoard = f gameState.blobBoard }

{-| Set one cell in the board -}
setCell : BlobIndex -> BlobSpace -> BlobBoard -> BlobBoard
setCell i sp cells = (i, sp) :: List.filter (\(j, _) -> i /= j) cells

{-| Convert all adjacent occupied cells to a color -}
convertCell : BlobIndex -> Player -> BlobBoard -> BlobBoard
convertCell i p b = 
  let
    convert entry =
      case entry of
        (j, Occupied p2) -> if distance i j == 1
                              then (j, Occupied p)
                              else entry
        _ -> entry
  in
  setCell i (Occupied p) (List.map convert b)


-- * Rendering
--------------

{-| Message type. Essentially corresponds to all the "events" that can happen -}
type Msg
     = SlideBlob BlobIndex BlobIndex {- slide a blob from one spot to another -}
     | LeapBlob BlobIndex BlobIndex  {- jump a blob from one spot to another  -}
     | SelectMe BlobIndex            {- alert (click) on a blob               -}
     | UnSelectMe                    {- at ease all blobs (un-alert)          -}
     | NewGame                       {- click on the new game button          -}
     | PassTurn                      {- click on the pass turn button         -}

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


-- * Top-level state updating and rendering
-------------------------------------------

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

