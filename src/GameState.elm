module GameState exposing
  ( GameState, Player, BlobBoard, BlobIndex, BlobSpace(..), Msg(..)
  , initialGameState
  , distance, advancePlayer, setAlertBlob, mapBoard, setCell, convertCell
  )

-- elm/core
import Array exposing ( Array )

-- * The game state
---------------------

{-| State kept by the game -}
type alias GameState =
     { players       : Array Player
     , turnNumber    : Int
     , currentPlayer : Player          {- must be in 'players'!   -}
     , selectedBlob  : Maybe BlobIndex {- must be in 'blobBoard'! -}
     , blobBoard     : BlobBoard
     , boardHeight   : Int             {- height of board         -}
     , boardWidth    : Int             {- width of board          -}
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

{-| Message type. Essentially corresponds to all the "events" that can happen -}
type Msg
     = SlideBlob BlobIndex BlobIndex {- slide a blob from one spot to another -}
     | LeapBlob BlobIndex BlobIndex  {- jump a blob from one spot to another  -}
     | SelectMe BlobIndex            {- alert (click) on a blob               -}
     | UnSelectMe                    {- at ease all blobs (un-alert)          -}
     | NewGame                       {- click on the new game button          -}
     | PassTurn                      {- click on the pass turn button         -}


-- * Sample initial game state
------------------

{-| Helper to convert from nested lists into 'BlobBoard' -}
boardCells : List (List BlobSpace) -> List (BlobIndex, BlobSpace)
boardCells = List.indexedMap (\i -> List.indexedMap (\j cell -> ((i,j), cell)))
          >> List.concat

{-| Helper to extract the height of a board from nested lists -}
boardRows : List (List BlobSpace) -> Int
boardRows = List.length

{-| Helper to extract the height of a board from nested lists -}
boardColumns : List (List BlobSpace) -> Int
boardColumns = List.map List.length >> List.maximum >> Maybe.withDefault 0

-- We could easily accomodate more than 2 players...
blue = Occupied "blue"
red  = Occupied "red"

-- We could get much more creative with boards...
board =
  [ [ blue,    Vacant, Vacant, Invalid,  Vacant, Vacant, Invalid ]
  , [ Vacant,  Vacant, Vacant, Vacant,  Vacant, Vacant, Vacant  ]
  , [ Vacant,  Invalid, Vacant, Vacant,  Vacant, Invalid, Vacant  ]
  , [ Invalid,  Vacant, Vacant, Invalid, Vacant, Vacant, Invalid  ]
  , [ Invalid,  Vacant, Vacant, Invalid, Vacant, Vacant, Invalid  ]
  , [ Invalid,  Vacant, Vacant, Invalid, Vacant, Vacant, Invalid  ]
  , [ Vacant,  Invalid, Vacant, Vacant,  Vacant, Invalid, Vacant  ]
  , [ Vacant,  Vacant, Vacant, Vacant,  Vacant, Vacant, Vacant  ]
  , [ Invalid, Vacant, Vacant, Invalid,  Vacant, Vacant, red     ] ]

board2 =
  [ [ blue,    Vacant, Vacant, Vacant,  Vacant, Vacant, Invalid ]
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
                   , blobBoard = boardCells board
                   , boardHeight = boardRows board
                   , boardWidth = boardColumns board }


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


