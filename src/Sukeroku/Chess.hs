{-# LANGUAGE RecordWildCards #-}

module Sukeroku.Chess where

import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)


import Text.PrettyPrint (Doc, render, ptext, hcat, vcat)

data Game = Game
  { game_board               :: Board
  , game_nextToMove          :: Color
  , game_moves               :: [Move]
  , game_canWhiteShortCast   :: Bool
  , game_canWhiteLongCast    :: Bool
  , game_canBlackShortCast   :: Bool
  , game_canBlackLongCast    :: Bool
  }

newInitialGame :: Game
newInitialGame = Game
  { game_board             = initialBoard
  , game_moves             = []
  , game_nextToMove        = White
  , game_canWhiteShortCast = True
  , game_canWhiteLongCast  = True
  , game_canBlackShortCast = True
  , game_canBlackLongCast  = True
  }


type Position = (Int,Int)

type Board = Map.Map Position Piece

data Piece = Piece
  { piece_type   :: PieceType
  , piece_color  :: Color
  }

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq)

instance Show PieceType where
  show King   = "K"
  show Queen  = "Q"
  show Rook   = "R"
  show Bishop = "B"
  show Knight = "N"
  show Pawn   = "P"

data Color = White | Black
  deriving (Show, Eq)

turnColor :: Color -> Color
turnColor White = Black
turnColor Black = White

type Move = (Position, Position)

data AlgNotation = AlgNotation
  { algnot_pieceType :: PieceType
  , algnot_mFromFile :: Maybe Int
  , algnot_mFromRank :: Maybe Int
  , algnot_to        :: Position
  }
  deriving (Show)


initialBoard :: Board
initialBoard =
  Map.fromList $
    [ ((1,1), Piece Rook   White)
    , ((2,1), Piece Knight White)
    , ((3,1), Piece Bishop White)
    , ((4,1), Piece Queen  White)
    , ((5,1), Piece King   White)
    , ((6,1), Piece Bishop White)
    , ((7,1), Piece Knight White)
    , ((8,1), Piece Rook   White)
    , ((1,8), Piece Rook   Black)
    , ((2,8), Piece Knight Black)
    , ((3,8), Piece Bishop Black)
    , ((4,8), Piece Queen  Black)
    , ((5,8), Piece King   Black)
    , ((6,8), Piece Bishop Black)
    , ((7,8), Piece Knight Black)
    , ((8,8), Piece Rook   Black)
    ]
    ++ [ ((x,2), Piece Pawn White) | x <-[1..8]]
    ++ [ ((x,7), Piece Pawn Black) | x <-[1..8]]


showBoard :: Board -> String
showBoard board =
    render (vcat[hcat[ptext (maybe "." showPiece (Map.lookup (x,y) board)) |x <-[1..8]]| y <-[8,7..1]])
  where
    showPiece (Piece ty White) = show ty
    showPiece (Piece ty Black) = map toLower (show ty)

stepForward :: Move -> Game -> Game
stepForward (from, to) game@Game{..} =
    case Map.lookup from game_board of
      Nothing -> game
      Just p  -> game
        { game_board = Map.delete from (Map.insert to p game_board)
        , game_nextToMove = turnColor game_nextToMove
        }



algToMove :: Color -> AlgNotation -> Board -> Maybe Move
algToMove color algnot@AlgNotation{..} board =
    listToMaybe
      [ (pos, algnot_to)
      | (pos@(x,y),piese@Piece{..}) <-Map.assocs board
      , piece_type == algnot_pieceType
      , piece_color == color
      , maybe True (x==) algnot_mFromFile
      , maybe True (y==) algnot_mFromRank
      , canMoveFromTo pos algnot_to board
      ]

isChessFile :: Char -> Bool
isChessFile = (`elem` "abcdefghABCDEFGH")

chessFileToInt :: Char -> Int
chessFileToInt c = (fromEnum (toLower c)) - (fromEnum 'a') + 1

isChessRank :: Char -> Bool
isChessRank = (`elem` "12345678")

chessRankToInt :: Char -> Int
chessRankToInt c = read[c]

canMoveFromTo :: Position -> Position -> Board -> Bool
canMoveFromTo from to board =
    to `elem` (canMoveFrom board from)

canMoveFrom :: Board -> Position -> [Position]
canMoveFrom board from@(x,y) =
  case Map.lookup from board of
    Nothing -> []
    Just (Piece King color) ->
      [ to
      | move <-[posUp,posLeft,posDown,posRight,posUpLeft,posUpRight,posDownLeft,posDownRight]
      , let to=move from, inBoard to, isEmptyOrEnemy color to
      ]
    Just (Piece Rook color) -> concat
      [ snd $ foldl (f color) (True,[]) $ filter inBoard $ take 7 $ tail $ iterate move from
      | move <-[posUp,posLeft,posDown,posRight]
      ]
    Just (Piece Bishop color) -> concat
      [ snd $ foldl (f color) (True,[]) $ filter inBoard $ take 7 $ tail $ iterate move from
      | move <-[posUpLeft,posUpRight,posDownLeft,posDownRight]
      ]
    Just (Piece Queen color) -> concat
      [ snd $ foldl (f color) (True,[]) $ filter inBoard $ take 7 $ tail $ iterate move from
      | move <-[posUp,posLeft,posDown,posRight,posUpLeft,posUpRight,posDownLeft,posDownRight]
      ]
    Just (Piece Knight color) ->
      [ to | to <-posKnight from, inBoard to, isEmptyOrEnemy color to]
    Just (Piece Pawn White) -> filter inBoard
       $ [ to | let to=posUp from, isEmpty White to]
      ++ [ to | let to=posUp (posUp from), y==2, isEmpty White (posUp from), isEmpty White to]
      ++ [ to | move <-[posUpLeft,posUpRight], let to=move from, isEnemy White to]
    Just (Piece Pawn Black) -> filter inBoard
       $ [ to | let to=posDown from, isEmpty Black to]
      ++ [ to | let to=posDown (posDown from), y==7, isEmpty Black (posDown from), isEmpty Black to]
      ++ [ to | move <-[posDownLeft,posDownRight], let to=move from, isEnemy Black to]
  where
    isEmpty :: Color -> Position -> Bool
    isEmpty color pos =
        maybe True (const False) (Map.lookup pos board)

    isEnemy :: Color -> Position -> Bool
    isEnemy color pos =
        case Map.lookup pos board of
          Nothing          -> False
          Just (Piece _ c) -> color /= c

    isEmptyOrEnemy :: Color -> Position -> Bool
    isEmptyOrEnemy color pos =
        case Map.lookup pos board of
          Nothing          -> True
          Just (Piece _ c) -> color /= c

    f :: Color -> (Bool,[Position]) -> Position -> (Bool,[Position])
    f color (False,rs) pos = (False,rs)
    f color (True ,rs) pos =
        case Map.lookup pos board of
          Nothing                       -> (True, pos:rs)
          Just (Piece _ c) | c == color -> (False, rs)
                           | otherwise  -> (False, pos:rs)


inBoard :: Position -> Bool
inBoard (x,y) = and[1 <= i && i <= 8 | i <-[x,y]]

posUp :: Position -> Position
posUp (x,y) = (x,y+1)

posDown :: Position -> Position
posDown (x,y) = (x,y-1)

posLeft :: Position -> Position
posLeft (x,y) = (x-1,y)

posRight :: Position -> Position
posRight (x,y) = (x+1,y)

posUpLeft :: Position -> Position
posUpLeft = posLeft.posUp

posUpRight :: Position -> Position
posUpRight = posRight.posUp

posDownLeft :: Position -> Position
posDownLeft = posLeft.posDown

posDownRight :: Position -> Position
posDownRight = posRight.posDown

posKnight :: Position -> [Position]
posKnight (x,y) = filter inBoard (concat [[(x+i,y+j),(x+j,y+i)] | i <-[-2,2], j <-[-1,1]])

