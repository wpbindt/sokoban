module Sokoban where
import Prelude hiding (Left, Right, lookup)
import Data.List hiding (lookup)
import Data.Maybe
import Data.Map

import Game

data Tile = Floor | Wall | Boulder | Hole | Exit deriving (Show, Eq)

data Location = Location
    { getX :: Int
    , getY :: Int } deriving (Show, Eq, Ord)

type GameMap = Map Location Tile

data Direction = Up | Down | Left | Right deriving (Show, Eq)

data GameState = Level Location GameMap
                 | Win deriving (Eq)


sokoban :: Game Direction GameState
sokoban = Game
    { parseInput=parseDirection
    , advanceState=move
    , draw=show }

----------------------------------------------------------------
-- PARSING
parseGameState :: String -> GameState
parseGameState input = parseGameStateFromTiles tileRows playerLocation
    where (firstLine:secondLine:remainder) = lines input
          playerLocation = Location{getX=read firstLine, getY=read secondLine}
          tileRows = Prelude.map (Prelude.map parseTile) remainder


parseTile :: Char -> Tile
parseTile '.' = Floor
parseTile '#' = Wall
parseTile '0' = Boulder
parseTile '^' = Hole
parseTile '>' = Exit
          

parseGameStateFromTiles :: [[Tile]] -> Location -> GameState
parseGameStateFromTiles tileRows playerPosition = Level playerPosition (mapFromTileRows tileRows)


mapFromTileRows :: [[Tile]] -> GameMap
mapFromTileRows tileRows = fromList $ do
    (y, row) <- zip [0..] (reverse tileRows)
    (x, tile) <- zip [0..] row
    return (Location{getX=x, getY=y}, tile)


initialState :: GameState
initialState = parseGameState $ unlines 
    [ "7" 
    , "8"
    , "###############"
    , "##....####...##"
    , "##.0..####.0.##"
    , "##.0......0..##"
    , "##..###.###0.##"
    , "###########.###"
    , "##..^^^>#.....#"
    , "##..#####0....#"
    , "###^#####.0...#"
    , "###^#####.0...#"
    , "###..^^^^0.0..#"
    , "###..##########"
    , "###############" ]

---------------------------------------------------------------

---------------------------------------------------------------
-- DRAWING
instance Show GameState where
    show state@(Level _ map) = intercalate "\n" [ printRow y state | y <- reverse [0..getY tR] ]
        where tR = topRightCorner map
    show Win = "You win!"


printRow :: Int -> GameState -> String
printRow y state@(Level _ map) = [printLocation Location{getX=x, getY=y} state | x <- [0..getX $ topRightCorner map]]


printLocation :: Location -> GameState -> Char
printLocation location (Level playerLocation map) = if location == playerLocation then '@' else printTile (getTile location map)


printTile :: Tile -> Char
printTile Floor = '.'
printTile Wall = '#'
printTile Boulder = '0'
printTile Hole = '^'
printTile Exit = '>'


topRightCorner :: GameMap -> Location
topRightCorner = maximum . keys

---------------------------------------------------------------



---------------------------------------------------------------
-- GAME ADVANCEMENT

findTargetLocation :: Direction -> Location -> Location
findTargetLocation Left Location{getX=x, getY=y} = Location{getX=x-1, getY=y}
findTargetLocation Right Location{getX=x, getY=y} = Location{getX=x+1, getY=y}
findTargetLocation Up Location{getX=x, getY=y} = Location{getX=x, getY=y+1}
findTargetLocation Down Location{getX=x, getY=y} = Location{getX=x, getY=y-1}


getTile :: Location -> GameMap -> Tile
getTile location = fromMaybe Wall . lookup location


move :: Maybe Direction -> GameState -> GameState
move (Just direction) (Level beforeLocation beforeMap) = afterState
    where targetLocation = findTargetLocation direction beforeLocation
          targetTile = getTile targetLocation beforeMap
          afterState = if targetTile == Exit then Win else Level afterLocation afterMap
          afterLocation = case targetTile of 
            Floor -> targetLocation
            Boulder -> if boulderTargetTile == Floor || boulderTargetTile == Hole then targetLocation else beforeLocation
                where boulderTarget = findTargetLocation direction targetLocation
                      boulderTargetTile = getTile boulderTarget beforeMap
            _ -> beforeLocation 
          afterMap = if targetTile /= Boulder then beforeMap else moveBoulder direction targetLocation beforeMap
move _ gameState = gameState


moveBoulder :: Direction -> Location -> GameMap -> GameMap
moveBoulder direction boulderLocation map = case openForMovement of
        True -> (adjust coverWithBoulder targetLocation) . (adjust (const Floor) boulderLocation) $ map
        False -> map
    where targetLocation = findTargetLocation direction boulderLocation
          openForMovement = targetTile == Floor || targetTile == Hole
          targetTile = getTile targetLocation map


coverWithBoulder :: Tile -> Tile
coverWithBoulder Hole = Floor
coverWithBoulder _ = Boulder




---------------------------------------------------------------


---------------------------------------------------------------
-- INPUT
parseDirection :: Char -> Maybe Direction
parseDirection c = case c of
    'j' -> Just Down
    'k' -> Just Up
    'h' -> Just Left
    'l' -> Just Right
    'w' -> Just Up
    'a' -> Just Left
    's' -> Just Down
    'd' -> Just Right
    _ -> Nothing
---------------------------------------------------------------
