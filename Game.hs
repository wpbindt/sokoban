module Game where

import System.Process


runGameLoop :: b -> Game a b -> IO ()
runGameLoop initialState game = do
    system "clear"
    putStrLn . draw game $ initialState
    input <- (parseInput game) <$> getChar
    let newState = advanceState game input initialState
    runGameLoop newState game


data Game a b = Game
    { parseInput :: Char -> Maybe a
    , advanceState :: Maybe a -> b -> b
    , draw :: b -> String }
