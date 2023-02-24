import Prelude hiding (Left, Right, lookup)
import Data.List hiding (lookup)
import Data.Maybe
import Data.Map
import System.IO
import System.Process

import Game (runGameLoop)
import Sokoban (sokoban, initialState)


main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    runGameLoop initialState sokoban
