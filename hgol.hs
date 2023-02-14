import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import System.Random (mkStdGen, random)

type Grid = [[Int]]

emptyGrid :: Int -> Int -> Float -> IO Grid
emptyGrid n m p = do
  let emptyCell (i, j)
        | rnd < p = 1
        | otherwise = 0
        where
          rnd = head (randoms (mkStdGen (i*m+j))) :: Float
      randoms g = let (x,g') = random g in x : randoms g'
  return [[emptyCell (i, j) | j <- [0..m-1]] | i <- [0..n-1]]

-- Print a grid to the console
printGrid :: Grid -> IO ()
printGrid grid = do
  putStr "\ESC[2J" -- Clear the screen
  putStr "\ESC[0;0H" -- Move the cursor to the top left corner
  putStrLn (unlines [concat [cellToStr (grid !! i !! j) | j <- [0..m-1]] | i <- [0..n-1]])
  where
    n = length grid
    m = length (head grid)
    cellToStr 0 = " " -- Dead cell
    cellToStr 1 = "O" -- Live cell

-- Compute the next generation of a grid
nextGen :: Grid -> Grid
nextGen grid = [[cellState (grid !! i !! j) (liveNeighbors (i, j)) | j <- [0..m-1]] | i <- [0..n-1]]
  where
    n = length grid
    m = length (head grid)
    liveNeighbors (i, j) = sum [grid !! i' !! j' | i' <- [i-1..i+1], j' <- [j-1..j+1], (i', j') /= (i, j), i' >= 0, i' < n, j' >= 0, j' < m]
    cellState 1 n | n < 2 || n > 3 = 0 -- Live cell dies
    cellState 1 _ = 1 -- Live cell survives
    cellState 0 3 = 1 -- Dead cell becomes alive
    cellState 0 _ = 0 -- Dead cell remains dead

-- Simulate the Game of Life
-- time t in milisseconds
simulate :: Grid -> Float -> Int -> IO ()
simulate grid p t = loop grid
  where
    loop g = do
      printGrid g
      threadDelay t 
      loop (nextGen g)
    n = length grid
    m = length (head grid)
    emptyCell (i, j)
      | rnd < p = 1
      | otherwise = 0
      where
        rnd = head (randoms (mkStdGen (i*m+j))) :: Float
    emptyGrid n m p = [[emptyCell (i, j) | j <- [0..m-1]] | i <- [0..n-1]]
    randoms g = let (x,g') = random g in x : randoms g'

-- Entry point of the program
main :: IO ()
main = do
  let x = 25
      y = 49
      r = 0.5
      t = 200000 -- Pause for 400 milliseconds 
  grid <- emptyGrid x y r
  simulate grid r t