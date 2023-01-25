cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 20

height :: Int
height = 20

type Board = [Pos]

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1, y-1), (x,   y-1),
                          (x+1, y-1), (x-1, y),
                          (x+1, y),   (x-1, y+1),
                          (x,   y+1), (x-1, y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                     isEmpty b p,
                     liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

isBoardEmpty :: Board -> Bool
isBoardEmpty b = and [isEmpty b p | p <- rmdups (concat (map neighbs b))]

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

life :: Board -> IO ()
life b = if not (isBoardEmpty b) then
            do cls
               showcells b
               wait 500000
               life (nextgen b)
         else
            do cls
               goto (1,1)
               putStrLn "Game Over !"

example :: Board
example = [(1, 1), (1, 2), (2, 2), (2, 3), (1, 3),
           (5, 1), (5, 2), (4, 2), (4, 1), (5, 3),
           (9, 10), (9, 11), (9, 12), (9, 13),
           (10, 10), (10, 11), (10, 12), (10, 13),
           (10,
           20), (11, 20), (12, 20), (13, 20)]

main :: IO ()
main = life example
