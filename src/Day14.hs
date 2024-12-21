module Day14 where

data Robot =
  Robot
    { position, velocity :: (Int, Int)
    }
  deriving (Show, Eq)

getNewCoor ::
     Int -- position
  -> Int -- velocity
  -> Int -- wall position
  -> Int
getNewCoor p v wall =
  let distanceFromWall = wall - p
   in if v > distanceFromWall
        then v - distanceFromWall - 1
        else if -v > p
               then wall + v + p + 1
               else p + v

moveRobot :: (Int, Int) -> Robot -> Robot
moveRobot (xmax, ymax) (Robot (x, y) (vx, vy)) =
  Robot (getNewCoor x vx (xmax - 1), getNewCoor y vy (ymax - 1)) (vx, vy)

moveRobots :: Int -> (Int, Int) -> [Robot] -> [Robot]
moveRobots 0 _ robots = robots
moveRobots n walls robots =
  moveRobots (n - 1) walls (map (moveRobot walls) robots)
