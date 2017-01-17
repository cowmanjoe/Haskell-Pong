module Display  where 

import Graphics.UI.GLUT
import Data.IORef
import Data.Time
import Control.Monad
import Games2


-- Either Movement up or down 
data MoveDirection = DNone | DUp | DDown

-- Type for (leftPaddle, rightPaddle) of vertical paddle movement 
type MoveDirections = (MoveDirection, MoveDirection)

-- GameObjects are either Ball position velocity radius or Paddle corner1 corner2 corner3 corner4
data GameObject = Ball (Vertex3 GLfloat) (Vertex3 GLfloat) GLfloat | 
    Paddle (Vertex3 GLfloat) (Vertex3 GLfloat) (Vertex3 GLfloat) (Vertex3 GLfloat)

-- GameState is a list of game objects and movement of paddle controls 
data GameState = GameState { gameObjects :: [GameObject], moveDirections :: MoveDirections }


-- A GameAction has a move type of MoveDirection, a state type of 
type GameAction = Action MoveDirection 


paddleSpeed = 0.0001
upVertex = Vertex3 0 paddleSpeed 0
downVertex = Vertex3 0 (-paddleSpeed) 0

display :: IORef GameState -> DisplayCallback
display gameState = do
  clear [ColorBuffer]
  gs <- get gameState
  
  forM_ (gameObjects gs) drawObject
  flush
 
idle :: IORef GameState -> IdleCallback
idle gameState = do
  gs <- get gameState 
  gameState $~! update
  postRedisplay Nothing


drawObject :: GameObject -> IO () 
drawObject (Paddle 
    corner1
    corner2
    corner3 
    corner4) = 
  renderPrimitive Quads $ mapM_ vertex [corner1, corner2, corner3, corner4]
drawObject (Ball _ _ _)  = error("Ball is not yet implemented in drawObject")


-- this will move the ball in its velocity direction
update :: GameState -> GameState
update (GameState (leftPaddle:rightPaddle:xs) (dirLeft, dirRight)) = 
    GameState [movePaddle leftPaddle dirLeft, movePaddle rightPaddle dirRight] (dirLeft, dirRight)


movePaddle :: GameObject -> MoveDirection -> GameObject 
movePaddle (Paddle c1 c2 c3 c4) DUp = 
  Paddle (add c1 upVertex) (add c2 upVertex) (add c3 upVertex) (add c4 upVertex)
movePaddle (Paddle c1 c2 c3 c4) DDown = 
  Paddle (add c1 downVertex) (add c2 downVertex) (add c3 downVertex) (add c4 downVertex)
movePaddle p DNone = p


add :: (Vertex3 GLfloat) -> (Vertex3 GLfloat) -> (Vertex3 GLfloat) 
add (Vertex3 a b c) (Vertex3 d e f) = Vertex3 (a+d) (b+e) (c+f)
