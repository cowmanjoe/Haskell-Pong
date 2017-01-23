module Display(
    display, 
	reshape, 
	update, 
	makeGameObjects, 
	getMoveDirection, 
	GameState(GameState), 
	Side(PLeft,PRight), 
	MoveDirection(DUp, DDown, DNone), 
	paddleCommand)  where 

import Graphics.UI.GLUT
import Data.IORef
import Data.Time
import Control.Monad
import Games2
import Bindings


display :: IORef GameState -> DisplayCallback
display gameState = do
  clear [ColorBuffer]
  gs <- get gameState
  
  forM_ (gameObjects gs) drawObject
  flush
 
reshape :: ReshapeCallback 
reshape size = do 
  viewport $= (Position 0 0, size)
 
update :: IORef GameState -> TimerCallback
update gameState = do
  gs <- get gameState 
  gameState $~! updateMovement
  postRedisplay Nothing
  addTimerCallback 30 (update gameState)


drawObject :: GameObject -> IO () 
drawObject (Paddle 
    corner1
    corner2
    corner3 
    corner4) = 
  renderPrimitive Quads $ mapM_ vertex [corner1, corner2, corner3, corner4]
drawObject (Ball _ _ _)  = error("Ball is not yet implemented in drawObject")


-- this will move the paddles in their velocity direction
updateMovement :: GameState -> GameState
updateMovement (GameState (leftPaddle:rightPaddle:xs) (dirLeft, dirRight)) = 
    GameState [movePaddle leftPaddle dirLeft, movePaddle rightPaddle dirRight] (dirLeft, dirRight)


movePaddle :: GameObject -> MoveDirection -> GameObject 
movePaddle (Paddle c1 c2 c3 c4) DUp = 
  Paddle (add c1 upVertex) (add c2 upVertex) (add c3 upVertex) (add c4 upVertex)
movePaddle (Paddle c1 c2 c3 c4) DDown = 
  Paddle (add c1 downVertex) (add c2 downVertex) (add c3 downVertex) (add c4 downVertex)
movePaddle p DNone = p


add :: Num a => (Vertex3 a) -> (Vertex3 a) -> (Vertex3 a) 
add (Vertex3 a b c) (Vertex3 d e f) = Vertex3 (a+d) (b+e) (c+f)


getMoveDirection :: Side -> GameState -> MoveDirection 
getMoveDirection PLeft = fst . moveDirections 
getMoveDirection PRight = snd . moveDirections 