module Bindings (display, update, reshape, keyboardMouse, makeGameObjects) where 

import Graphics.UI.GLUT
import Data.IORef
import Display 
import Games2




reshape :: ReshapeCallback 
reshape size = do 
  viewport $= (Position 0 0, size) 


keyboardMouse :: IORef GameState -> KeyboardMouseCallback
keyboardMouse gameState _key _state _modifiers _position 
  | _key == Char 's' = do 
    gs <- get gameState
    gameState $~! (paddleCommand 
		(if _state == Down 
		then DDown 
		else (if getMoveDirection PLeft gs /= DUp then DNone else DUp))
		PLeft)
  | _key == Char 'w' = do
    gs <- get gameState
    gameState $~! (paddleCommand 
	    (if _state == Down 
		then DUp 
		else (if getMoveDirection PLeft gs /= DDown then DNone else DDown)) 
		PLeft)
  | _key == SpecialKey KeyDown = do 
    gs <- get gameState
    gameState $~! (paddleCommand 
		(if _state == Down 
		then DDown 
		else (if getMoveDirection PRight gs /= DUp then DNone else DUp)) 
		PRight) 
  | _key == SpecialKey KeyUp = do
    gs <- get gameState
    gameState $~! (paddleCommand 
	    (if _state == Down 
		then DUp 
		else (if getMoveDirection PRight gs /= DDown then DNone else DDown))  
		PRight) 
  | otherwise = return ()

{-
keyboardMouse :: IORef GameState -> KeyboardMouseCallback
keyboardMouse gameState (Char 's') _state _ _ = do 
  gameState $~! (paddleCommand (if _state == Down then DDown else DNone) PLeft)
keyboardMouse _ _ _ _ _ = return ()
-}

makeGameObjects :: GameState
makeGameObjects = GameState [myPaddle PLeft 0.0, myPaddle PRight 0.0] (DNone, DNone)

myPaddle :: Side -> GLfloat -> GameObject 
myPaddle PLeft y = Paddle 
    (Vertex3 (-0.95)  y  0)
    (Vertex3 (-0.95) (y+0.2) 0) 
    (Vertex3 (-0.9) (y+0.2) 0)
    (Vertex3 (-0.9) y 0)
myPaddle PRight y = Paddle 
    (Vertex3 0.95  y  0)
    (Vertex3 0.95 (y+0.2) 0) 
    (Vertex3 0.9 (y+0.2) 0)
    (Vertex3 0.9 y 0)


paddleCommand :: MoveDirection -> Side -> GameState -> GameState 
paddleCommand d PLeft (GameState [leftPaddle, rightPaddle] (dirLeft, dirRight)) =
    GameState [leftPaddle, rightPaddle] (d, dirRight) 
paddleCommand d PRight (GameState [leftPaddle, rightPaddle] (dirLeft, dirRight)) = 
    GameState [leftPaddle, rightPaddle] (dirLeft, d)