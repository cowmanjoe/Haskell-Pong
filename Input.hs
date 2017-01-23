module Input where

import Display
import Graphics.UI.GLUT
import Data.IORef

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
