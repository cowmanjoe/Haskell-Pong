module Bindings (display, update, reshape, keyboardMouse, makeGameObjects) where 

import Graphics.UI.GLUT
import Data.IORef
import Display 
import Games2


data Side = PLeft | PRight

reshape :: ReshapeCallback 
reshape size = do 
  viewport $= (Position 0 0, size) 


keyboardMouse :: IORef GameState -> KeyboardMouseCallback
keyboardMouse gameState _key _state _modifiers _position 
  | _key == Char 's' = do 
    gameState $~! (paddleCommand (if _state == Down then DDown else DNone) PLeft)
  | _key == Char 'w' = do
    gameState $~! (paddleCommand (if _state == Down then DUp else DNone) PLeft)
  | _key == SpecialKey KeyDown = do 
    gameState $~! (paddleCommand (if _state == Down then DDown else DNone) PRight) 
  | _key == SpecialKey KeyUp = do
    gameState $~! (paddleCommand (if _state == Down then DUp else DNone) PRight) 
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