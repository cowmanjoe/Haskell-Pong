 

import Bindings
import Graphics.Rendering.OpenGL
import Data.IORef
import Graphics.UI.GLUT




main :: IO ()
main = do 
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Game" 
	
	reshapeCallback $= Just reshape

	gameState <- newIORef makeGameObjects

	displayCallback $= display gameState 
	
	keyboardMouseCallback $= Just (keyboardMouse gameState)
	

	idleCallback $= Just (idle gameState)
	mainLoop 

