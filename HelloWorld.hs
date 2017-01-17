import Graphics.UI.GLUT
import Data.Time

data Side = Le | Ri


ly <- newIORef 0.0 
ry <- newIORef 0.0

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [(sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12]]



myPaddle :: Side -> Double -> [(Double, Double, Double)]
myPaddle Le y = [(-0.95,y,0),(-0.95,y+0.2,0),(-0.9,y+0.2,0),(-0.9,y,0)]
myPaddle Ri y = [(0.95,y,0),(0.95,y+0.2,0),(0.9,y+0.2,0),(0.9,y,0)]




main :: IO ()
main = do 
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Hello World" 
	displayCallback $= display 
	
	mainLoop 
	
display :: DisplayCallback 
display = do 
	clear [ ColorBuffer ]
	curLy <- get ly
	curRy <- get ry 
	renderPrimitive Quads $ 
		mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) (myPaddle Le curLy ++ myPaddle Ri curRy)
	ly $= curLy - 0.01
	ry $= curRy + 0.01
	flush 