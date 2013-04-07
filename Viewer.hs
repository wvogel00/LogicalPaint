module Viewer (showAsGUI,showAsCUI)  where

import Interface as I
import Operate
import Graphics.UI.GLUT
import Data.Foldable (forM_)
import Data.IORef
import Control.Monad (foldM_)

showAsCUI :: Map -> IO()
showAsCUI mp = do
	forM_ [1..w] $ \i -> do
		forM_ [1..h] (\j -> return () )
	where
		(w,h) = size mp

showAsGUI :: FilePath -> Map -> IO()
showAsGUI filename mp = do
	getArgsAndInitialize
	initialDisplayMode $= [RGBAMode,DoubleBuffered]
	initialWindowSize $= Size 400 400
	initialWindowPosition $= Position 300 200
	createWindow filename

	clearColor $= Color4 0.6 0.4 0.2 0.0
	clear [ColorBuffer]
	loadIdentity

	preservingMatrix $ do
		rotate 0.5 (Vector3 0.0 0.0 1.0 :: Vector3 GLdouble)
		renderPrimitive Quads $ mapM_ vertex [
			Vertex3 0.10 0.10 0.0,
			Vertex3 (-0.10) 0.10 0.0,
			Vertex3 (-0.10) (-0.10) 0.0,
			Vertex3 0.10 (-0.10) 0.0 :: Vertex3 GLfloat]

		xs <- convertMap mp
		renderPrimitive Quads $ foldM_ (showMap (size mp)) (w,h) xs
	swapBuffers
	mainLoop
	where
		(w,h) = size mp

convertMap :: Map -> IO [Bool]
convertMap mp = do
	xs <- newIORef []
	forM_ [1..w] $ \i -> do
		forM_ [1..h] (\j ->	readMap i j mp >>= \v-> modifyIORef xs (v:))
	return =<< readIORef xs
	where
		(w,h) = size mp

showMap :: I.Size -> (Int,Int) -> Bool -> IO (Int,Int)
showMap (w,h) (i,j) v = do
	color $ if j==7 || j==5 || i==2 || i==6 ||(i,j)==(4,6)  then Color3 0.3 0.0 0.5 else Color3 1.0 1.0 1.0 :: Color3 GLfloat

	putStr $"(" ++ show i ++ "," ++ show j ++ ") ->"
	print $ transformPos (w,h) (i,j)
	mapM_ vertex $ transformPos (w,h) (i,j) ^+^ box
	return (i',j')
	where
		boxRate = 1.6 / fromIntegral (max w h) :: GLfloat
		box = [ Vertex3 0 0 0,
				Vertex3 boxRate 0 0,
				Vertex3 boxRate boxRate 0,
				Vertex3 0 boxRate 0 :: Vertex3 GLfloat]
		(i',j') = if i == 1  then (w,j-1) else (i-1,j)

(^+^) :: (GLfloat,GLfloat) -> [Vertex3 GLfloat] -> [Vertex3 GLfloat]
(^+^) (x,y) xs = map f xs
	where f (Vertex3 a b z) = Vertex3 (a+x) (b+y) z

transformPos :: I.Size -> (Int,Int) -> (GLfloat,GLfloat)
transformPos (w,h) (i,j) = ((i'-w'/2)/w'*1.6 , (j'-h'/2)/h'*1.6)
	where
		(w',h') = (fromIntegral w,fromIntegral h)
		(i',j') = (fromIntegral i,fromIntegral j)
