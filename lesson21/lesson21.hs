{-
	The MIT License
	Copyright (c) 2009 Korcan Hussein

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in
	all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
	THE SOFTWARE.
-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module Main where

import Data.List
import Data.Word
import Data.Array.IArray
import Monad
import Control.Monad
import Control.Monad.State
import Control.Applicative

import Graphics.UI.SDL
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.WindowManagement
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Image

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render

import Graphics.UI.SDL.Mixer
import Timer

screenWidth		=	640
screenHeight	=	480
screenBpp		=	32

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s =	return s
setColorKey' (Just (r, g, b)) surface	=	(mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
	where offset	=	Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

isInside :: Rect -> Int -> Int -> Bool
isInside Rect {rectX=rx,rectY=ry,rectW=rw,rectH=rh } x y = (x > rx) && (x < rx + rw) && (y > ry) && (y < ry + rh)  

newtype RectArray = RectArray { rectArray ::  [Rect] }

data Dot = Dot { pos :: (Int, Int), vel :: (Int, Int) }

dotWidth	=	20
dotHeight	=	20

levelWidth	=	1280
levelHeight	=	960

defaultDot	=	Dot (0,0) (0,0)
 
class Collidable a b where
	intersects :: a -> b -> Bool

instance Collidable Rect Rect where
	--intersects :: Rect -> Rect -> Bool
	intersects Rect {rectX=ax,rectY=ay,rectW=aw,rectH=ah } Rect {rectX=bx,rectY=by,rectW=bw,rectH=bh }
		| bottomA <= topB || topA >= bottomB	=	False
		| rightA <= leftB || leftA >= rightB	=	False
		| otherwise	=	True
		--bottomA > topB && topA < bottomB && rightA > leftB && leftA < rightB
 	 where
 		leftA	=	ax
 		rightA	=	ax + aw
 		topA	=	ay
 		bottomA	=	ay + ah
 		
 		leftB	=	bx
	 	rightB	=	bx + bw
	 	topB	=	by
	 	bottomB	=	by + bh

instance (Collidable a b) => Collidable [a] [b] where
	intersects lhs rhs = any (\x -> any (intersects x) rhs) lhs

instance Collidable RectArray RectArray where
	--intersects :: Rect -> Rect -> Bool
	intersects (RectArray lhs) (RectArray rhs) = intersects lhs rhs

distance :: Int -> Int -> Int -> Int -> Float
distance x1 y1 x2 y2 = sqrt $ fromIntegral $ x' + y'
 where
 	x'	=	(x2 - x1) ^ 2
 	y'	=	(y2 - y1) ^ 2


halfDotWidth	=	dotWidth `div` 2
halfDotHeight	=	dotHeight `div` 2

handleInput :: Dot -> Event -> Dot
handleInput Dot { pos=p, vel=(dx,dy) } (KeyDown (Keysym SDLK_UP _ _)) = Dot { pos=p,vel=(dx, dy - halfDotHeight) }
handleInput Dot { pos=p, vel=(dx,dy) } (KeyDown (Keysym SDLK_DOWN _ _)) = Dot { pos=p,vel=(dx, dy + halfDotHeight) }
handleInput Dot { pos=p, vel=(dx,dy) } (KeyDown (Keysym SDLK_LEFT _ _)) = Dot { pos=p,vel=(dx - halfDotWidth, dy) }
handleInput Dot { pos=p, vel=(dx,dy) } (KeyDown (Keysym SDLK_RIGHT _ _)) = Dot { pos=p,vel=(dx + halfDotWidth, dy) }

handleInput Dot { pos=p, vel=(dx,dy) } (KeyUp (Keysym SDLK_UP _ _)) = Dot { pos=p,vel=(dx, dy + halfDotHeight) }
handleInput Dot { pos=p, vel=(dx,dy) } (KeyUp (Keysym SDLK_DOWN _ _)) = Dot { pos=p,vel=(dx, dy - halfDotHeight) }
handleInput Dot { pos=p, vel=(dx,dy) } (KeyUp (Keysym SDLK_LEFT _ _)) = Dot { pos=p,vel=(dx + halfDotWidth, dy) }
handleInput Dot { pos=p, vel=(dx,dy) } (KeyUp (Keysym SDLK_RIGHT _ _)) = Dot { pos=p,vel=(dx - halfDotWidth, dy) }

handleInput d _ = d

move :: Dot -> Dot
move Dot { pos=(x,y), vel=v@(dx,dy) } = Dot { pos=(x'', y''), vel=v }
 where
	(x', y')	=	(x + dx, y + dy)
		
	x''	=	if x' < 0 || (x' + dotWidth) > levelWidth then x else x'
	
	y''	=	if y' < 0 || (y' + dotHeight) > levelHeight then y else y'

type Camera = Rect

setCamera :: Dot -> Camera -> Camera
setCamera Dot { pos=(x,y) } (Rect _ _ w h) = (Rect x'' y'' w h)
 where
 	x'	=	(x + dotWidth `div` 2) - screenWidth `div` 2
 	y'	=	(y + dotWidth `div` 2) - screenHeight `div` 2
 	x''	=	min (levelWidth - w) $ max x' 0
 	y''	=	min (levelHeight - h) $ max y' 0 
 
showDot Dot { pos=(x,y) } (Rect cx cy _ _) = applySurface (x - cx) (y - cy)

main =
	do
		Graphics.UI.SDL.General.init [InitEverything]
				
		result <- TTFG.init
		if not result
			then do
				putStr "Failed to init ttf\n"
				return ()
			else do
				screen	<-	setVideoMode screenWidth screenHeight screenBpp [HWSurface, DoubleBuf, AnyFormat]
				setCaption "Move the Dot" []
				
				background	<-	loadImage "bg.png" Nothing
				dot			<-	loadImage "dot.bmp" (Just (0x00, 0xff, 0xff))
								
				let render = (do
					(fps, myDot, camera) <- get
					
					liftIO $ applySurface 0 0 background screen (Just camera)
										
					liftIO $ showDot myDot camera dot screen Nothing
																				
					liftIO $ Graphics.UI.SDL.flip screen
					
					ticks <- liftIO $ getTimerTicks fps
					if ticks < secsPerFrame
						then liftIO $ delay $ secsPerFrame - ticks
						else return ()) :: StateT (Timer, Dot, Camera) IO ()
				
				let loop = do
					(fps, myDot, camera)	<-	get
					event			<-	liftIO pollEvent					
					case event of
						Quit	->	return ()
						NoEvent	->	do
							let myDot' = move myDot
							put (fps, myDot', setCamera myDot' camera)
							render
							(fps', d, c) <- get
							fps'' <- liftIO $ start fps
							put (fps'', d, c)
							loop
						_		-> put (fps, handleInput myDot event, camera) >> loop
				
				fps		<-	start defaultTimer
				execStateT loop (fps, myDot, camera)
				
				--closeFont font
				TTFG.quit
		quit
	where
		mapRGB'			=	mapRGB . surfaceGetPixelFormat
		textColor		=	Color 0 0 0
		framesPerSecond	=	20
		secsPerFrame	=	1000 `div` framesPerSecond
		myDot			=	defaultDot
		camera			=	Rect  0 0 screenWidth screenHeight
