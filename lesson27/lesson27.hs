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

move :: Dot -> Dot
move Dot { pos=(x,y), vel=v@(dx,dy) } = Dot { pos=(x'', y''), vel=v }
 where
	(x', y')	=	(x + dx, y + dy)
		
	x''	=	if x' < 0 || (x' + dotWidth) > screenWidth then x else x'
	
	y''	=	if y' < 0 || (y' + dotHeight) > screenHeight then y else y'

showDot Dot { pos=(x,y) } = applySurface x y

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
				setCaption "Alpha Test" []
				
				back	<-	loadImage "fadein.png" Nothing
				front	<-	loadImage "fadeout.png" Nothing
								
				let render fps alpha = do
					setAlpha front [SrcAlpha] alpha
					
					applySurface 0 0 back screen Nothing
					applySurface 0 0 front screen Nothing
					
					Graphics.UI.SDL.flip screen
					
					ticks <- getTimerTicks fps
					if ticks < secsPerFrame
						then delay $ secsPerFrame - ticks
						else return ()

				let loop alpha fps = do
					event	<-	pollEvent					
					case event of
						Quit	->	return ()
						KeyDown (Keysym SDLK_ESCAPE _ _)->	return ()
						KeyDown (Keysym SDLK_UP _ _)	-> do
							let alpha' = if alpha < alphaOpaque then alpha + 5 else alpha
							loop alpha' fps
						KeyDown (Keysym SDLK_DOWN _ _)	->	do
							let alpha' = if alpha > alphaTransparent then alpha - 5 else alpha
							loop alpha' fps
						NoEvent							->	do
							render fps  alpha
							fps' <- start fps
							loop alpha fps'		
						_								->	loop alpha fps
				
				fps		<-	start defaultTimer
				
				loop alphaOpaque fps
				TTFG.quit
		quit
	where
		mapRGB'			=	mapRGB . surfaceGetPixelFormat
		textColor		=	Color 0xFF 0xFF 0xFF
		framesPerSecond	=	20
		secsPerFrame	=	1000 `div` framesPerSecond
		alphaOpaque		=	255
		alphaTransparent=	0
