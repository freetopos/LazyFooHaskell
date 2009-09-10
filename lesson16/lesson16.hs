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
module Main where

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

data Dot = Dot { pos :: (Int, Int), vel :: (Int, Int) }

defaultDot = Dot { pos=(0,0), vel=(0,0) }

dotWidth	=	20
dotHeight	=	20

halfDotWidth	= dotWidth `div` 2
halfDotHeight	= dotHeight `div` 2

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
	x'	=	x + dx
	y'	=	y + dy
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
				setCaption "Move the Dot" []
				
				dot	<-	loadImage "dot.bmp" Nothing--(Just (0x00, 0xff, 0xff))
								
				let render = (do
					(fps, myDot) <- get
					
					jrect	<-	Just <$> (liftIO $ getClipRect screen)
					color	<-	liftIO $ (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff
					
					liftIO $ fillRect screen jrect color 
					liftIO $ showDot myDot dot screen Nothing
					
					liftIO $ Graphics.UI.SDL.flip screen
					
					ticks <- liftIO $ getTimerTicks fps
					if ticks < secsPerFrame
						then liftIO $ delay $ secsPerFrame - ticks
						else return ()) :: StateT (Timer, Dot) IO ()
				
				let loop = do
					(fps, myDot)	<-	get
					event			<-	liftIO pollEvent					
					case event of
						Quit	->	return ()
						NoEvent	->	do
							put (fps, move myDot)
							render
							(fps', myDot') <- get
							fps'' <- liftIO $ start fps
							put (fps'', myDot')
							loop
						_		-> put (fps, handleInput myDot event) >> loop
				
				fps		<-	start defaultTimer
				execStateT loop (fps, defaultDot)
				
				--closeFont font
				TTFG.quit
		quit
	where
		textColor		=	Color 0 0 0
		framesPerSecond	=	20
		secsPerFrame	=	1000 `div` framesPerSecond
