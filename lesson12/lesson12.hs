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

type TimerState a = StateT (Word32, Bool) IO a

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
				setCaption "Timer Test" []
				
				background	<-	loadImage "background.png" Nothing--(Just (0x00, 0xff, 0xff))
				font		<-	openFont "lazy.ttf" 36
				startStop	<-	renderTextSolid font "Press S to start or stop the timer" textColor
				
				let render = do
					liftIO $ applySurface 0 0 background screen Nothing
					liftIO $ applySurface ((screenWidth - (surfaceGetWidth startStop)) `div` 2) 200 startStop screen Nothing
					
					(start', running) <- get
					if running
						then do
							currTime <- liftIO getTicks
							let time = "Timer: " ++ show (currTime - start')
							seconds <- liftIO $ renderTextSolid font time textColor
							liftIO $ applySurface ((screenWidth - (surfaceGetWidth seconds)) `div` 2) 50 seconds screen Nothing
							return ()
						else return ()
					liftIO $ Graphics.UI.SDL.flip screen
				
				let loop = do
					event	<-	liftIO pollEvent
					case event of
						Quit	->	return ()
						(KeyDown (Keysym SDLK_s _ _)) -> do
							(_, running) <- get							
							if running
								then put (0, False)
								else do
									start' <- liftIO getTicks
									put (start', True)
							loop
						NoEvent	-> render >> loop  
						_		-> loop
				
				start		<-	getTicks
				execStateT loop (start, True)
				
				closeFont font
				TTFG.quit
		quit
	where
		textColor	=	Color 0xF0 0xFF 0xF0
