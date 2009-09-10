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

main =
	do
		Graphics.UI.SDL.General.init [InitEverything]
		openAudio 22050 AudioS16Sys 2 4096
		
		result <- TTFG.init
		if not result
			then do
				putStr "Failed to init ttf\n"
				return ()
			else do
				screen	<-	setVideoMode screenWidth screenHeight screenBpp [HWSurface, DoubleBuf, AnyFormat]
				setCaption "Monitor Music" []
				
				background	<-	loadImage "background.png" (Just (0x00, 0xff, 0xff))
				font		<-	openFont "lazy.ttf" 30
				
				music		<-	loadMUS "beat.wav"
				scratch		<-	loadWAV "scratch.wav"
				high		<-	loadWAV "high.wav"
				med			<-	loadWAV "medium.wav"
				low			<-	loadWAV "low.wav"
				
				message		<-	renderTextSolid font "Press 1, 2, 3, or 4 to play a sound effect" textColor
				message2	<-	renderTextSolid font "Press 9 to play or pause the music" textColor
				message3	<-	renderTextSolid font "Press 0 to stop the music" textColor
				
				let loop		= do
					event	<-	pollEvent
					case event of
						Quit	->	return ()
						(KeyDown (Keysym SDLK_1 _ _)) -> playChannel (-1) scratch 0 >> loop
						(KeyDown (Keysym SDLK_2 _ _)) -> playChannel (-1) high 0 >> loop
						(KeyDown (Keysym SDLK_3 _ _)) -> playChannel (-1) med 0 >> loop
						(KeyDown (Keysym SDLK_4 _ _)) -> playChannel (-1) low 0 >> loop
						(KeyDown (Keysym SDLK_9 _ _)) -> do--unless <$> playingMusic <*> pure (playMusic music (-1))
							isPlayingMusic	<- playingMusic
							if  not isPlayingMusic
								then playMusic music (-1)
								else do
									pausedMusic	<- pausedMusic
									if pausedMusic
										then resumeMusic
										else pauseMusic
							loop
						(KeyDown (Keysym SDLK_0 _ _)) -> haltMusic >> loop
						NoEvent	-> pure id >> loop  
						_		-> loop
				
				let x	=	(screenWidth - (surfaceGetWidth message)) `div` 2
				applySurface 0 0 background screen Nothing				
				applySurface x 100 message screen Nothing
				applySurface x 200 message2 screen Nothing
				applySurface x 300 message3 screen Nothing
				
				Graphics.UI.SDL.flip screen
				loop
				
				freeMusic music
				closeFont font
				TTFG.quit
				closeAudio			
		quit
	where
		textColor	=	Color 0 0 0
