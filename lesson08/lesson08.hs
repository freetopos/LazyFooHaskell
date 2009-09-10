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
				setCaption "Press an Arrow Key" []
				
				background		<-	loadImage "background.png" (Just (0x00, 0xff, 0xff))
				font			<-	openFont "lazy.ttf" 72
				
				upMessage		<-	renderTextSolid font "Up was pressed" textColor
				downMessage		<-	renderTextSolid font "Down was pressed" textColor
				leftMessage		<-	renderTextSolid font "Left was pressed" textColor
				rightmessage	<-	renderTextSolid font "Right was pressed" textColor
				
				let handleEvent event =
					case event of
						(KeyDown (Keysym key _ _))	->
							case key of
								SDLK_UP		->	Just upMessage
								SDLK_DOWN	->	Just downMessage
								SDLK_LEFT	->	Just leftMessage
								SDLK_RIGHT	->	Just rightmessage
								_			->	Nothing
						_	->	Nothing
				
		 		let renderMsg msg =					
					case msg of
						Nothing			->	return ()	
						Just message	->	applySurface 0 0 background screen Nothing >> applySurface ((screenWidth - surfaceGetWidth message) `div` 2) ((screenHeight - surfaceGetHeight message) `div` 2) message screen Nothing >> return ()
				
				let render msg = renderMsg msg >> Graphics.UI.SDL.flip screen
				
				let loop currMsg =
					do
						event	<-	pollEvent
						let nextMessage	=	case (handleEvent event) of
												Nothing -> currMsg
												rest -> rest
						
						case event of
							Quit	->	return ()
							NoEvent	->	render nextMessage >> loop Nothing  
							_		->	loop nextMessage
				
				applySurface 0 0 background screen Nothing
				
				loop Nothing
				
				closeFont font
				TTFG.quit
				quit
	where
		textColor	=	Color 0 0 0
