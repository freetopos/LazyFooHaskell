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
import Control.Monad

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s =	return s
setColorKey' (Just (r, g, b)) surface	=	(mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
	where offset	=	Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

main = withInit [InitEverything] $ do -- withInit calls quit for us.

	result <- TTFG.init
	if not result
		then do
			putStr "Failed to init ttf\n"
			return ()
		else do
			screen	<-	setVideoMode screenWidth screenHeight screenBpp [SWSurface]
			setCaption "TTF Test" []
			
			background	<-	loadImage "background.png" (Just (0x00, 0xff, 0xff))
			font		<-	openFont "lazy.ttf" 28
			message		<-	renderTextSolid font "The quick brown fox jumps over the lazy hound" textColor
			
			applySurface 0 0 background screen Nothing
			applySurface 0 200 message screen Nothing
									
			Graphics.UI.SDL.flip screen		
					
			loop
			
			TTFG.quit
 where
 	textColor		=	Color 255 255 255
	screenWidth		=	640
	screenHeight	=	480
	screenBpp		=	32
	
	loop = do -- or whileEvents >>= (Prelude.flip unless) loop 
		quit <- whileEvents
		unless quit loop
	
	whileEvents = do
		event	<-	pollEvent
		case event of
			Quit	->	return True
			NoEvent	->	return False
			_		->	whileEvents