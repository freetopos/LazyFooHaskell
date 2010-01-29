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

import Control.Applicative
import Control.Monad

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s =	return s
setColorKey' (Just (r, g, b)) surface	=	(mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
	where offset	=	Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

main = withInit [InitEverything] $ do -- withInit calls quit for us.
	
	screen	<-	setVideoMode screenWidth screenHeight screenBpp [SWSurface]
	setCaption "Split the dots" []
	
	dots		<-	loadImage "dots.png" (Just (0x00, 0xff, 0xff))
			
	bgColor		<-	(mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff
	clipRect	<-	Just <$> (getClipRect screen)
	fillRect screen clipRect bgColor
			
	applySurface 0 0 dots screen $ clips ! 0
	applySurface 540 0 dots screen $ clips ! 1
	applySurface 0 380 dots screen $ clips ! 2
	applySurface 540 380 dots screen $ clips ! 3
	
	Graphics.UI.SDL.flip screen		
	
	loop
	
 where
	clips	=	listArray (0, 3) [	Just $ Rect { rectX=0, rectY=0, rectW=100, rectH=100 },
									Just $ Rect { rectX=100, rectY=0, rectW=100, rectH=100 },
									Just $ Rect { rectX=0, rectY=100, rectW=100, rectH=100 },
									Just $ Rect { rectX=100, rectY=100, rectW=100, rectH=100 }] :: Array Int (Maybe Rect)
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
	
