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

import Control.Monad
import Graphics.UI.SDL
import Graphics.UI.SDL.Image

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormat

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src Nothing dst offset
 where offset	=	Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

main = withInit [InitEverything] $ do -- withInit calls quit for us.
		
	screen	<-	setVideoMode screenWidth screenHeight screenBpp [SWSurface]
	
	setCaption "Event test" []
	
	image		<-	loadImage "x.png"
	
	applySurface 0 0 image screen
	
	Graphics.UI.SDL.flip screen	
	
	loop
		
 where
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