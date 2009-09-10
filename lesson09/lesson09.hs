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

data Clip =
	MouseOver
	| MouseOut
	| MouseDown
	| MouseUp
	deriving (Eq, Ord, Enum, Show)

data Button = Button { box :: Rect, clip :: Clip }

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

makeButton :: Int -> Int -> Int -> Int -> Button
makeButton x y w h = Button { box=Rect { rectX=x, rectY=y, rectW=w, rectH=h }, clip=MouseOut }

isInside :: Rect -> Int -> Int -> Bool
isInside Rect {rectX=rx,rectY=ry,rectW=rw,rectH=rh } x y = (x > rx) && (x < rx + rw) && (y > ry) && (y < ry + rh)  

handleEvent :: Button -> Event -> Button
handleEvent Button { box=b } (MouseMotion x y _ _) = Button { box=b, clip=clip' }
	where clip'	= if isInside b (fromIntegral x) (fromIntegral y) then MouseOver else MouseOut
	
handleEvent Button { box=b, clip=c } (MouseButtonDown x y ButtonLeft) = Button { box=b, clip=clip' } 
	where
		isIn	=	isInside b (fromIntegral x) (fromIntegral y)
		clip'	=	if isIn then MouseDown else c
	
handleEvent Button { box=b, clip=c } (MouseButtonUp x y ButtonLeft) = Button { box=b, clip=clip' }
	where
		isIn	=	isInside b (fromIntegral x) (fromIntegral y)
		clip'	=	if isIn then MouseUp else c

handleEvent button _ = button

showButton :: Button -> Array Int Rect -> Surface -> Surface -> IO Bool
showButton Button { box=Rect {rectX=x,rectY=y}, clip=clip } clips buttonSheet screen =
		applySurface x y buttonSheet screen (Just clipRect)
	where
		clipIndex	=	fromEnum clip
		clipRect	=	clips ! clipIndex

main =
	do
		Graphics.UI.SDL.General.init [InitEverything]
		screen	<-	setVideoMode screenWidth screenHeight screenBpp [HWSurface, DoubleBuf, AnyFormat]
		setCaption "Button Test" []
			
		buttonSheet		<-	loadImage "button.png" (Just (0x00, 0xff, 0xff))
		
		let render button = do
			bgColor		<-	(mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff	
			clipRect	<-	Just <$> (getClipRect screen)
			fillRect screen clipRect bgColor
			showButton button clips buttonSheet screen
			Graphics.UI.SDL.flip screen
		
		--loop :: StateT Button IO ()
		let loop		= do
			button	<-	get
				
			event	<-	liftIO pollEvent
			case event of
				Quit	->	return ()
				NoEvent	-> liftIO (render button) >> loop  
				_		-> put (handleEvent button event) >> loop
		
		evalStateT loop myButton
		
		quit
	where
		myButton	=	makeButton 170 120 320 240
		clips		=	listArray (0, 3) [	Rect { rectX=0, rectY=0, rectW=320, rectH=240 },
											Rect { rectX=320, rectY=0, rectW=320, rectH=240 },
											Rect { rectX=0, rectY=240, rectW=320, rectH=240 },
											Rect { rectX=320, rectY=240, rectW=320, rectH=240 }] :: Array Int Rect
