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

data StringInput = StringInput { str :: String, text :: Maybe Surface }

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

handleInput :: StringInput -> Event -> StringInput

handleInput (StringInput s surface) (KeyDown (Keysym SDLK_BACKSPACE _ _)) = (StringInput s' surface)
 where
 	s'	=	case s of
 				[] -> [] 
				x : xs -> xs

handleInput (StringInput str surface) (KeyDown (Keysym _ _ ks)) =	StringInput resultStr surface
 where
 	addKey c t
 		| c == ' '
 		|| c >= '0' && c <= '9'
 		|| c >= 'A' && c <= 'Z'
 		|| c >= 'a' && c <= 'z' = t ++ [c]
 		| otherwise = t
 	len	=	length str
 	resultStr	=	if len <= 16 then addKey ks str else str 

handleInput s _ = s

showCentered :: StringInput -> Surface -> IO Bool
showCentered (StringInput _ Nothing) dst		=	return False
showCentered (StringInput _ (Just text)) dst	=	applySurface x y text dst Nothing
 where
 	x	=	(screenWidth - surfaceGetWidth text) `div` 2
 	y	=	(screenHeight - surfaceGetHeight text) `div` 2

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
				setCaption "High Score" []
				enableUnicode True
				
				font		<-	openFont "lazy.ttf" 42
				background	<-	loadImage "background.png" Nothing
				message		<-	renderTextSolid font "New High Score! Enter Name:" textColor
				
				let render = do
					(fps, name, _, message) <- get
					
					liftIO $ applySurface 0 0 background screen Nothing
					liftIO $ applySurface ((screenWidth - surfaceGetWidth message) `div` 2) ((screenHeight `div` 2 - surfaceGetHeight message) `div` 2) message screen Nothing
					
					liftIO $ showCentered name screen
					
					liftIO $ Graphics.UI.SDL.flip screen
					
					ticks <- liftIO $ getTimerTicks fps
					if ticks < secsPerFrame
						then liftIO $ delay $ secsPerFrame - ticks
						else return () --) :: StateT (Timer, Dot, Camera) IO ()
				
				let loop = do
					(fps, name, nameEntered, message)	<-	get
					event			<-	liftIO pollEvent					
					case event of
						Quit	->	return ()
						NoEvent	->	do
							render
							(fps', name, nameEntered, message) <- get
							fps'' <- liftIO $ start fps
							put (fps'', name, nameEntered, message)
							loop
						_	-> do
							if nameEntered
								then return ()
								else do
									let (StringInput s t) = handleInput name event
									text <- if s /= (str name)
												then if null s 
													then return Nothing
													else liftIO $ Just <$> renderTextSolid font s textColor
												else return t
									let name' = StringInput s text
									case event of
										(KeyDown (Keysym SDLK_RETURN _ _)) -> do
											message' <- liftIO $ renderTextSolid font "Rank: 1st" textColor
											put (fps, name', True, message')
										_ -> put (fps, name', False, message)
							loop --) :: StateT (Timer, StringInput, Bool, Surface) IO ()
				
				fps		<-	start defaultTimer
				execStateT loop (fps, (StringInput [] Nothing), False, message)
				
				enableUnicode False
				closeFont font
				TTFG.quit
		quit
	where
		mapRGB'			=	mapRGB . surfaceGetPixelFormat
		textColor		=	Color 0xFF 0xFF 0xFF
		framesPerSecond	=	20
		secsPerFrame	=	1000 `div` framesPerSecond
