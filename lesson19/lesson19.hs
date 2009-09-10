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

data Circle = Circle { pos :: (Int, Int), radius :: Int }

newtype RectArray = RectArray { rectArray ::  [Rect] }

data Dot = Dot { circle :: Circle, vel :: (Int, Int) }

dotWidth	=	20
dotHeight	=	20

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

instance Collidable Circle Circle where
	intersects (Circle (aX,aY) aRad) (Circle (bX, bY) bRad) = distance aX aY bX bY < (fromIntegral $ aRad + bRad)

instance Collidable Circle RectArray where
	intersects (Circle (x,y) rad) (RectArray rs) =
		any (\r@(Rect bx by bw bh) ->
				let (bx1, by1)	=	(bx, by);
					(bx2, by2)	=	(bx + bw, by);
					(bx3, by3)	=	(bx, by + bh);
					(bx4, by4)	=	(bx + bw, by + bh)
				in
					intersects r vertLine || intersects r horizLine ||
					distance x y bx1 by1 < rad' || distance x y bx2 by2 < rad' ||
					distance x y bx3 by3 < rad' || distance x y bx4 by4 < rad') rs					
	 where
	 	rad'		=	fromIntegral rad
	 	diameter	=	rad * 2
	 	vertLine	=	Rect x (y - rad) 0 diameter
	 	horizLine	=	Rect (x - rad) y diameter 0

dotWidthHalf	=	dotWidth `div` 2
dotHeightHalf	=	dotHeight `div` 2
defaultDot	=	Dot { circle=Circle (dotWidthHalf,dotWidthHalf) dotWidthHalf, vel=(0,0) }

dotAccel		=	1

handleInput :: Dot -> Event -> Dot
handleInput Dot { vel=(dx,dy), circle=c } (KeyDown (Keysym SDLK_UP _ _))	=	Dot { vel=(dx, dy - dotAccel), circle=c }
handleInput Dot { vel=(dx,dy), circle=c } (KeyDown (Keysym SDLK_DOWN _ _))	=	Dot { vel=(dx, dy + dotAccel), circle=c }
handleInput Dot { vel=(dx,dy), circle=c } (KeyDown (Keysym SDLK_LEFT _ _))	=	Dot { vel=(dx - dotAccel, dy), circle=c }
handleInput Dot { vel=(dx,dy), circle=c } (KeyDown (Keysym SDLK_RIGHT _ _))	=	Dot { vel=(dx + dotAccel, dy), circle=c }

handleInput Dot { vel=(dx,dy), circle=c } (KeyUp (Keysym SDLK_UP _ _))		=	Dot { vel=(dx, dy + dotAccel), circle=c }
handleInput Dot { vel=(dx,dy), circle=c } (KeyUp (Keysym SDLK_DOWN _ _))	=	Dot { vel=(dx, dy - dotAccel), circle=c }
handleInput Dot { vel=(dx,dy), circle=c } (KeyUp (Keysym SDLK_LEFT _ _))	=	Dot { vel=(dx + dotAccel, dy), circle=c }
handleInput Dot { vel=(dx,dy), circle=c } (KeyUp (Keysym SDLK_RIGHT _ _))	=	Dot { vel=(dx - dotAccel, dy), circle=c }

handleInput d _ = d

move :: Dot -> RectArray -> Circle -> Dot
move Dot { vel=v@(dx,dy), circle=c1@(Circle (x,y) rad) } rs c2 = Dot { circle=Circle (x'', y'') rad, vel=v }
 where
	(x', y')	=	(x + dx, y + dy)
	
	c1'	=	Circle (x',y) rad
	x''	=	if (x' - dotWidthHalf) < 0 || (x' + dotWidthHalf) > screenWidth || intersects c1' rs || intersects c1' c2 then x else x'
	
	c1'' = Circle (x'', y') rad
	y''	=	if (y' - dotHeightHalf) < 0 || (y' + dotHeightHalf) > screenHeight || intersects c1'' rs || intersects c1'' c2 then y else y'

showCircle (Circle (x,y) r) = applySurface (x - r) (y - r) 
showDot Dot { circle=c } = showCircle c 

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
				
				dot	<-	loadImage "dot.bmp" (Just (0x00, 0xff, 0xff))
								
				let render = do
					(fps, myDot) <- get
					 
					jrect		<-	Just <$> (liftIO $ getClipRect screen)
					white		<-	liftIO $ mapRGB' screen 0xff 0xff 0xff
					black		<-	liftIO $ mapRGB' screen 0x00 0x00 0x00
					
					liftIO $ fillRect screen jrect white
					liftIO $ fillRect screen (Just $ head $ rectArray box) black
					
					liftIO $ showCircle otherDot dot screen Nothing
					liftIO $ showDot myDot dot screen Nothing
																				
					liftIO $ Graphics.UI.SDL.flip screen
					
					ticks <- liftIO $ getTimerTicks fps
					if ticks < secsPerFrame
						then liftIO $ delay $ secsPerFrame - ticks
						else return () --) :: StateT (Timer, Dot) IO ()
				
				let loop = do
					(fps, myDot)	<-	get
					event			<-	liftIO pollEvent					
					case event of
						Quit	->	return ()
						NoEvent	->	do
							put (fps, move myDot box otherDot)
							render
							(fps', myDot') <- get
							fps'' <- liftIO $ start fps
							put (fps'', myDot')
							loop
						_		-> put (fps, handleInput myDot event) >> loop
				
				fps		<-	start defaultTimer
				execStateT loop (fps, myDot)
				
				--closeFont font
				TTFG.quit
		quit
	where
		mapRGB'			=	mapRGB . surfaceGetPixelFormat
		textColor		=	Color 0 0 0
		framesPerSecond	=	20
		secsPerFrame	=	1000 `div` framesPerSecond
		
		box				=	RectArray [Rect 60 60 40 40]
		myDot			=	defaultDot
		otherDot		=	Circle (30, 30) (dotWidth `div` 2)
