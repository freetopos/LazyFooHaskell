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

data Dot = Dot { pos :: (Int, Int), box :: RectArray, vel :: (Int, Int) }

dotWidth		=	20
dotHeight	=	20

class Collidable a where
	intersects :: a -> a -> Bool

instance Collidable Rect where
	--intersects :: Rect -> Rect -> Bool
	intersects Rect {rectX=ax,rectY=ay,rectW=aw,rectH=ah } Rect {rectX=bx,rectY=by,rectW=bw,rectH=bh } =
		bottomA > topB && topA < bottomB && rightA > leftB && leftA < rightB
 	 where
 		leftA	=	ax
 		rightA	=	ax + aw
 		topA	=	ay
 		bottomA	=	ay + ah
 		
 		leftB	=	bx
	 	rightB	=	bx + bw
	 	topB	=	by
	 	bottomB	=	by + bh

instance Collidable RectArray where
	--intersects :: Rect -> Rect -> Bool
	intersects (RectArray lhs) (RectArray rhs) = any (\x -> any (intersects x) rhs) lhs 

shiftBoxes :: Dot -> Dot
shiftBoxes Dot { pos=(x,y), box=RectArray rs, vel=v } = Dot { pos=(x,y), vel=v, box=RectArray $ snd $ mapAccumL shiftBox 0 rs }
 where
 	shiftBox r (Rect _ _ bw bh) = (r + bh, Rect (x + ((dotWidth - bw) `div` 2)) (y + r) bw bh)
 		

makeDot x y = shiftBoxes Dot
	{
		pos=(x,y),
		box=RectArray [Rect 0 0 6 1,  Rect 0 0 10 1, Rect 0 0 14 1, Rect 0 0 16 2, Rect 0 0 18 2, Rect 0 0 20 6,
					   Rect 0 0 18 2, Rect 0 0 16 2, Rect 0 0 14 1, Rect 0 0 10 1, Rect 0 0 6 1],
		vel=(0,0)
	}

dotAccel		=	1

handleInput :: Dot -> Event -> Dot
handleInput Dot { box=b, vel=(dx,dy), pos=p } (KeyDown (Keysym SDLK_UP _ _))	=	Dot { box=b, vel=(dx, dy - dotAccel), pos=p }
handleInput Dot { box=b, vel=(dx,dy), pos=p } (KeyDown (Keysym SDLK_DOWN _ _))	=	Dot { box=b, vel=(dx, dy + dotAccel), pos=p }
handleInput Dot { box=b, vel=(dx,dy), pos=p } (KeyDown (Keysym SDLK_LEFT _ _))	=	Dot { box=b, vel=(dx - dotAccel, dy), pos=p }
handleInput Dot { box=b, vel=(dx,dy), pos=p } (KeyDown (Keysym SDLK_RIGHT _ _))	=	Dot { box=b, vel=(dx + dotAccel, dy), pos=p }

handleInput Dot { box=b, vel=(dx,dy), pos=p } (KeyUp (Keysym SDLK_UP _ _))		=	Dot { box=b, vel=(dx, dy + dotAccel), pos=p }
handleInput Dot { box=b, vel=(dx,dy), pos=p } (KeyUp (Keysym SDLK_DOWN _ _))	=	Dot { box=b, vel=(dx, dy - dotAccel), pos=p }
handleInput Dot { box=b, vel=(dx,dy), pos=p } (KeyUp (Keysym SDLK_LEFT _ _))	=	Dot { box=b, vel=(dx + dotAccel, dy), pos=p }
handleInput Dot { box=b, vel=(dx,dy), pos=p } (KeyUp (Keysym SDLK_RIGHT _ _))	=	Dot { box=b, vel=(dx - dotAccel, dy), pos=p }

handleInput d _ = d

move :: Dot -> RectArray -> Dot
move Dot { box=b, vel=v@(dx,dy), pos=(x,y) } rs = Dot { box=box'', pos=(x'', y''), vel=v }
 where
	(x', y')	=	(x + dx, y + dy)
	
	rs'			=	box $ shiftBoxes $ Dot { pos=(x',y), vel=v, box=b }
	(x'', box')	=	if x' < 0 || (x' + dotWidth) > screenWidth || intersects rs' rs then (x, b) else (x', rs')
	
	rs''			=	box $ shiftBoxes $ Dot { pos=(x'',y'), vel=v, box=b } 
	(y'', box'')	=	if y' < 0 || (y' + dotHeight) > screenHeight || intersects rs'' rs then (y, box') else (y', rs'')

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
				
				dot	<-	loadImage "dot.bmp" (Just (0x00, 0xff, 0xff))
								
				let render = (do
					(fps, myDot) <- get
					 
					jrect		<-	Just <$> (liftIO $ getClipRect screen)
					white		<-	liftIO $ mapRGB' screen 0xff 0xff 0xff
					blue		<-	liftIO $ mapRGB' screen 0x00 0x00 0xff
					
					
					liftIO $ fillRect screen jrect white
					
					liftIO $ showDot myDot dot screen Nothing
					--mapM_ (\x -> liftIO $ fillRect screen (Just x) blue) $ rectArray $ box myDot 
					liftIO $ showDot otherDot dot screen Nothing
										
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
							put (fps, move myDot $ box otherDot)
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
		
		myDot			=	makeDot 0 0
		otherDot		=	makeDot 20 20
