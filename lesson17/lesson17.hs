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
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Word

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Timer

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

squareWidth  = 20
squareHeight = 20

halfDotWidth  = squareWidth `div` 2
halfDotHeight = squareHeight `div` 2

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data Square = Square { box :: Rect, vel :: (Int, Int) }

squareDot = Square { box=Rect {rectX=0,rectY=0,rectW=squareWidth,rectH=squareHeight }, vel=(0,0) }

intersects :: Rect -> Rect -> Bool
intersects Rect {rectX=ax,rectY=ay,rectW=aw,rectH=ah } Rect {rectX=bx,rectY=by,rectW=bw,rectH=bh } =
    bottomA > topB && topA < bottomB && rightA > leftB && leftA < rightB
 where
    leftA   = ax
    rightA  = ax + aw
    topA    = ay
    bottomA = ay + ah
    
    leftB   = bx
    rightB  = bx + bw
    topB    = by
    bottomB = by + bh

handleInput :: Event -> Square -> Square
handleInput (KeyDown (Keysym SDLK_UP _ _)) s@Square { vel=(dx,dy) }    = s { vel=(dx, dy - halfDotHeight) }
handleInput (KeyDown (Keysym SDLK_DOWN _ _)) s@Square { vel=(dx,dy) }  = s { vel=(dx, dy + halfDotHeight) }
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) s@Square { vel=(dx,dy) }  = s { vel=(dx - halfDotWidth, dy) }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) s@Square { vel=(dx,dy) } = s { vel=(dx + halfDotWidth, dy) }

handleInput (KeyUp (Keysym SDLK_UP _ _)) s@Square { vel=(dx,dy) }    = s { vel=(dx, dy + halfDotHeight) }
handleInput (KeyUp (Keysym SDLK_DOWN _ _)) s@Square { vel=(dx,dy) }  = s { vel=(dx, dy - halfDotHeight) }
handleInput (KeyUp (Keysym SDLK_LEFT _ _)) s@Square { vel=(dx,dy) }  = s { vel=(dx + halfDotWidth, dy) }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) s@Square { vel=(dx,dy) } = s { vel=(dx - halfDotWidth, dy) }

handleInput _ d = d

move :: Rect -> Square -> Square
move wall s@Square { box=Rect x y w h, vel=v@(dx,dy) } = s { box=Rect x'' y'' w h } 
 where
    x'  = x + dx
    y'  = y + dy
    x'' = if x' < 0 || (x' + squareWidth) > screenWidth || intersects (Rect x' y w h)  wall then x else x' 
    y'' = if y' < 0 || (y' + squareHeight) > screenHeight || intersects (Rect x'' y' w h) wall then y else y'

showSq Square { box=Rect x y _ _ } = applySurface x y 

data AppData = AppData {
    square :: Square,
    fps    :: Timer
}

data AppConfig = AppConfig {
    screen       :: Surface,
    squareSprite :: Surface
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

getFPS :: MonadState AppData m => m Timer
getFPS = liftM fps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { fps = t }

modifyFPSM :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyFPSM act = getFPS >>= act >>= putFPS

getSquare :: MonadState AppData m => m Square
getSquare = liftM square get

putSquare :: MonadState AppData m => Square -> m ()
putSquare t = modify $ \s -> s { square = t }

modifySqM :: MonadState AppData m => (Square -> m Square) -> m ()
modifySqM act = getSquare >>= act >>= putSquare

modifySq :: MonadState AppData m => (Square -> Square) -> m ()
modifySq fn = fn `liftM` getSquare >>= putSquare

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getSquareSprite :: MonadReader AppConfig m => m Surface
getSquareSprite = liftM squareSprite ask

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Move the Square" []
    
    square <- loadImage "square.bmp" Nothing--(Just (0x00, 0xff, 0xff))
    
    fps <- start defaultTimer    
    return (AppConfig screen square, AppData squareDot fps) 

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit <- whileEvents $ modifySq . handleInput
    
    modifySq $ move wall
    
    fps          <- getFPS
    mySquare     <- getSquare
    squareSprite <- getSquareSprite
    screen       <- getScreen    
    liftIO $ do
        jrect     <- Just `liftM` getClipRect screen
        white     <- mapRGB' screen 0xff 0xff 0xff
        wallColor <- mapRGB' screen 0x77 0x77 0x77
        
        fillRect screen jrect white
        fillRect screen (Just wall) wallColor
         
        showSq mySquare squareSprite screen Nothing
        
        Graphics.UI.SDL.flip screen
        
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks

    unless quit loop
 where
    framesPerSecond = 20
    secsPerFrame    = 1000 `div` framesPerSecond
    wall            = Rect 300 40 40 400
    mapRGB'         = mapRGB . surfaceGetPixelFormat
    applySurface' x y src dst clip = liftIO (applySurface x y src dst clip)

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    (env, state) <- initEnv
    runLoop env state
