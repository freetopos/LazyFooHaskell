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
import Data.Array.IArray

import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Timer

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

fooWidth  = 64
fooHeight = 205

dotWidth  = 20
dotHeight = 20

dotWidthHalf = dotWidth `div` 2
dotHeightHalf = dotHeight `div` 2

dotAccel = fooWidth `div` 4

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data FooDir = MoveLeft | MoveRight deriving (Show, Enum, Bounded, Eq, Ord)

data Foo = Foo { offset :: Int, velocity :: Int, frame :: Int, status :: FooDir }

defaultFoo = Foo 0 0 0 MoveRight

handleInput :: Event -> Foo -> Foo
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) foo@Foo { velocity=velocity }  = foo { velocity=velocity - dotAccel }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) foo@Foo { velocity=velocity } = foo { velocity=velocity + dotAccel }

handleInput (KeyUp (Keysym SDLK_LEFT _ _)) foo@Foo { velocity=velocity }  = foo { velocity=velocity + dotAccel }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) foo@Foo { velocity=velocity } = foo { velocity=velocity - dotAccel }
handleInput _ d = d

move :: Foo -> Foo
move foo@Foo { offset=offset, velocity=velocity } = foo { offset=offset'' }
 where
    offset' = offset + velocity
    offset'' = if offset' < 0 || offset' + fooWidth > screenWidth then offset else offset'

updateFoo :: Foo -> Foo
updateFoo foo@ Foo { velocity=vel, frame=frame, status=status }
    | vel < 0   = foo { frame=frame', status=MoveLeft }
    | vel > 0   = foo { frame=frame', status=MoveRight }
    | otherwise = foo { frame=0, status=status }
 where
    frame' = if (frame + 1) >= 4 then 0 else frame + 1

showFoo Foo { offset=x, frame=f, status=MoveLeft } src dst clipsLeft _   = applySurface x (screenHeight - fooHeight) src dst (Just (clipsLeft ! (fromIntegral f)))
showFoo Foo { offset=x, frame=f, status=MoveRight } src dst _ clipsRight = applySurface x (screenHeight - fooHeight) src dst (Just (clipsRight ! (fromIntegral f)))

data AppData = AppData {
    foo :: Foo,
    fps :: Timer
}

data AppConfig = AppConfig {
    screen    :: Surface,
    fooSprite :: Surface
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

getFPS :: MonadState AppData m => m Timer
getFPS = liftM fps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { fps = t }

modifyFPSM :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyFPSM act = getFPS >>= act >>= putFPS

getFoo :: MonadState AppData m => m Foo
getFoo = liftM foo get

putFoo :: MonadState AppData m => Foo -> m ()
putFoo t = modify $ \s -> s { foo = t }

modifyFooM :: MonadState AppData m => (Foo -> m Foo) -> m ()
modifyFooM act = getFoo >>= act >>= putFoo

modifyFoo :: MonadState AppData m => (Foo -> Foo) -> m ()
modifyFoo fn = fn `liftM` getFoo >>= putFoo

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getFooSprite :: MonadReader AppConfig m => m Surface
getFooSprite = liftM fooSprite ask

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Animation Test" []
    foo <-  loadImage "foo.png" (Just (0x00, 0xff, 0xff))
    
    fps <- start defaultTimer
    
    return (AppConfig screen foo, AppData walk fps) 
 where walk = defaultFoo

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit <- whileEvents $ modifyFoo . handleInput
    
    modifyFoo $ updateFoo . move
    
    fps       <- getFPS
    walk      <- getFoo
    fooSprite <- getFooSprite
    screen    <- getScreen    
    liftIO $ do
        jrect <- Just `liftM` getClipRect screen
        white <- mapRGB' screen 0xff 0xff 0xff
        
        fillRect screen jrect white
        showFoo walk fooSprite screen clipsLeft clipsRight
        
        Graphics.UI.SDL.flip screen
        
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks

    unless quit loop
 where
    framesPerSecond = 20
    secsPerFrame    = 1000 `div` framesPerSecond
    mapRGB'         = mapRGB . surfaceGetPixelFormat
    applySurface' x y src dst clip = liftIO (applySurface x y src dst clip)
    clipsRight = listArray (0, 3) [Rect 0 0 fooWidth fooHeight, Rect fooWidth 0 fooWidth fooHeight,
                                   Rect (fooWidth * 2) 0 fooWidth fooHeight, Rect (fooWidth * 3) 0 fooWidth fooHeight] :: Array Int Rect
    clipsLeft = listArray (0, 3) [Rect 0 fooHeight fooWidth fooHeight, Rect fooWidth fooHeight fooWidth fooHeight,
                                  Rect (fooWidth * 2) fooHeight fooWidth fooHeight, Rect (fooWidth * 3) fooHeight fooWidth fooHeight] :: Array Int Rect
    

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