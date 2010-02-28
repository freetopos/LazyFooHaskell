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
{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Data.Word

import Foreign
import Foreign.C.Types

import Control.Monad.Reader
import Control.Monad.State

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Utilities as Util

import Timer

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

alphaOpaque = 255
alphaTransparent = 0

-- `SDL_GetKeyState' is not defined in Graphic.UI.SDL
foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

type KeyProc = SDLKey -> Bool
-- this function comes from mokehehe's super nario bros: http://github.com/mokehehe/monao in the file "AppUtil.hs"
getKeyState :: IO KeyProc
getKeyState = alloca $ \numkeysPtr -> do
    keysPtr <- sdlGetKeyState numkeysPtr
    return $ \k -> (/= 0) $ unsafePerformIO $ (peekByteOff keysPtr $ fromIntegral $ Util.fromEnum k :: IO Word8)

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data AppData = AppData {
    alpha :: Word8,
    fps   :: Timer
}

data AppConfig = AppConfig {
    screen :: Surface,
    front  :: Surface,
    back   :: Surface
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

getFPS :: MonadState AppData m => m Timer
getFPS = liftM fps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { fps = t }

modifyFPSM :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyFPSM act = getFPS >>= act >>= putFPS

getAlpha :: MonadState AppData m => m Word8
getAlpha = liftM alpha get

putAlpha :: MonadState AppData m => Word8 -> m ()
putAlpha t = modify $ \s -> s { alpha = t }

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getFront :: MonadReader AppConfig m => m Surface
getFront = liftM front ask

getBack :: MonadReader AppConfig m => m Surface
getBack = liftM back ask

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Alpha Test" []
    
    back  <- loadImage "fadein.png" Nothing
    front <- loadImage "fadeout.png" Nothing
    fps   <- start defaultTimer
    
    return (AppConfig screen front back, AppData alpha fps) 
 where alpha = alphaOpaque

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit <- whileEvents $ \_ -> return ()
    
    keyState <- liftIO getKeyState
    
    when (keyState SDLK_UP) $ do
        alpha <- getAlpha
        when (alpha < alphaOpaque) $ do
            putAlpha (alpha + 5)
    
    when (keyState SDLK_DOWN) $ do
        alpha <- getAlpha
        when (alpha > alphaTransparent) $ do
            putAlpha (alpha - 5)
    
    fps    <- getFPS
    alpha  <- getAlpha
    front  <- getFront
    back   <- getBack
    screen <- getScreen    
    liftIO $ do
        setAlpha front [SrcAlpha] alpha
        
        applySurface 0 0 back screen Nothing
        applySurface 0 0 front screen Nothing
                
        Graphics.UI.SDL.flip screen
        
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks

    unless quit loop
 where
    framesPerSecond = 20
    secsPerFrame    = 1000 `div` framesPerSecond

whileEvents :: (MonadIO m) => (Event -> m ()) -> m Bool
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
