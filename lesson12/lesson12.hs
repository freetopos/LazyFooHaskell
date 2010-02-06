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
import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Graphics.UI.SDL.TTF
import qualified Graphics.UI.SDL.TTF.General as TTFG

import Graphics.UI.SDL.Mixer

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

type TimerState = (Word32, Bool)

data AppConfig = AppConfig {
    screen       :: Surface,
    background   :: Surface,
    startStop    :: Surface,
    font         :: Font,
    textColor    :: Color
}

type AppState = StateT TimerState IO
type AppEnv = ReaderT AppConfig AppState

initEnv :: IO (AppConfig, TimerState)
initEnv = do
    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Timer Test" []
    
    background <- loadImage "background.png" Nothing
    font       <- openFont "lazy.ttf" 36
    startStop  <- renderTextSolid font "Press S to start or stop the timer" textColor
    start      <- getTicks
    
    return (AppConfig screen background startStop font textColor, (start, True))    
 where textColor = Color 0xF0 0xFF 0xF0

loop :: AppEnv ()
loop = do

    quit <- whileEvents $ \event -> do
                case event of
                    (KeyDown (Keysym SDLK_s _ _)) -> do
                        running <- fmap snd get                         
                        if running
                            then put (0, False)
                            else do
                                start <- liftIO getTicks
                                put (start, True)
                    _ -> return ()
    
    AppConfig screen background startStop font textColor <- ask
    
    applySurface' 0 0 background screen Nothing
    applySurface' ((screenWidth - (surfaceGetWidth startStop)) `div` 2) 200 startStop screen Nothing
    
    (start, running) <- get
    when running $ liftIO $ do
        currTime <- getTicks
        let time = "Timer: " ++ show (currTime - start)
        seconds <- renderTextSolid font time textColor
        applySurface ((screenWidth - (surfaceGetWidth seconds)) `div` 2) 50 seconds screen Nothing
        return ()

    liftIO $ Graphics.UI.SDL.flip screen
    unless quit loop

 where
    applySurface' x y src dst clip = liftIO (applySurface x y src dst clip)
    
whileEvents :: (MonadIO m) => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> TimerState -> IO ()
runLoop = evalStateT . runReaderT loop

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    result <- TTFG.init
    if not result
    	then putStr "Failed to init ttf\n"
    	else do
            (env, timerState) <- initEnv
            runLoop env timerState
            TTFG.quit