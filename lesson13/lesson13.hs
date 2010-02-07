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
import Control.Applicative

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Graphics.UI.SDL.TTF
import qualified Graphics.UI.SDL.TTF.General as TTFG

import Timer

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

data AppConfig = AppConfig {
    screen       :: Surface,
    background   :: Surface,
    startStop    :: Surface,
    pauseMessage :: Surface,
    font         :: Font,
    textColor    :: Color
}

type AppState = StateT Timer IO
type AppEnv = ReaderT AppConfig AppState

initEnv :: IO (AppConfig, Timer)
initEnv = do

    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Timer Test" []

    background   <- loadImage "background.png" Nothing--(Just (0x00, 0xff, 0xff))
    font         <- openFont "lazy.ttf" 24
    startStop    <- renderTextSolid font "Press S to start or stop the timer" textColor
    pauseMessage <- renderTextSolid font "Press P to pause or unpause the timer" textColor
    
    myTimer <- start defaultTimer
    
    return (AppConfig screen background startStop pauseMessage font textColor, myTimer) 
 where textColor = Color 0xF0 0xFF 0xF0

loop :: AppEnv ()
loop = do
    quit <- whileEvents $ \event -> do
                case event of
                    KeyDown (Keysym SDLK_s _ _) -> do
                        timer  <- get                            
                        timer' <- liftIO $ if isStarted timer
                                            then return $ stop timer
                                            else start timer
                        put timer'
                    KeyDown (Keysym SDLK_p _ _) -> do
                        timer  <- get
                        timer' <- liftIO $ if isPaused timer
                                            then unpause timer
                                            else Timer.pause timer
                        put timer'
                    _ -> return ()
    
    AppConfig screen background startStop pauseMessage font textColor <- ask    
    timer <- get
    
    -- a local lambda so we don't have use liftIO for all the SDL/Timer actions used which are in IO.
    liftIO $ do
        applySurface 0 0 background screen Nothing
        applySurface ((screenWidth - (surfaceGetWidth startStop)) `div` 2) 200 startStop screen Nothing
        applySurface ((screenWidth - (surfaceGetWidth pauseMessage)) `div` 2) 250 pauseMessage screen Nothing
       
        time <- ((/ 1000.0) . fromIntegral) <$> getTimerTicks timer
        seconds <-  renderTextSolid font ("Timer: " ++ show time) textColor
        
        applySurface ((screenWidth - (surfaceGetWidth seconds)) `div` 2) 0 seconds screen Nothing
        
        Graphics.UI.SDL.flip screen
    
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

runLoop :: AppConfig -> Timer -> IO ()
runLoop = evalStateT . runReaderT loop

main = withInit [InitEverything] $ do -- withInit calls quit for us.

    result <- TTFG.init
    if not result
        then putStr "Failed to init ttf\n"
        else do
            (env, state) <- initEnv
            runLoop env state         
            TTFG.quit