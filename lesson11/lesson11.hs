{-
	The MIT License
	Copyright (c) 2010 Korcan Hussein

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
import Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Graphics.UI.SDL.TTF
import qualified Graphics.UI.SDL.TTF.General as TTFG

import Graphics.UI.SDL.Mixer

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data AppConfig = AppConfig {
    music   :: Music,
    scratch :: Chunk,
    high    :: Chunk,
    med     :: Chunk,
    low     :: Chunk
}

type AppEnv = ReaderT AppConfig IO

initEnv :: IO AppConfig
initEnv = do

    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Monitor Music" []
    
    background <- loadImage "background.png" (Just (0x00, 0xff, 0xff))
    font       <- openFont "lazy.ttf" 30
    
    openAudio 22050 AudioS16Sys 2 4096
    
    music      <- loadMUS "beat.wav"
    scratch    <- loadWAV "scratch.wav"
    high       <- loadWAV "high.wav"
    med        <- loadWAV "medium.wav"
    low        <- loadWAV "low.wav"
    
    message    <- renderTextSolid font "Press 1, 2, 3, or 4 to play a sound effect" textColor
    message2   <- renderTextSolid font "Press 9 to play or pause the music" textColor
    message3   <- renderTextSolid font "Press 0 to stop the music" textColor
    
    let x = (screenWidth - (surfaceGetWidth message)) `div` 2
    applySurface 0 0 background screen Nothing              
    applySurface x 100 message screen Nothing
    applySurface x 200 message2 screen Nothing
    applySurface x 300 message3 screen Nothing
    
    Graphics.UI.SDL.flip screen
    
    return $ AppConfig music scratch high med low
    
 where
    screenWidth  = 640
    screenHeight = 480
    screenBpp    = 32
    textColor    = Color 0 0 0
    
loop :: AppEnv ()
loop = do
    app <- ask
    quit <- whileEvents $ \event -> liftIO $ do
        case event of
            (KeyDown (Keysym SDLK_1 _ _)) -> playChannel (-1) (scratch app) 0 >> return ()
            (KeyDown (Keysym SDLK_2 _ _)) -> playChannel (-1) (high app) 0 >> return ()
            (KeyDown (Keysym SDLK_3 _ _)) -> playChannel (-1) (med app) 0 >> return ()
            (KeyDown (Keysym SDLK_4 _ _)) -> playChannel (-1) (low app) 0 >> return ()
            (KeyDown (Keysym SDLK_9 _ _)) -> do
                isNotPlayingMusic <- fmap not playingMusic
                if isNotPlayingMusic
                    then playMusic (music app) (-1)
                    else do
                        pausedMusic <- pausedMusic
                        if pausedMusic
                            then resumeMusic
                            else pauseMusic
            (KeyDown (Keysym SDLK_0 _ _)) -> haltMusic
            _ -> return ()
    
    unless quit loop

whileEvents :: (MonadIO m) => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> IO ()
runLoop = runReaderT loop

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    result <- TTFG.init
    if not result
        then putStr "Failed to init ttf\n"
        else do
            env <- initEnv            
            runLoop env            
            TTFG.quit
            closeAudio