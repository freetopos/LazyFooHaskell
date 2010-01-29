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

data MessageDir = MessageDir {
    upMessage    :: Surface,
    downMessage  :: Surface,
    leftMessage  :: Surface,
    rightmessage :: Surface
}

data AppConfig = AppConfig {
    screen       :: Surface,
    background   :: Surface,
    messageDir   :: MessageDir,
    screenWidth  :: Int,
    screenHeight :: Int,
    screenBpp    :: Int
}

type AppState = StateT (Maybe Surface) IO
type AppEnv = ReaderT AppConfig AppState

runLoop :: AppConfig -> IO ()
runLoop config = (evalStateT . runReaderT loop) config Nothing

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

initEnv :: IO AppConfig
initEnv = do
    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Press an Arrow Key" []
    
    background   <- loadImage "background.png" $ Just (0x00, 0xff, 0xff)
    font         <- openFont "lazy.ttf" 72
    
    upMessage    <- renderTextSolid font "Up was pressed" textColor
    downMessage  <- renderTextSolid font "Down was pressed" textColor
    leftMessage  <- renderTextSolid font "Left was pressed" textColor
    rightmessage <- renderTextSolid font "Right was pressed" textColor
    
    applySurface 0 0 background screen Nothing
    
    let msgDir = MessageDir upMessage downMessage leftMessage rightmessage
    return $ AppConfig screen background msgDir screenWidth screenHeight screenBpp
 where
    textColor    = Color 0 0 0
    screenWidth  = 640
    screenHeight = 480
    screenBpp    = 32

loop :: AppEnv ()
loop = do
    
    quit <- whileEvents
    
    screen       <- fmap screen ask
    background   <- fmap background ask
    screenWidth  <- fmap screenWidth ask
    screenHeight <- fmap screenHeight ask
    msg          <- get
    
    case msg of
        Nothing -> return ()
        Just message -> do
            applySurface' 0 0 background screen Nothing
            applySurface' ((screenWidth - surfaceGetWidth message) `div` 2) ((screenHeight - surfaceGetHeight message) `div` 2) message screen Nothing
            put Nothing			
    
    liftIO $ Graphics.UI.SDL.flip screen
    
    unless quit loop

 where
    applySurface' x y src dst clip = liftIO (applySurface x y src dst clip)
 	
whileEvents :: AppEnv Bool
whileEvents = do
    mdir  <- fmap messageDir ask
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        (KeyDown (Keysym key _ _)) -> do
            case key of
            	SDLK_UP    -> put $ Just $ upMessage mdir
            	SDLK_DOWN  -> put $ Just $ downMessage mdir
            	SDLK_LEFT  -> put $ Just $ leftMessage mdir
            	SDLK_RIGHT -> put $ Just $ rightmessage mdir
            	_          -> put Nothing
            return False
        NoEvent -> return False
        _       ->  whileEvents

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    
    result <- TTFG.init
    if not result
        then putStr "Failed to init ttf\n"
        else do
            env <- initEnv
            
            runLoop env
            			
            TTFG.quit