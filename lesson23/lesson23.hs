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
import Data.List as List

import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Graphics.UI.SDL.TTF as TTFG

import Timer

screenWidth  = 640
screenHeight = 480
screenBpp    = 32
textColor    = Color 0xFF 0xFF 0xFF

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data StringInput = StringInput { str :: String, text :: Maybe Surface }

handleInput :: Event -> StringInput -> StringInput

handleInput (KeyDown (Keysym SDLK_BACKSPACE _ _)) si@StringInput { str=s } = si { str=s'  }
 where s' = case s of
                [] -> []
                _ -> List.init s

handleInput (KeyDown (Keysym _ _ ks)) si@StringInput { str=str } = si { str=resultStr }
 where
    addKey c t
        | c == ' '
        || c >= '0' && c <= '9'
        || c >= 'A' && c <= 'Z'
        || c >= 'a' && c <= 'z' = t ++ [c]
        | otherwise = t
    len = length str
    resultStr = if len <= 16 then addKey ks str else str 

handleInput _ s = s

showCentered :: StringInput -> Surface -> IO Bool
showCentered (StringInput _ Nothing) dst     = return False
showCentered (StringInput _ (Just text)) dst = applySurface x y text dst Nothing
 where
    x = (screenWidth - surfaceGetWidth text) `div` 2
    y = (screenHeight - surfaceGetHeight text) `div` 2

data AppData = AppData {
    apName       :: StringInput,
    message      :: Surface,
    namedEntered :: Bool
}

data AppConfig = AppConfig {
    screen     :: Surface,
    background :: Surface,
    front      :: Font
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

getName :: MonadState AppData m => m StringInput
getName = liftM apName get

putName :: MonadState AppData m => StringInput -> m ()
putName t = modify $ \s -> s { apName = t }

modifyName :: MonadState AppData m => (StringInput -> StringInput) -> m ()
modifyName fn = fn `liftM` getName >>= putName

modifyNameM :: MonadState AppData m => (StringInput -> m StringInput) -> m ()
modifyNameM act = getName >>= act >>= putName

getNamedEntered :: MonadState AppData m => m Bool
getNamedEntered = liftM namedEntered get

putNamedEntered :: MonadState AppData m => Bool -> m ()
putNamedEntered t = modify $ \s -> s { namedEntered = t }

modifyNamedEntered :: MonadState AppData m => (Bool -> Bool) -> m ()
modifyNamedEntered fn = fn `liftM` getNamedEntered >>= putNamedEntered

getMessage :: MonadState AppData m => m Surface
getMessage = liftM message get

putMessage :: MonadState AppData m => Surface -> m ()
putMessage t = modify $ \s -> s { message = t }

modifyMessage :: MonadState AppData m => (Surface -> Surface) -> m ()
modifyMessage fn = fn `liftM` getMessage >>= putMessage

modifyMessageM :: MonadState AppData m => (Surface -> m Surface) -> m ()
modifyMessageM act = getMessage >>= act >>= putMessage

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getBg :: MonadReader AppConfig m => m Surface
getBg = liftM background ask

getFont :: MonadReader AppConfig m => m Font
getFont = liftM front ask

handleEvent' :: Event -> AppEnv ()
handleEvent' event = do
    temp <- str `liftM` getName
    modifyName $ handleInput event
    modifyNameM $ \si@(StringInput s t) -> do
        text' <- if s /= temp
                 then if null s 
                     then return Nothing
                     else do
                        font <- getFont
                        liftIO $ Just `liftM` renderTextSolid font s textColor
                 else return t
        return $ si { text=text' }

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "High Score" []
    enableUnicode True
    
    font       <- openFont "lazy.ttf" 42
    background <- loadImage "background.png" Nothing
    message    <- renderTextSolid font "New High Score! Enter Name:" textColor
        
    return (AppConfig screen background font, AppData name message namedEntered) 
 where
    name = StringInput [] Nothing
    namedEntered = False

loop :: AppEnv ()
loop = do
    
    quit <- whileEvents $ \event -> do
        namedEntered <- getNamedEntered
        when (not namedEntered) $ do
            handleEvent' event           
            case event of
                (KeyDown (Keysym SDLK_RETURN _ _)) -> do
                    putNamedEntered True
                    modifyMessageM $ \_ -> do
                        font <- getFont
                        liftIO $ renderTextSolid font "Rank: 1st" textColor
                _ -> return ()
    
    background <- getBg
    message <- getMessage
    screen  <- getScreen
    name    <- getName
    
    applySurface' 0 0 background screen Nothing
    applySurface' ((screenWidth - surfaceGetWidth message) `div` 2) ((screenHeight `div` 2 - surfaceGetHeight message) `div` 2) message screen Nothing
    liftIO $ showCentered name screen
    
    liftIO $ Graphics.UI.SDL.flip screen
    
    unless quit loop
 where
    mapRGB'         = mapRGB . surfaceGetPixelFormat
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

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main =  withInit [InitEverything] $ do -- withInit calls quit for us.result <- TTFG.init
    result <- TTFG.init
    if not result
        then putStr "Failed to init ttf\n"
        else do
            (env, state) <- initEnv
            runLoop env state
            enableUnicode False
            TTFG.quit