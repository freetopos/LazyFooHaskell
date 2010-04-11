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
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL

import Graphics.Rendering.OpenGL hiding (Rect, get)
import Timer

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

squareWidth  = 20
squareHeight = 20

halfSquareWidth  = squareWidth `div` 2
halfSquareHeight = squareHeight `div` 2
 
data Square = Square { pos :: (Int, Int), vel :: (Int, Int) }

defaultSquare = Square { pos=(0,0), vel=(0,0) }

handleInput :: Event -> Square -> Square
handleInput (KeyDown (Keysym SDLK_UP _ _)) dot@Square { vel=(dx,dy) }    = dot { vel=(dx, dy - halfSquareHeight) }
handleInput (KeyDown (Keysym SDLK_DOWN _ _)) dot@Square { vel=(dx,dy) }  = dot { vel=(dx, dy + halfSquareHeight) }
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) dot@Square { vel=(dx,dy) }  = dot { vel=(dx - halfSquareWidth, dy) }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) dot@Square { vel=(dx,dy) } = dot { vel=(dx + halfSquareWidth, dy) }

handleInput (KeyUp (Keysym SDLK_UP _ _)) dot@Square { vel=(dx,dy) }    = dot { vel=(dx, dy + halfSquareHeight) }
handleInput (KeyUp (Keysym SDLK_DOWN _ _)) dot@Square { vel=(dx,dy) }  = dot { vel=(dx, dy - halfSquareHeight) }
handleInput (KeyUp (Keysym SDLK_LEFT _ _)) dot@Square { vel=(dx,dy) }  = dot { vel=(dx + halfSquareWidth, dy) }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) dot@Square { vel=(dx,dy) } = dot { vel=(dx - halfSquareWidth, dy) }

handleInput _ d = d

move :: Square -> Square
move dot@Square { pos=(x,y), vel=(dx,dy) } = dot { pos=(x'', y'') } 
 where
    x'  = x + dx
    y'  = y + dy
    x'' = if x' < 0 || (x' + squareWidth) > screenWidth then x else x' 
    y'' = if y' < 0 || (y' + squareHeight) > screenHeight then y else y'

data AppData = AppData {
    dot :: Square,
    fps :: Timer
}

data AppConfig = AppConfig {
    screen :: Surface
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
getSquare = liftM dot get

putSquare :: MonadState AppData m => Square -> m ()
putSquare t = modify $ \s -> s { dot = t }

modifySquareM :: MonadState AppData m => (Square -> m Square) -> m ()
modifySquareM act = getSquare >>= act >>= putSquare

modifySquare :: MonadState AppData m => (Square -> Square) -> m ()
modifySquare fn = fn `liftM` getSquare >>= putSquare

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

initGL :: IO ()
initGL = do
    clearColor $= Color4 (0 :: GLfloat) 0 0 0
    matrixMode $= Projection
    loadIdentity    
    ortho 0 (fromIntegral screenWidth) (fromIntegral screenHeight) 0 (-1) 1
    
    matrixMode $= Modelview 0
    loadIdentity

initEnv :: IO (AppConfig, AppData)
initEnv = do
    glSetAttribute glDoubleBuffer 1    
    screen <- setVideoMode screenWidth screenHeight screenBpp [OpenGL]
    setCaption "OpenGL Test" []
    
    initGL
    
    fps <- start defaultTimer    
    return (AppConfig screen, AppData defaultSquare fps) 

showSquare :: Square -> IO ()
showSquare Square { pos=(x,y) } = do
    -- Move to offset
    translate $ Vector3 (fromIntegral x :: GLfloat)  (fromIntegral y) 0
    -- Start quad
    renderPrimitive Quads $ do
        -- Set color to white
        color $ Color4 (1 :: GLfloat) 1 1 1
        
        -- Draw square
        vertex $ Vertex3 (0 :: GLfloat) 0 0
        vertex $ Vertex3 screenWidth' 0 0
        vertex $ Vertex3 screenWidth' screenHeight' 0
        vertex $ Vertex3 0 screenHeight' 0
    
    loadIdentity
 where  screenWidth', screenHeight' :: GLfloat
        screenWidth'  = fromIntegral screenWidth
        screenHeight' = fromIntegral screenHeight

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit <- whileEvents $ modifySquare . handleInput
    
    modifySquare move
    
    fps     <- getFPS
    square  <- getSquare
    
    liftIO $ do
        clear [ColorBuffer]
        
        showSquare square
        
        glSwapBuffers
        
        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ do
            delay $ secsPerFrame - ticks

    unless quit loop
 where
    framesPerSecond = 60
    secsPerFrame    = 1000 `div` framesPerSecond

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
