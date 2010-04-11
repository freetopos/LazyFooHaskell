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
{-
    WARNING: In this program we have video functions running in seperate threads.
    This is for demonstrative purposes only. You should never in a real application
    have video functions running in seperate threads.
-}
module Main where

import System.Random
import System.IO.Unsafe

import Data.Word
import Data.Maybe

import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Concurrent

import Graphics.UI.SDL.Image
import Graphics.UI.SDL

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

foldM' :: Monad m => a -> [b] -> (a -> b -> m a) -> m ()
foldM' x ys fn = foldM_ fn x ys

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = mapRGB' surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

applySurface' :: MonadIO m => Int -> Int -> Surface -> Surface -> Maybe Rect -> m Bool
applySurface' x y src dst = liftIO . applySurface x y src dst

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

newtype Mutex = Mutex { mutex :: MVar () }
newtype Cond  = Cond { cond :: MVar () }

createMutex :: IO Mutex
createMutex = Mutex `liftM` newMVar ()

createCond :: IO Cond
createCond = Cond `liftM` newEmptyMVar

withLock :: Mutex -> IO a -> IO a
withLock (Mutex mutex) act = do
    takeMVar mutex
    res <- act
    putMVar mutex ()
    return res

condWait :: Cond -> Mutex -> IO ()
condWait (Cond cond) (Mutex mutex) = do
    putMVar mutex () -- release lock
    tryTakeMVar cond -- make sure cond is empty 
    takeMVar cond    -- wait for signal
    tryTakeMVar cond -- make sure cond is empty next time condWait is called.
    takeMVar mutex   -- re-acquire lock

condSignal :: Cond -> IO ()
condSignal (Cond cond) = tryPutMVar cond () >> return ()

data AppData = AppData {
    bufferLock :: Mutex,
    canConsume :: Cond,
    canProduce :: Cond,
    buffer     :: MVar (Maybe Surface),
    rgen       :: MVar StdGen
}

data AppConfig = AppConfig {
    screen :: Surface,
    images :: [Surface]
}

data AppEnv = AppEnv {
    state  :: AppData,
    config :: AppConfig 
}

rand :: (Random a, RandomGen g) => MVar g -> a -> a -> IO a
rand mv lo hi =
    modifyMVar mv $ \r -> do
        let (val, r') = randomR (lo, hi) r
        return (r', val)

produce :: AppEnv -> Int -> Int -> IO ()
produce (AppEnv (AppData bufferLock canConsume canProduce buffer rgen) (AppConfig screen images)) x y = do
    withLock bufferLock $ do
        
        bufferFull <- isJust `liftM` readMVar buffer
        when bufferFull $ do
            condWait canProduce bufferLock
        
        modifyMVar_ buffer $ \_ -> do -- or rand 0 5 >>= return . Just . (images !!)
            r <- (images !!) `liftM` rand rgen 0 4
            return $ Just r
        
        withMVar buffer $ \(Just buffer') -> do
            applySurface x y buffer' screen Nothing
            Graphics.UI.SDL.flip screen
    
    condSignal canConsume

consume :: AppEnv -> Int -> Int -> IO ()
consume (AppEnv (AppData bufferLock canConsume canProduce buffer _) (AppConfig screen _)) x y = do
    withLock bufferLock $ do
        bufferEmpty <- isNothing `liftM` readMVar buffer
        when bufferEmpty $ do
            condWait canConsume bufferLock
        
        Just buffer' <- modifyMVar buffer $ \x -> return (Nothing, x)
        applySurface x y buffer' screen Nothing
        Graphics.UI.SDL.flip screen
        
    condSignal canProduce

producer :: AppEnv -> IO ()
producer env@AppEnv { state=AppData { rgen=rg } } = do
    
    modifyMVar_ rg $ \_ -> (mkStdGen . fromIntegral) `liftM` getTicks
    
    foldM' 10 [0..4] $ \y _ -> do
        t <- rand rg 0 1000 :: IO Int
        delay $ fromIntegral t 
        
        produce env 10 y 
        
        return $ y + 90

consumer :: AppEnv -> IO ()
consumer env@AppEnv { state=AppData { rgen=rg } } = do
    
    foldM' 10 [0..4] $ \y _ -> do
        t <- rand rg 0 1000 :: IO Int
        delay $ fromIntegral t 
        
        consume env 300 y 
        
        return $ y + 90

initEnv :: IO AppEnv
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Producer / Consumer Test" []
    
    -- Create the mutex
    bufferLock <- createMutex

    -- Create Conditions
    canProduce <- createCond
    canConsume <- createCond
    
    emptyBuffer <- newMVar Nothing
    rg          <- newMVar $ mkStdGen 0
    
    images <- sequence [loadImage "1.png" Nothing,
                        loadImage "2.png" Nothing,
                        loadImage "3.png" Nothing,
                        loadImage "4.png" Nothing,
                        loadImage "5.png" Nothing]
    
    jrect <- Just `liftM` getClipRect screen
    color <- mapRGB' screen 0x80 0x80 0x80
    fillRect screen jrect color
    Graphics.UI.SDL.flip screen
    
    return $ AppEnv (AppData bufferLock canProduce canConsume emptyBuffer rg) (AppConfig screen images)

loop :: IO ()
loop = do
    quit <- whileEvents $ \_ -> return ()
    unless quit loop

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    
    env <- initEnv
    
    forkChild $ producer env
    forkChild $ consumer env
    
    waitForChildren
    
    loop

-- This section of code is copied from http://www.haskell.org/ghc/staging/docs/latest/html/libraries/base-4.2.0.0/Control-Concurrent.html#11
-- These helper functions allows for the main thread to wait for a child thread to finish.
children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
    cs <- takeMVar children
    case cs of
        []   -> return ()
        m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar   <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkIO (io `finally` putMVar mvar ())