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

import Foreign

import Data.Char
import Data.Word
import Data.Maybe
import Data.Array

import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

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

applySurface' :: MonadIO m => Int -> Int -> Surface -> Surface -> Maybe Rect -> m Bool
applySurface' x y src dst = liftIO . applySurface x y src dst

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

getPixel32 :: Int -> Int -> Surface -> IO Pixel
getPixel32 x y s = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    Pixel `liftM` peekElemOff pixels ((y * surfaceGetWidth s) + x)

data BitmapFont = BitmapFont {
    bitmap   :: Surface,
    chars    :: Array Int Rect,
    newLine,
    space    :: Int
}

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return $ Nothing
findM f (x : xs) = f x >>= \result -> if result then return $ Just x else findM f xs

buildFont :: Surface -> IO BitmapFont
buildFont surface = do
    bgColor <- mapRGB' surface 0x00 0xFF 0xFF
    
    (chars, (top, baseA)) <- Prelude.flip runStateT (cellH,cellH) $ do
        
        forM [(r,c) | r <- [0..15], c <- [0..15]] $ \(rows,cols) -> do
            let currentChar = rows * 16 + cols
            
            let notBgPixel (pCol,pRow) = do          
                let (pX, pY) = ((cellW * cols) + pCol, (cellH * rows) + pRow)                
                pixel <- liftIO $ getPixel32 pX pY surface
                -- If a non colorkey pixel is found
                return $ pixel /= bgColor
            
            leftSide   <- findM notBgPixel [(pCol, pRow) | pCol <- [0..cellW - 1],   pRow <- [0..cellH - 1]]
            rightSide  <- findM notBgPixel [(pCol, pRow) | pCol <- [(cellW - 1),(cellW - 2)..0], pRow <- [0..cellH - 1]]
            topSide    <- findM notBgPixel [(pCol, pRow) | pRow <- [0..cellH - 1],   pCol <- [0..cellW - 1]]
            bottomSide <- if currentChar == ord 'A'
                            then findM notBgPixel [(pCol, pRow) | pRow <- [(cellH - 1),(cellH - 2)..0], pCol <- [0..cellW - 1]]
                            else return $ Nothing
                        
            let x = maybe (cellW * cols) (\(pCol,_) -> cellW * cols + pCol) leftSide
            let w = maybe cellW (\(pCol,_) -> ((cellW * cols + pCol) - x) + 1) rightSide
            
            case topSide of
                Just (_, pRow) -> modify $ \s@(t,b) -> if pRow < t then (pRow,b) else s 
                _ -> return ()
            case bottomSide of
                Just (_, pRow) -> modify $ \(t,_) -> (t,pRow)
                _ -> return ()
            
            return $ Rect x (cellH * rows) w cellH
    
    let chars'    = map (\(Rect x y w h) -> Rect x (y + top) w (h - top)) chars
    let charArray = listArray (0,255) chars'
    return $ BitmapFont surface charArray (baseA - top) (cellW `div` 2)

 where cellW = surfaceGetWidth surface `div` 16
       cellH = surfaceGetHeight surface `div` 16

showText :: Int -> Int -> String -> Surface -> BitmapFont -> IO ()
showText x y text surface (BitmapFont bitmap chars newLine space) =
    Prelude.flip evalStateT (x,y) $ do
        forM_ text $ \c -> do
            case c of
                ' '  -> modify $ \(u,v) -> (u + space, v)
                '\n' -> modify $ \(u,v) -> (x, v + newLine)
                _ -> do
                    let ascii = ord c
                    let char  = chars ! ascii
                    (u,v) <- get
                    applySurface' u v bitmap surface $ Just char 
                    put (u + rectW char + 1,v)

initEnv :: IO ()
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Bitmap font test" []

    bitmapFont <- loadImage "lazyfont.png" $ Just (0x00, 0xff, 0xff)
    font       <- buildFont bitmapFont
    
    jrect <- Just `liftM` getClipRect screen
    white <- mapRGB' screen 0xff 0xff 0xff
    fillRect screen jrect white
    
    showText 100 100 "Bitmap Font:\nABDCEFGHIJKLMNOPQRSTUVWXYZ\nabcdefghijklmnopqrstuvwxyz\n0123456789" screen font 
    
    Graphics.UI.SDL.flip screen

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
    initEnv
    loop