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

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Data.Word
import Data.Array.IArray

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

type RectArray = Array Int Rect

data Clip =
    MouseOver
    | MouseOut
    | MouseDown
    | MouseUp
    deriving (Eq, Ord, Enum, Show)

data Button = Button { box :: Rect, clip :: Clip }

button :: Int -> Int -> Int -> Int -> Button
button x y w h = Button { box=Rect { rectX=x, rectY=y, rectW=w, rectH=h }, clip=MouseOut }

isInside :: Integral a => Rect -> a -> a -> Bool
isInside (Rect rx ry rw rh) x y = (x' > rx) && (x' < rx + rw) && (y' > ry) && (y' < ry + rh)
 where (x', y') = (fromIntegral x, fromIntegral y)

handleEvent :: Button -> Event -> Button
handleEvent button@Button { box=b } (MouseMotion x y _ _) = button { clip=clip' }
 where clip' = if isInside b x y then MouseOver else MouseOut

handleEvent button@Button { box=b, clip=c } (MouseButtonDown x y ButtonLeft) = button { clip=clip' } 
 where clip' = if isInside b x y then MouseDown else c
	
handleEvent button@Button { box=b, clip=c } (MouseButtonUp x y ButtonLeft) = button { clip=clip' }
 where clip' = if isInside b x y then MouseUp else c

handleEvent button _ = button

showButton :: Button -> RectArray -> Surface -> Surface -> IO Bool
showButton Button { box=Rect {rectX=x,rectY=y}, clip=clip } clips buttonSheet screen =
    applySurface x y buttonSheet screen $ Just clipRect
 where clipRect = clips ! fromEnum clip

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

data AppConfig = AppConfig {
    screen       :: Surface,
    buttonSheet  :: Surface,
    clips        :: RectArray
}

type AppState = StateT Button IO
type AppEnv = ReaderT AppConfig AppState

initEnv :: IO AppConfig
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Button Test" []
        
    buttonSheet <- loadImage "button.png" $ Just (0x00, 0xff, 0xff)
    
    return $ AppConfig screen buttonSheet clips
 where clips = listArray (0, 3) [Rect { rectX=0, rectY=0, rectW=320, rectH=240 },
                                 Rect { rectX=320, rectY=0, rectW=320, rectH=240 },
                                 Rect { rectX=0, rectY=240, rectW=320, rectH=240 },
                                 Rect { rectX=320, rectY=240, rectW=320, rectH=240 }] :: RectArray

loop :: AppEnv ()
loop = do

    quit <- whileEvents $ modify . handleEvent'
    
    screen      <- screen `liftM` ask
    clips       <- clips `liftM` ask
    buttonSheet <- buttonSheet `liftM` ask
    button      <- get
    
    -- a local lambda so we don't have use liftIO for all the SDL actions used which are in IO.
    liftIO $ do
        bgColor  <- (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff  
        clipRect <- Just `liftM` getClipRect screen
        fillRect screen clipRect bgColor
        showButton button clips buttonSheet screen
        Graphics.UI.SDL.flip screen
    
    unless quit loop

 where handleEvent' = Prelude.flip handleEvent

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> Button -> IO ()
runLoop = evalStateT . runReaderT loop

main = withInit [InitEverything] $ do -- withInit calls quit for us.
    env <- initEnv
    runLoop env myButton
 where myButton = button 170 120 320 240