module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)

main = do
    (progname, _) <- getArgsAndInitialize
    initialDisplayMode $= [ SingleBuffered, RGBMode
                          , WithAlphaComponent, WithDepthBuffer]
    initialWindowSize  $= Size 640 480
    initialWindowPosition $= Position 0 0
    createWindow "Nehe's GL Tutorial in Haskell"
    displayCallback $= drawGLScene
    fullScreen
    idleCallback $= Just drawGLScene
    reshapeCallback $= Just resizeGLScene
    keyboardMouseCallback $= Just keyPressed
    initGL $ Size 640 480
    mainLoop

initGL size@(Size w h) = do
    clearColor $= Color4 0 0 0 0
    clearDepth $= 1
    depthFunc  $= Just Less
    shadeModel $= Smooth
    matrixMode $= Projection
    loadIdentity
    let h' = if h == 0 then 1 else h
    perspective 45 (fromIntegral w / fromIntegral h') 0.1 100

resizeGLScene size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    let h' = if h == 0 then 1 else h
    perspective 45 (fromIntegral w / fromIntegral h') 0.1 100

keyPressed key keyState modifiers position = do
    threadDelay 100
    case key of
        Char '\ESC' -> do
            window <- fmap fromJust $ get currentWindow
            destroyWindow window
        _ -> return ()

drawGLScene = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    swapBuffers
