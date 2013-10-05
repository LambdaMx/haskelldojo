-- | Compile with: fay examples/canvaswater.hs

{-# LANGUAGE EmptyDataDecls    #-}


module TicTacToe_UI (main) where

import Prelude
import FFI
import TicTacToe_Web as Game


-- | Main entry point.
main :: Fay ()
main = do
  player  <- newRef (1::Int)
  board   <- newRef Game.newBoard
  playing <- newRef True
  let 
      newGame :: Fay ()
      newGame = do
        board <- drawBoard
        step  <- newRef (1 :: Int)
        --addEventListener board "click" log_pos False
        addEventListener board "click" doMovement False

      drawBoard :: Fay (Element)
      drawBoard = do img     <- newImage
                     canvas  <- getElementById "game"
                     context <- getContext canvas "2d"
                     setSrc img "board.png"
                     drawImage context img 0 0
                     return canvas

      doMovement :: Event -> Fay ()
      doMovement e = do play <- readRef playing
                        if play 
                        then do currentPlayer <- readRef player
                                currentBoard  <- readRef board
                                (row, col)    <- readMovement e
                                case Game.doMovement currentBoard row col currentPlayer of
                                  Just b -> do writeRef board b
                                               drawMovement currentPlayer row col
                                               if Game.isWinner b currentPlayer 
                                               then endGame currentPlayer
                                               else do console_log $ "Player " ++ (show currentPlayer) ++ ": " ++ (show (row,col))
                                                       Game.showBoard b
                                                       writeRef player   $ Game.otherPlayer currentPlayer
                                                       changePlayerLabel $ Game.otherPlayer currentPlayer
                                  Nothing    -> return ()
                        else return ()

      readMovement :: Event -> Fay (Int, Int)
      readMovement e = do x <- xpos e
                          y <- ypos e
                          let col = x `div` 200
                          let row = y `div` 200
                          console_log $ show row ++ "," ++ show col
                          return (row, col)
      
      drawMovement :: Int -> Int -> Int -> Fay()
      drawMovement player r c = do canvas  <- getElementById "game"
                                   context <- getContext canvas "2d"
                                   img     <- newImage
                                   setSrc img $ if player == 1 then "x.png" else "o.png"
                                   drawImage context img (c * 200) (r * 200)
      
      changePlayerLabel :: Int -> Fay()
      changePlayerLabel p = innerText "playerName" $ "Player " ++ (show p)
      
      endGame :: Int -> Fay()
      endGame winner = do console_log $ "Winner: " ++ (show winner)
                          innerText "instruct" $ "You won player " ++ (show winner) ++ "!"
                          board  <- getElementById "game"
                          writeRef playing False

  window <- getWindow
  addEventListener_ window "load" newGame False


--------------------------------------------------------------------------------
-- Elements

class Eventable a

-- | A DOM element.
data Element
instance Eventable Element
instance Show Element

-- | Add an event listener to an element.
addEventListener_ :: (Eventable a) => a -> String -> Fay () -> Bool -> Fay ()
addEventListener_ = ffi "%1['addEventListener'](%2,%3,%4)"

data Event

addEventListener :: (Eventable a) => a -> String -> (Event -> Fay ()) -> Bool -> Fay ()
addEventListener = ffi "%1['addEventListener'](%2,%3,%4)"

removeEventListener :: (Eventable a) => a -> String -> (Event -> Fay ()) -> Bool -> Fay ()
removeEventListener = ffi "%1['removeEventListener'](%2,%3,%4)"

documentGetElements :: String -> Fay [Element]
documentGetElements = ffi "document.getElementsByTagName(%1)"

-- | Get an element by its ID.
getElementById :: String -> Fay Element
getElementById = ffi "document['getElementById'](%1)"

data Window
instance Eventable Window

getWindow :: Fay (Window)
getWindow = ffi "window"

--------------

log_pos :: Event -> Fay ()
log_pos e = do x <- xpos e
               y <- ypos e 
               console_log   $ show y ++ "," ++ show x

xpos :: Event -> Fay (Int)
xpos = ffi "%1['offsetX']" 

ypos :: Event -> Fay (Int)
ypos = ffi "%1['offsetY']" 

--------------------------------------------------------------------------------
-- Images

data Image
instance Eventable Image

-- | Make a new image.
newImage :: Fay Image
newImage = ffi "new Image()"

-- | Make a new image.
setSrc :: Image -> String -> Fay ()
setSrc = ffi "%1['src'] = %2"

--------------------------------------------------------------------------------
-- Canvas

-- | A canvas context.
data Context

-- | Get an element by its ID.
getContext :: Element -> String -> Fay Context
getContext = ffi "%1['getContext'](%2)"

-- | Draw an image onto a canvas rendering context.
drawImage :: Context -> Image -> Int -> Int -> Fay ()
drawImage = ffi "%1['drawImage'](%2,%3,%4)"

-- | Draw an image onto a canvas rendering context.
--
--   Nine arguments: the element, source (x,y) coordinates, source width and
--   height (for cropping), destination (x,y) coordinates, and destination width
--   and height (resize).
--
--   context.drawImage(img_elem, sx, sy, sw, sh, dx, dy, dw, dh);
drawImageSpecific :: Context -> Image
                  -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
                  -> Fay ()
drawImageSpecific = ffi "%1['drawImage'](%2,%3,%4,%5,%6,%7,%8,%9,%10)"

-- | Set the fill style.
setFillStyle :: Context -> String -> Fay ()
setFillStyle = ffi "%1['fillStyle']=%2"

-- | Set the fill style.
setFillRect :: Context -> Double -> Double -> Double -> Double -> Fay ()
setFillRect = ffi "%1['fillRect'](%2,%3,%4,%5)"

--------------------------------------------------------------------------------
-- Ref

-- | A mutable reference like IORef.
data Ref a

-- | Make a new mutable reference.
newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

-- | Replace the value in the mutable reference.
writeRef :: Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

-- | Get the referred value from the mutable value.
readRef :: Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"

--------------------------------------------------------------------------------
-- Misc

-- | Alert using window.alert.
alert :: a -> Fay ()
alert = ffi "window['alert'](%1)"

-- | Alert using window.alert.
console_log :: String -> Fay ()
console_log = ffi "console['log'](%1)"

innerText :: String -> String -> Fay()
innerText = ffi "document['getElementById'](%1)['innerText']=%2"