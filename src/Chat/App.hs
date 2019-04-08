module Chat.App
  ( brickMain
  ) where

import Brick
import Brick.BChan (BChan)
import qualified Graphics.Vty as V

import Control.Monad

import Chat.Protocol
import Chat.State
import Chat.UI
import Chat.Handler

app :: App State Message Name
app = App { appDraw = drawUI
          , appChooseCursor = chooseCursor
          , appHandleEvent = handleEvent
          , appStartEvent = startEvent
          , appAttrMap = const theMap
          }

brickMain :: State -> BChan Message -> IO ()
brickMain s eventChan = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just eventChan) app s
