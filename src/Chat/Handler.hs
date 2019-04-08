module Chat.Handler
  ( startEvent
  , handleEvent
  ) where

import Brick
import Brick.Forms
import qualified Brick.Widgets.Edit as ED
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

import Data.Function hiding(on)
import qualified Data.Vector as Vec
import Data.Text (Text)
import Data.Text.Zipper
import qualified Data.Text as Text

import Control.Monad.IO.Class

import Lens.Micro

import Chat.Protocol
import Chat.State

startEvent :: State -> EventM Name State
startEvent s = sender RequestJoin >> return s where
  sender = liftIO . (s^.sink)

scroll = viewportScroll Viewport

handleEvent :: State -> BrickEvent Name Message -> EventM Name (Next State)
handleEvent s (AppEvent (Left _)) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s ev = case s^.mode of
  ContactServer -> handleWait s ev
  GetName -> handleForm s ev
  ChooseList -> handleList s ev
  Chat -> handleChat s ev

handleWait :: State -> BrickEvent Name Message -> EventM Name (Next State)
handleWait s (AppEvent (Right ConfirmJoin)) = continue $ s & mode .~ GetName
handleWait s _ = continue s

submitForm :: State -> IO State
submitForm s = do
  let f = s^.form
      n = Text.unpack $ formState f ^. name
      sender = s^.sink
  if allFieldsValid f
    then do
      sender $ RegisterName n
      sender $ GetClients
      return $ s & mode .~ ChooseList
    else return s

handleForm :: State -> BrickEvent Name Message -> EventM Name (Next State)
handleForm s (AppEvent (Right (NameTaken taken))) = continue $ s & form %~ setFieldValid (not taken) NameField
handleForm s (VtyEvent (V.EvKey V.KEnter [])) = liftIO (submitForm s) >>= continue
handleForm s ev = do
  s' <- handleEventLensed s form handleFormEvent ev
  let clinfo = formState (s'^.form)
  liftIO (s'^.sink $ (CheckName (Text.unpack $ clinfo^.name)))
  continue s'

submitChoice :: State -> IO State
submitChoice s = do
  let sender = s^.sink
      ls = s^.list
      es = L.listElements ls
      mi = L.listSelected ls
  case mi of
    Just i -> do
      sender $ ChooseRecip (es Vec.! i)
      return $ s & mode .~ Chat
    Nothing -> return s

handleList :: State -> BrickEvent Name Message -> EventM Name (Next State)
handleList s (AppEvent (Right (ActiveClients cs'))) = do
  let ls = s^.list
      cs = Vec.toList $ L.listElements ls
      i = L.listSelected ls
  continue $ if cs /= cs' then s & list %~ L.listReplace (Vec.fromList cs') i else s
handleList s (VtyEvent ev) = case ev of
  V.EvKey V.KEnter [] -> liftIO (submitChoice s) >>= continue
  _ -> do
    s' <- handleEventLensed s list L.handleListEvent ev
    liftIO (s^.sink $ GetClients)
    continue s'
  

submitText :: State -> IO State
submitText s = do
  let msg = Text.unpack . head . ED.getEditContents $ s^.input
      nick = formState (s^.form) ^. name
      x = Text.unpack nick ++ "> " ++ msg

  s^.sink $ Message msg
  return $ s & input %~ ED.applyEdit clearZipper
             & messages %~ (x :)

handleChat :: State -> BrickEvent Name Message -> EventM Name (Next State)
handleChat s (AppEvent (Right (MessageFrom user msg))) = continue $ s & messages %~ (x :)
  where x = user ++ "> " ++ msg
handleChat s (AppEvent (Right (Notice msg))) = continue $ s & messages %~ (x :)
  where x = "[Notice] " ++ msg
handleChat s (VtyEvent ev) = case ev of
  V.EvKey V.KDown  [V.MCtrl] -> vScrollBy scroll 1 >> continue s
  V.EvKey V.KUp [V.MCtrl] -> vScrollBy scroll (-1) >> continue s
  V.EvKey V.KEnter [] -> liftIO (submitText s) >>= continue
  _ -> handleEventLensed s input ED.handleEditorEvent ev >>= continue
