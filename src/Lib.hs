module Lib
    ( appEntry
    ) where

import qualified Control.Exception as E
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict (evalStateT)
import Data.Function hiding(on)
import qualified Data.Vector as Vec
import Data.Text (Text)
import Data.Text.Zipper
import qualified Data.Text as Text

import Lens.Micro
import Lens.Micro.TH
import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Forms
import Brick.Focus
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as ED
import Brick.Widgets.List (GenericList)
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

import Pipes (Consumer', (>->), (>~), runEffect, for, cat, lift)
import qualified Pipes.Prelude as P
import Pipes.Network.TCP (fromSocket, toSocket)
import Pipes.Binary (DecodingError(..), decode, encode)
import Pipes.Parse (parsed)
import Chat.Protocol

type Message = Either DecodingError ServerMsg

data Name = InputField
          | NameField
          | ClientList
          | Viewport
          deriving(Eq, Ord, Show)

data Mode = ContactServer
          | GetName
          | ChooseList
          | Chat
          deriving(Eq, Show)

data ClientInfo = ClientInfo { _name :: Text } deriving(Show)

data State = State
  { _mode :: Mode
  , _form :: Form ClientInfo Message Name
  , _list :: L.List Name String
  , _messages :: [String]
  , _input :: ED.Editor Text Name
  , _sink :: ClientMsg -> IO ()
  }

makeLenses ''ClientInfo
makeLenses ''State

mkForm :: ClientInfo -> Form ClientInfo Message Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Name" @@=
                   editTextField name NameField (Just 1)
               ]

initState :: (ClientMsg -> IO ()) -> State
initState = State ContactServer f l [] ed where
  l = L.list ClientList Vec.empty 1
  f = setFieldValid False NameField . mkForm $ ClientInfo ""
  ed = ED.editorText InputField (Just 1) ""

app :: App State Message Name
app = App { appDraw = drawUI
          , appChooseCursor = chooseCursor
          , appHandleEvent = handleEvent
          , appStartEvent = startEvent
          , appAttrMap = const theMap
          }

startEvent :: State -> EventM Name State
startEvent s = sender RequestJoin >> return s where
  sender = liftIO . (s^.sink)

chooseCursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor s xs = case s^.mode of
  ContactServer -> neverShowCursor s xs
  GetName -> focusRingCursor (\s -> formFocus $ s^.form) s xs
  ChooseList -> showFirstCursor s xs
  Chat -> showFirstCursor s xs

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

-- Drawing

waitUI :: State -> Widget Name
waitUI s = C.center . B.border $ C.center (str "Contacting Server...")

errorWidget :: State -> Widget Name
errorWidget s | allFieldsValid (s^.form) = str ""
              | otherwise = str "Error: Name taken. Please choose another name."

nameUI :: State -> Widget Name
nameUI s = C.center . B.border $
  vBox $ [ errorWidget s
         , renderForm (s^.form)
         ]

listUI :: State -> Widget Name
listUI s = C.center . B.border $ L.renderList (\_ e -> str e) True (s^.list)

chatUI :: State -> Widget Name
chatUI s = C.center $ B.borderWithLabel (str "Chat") $
       vBox [ viewport Viewport Vertical . vBox . map (visible . str) . reverse $ s^.messages
            , B.hBorder
            , str "> " <+> ED.renderEditor (txt . Text.unlines) True (s^.input)
            ]

drawUI :: State -> [Widget Name]
drawUI s = case s^.mode of
  ContactServer -> [waitUI s]
  GetName -> [nameUI s]
  ChooseList -> [listUI s]
  Chat -> [chatUI s]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (ED.editAttr, (V.white `on` V.black) `V.withStyle` V.dim)
  , (ED.editFocusedAttr, V.white `on` V.black)
  , (L.listSelectedFocusedAttr, V.black `on` V.white)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

appEntry :: IO ()
appEntry = do
  eventChan <- newBChan 10
  upChan <- newChan
  addr <- resolve "127.0.0.1" "4242"
  sock <- open addr
  network <- forkFinally (talk eventChan upChan sock) (const $ close sock)
  let s = initState $ writeChan upChan
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just eventChan) app s
  killThread network
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

talk :: BChan Message -> Chan ClientMsg -> Socket -> IO ()
talk eventChan upChan sock = do

  down <- forkIO $ do
    (e, _) <- runEffect $ do
      let bytesReciever = fromSocket sock 4096
          decoder = parsed decode bytesReciever
      decoder >-> P.mapM_ (writeBChan eventChan . Right)
    writeBChan eventChan $ Left e

  runEffect $ lift (readChan upChan) >~ for cat encode >-> toSocket sock
  killThread down
