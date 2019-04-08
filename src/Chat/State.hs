module Chat.State
  ( Message
  , Name(..)
  , Mode(..)
  , ClientInfo
  , name
  , State
  , mode
  , form
  , list
  , messages
  , input
  , sink
  , initState
  ) where

import qualified Brick.Widgets.Edit as ED
import qualified Brick.Widgets.List as L
import Brick
import Brick.Forms
import Lens.Micro
import Lens.Micro.TH
import Data.Text
import qualified Data.Vector as Vec
import Pipes.Binary (DecodingError)

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
