module Chat.UI
  ( chooseCursor
  , drawUI
  , theMap
  ) where

import Chat.State
import Lens.Micro
import Brick
import Brick.Forms
import Brick.Focus
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as ED
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Data.Text as Text

chooseCursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor s xs = case s^.mode of
  ContactServer -> neverShowCursor s xs
  GetName -> focusRingCursor (\s -> formFocus $ s^.form) s xs
  ChooseList -> showFirstCursor s xs
  Chat -> showFirstCursor s xs

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
listUI s = C.center . B.border $ L.renderList (\_ (_, e) -> str e) True (s^.list)

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
