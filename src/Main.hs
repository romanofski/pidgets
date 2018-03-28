{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Control.Lens.TH (makeLenses)
import Control.Monad (void)
import Graphics.Vty (Event (..))
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.CircularList as CList
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Focus as Brick
import qualified Brick.AttrMap as A
import Brick.Util (on)
import Brick.Widgets.Core
       (fill, emptyWidget, str, hLimit, padLeft, txt, vBox, vLimit,
        withAttr, (<+>), (<=>))
import Brick.Types
       (Widget, BrickEvent(..), Next, EventM, Padding(..),
        handleEventLensed)

-- | Used to identify widgets in brick
data Name =
    ListOfThreads
    | StatusBar
    | ListOfMails
    | ComposeFrom
    | ComposeTo
    | ComposeSubject
    | SearchThreadsEditor
    | ManageMailTagsEditor
    | ManageThreadTagsEditor
    | ListOfAttachments
    deriving (Eq,Show,Ord)

data Compose = Compose
    { _cFrom :: E.Editor T.Text Name
    , _cTo :: E.Editor T.Text Name
    , _cSubject :: E.Editor T.Text Name
    , _cFocusFields :: Brick.FocusRing Name
    , _cAttachments :: L.List Name String
    }
makeLenses ''Compose

data MailIndex = MailIndex
    { _miListOfMails  :: L.List Name T.Text
    , _miListOfThreads :: L.List Name T.Text
    , _miSearchThreadsEditor :: E.Editor T.Text Name
    , _miMailTagsEditor :: E.Editor T.Text Name
    , _miThreadTagsEditor :: E.Editor T.Text Name
    }
makeLenses ''MailIndex

data InternalKeybinding = InternalKeybinding Vty.Event InternalAction
newtype InternalAction = InternalAction (AppState -> EventM Name (Next AppState))

iAction :: Lens' InternalAction (AppState -> EventM Name (Next AppState))
iAction f (InternalAction a) = fmap InternalAction (f a)

data AppState = AppState
    { _asMailIndex :: MailIndex
    , _asCompose :: Compose
    , _asWidgets :: V.Vector Name
    , _asFocus :: Brick.FocusRing Name
    , _asKeybindings :: Map.Map Name [InternalKeybinding]
    }
makeLenses ''AppState

data Action (ctx :: Name) = Action
    { _aDescription :: String
    , _aAction :: AppState -> EventM Name (Next AppState)
    }
makeLenses ''Action

data Keybinding (ctx :: Name) = Keybinding
    { _kbEvent :: Vty.Event
    , _kbAction :: Action ctx
    }
makeLenses ''Keybinding


-- Drawing code
--
drawUI :: AppState -> [Widget Name]
drawUI s = let view' = view asWidgets s
           in [foldr (renderWidget s) emptyWidget view']

renderWidget :: AppState -> Name -> Widget Name -> Widget Name
renderWidget s ListOfThreads widgets = draw (Proxy :: Proxy 'ListOfThreads) s <=> widgets
renderWidget s SearchThreadsEditor widgets = draw (Proxy :: Proxy 'SearchThreadsEditor) s <=> widgets
renderWidget s StatusBar widgets = draw (Proxy :: Proxy 'StatusBar) s <=> widgets
renderWidget s ListOfMails widgets = draw (Proxy :: Proxy 'ListOfMails) s <=> widgets

class UIDrawer (a :: Name) where
  draw :: Proxy a -> AppState -> Widget Name

instance UIDrawer 'StatusBar where
  draw _ s =
        withAttr statusbarAttr $
        str (show (Brick.focusGetCurrent (view asFocus s))) <+>
        padLeft
            (Pad 1)
            (str
                 (show
                      (view
                           (asMailIndex . miListOfThreads . L.listSelectedL)
                           s))) <+>
        vLimit 1 (fill ' ')

instance UIDrawer 'ListOfThreads where
  draw _ = renderMailList

instance UIDrawer 'ListOfMails where
  draw _ s = let hasFocus = Just ListOfMails == Brick.focusGetCurrent (view asFocus s)
             in L.renderList listDrawElement hasFocus $ view (asMailIndex . miListOfMails) s

instance UIDrawer 'SearchThreadsEditor where
  draw _ s = let hasFocus = Just SearchThreadsEditor == Brick.focusGetCurrent (view asFocus s)
             in vLimit 1 $ E.renderEditor (txt . T.unlines) hasFocus $ view (asMailIndex . miSearchThreadsEditor) s

renderMailList :: AppState -> Widget Name
renderMailList s = let hasFocus = Just ListOfThreads == Brick.focusGetCurrent (view asFocus s)
                   in L.renderList listDrawElement hasFocus $ view (asMailIndex . miListOfThreads) s

listDrawElement :: Bool -> T.Text -> Widget Name
listDrawElement selected item = if selected then withAttr L.listSelectedAttr (txt item) <+> vLimit 1 (fill ' ')
                                else txt item

-- Event handling
--
-- | quickfix internal actions, which are usually generated and stripped of
-- their modes. Also they're not composable and don't have any descriptions.
listUp :: InternalAction
listUp = InternalAction (M.continue . over (asMailIndex . miListOfThreads) L.listMoveUp)

listDown :: InternalAction
listDown = InternalAction (M.continue . over (asMailIndex . miListOfThreads) L.listMoveDown)

backToIndex :: InternalAction
backToIndex = InternalAction (M.continue
                              . over asFocus (Brick.focusRingModify (CList.update ListOfThreads))
                              . over asWidgets (V.// [(0, ListOfThreads)]))

activateListOfMails :: InternalAction
activateListOfMails = InternalAction (M.continue
                                      . over asFocus (Brick.focusRingModify (CList.update ListOfMails))
                                      . over asWidgets (V.// [(0, ListOfMails)]))

activateSearchThreads :: InternalAction
activateSearchThreads = InternalAction (M.continue . over asFocus (Brick.focusSetCurrent SearchThreadsEditor))

-- | quickfix map which has already the Keybindings hacked in. Ideally we'd like
--- to fill this based on currently focuesed widget names and the Keybindings come out of the config
keybindingMap :: Map.Map Name [InternalKeybinding]
keybindingMap =
    Map.fromList
        [ ( ListOfThreads
          , [ InternalKeybinding (Vty.EvKey (Vty.KChar 'j') []) listDown
            , InternalKeybinding (Vty.EvKey (Vty.KChar 'k') []) listUp
            , InternalKeybinding (Vty.EvKey Vty.KEnter []) activateListOfMails
            , InternalKeybinding
                  (Vty.EvKey (Vty.KChar 'q') [])
                  (InternalAction M.halt)
            , InternalKeybinding (Vty.EvKey (Vty.KChar ':') []) activateSearchThreads])
        , ( ListOfMails
          , [InternalKeybinding (Vty.EvKey (Vty.KChar 'q') []) backToIndex])
        -- The search editor isn't doing anything. In fact it does nothing,
        -- since this POC does not support sending all non-matching input for
        -- the editor to the fallback key handler
        , ( SearchThreadsEditor
          , [InternalKeybinding (Vty.EvKey Vty.KEsc []) backToIndex])
        ]

lookupKeybinding :: Event -> Maybe [InternalKeybinding] -> Maybe InternalKeybinding
lookupKeybinding _ Nothing = Nothing
lookupKeybinding e (Just kbs) = find (\(InternalKeybinding kbEv _) -> kbEv == e) kbs

dispatch :: AppState -> Event -> EventM Name (Next AppState)
dispatch s e = let ring = view asFocus s
                   currentlyFocused = fromMaybe ListOfThreads (Brick.focusGetCurrent ring)
                   kbs = Map.lookup currentlyFocused keybindingMap
               in case lookupKeybinding e kbs of
                      Just (InternalKeybinding _ kb) -> s & view iAction kb
                      Nothing -> M.continue s -- would be the fallback

appEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
appEvent s (VtyEvent ev) = dispatch s ev
appEvent s _ = M.continue s

-- main stuff
--
statusbarAttr :: A.AttrName
statusbarAttr = "statusbar"

theme :: A.AttrMap
theme =
    A.attrMap
        Vty.defAttr
        [ (L.listSelectedAttr, Vty.blue `on` Vty.white)
        , (statusbarAttr, Vty.black `on` Vty.brightWhite)]

theApp :: M.App AppState e Name
theApp =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const theme
    }

initialState :: AppState
initialState =
    let mi =
            MailIndex
                (L.list
                     ListOfMails
                     (V.fromList ["Mail 1", "Mail 2", "Mail 3"])
                     1)
                (L.list ListOfThreads (V.fromList ["Thread 1", "Thread 2"]) 1)
                (E.editorText SearchThreadsEditor Nothing "tag:foobar")
                (E.editorText ManageMailTagsEditor Nothing "")
                (E.editorText ManageThreadTagsEditor Nothing "")
        compose =
            Compose
                (E.editorText ComposeFrom (Just 1) "")
                (E.editorText ComposeTo (Just 1) "")
                (E.editorText ComposeSubject (Just 1) "")
                (Brick.focusRing
                     [ComposeFrom, ComposeTo, ComposeSubject, ListOfAttachments])
                (L.list ListOfAttachments V.empty 1)
        view' = V.fromList [ListOfThreads, StatusBar, SearchThreadsEditor]
        ring = Brick.focusRing $ V.toList view'
    in AppState mi compose view' ring Map.empty

main :: IO ()
main = void $ M.defaultMain theApp initialState
