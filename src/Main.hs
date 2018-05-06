{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Control.Lens.TH (makeLenses)
import Control.Monad (void, (>=>))
import Graphics.Vty (Event (..))
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>), Semigroup)
import Data.Monoid (Monoid, mempty, mappend)
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

data AppState = AppState
    { _asMailIndex :: MailIndex
    , _asCompose :: Compose
    , _asWidgets :: V.Vector Name
    , _asFocus :: Brick.FocusRing Name
    }
makeLenses ''AppState

data Action ctx a = Action
    { _aDescription :: String
    , _aAction :: Lens' AppState ctx -> AppState -> EventM Name a
    , _aLens :: Lens' AppState ctx
    }

data Keybinding ctx a = Keybinding
    { _kbEvent :: Vty.Event
    , _kbAction :: Action ctx a
    }

kbAction :: Getter (Keybinding ctx a) (Action ctx a)
kbAction = to (\(Keybinding _ b) -> b)

data Configuration = Configuration
  { _confListOfThreadsKeybindings :: [Keybinding (L.List Name T.Text) (Next AppState)]
  , _confSearchThreadsKeybindings :: [Keybinding (E.Editor T.Text Name) (Next AppState)]
  }


-- Drawing code
--
drawUI :: AppState -> [Widget Name]
drawUI s = [unVBox $ foldMap (VBox . renderWidget s) (view asWidgets s)]

newtype VBox = VBox { unVBox :: Widget Name }

instance Semigroup VBox where
  VBox a <> VBox b = VBox (a <=> b)

instance Monoid VBox where
  mappend = (<>)
  mempty = VBox emptyWidget

renderWidget :: AppState -> Name -> Widget Name
renderWidget s SearchThreadsEditor = drawSearchThreadsEditor s
renderWidget s StatusBar = drawStatusBar s
renderWidget s ListOfMails = drawListOfMails s
renderWidget s _ = drawListOfThreads s

drawListOfThreads :: AppState -> Widget Name
drawListOfThreads = renderMailList

drawStatusBar :: AppState -> Widget Name
drawStatusBar s =
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

drawListOfMails :: AppState -> Widget Name
drawListOfMails s =
  let hasFocus = Just ListOfMails == Brick.focusGetCurrent (view asFocus s)
  in L.renderList listDrawElement hasFocus $ view (asMailIndex . miListOfMails) s

drawSearchThreadsEditor :: AppState -> Widget Name
drawSearchThreadsEditor s =
  let hasFocus = Just SearchThreadsEditor == Brick.focusGetCurrent (view asFocus s)
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
continue :: Action ctx (Next AppState)
continue =
    Action
    { _aDescription = ""
    , _aAction = (\_ s -> M.continue s)
    }

quit :: Action ctx (Next AppState)
quit =
    Action
    { _aDescription = ""
    , _aAction = (\_ s -> M.halt s)
    }

-- list actions currently only made to interact with the list of threads
listUp :: Lens' AppState (L.List Name T.Text) -> Action (L.List Name T.Text) AppState
listUp l =
    Action
    { _aDescription = "list up"
    , _aAction = \l' s -> pure $ over l' L.listMoveUp s
    , _aLens = l
    }

listDown :: Lens' AppState (L.List Name T.Text) -> Action (L.List Name T.Text) AppState
listDown l =
    Action
    { _aDescription = "list down"
    , _aAction = \l' s -> pure $ over l' L.listMoveDown s
    , _aLens = l
    }

focus :: Name -> Action a AppState
focus n =
    Action
    { _aDescription = "switch mode"
    , _aAction = (\_ ->
                       pure .
                       over asFocus (Brick.focusSetCurrent n) .
                       over asWidgets (addOrUpdate n))
    }

-- Helper function to either add the new name into our list of widgets to render
-- or keep the existing if it's already in there. The point is to avoid
-- duplicate items. TODO: perhaps simply use a set and also figure out how to
-- remove names
addOrUpdate :: Eq a => a -> V.Vector a -> V.Vector a
addOrUpdate item vec =
    if V.elem item vec
        then vec
        else (vec V.// [(0, item)])


simulateNewSearchResult :: Lens' AppState (L.List Name T.Text) -> Action (L.List Name T.Text) AppState
simulateNewSearchResult l = Action {
  _aDescription = ""
  , _aAction = \_ -> pure . over l (L.listReplace (V.fromList ["Result 1", "Result 2"]) (Just 0))
  , _aLens = l
}

chain :: Action ctx AppState -> Action ctx a -> Action ctx a
chain (Action d1 f1 l) (Action d2 f2 _) =
  Action (if null d2 then d1 else d1 <> " and then " <> d2) (f1 `chainF` f2) l

chainF
    :: (Lens' AppState ctx -> AppState -> EventM Name AppState)
    -> (Lens' AppState ctx -> AppState -> EventM Name b)
    -> (Lens' AppState ctx)
    -> AppState
    -> EventM Name b
chainF fx gx l = fx l >=> gx l

keybindingMap' :: Map.Map Name [Keybinding (L.List Name T.Text) (Next AppState)]
keybindingMap' =
    Map.fromList
        [ ( ListOfThreads
          , [ Keybinding (Vty.EvKey (Vty.KChar 'j') []) (listDown (asMailIndex . miListOfThreads) `chain` continue)
            , Keybinding (Vty.EvKey (Vty.KChar 'k') []) (listUp (asMailIndex . miListOfThreads) `chain` continue)
            , Keybinding (Vty.EvKey Vty.KEnter []) (focus ListOfMails `chain` continue)
            , Keybinding (Vty.EvKey (Vty.KChar ':') []) (focus SearchThreadsEditor `chain` continue)
            , Keybinding (Vty.EvKey (Vty.KChar 'q') []) quit])
        , ( ListOfMails
          , [ Keybinding (Vty.EvKey (Vty.KChar 'j') []) (listDown (asMailIndex . miListOfMails) `chain` continue)
            , Keybinding (Vty.EvKey (Vty.KChar 'k') []) (listUp (asMailIndex . miListOfMails) `chain` continue)
            , Keybinding (Vty.EvKey (Vty.KChar 'q') []) (focus ListOfThreads `chain` continue)])
        , ( SearchThreadsEditor
          , [ Keybinding (Vty.EvKey Vty.KEsc []) (focus ListOfThreads `chain` continue)
            , Keybinding (Vty.EvKey Vty.KEnter []) (simulateNewSearchResult (asMailIndex . miListOfThreads) `chain` focus ListOfThreads `chain` continue)])
        ]

lookupKeybinding :: Event -> Maybe [Keybinding ctx a] -> Maybe (Keybinding ctx a)
lookupKeybinding _ Nothing = Nothing
lookupKeybinding e (Just kbs) = find (\(Keybinding kbEv _) -> kbEv == e) kbs

appEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
appEvent s (VtyEvent ev) = let currentlyFocused = fromMaybe ListOfThreads (Brick.focusGetCurrent (view asFocus s))
                               kbs = Map.lookup currentlyFocused keybindingMap'
                           in case lookupKeybinding ev kbs of
                                  Just (Keybinding _ (Action _ a l)) -> a l s
                                  Nothing -> M.continue s -- would be the fallback
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
        ring = Brick.focusRing [ListOfThreads, SearchThreadsEditor]
    in AppState mi compose view' ring

main :: IO ()
main = void $ M.defaultMain theApp initialState
