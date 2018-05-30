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
       (fill, emptyWidget, str, padLeft, txt, vLimit, withAttr, (<+>),
        (<=>))
import Brick.Types
       (Widget, BrickEvent(..), Next, EventM, Padding(..))

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

data ViewName
    = Threads
    | Mails
    | ComposeView
    deriving (Eq,Ord)

data View = View
    { _vFocus :: Brick.FocusRing Name
    , _vWidgets :: [Name]
    }

vWidgets :: Lens' View [Name]
vWidgets = lens _vWidgets (\settings x -> settings { _vWidgets = x })

vFocus :: Lens' View (Brick.FocusRing Name)
vFocus = lens _vFocus (\settings x -> settings { _vFocus = x})

data ViewSettings = ViewSettings
    { _vsViews :: Map.Map ViewName View
    , _vsFocusedView :: Brick.FocusRing ViewName
    }

vsViews :: Lens' ViewSettings (Map.Map ViewName View)
vsViews = lens _vsViews (\settings x -> settings { _vsViews = x })

vsFocusedView :: Lens' ViewSettings (Brick.FocusRing ViewName)
vsFocusedView = lens _vsFocusedView (\settings x -> settings { _vsFocusedView = x})

data AppState = AppState
    { _asMailIndex :: MailIndex
    , _asCompose :: Compose
    , _asViewSettings :: ViewSettings
    , _asKeybindings :: Map.Map Name [InternalKeybinding]
    , _asDefaultView :: ViewName -- should be in configuration
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

-- | utilities
--
focusedViewWidget :: AppState -> Name
focusedViewWidget s =
    let ring = view vFocus (focusedView s)
    in fromMaybe ListOfThreads $ Brick.focusGetCurrent ring

focusedViewWidgets :: AppState -> [Name]
focusedViewWidgets s =
    let defaultV = view asDefaultView s
        focused = fromMaybe defaultV $ Brick.focusGetCurrent $ view (asViewSettings . vsFocusedView) s
    in view (asViewSettings . vsViews . at focused . _Just . vWidgets) s

focusedViewName :: AppState -> ViewName
focusedViewName s =
    let defaultV = view asDefaultView s
    in fromMaybe defaultV $ Brick.focusGetCurrent $ view (asViewSettings . vsFocusedView) s

focusedView :: AppState -> View
focusedView s = let focused = view (asViewSettings . vsViews . at (focusedViewName s)) s
                in fromMaybe indexView focused

-- Drawing code
--
drawUI :: AppState -> [Widget Name]
drawUI s = [unVBox $ foldMap (VBox . renderWidget s) (focusedViewWidgets s)]

newtype VBox = VBox { unVBox :: Widget Name }

instance Semigroup VBox where
  VBox a <> VBox b = VBox (a <=> b)

instance Monoid VBox where
  mappend = (<>)
  mempty = VBox emptyWidget

renderWidget :: AppState -> Name -> Widget Name
renderWidget s ListOfThreads = drawListOfThreads s
renderWidget s SearchThreadsEditor = drawSearchThreadsEditor s
renderWidget s StatusBar = drawStatusBar s
renderWidget s ListOfMails = drawListOfMails s
renderWidget s ManageMailTagsEditor = drawMailTagsEditor s

drawListOfThreads :: AppState -> Widget Name
drawListOfThreads = renderMailList

drawStatusBar :: AppState -> Widget Name
drawStatusBar s =
  withAttr statusbarAttr $
  str (show (focusedViewWidget s)) <+>
  padLeft
      (Pad 1)
      (str
           (show
                (view
                     (asMailIndex . miListOfThreads . L.listSelectedL)
                     s))) <+>
  vLimit 1 (fill ' ')

drawListOfMails :: AppState -> Widget Name
drawListOfMails s = L.renderList listDrawElement True $ view (asMailIndex . miListOfMails) s

drawSearchThreadsEditor :: AppState -> Widget Name
drawSearchThreadsEditor s =
  vLimit 1 $ E.renderEditor (txt . T.unlines) True $ view (asMailIndex . miSearchThreadsEditor) s

drawMailTagsEditor :: AppState -> Widget Name
drawMailTagsEditor s =
  vLimit 1 $ E.renderEditor (txt . T.unlines) True $ view (asMailIndex . miMailTagsEditor) s

renderMailList :: AppState -> Widget Name
renderMailList s = L.renderList listDrawElement True $ view (asMailIndex . miListOfThreads) s

listDrawElement :: Bool -> T.Text -> Widget Name
listDrawElement selected item = if selected then withAttr L.listSelectedAttr (txt item) <+> vLimit 1 (fill ' ')
                                else txt item

-- Event handling
--
-- | quickfix internal actions, which are usually generated and stripped of
-- their modes. Also they're not composable and don't have any descriptions.

-- list actions currently only made to interact with the list of threads
listUp :: InternalAction
listUp = InternalAction (M.continue . over (asMailIndex . miListOfThreads) L.listMoveUp)

listDown :: InternalAction
listDown = InternalAction (M.continue . over (asMailIndex . miListOfThreads) L.listMoveDown)

-- These two were former mode changes.
-- Back to index means that we want to return to the list of threads, home
-- screen, whatever you want to call it. In purebred we activated the
-- 'BrowseThreads mode and draw what belonged to that mode, yet only the list of
-- threads had the focus. For searching you had to activate a different mode.
--
-- To get the same effect here, we'd replace whatever is in `asWidgets` with
-- what we say should be shown in the index screen, e.g. list of threads, status
-- bar and search threads.
backToIndex :: InternalAction
backToIndex = InternalAction (M.continue . changeView Threads)

-- Instead of activating another mode and statically draw everything belonging
-- to this mode, just replace the list widget and set the focus on the new list
-- widget. That keeps the status bar and the search as is. We can now use the
-- focus ring to jump between searching threads and showing the current thread
-- mails.
activateListOfMails :: InternalAction
activateListOfMails = InternalAction (M.continue . changeView Mails)

activateSearchThreads :: InternalAction
activateSearchThreads =
    InternalAction
        (\s -> M.continue $ over (asViewSettings
                     . vsViews
                     . at (focusedViewName s)
                     . _Just
                     . vFocus)
                  (Brick.focusSetCurrent SearchThreadsEditor)
                  s)

focusNext :: InternalAction
focusNext =
    InternalAction
        (\s ->
              M.continue $
              over
                  (asViewSettings .
                   vsViews . at (focusedViewName s) . _Just . vFocus)
                  Brick.focusNext
                  s)

nextView :: InternalAction
nextView = InternalAction (M.continue . over (asViewSettings . vsFocusedView) Brick.focusNext)

simulateNewSearchResult :: InternalAction
simulateNewSearchResult = InternalAction (M.continue
                                          . over (asMailIndex . miListOfThreads) (L.listReplace (V.fromList ["Result 1", "Result 2"]) (Just 0)))

changeView :: ViewName -> AppState -> AppState
changeView vn s = over (asViewSettings . vsFocusedView) (Brick.focusSetCurrent vn) s

-- | quickfix map which has already the Keybindings hacked in. Ideally we'd like
--- to fill this based on currently focused widget names and the Keybindings come out of the config
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
            , InternalKeybinding (Vty.EvKey (Vty.KChar ':') []) activateSearchThreads
            , InternalKeybinding (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) nextView
            ])
        , ( ListOfMails
          , [InternalKeybinding (Vty.EvKey (Vty.KChar 'q') []) backToIndex
            , InternalKeybinding (Vty.EvKey (Vty.KChar ':') []) activateSearchThreads
            , InternalKeybinding (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) nextView
            ])
        -- The search editor isn't doing anything. In fact it does nothing,
        -- since this POC does not support sending all non-matching input for
        -- the editor to the fallback key handler
        , ( SearchThreadsEditor
          , [InternalKeybinding (Vty.EvKey Vty.KEsc []) focusNext
            , InternalKeybinding (Vty.EvKey Vty.KEnter []) simulateNewSearchResult
            , InternalKeybinding (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) nextView
            ])
        ]

lookupKeybinding :: Event -> Maybe [InternalKeybinding] -> Maybe InternalKeybinding
lookupKeybinding _ Nothing = Nothing
lookupKeybinding e (Just kbs) = find (\(InternalKeybinding kbEv _) -> kbEv == e) kbs

dispatch :: AppState -> Event -> EventM Name (Next AppState)
dispatch s e = let currentlyFocused = focusedViewWidget s
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

indexView :: View
indexView =
    let widgets = [ListOfThreads, StatusBar, SearchThreadsEditor]
        ring = Brick.focusRing [ListOfThreads, SearchThreadsEditor]
    in View ring widgets

mailView :: View
mailView =
    let widgets = [ListOfMails, StatusBar, ManageMailTagsEditor]
        ring = Brick.focusRing [ListOfMails, ManageMailTagsEditor]
    in View ring widgets

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
        viewsettings =
            ViewSettings
            { _vsViews = Map.fromList [(Threads, indexView), (Mails, mailView)]
            , _vsFocusedView = Brick.focusRing [Threads, Mails]
            }
    in AppState mi compose viewsettings Map.empty Threads

main :: IO ()
main = void $ M.defaultMain theApp initialState
