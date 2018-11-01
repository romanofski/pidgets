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
import Data.Time.Clock (UTCTime(..), utctDay, secondsToDiffTime)
import Data.Time.Calendar (Day(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.CircularList as CList
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString as B
import qualified Graphics.Vty as Vty
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Focus as Brick
import qualified Brick.AttrMap as A
import Brick.Util (fg, on)
import Brick.Widgets.Core
       (fill, emptyWidget, str, padLeft, txt, hBox, vLimit,
        withAttr, (<+>), (<=>), hLimitPercent)
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

type Tag = B.ByteString

data NotmuchMail = NotmuchMail
    { _mailSubject :: T.Text
    , _mailFrom :: T.Text
    , _mailDate :: UTCTime
    , _mailTags :: [Tag]
    , _mailId :: B.ByteString
    } deriving (Show, Eq)
makeLenses ''NotmuchMail

data MailIndex = MailIndex
    { _miListOfMails  :: L.List Name NotmuchMail
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
drawUI s = [unVBox $ foldMap (VBox . renderWidget s) (view asWidgets s)]

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
                     (asMailIndex . miListOfMails . L.listSelectedL)
                     s))) <+>
  vLimit 1 (fill ' ')

drawListOfMails :: AppState -> Widget Name
drawListOfMails s =
  let hasFocus = Just ListOfMails == Brick.focusGetCurrent (view asFocus s)
  in L.renderList listDrawMail hasFocus $ view (asMailIndex . miListOfMails) s

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

listDrawMail :: Bool -> NotmuchMail -> Widget Name
listDrawMail sel a =
    let isNewMail = True
        nmNewTag = "unread"
        widget = hBox
          -- NOTE: I believe that inserting a `str " "` is more efficient than
          -- `padLeft/Right (Pad 1)`.  This hypothesis should be tested.
          [ padLeft (Pad 1) (txt $ formatDate (view mailDate a))
          , padLeft (Pad 1) (renderAuthors sel $ view mailFrom a)
          , padLeft (Pad 1) (renderTagsWidget (view mailTags a) nmNewTag)
          , padLeft (Pad 1) (txt (view mailSubject a))
          , fillLine
          ]
    in withAttr (getListAttr isNewMail sel) widget

getListAttr :: Bool  -- ^ new?
            -> Bool  -- ^ selected?
            -> A.AttrName
getListAttr True True = listNewMailSelectedAttr  -- new and selected
getListAttr True False = listNewMailAttr  -- new and not selected
getListAttr False True = listSelectedAttr  -- not new but selected
getListAttr False False = listAttr  -- not selected and not new

fillLine :: Widget Name
fillLine = vLimit 1 (fill ' ')

formatDate :: UTCTime -> T.Text
formatDate t = T.pack $ formatTime defaultTimeLocale "%d/%b" (utctDay t)

renderAuthors :: Bool -> T.Text -> Widget Name
renderAuthors isSelected authors =
    let attribute =
            if isSelected
                then mailSelectedAuthorsAttr
                else mailAuthorsAttr
    in withAttr attribute $ hLimitPercent 20 (txt authors <+> fillLine)

mailAttr :: A.AttrName
mailAttr = "mail"

mailTagsAttr :: A.AttrName
mailTagsAttr = mailAttr <> "tags"

mailAuthorsAttr :: A.AttrName
mailAuthorsAttr = mailAttr <> "authors"

mailSelectedAuthorsAttr :: A.AttrName
mailSelectedAuthorsAttr = mailAuthorsAttr <> "selected"

renderTagsWidget :: [Tag] -> Tag -> Widget Name
renderTagsWidget tgs ignored' =
    let ts = filter (/= ignored') tgs
    in withAttr mailTagsAttr $ vLimit 1 $ txt $ decodeLenient $ B.intercalate " " $ fmap getTag ts

getTag :: Tag -> B.ByteString
getTag = id

decodeLenient :: B.ByteString -> T.Text
decodeLenient = T.decodeUtf8With T.lenientDecode
-- Event handling
--
-- | quickfix internal actions, which are usually generated and stripped of
-- their modes. Also they're not composable and don't have any descriptions.

-- list actions currently only made to interact with the list of threads
listUp :: InternalAction
listUp = InternalAction (M.continue . over (asMailIndex . miListOfMails) L.listMoveUp)

listDown :: InternalAction
listDown = InternalAction (M.continue . over (asMailIndex . miListOfMails) L.listMoveDown)

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
backToIndex = InternalAction (M.continue
                              . over asFocus (Brick.focusRingModify (CList.update ListOfThreads))
                              . over asWidgets (V.// [(0, ListOfThreads)]))

-- Instead of activating another mode and statically draw everything belonging
-- to this mode, just replace the list widget and set the focus on the new list
-- widget. That keeps the status bar and the search as is. We can now use the
-- focus ring to jump between searching threads and showing the current thread
-- mails.
activateListOfMails :: InternalAction
activateListOfMails = InternalAction (M.continue
                                      . over asFocus (Brick.focusRingModify (CList.update ListOfMails))
                                      . over asWidgets (V.// [(0, ListOfMails)]))

activateSearchThreads :: InternalAction
activateSearchThreads = InternalAction (M.continue . over asFocus (Brick.focusSetCurrent SearchThreadsEditor))

focusNext :: InternalAction
focusNext = InternalAction (M.continue . over asFocus Brick.focusNext)

simulateNewSearchResult :: InternalAction
simulateNewSearchResult = InternalAction (M.continue
                                          . over (asMailIndex . miListOfThreads) (L.listReplace (V.fromList ["Result 1", "Result 2"]) (Just 0))
                                          -- would be a composed action
                                          . over asFocus (Brick.focusRingModify (CList.update ListOfThreads))
                                          . over asWidgets (V.// [(0, ListOfThreads)]))

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
            , InternalKeybinding (Vty.EvKey (Vty.KChar ':') []) activateSearchThreads])
        , ( ListOfMails
          , [InternalKeybinding (Vty.EvKey (Vty.KChar 'q') []) backToIndex
            , InternalKeybinding (Vty.EvKey (Vty.KChar 'j') []) listDown
            , InternalKeybinding (Vty.EvKey (Vty.KChar 'k') []) listUp
            , InternalKeybinding (Vty.EvKey (Vty.KChar ':') []) activateSearchThreads])
        -- The search editor isn't doing anything. In fact it does nothing,
        -- since this POC does not support sending all non-matching input for
        -- the editor to the fallback key handler
        , ( SearchThreadsEditor
          , [InternalKeybinding (Vty.EvKey Vty.KEsc []) focusNext
            , InternalKeybinding (Vty.EvKey Vty.KEnter []) simulateNewSearchResult ])
        ]

lookupKeybinding :: Event -> Maybe [InternalKeybinding] -> Maybe InternalKeybinding
lookupKeybinding _ Nothing = Nothing
lookupKeybinding e (Just kbs) = find (\(InternalKeybinding kbEv _) -> kbEv == e) kbs

dispatch :: AppState -> Event -> EventM Name (Next AppState)
dispatch s e = let ring = view asFocus s
                   currentlyFocused = fromMaybe ListOfMails (Brick.focusGetCurrent ring)
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

listAttr :: A.AttrName
listAttr = L.listAttr

listSelectedAttr :: A.AttrName
listSelectedAttr = L.listSelectedAttr

listNewMailAttr :: A.AttrName
listNewMailAttr = L.listAttr <> "newmail"

listNewMailSelectedAttr :: A.AttrName
listNewMailSelectedAttr = listNewMailAttr <> L.listSelectedAttr

theme :: A.AttrMap
theme =
    A.attrMap
        Vty.defAttr
        [ (L.listSelectedAttr, Vty.blue `on` Vty.white)
        , (listSelectedAttr, Vty.white `on` Vty.yellow)
        , (listNewMailAttr, fg Vty.brightGreen `Vty.withStyle` Vty.bold)
        , (listNewMailSelectedAttr, Vty.white `on` Vty.yellow `Vty.withStyle` Vty.bold)
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
                (L.list ListOfMails (V.replicate 500 m) 1)
                (L.list ListOfThreads (V.fromList ["Thread 1", "Thread 2"]) 1)
                (E.editorText SearchThreadsEditor Nothing "tag:foobar")
                (E.editorText ManageMailTagsEditor Nothing "")
                (E.editorText ManageThreadTagsEditor Nothing "")
        m =
            NotmuchMail
                "This is the Subject"
                "From"
                (UTCTime (ModifiedJulianDay 23) (secondsToDiffTime 12312))
                ["unread", "index"]
                "asdf"
        compose =
            Compose
                (E.editorText ComposeFrom (Just 1) "")
                (E.editorText ComposeTo (Just 1) "")
                (E.editorText ComposeSubject (Just 1) "")
                (Brick.focusRing
                     [ComposeFrom, ComposeTo, ComposeSubject, ListOfAttachments])
                (L.list ListOfAttachments V.empty 1)
        view' = V.fromList [ListOfMails, StatusBar, SearchThreadsEditor]
        ring = Brick.focusRing [ListOfMails, SearchThreadsEditor]
    in AppState mi compose view' ring Map.empty

main :: IO ()
main = void $ M.defaultMain theApp initialState
