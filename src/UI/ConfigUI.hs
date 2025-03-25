{-# LANGUAGE OverloadedLabels #-}

module UI.ConfigUI (runConfigUI) where

import Brick (AttrMap, BrickEvent (VtyEvent), Padding (Pad), Widget, attrMap, attrName, customMain, emptyWidget, fill, hLimit, halt, on, padBottom, str, strWrap, vLimit, withAttr, zoom, (<+>), (<=>))
import Brick.Focus (focusGetCurrent, focusRingCursor)
import Brick.Forms (Form (formFocus, formState), FormFieldState, editPasswordField, editTextField, focusedFormInputAttr, handleFormEvent, invalidFormInputAttr, newForm, renderForm, (@@=))
import Brick.Main qualified as Brick (App (..))
import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Edit qualified as E
import Config.Files (ConfigDirectory, getConfigFilePath)
import Config.YamlConfig (FintsConfig (..), LedgerConfig (..), Password (..), YamlConfig (..))
import Control.Lens (Iso', Lens', iso, (.=), (.~))
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as T (null, pack, unpack)
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Graphics.Vty.Config qualified
import Graphics.Vty.CrossPlatform qualified
import UI.ConfigFields qualified as Fields
import Utils ((??))

data UiConfig = UiConfig
  { account :: Text
  , blz :: Text
  , endpoint :: Text
  , password :: Maybe Password
  , journalFile :: Maybe FilePath
  }
  deriving (Show, Generic)

runConfigUI :: YamlConfig -> ConfigDirectory -> IO (Maybe YamlConfig)
runConfigUI initialConfig configDirectory = do
  initialVty <- buildVty

  let initialForm =
        mkForm $
          UiConfig
            { account = initialConfig.fints.account
            , blz = initialConfig.fints.blz
            , endpoint = initialConfig.fints.endpoint
            , password = initialConfig.fints.password
            , journalFile = initialConfig.ledger.journalFile
            }

  updatedForm <-
    customMain
      initialVty
      buildVty
      Nothing -- not using custom events
      (formApp configDirectory) -- the UI App
      (FormAppState initialForm False) -- initial state
  let uiConfig = formState updatedForm.form

  return $
    if updatedForm.aborted
      then Nothing
      else Just (updateYamlWithUiConfig uiConfig initialConfig)

updateYamlWithUiConfig :: UiConfig -> YamlConfig -> YamlConfig
updateYamlWithUiConfig uiConfig yamlConfig =
  yamlConfig
    & #fints . #account .~ uiConfig.account
    & #fints . #blz .~ uiConfig.blz
    & #fints . #endpoint .~ uiConfig.endpoint
    & #fints . #password .~ uiConfig.password
    & #ledger . #journalFile .~ uiConfig.journalFile

data FormAppState e = FormAppState
  { form :: Form UiConfig e Fields.Field
  , aborted :: Bool
  }
  deriving (Generic)

draw :: ConfigDirectory -> FormAppState e -> [Widget Fields.Field]
draw configDirectory state =
  [ form
      <+> vBorder
      <+> hLimit 20 help
      <=> configText
      <=> controls
  ]
 where
  form = renderForm state.form
  help = (strWrap . helpText <$> currentFocus) ?? emptyWidget
  configText =
    str $
      "The full config with more options is available at:\n"
        <> getConfigFilePath configDirectory
  controls =
    controlsPrimary (str " Ctrl+C ")
      <+> controlSecondary (str " Quit without saving ")
      <+> controlsPrimary (str " Esc ")
      <+> controlSecondary (str " Save and quit ")
  currentFocus = focusGetCurrent $ formFocus state.form
  controlsPrimary = withAttr $ attrName "primary"
  controlSecondary = withAttr $ attrName "secondary"

showFieldForUser :: Fields.Field -> String
showFieldForUser field = case field of
  Fields.Account -> "Account"
  Fields.Blz -> "BLZ"
  Fields.Password -> "Password"
  Fields.Endpoint -> "FinTS Endpoint"
  Fields.JournalFile -> "Journal File"
  _otherwise -> "<missing field name>"

helpText :: Fields.Field -> String
helpText field = case field of
  Fields.Account -> "The account number you use to log into your banking account."
  Fields.Blz -> "Your banks BLZ"
  Fields.Password -> "Your banking password.\nLeave empty if you don't want to store it."
  Fields.Endpoint -> "Your banks FinTS endpoint.\nFor example for ING this is: https://fints.ing.de/fints"
  Fields.JournalFile -> "The path to the ledger file where the transactions should be stored. For example 'journal.ledger' (the default) or '~/journal.ledger'"
  _otherwise -> "<missing help text>"

attributeMap :: AttrMap
attributeMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.brightBlack)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    , (attrName "primary", V.white `on` V.brightBlue)
    , (attrName "secondary", V.white `on` V.blue)
    ]

formApp :: ConfigDirectory -> Brick.App (FormAppState e) e Fields.Field
formApp configDirectory =
  Brick.App
    { appDraw = draw configDirectory
    , appHandleEvent = \event -> do
        case event of
          VtyEvent (V.EvResize{}) -> return ()
          VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> do
            #aborted .= True
            halt
          VtyEvent (V.EvKey V.KEsc []) -> halt
          _otherwise -> zoom #form $ handleFormEvent event
    , appChooseCursor = focusRingCursor $ formFocus . (.form)
    , appStartEvent = return ()
    , appAttrMap = const attributeMap
    }

buildVty :: IO V.Vty
buildVty = do
  v <- Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

mkForm :: UiConfig -> Form UiConfig e Fields.Field
mkForm =
  newForm
    [ textInput Fields.Account #account
    , textInput Fields.Blz #blz
    , textInput Fields.Endpoint #endpoint
    , passwordInput Fields.Password (#password . maybePasswordToPassword . #get)
    , textInput Fields.JournalFile (#journalFile . defaultValueToMaybe "" . textToString)
    ]

maybePasswordToPassword :: Iso' (Maybe Password) Password
maybePasswordToPassword =
  iso
    (?? Password "")
    (\password -> if T.null password.get then Nothing else Just password)

defaultValueToMaybe :: (Eq a) => a -> Iso' (Maybe a) a
defaultValueToMaybe defaultValue =
  iso
    (?? defaultValue)
    (\value -> if value == defaultValue then Nothing else Just value)

textToString :: Iso' String Text
textToString = iso T.pack T.unpack

label :: Fields.Field -> Widget n -> Widget n
label field widgetToModify =
  padBottom (Pad 1) $
    vLimit 1 (hLimit 15 $ str (showFieldForUser field) <+> fill ' ') <+> widgetToModify

textInput :: Fields.Field -> TextLens -> UiConfig -> FintsFormFieldState e
textInput field lens = label field @@= editTextField lens field (Just 1)

passwordInput :: Fields.Field -> TextLens -> UiConfig -> FintsFormFieldState e
passwordInput field lens = label field @@= editPasswordField lens field

type FintsFormFieldState e = FormFieldState UiConfig e Fields.Field
type TextLens = Lens' UiConfig Text
