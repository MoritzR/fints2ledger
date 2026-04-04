{-# LANGUAGE OverloadedLabels #-}

module UI.ConfigUI (runConfigUI) where

import Brick (BrickEvent (VtyEvent), Padding (Pad), Widget, attrName, emptyWidget, fill, hLimit, hLimitPercent, halt, padBottom, str, txt, txtWrap, vLimit, withAttr, zoom, (<+>), (<=>))
import Brick.Focus (focusGetCurrent, focusRingCursor)
import Brick.Forms (Form (formFocus, formState), FormFieldState, editPasswordField, editTextField, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Main qualified as Brick (App (..))
import Brick.Widgets.Border (vBorder)
import Config.Files (ConfigDirectory, getConfigFilePath)
import Config.YamlConfig (FintsConfig (..), LedgerConfig (..), Password (..), YamlConfig (..))
import Control.Lens (Iso', Lens', iso, (.=), (.~))
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as T (null, pack, unpack)
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import UI.ConfigFields qualified as Fields
import UI.Helper (attributeMap, main)
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
  let initialForm =
        mkForm $
          UiConfig
            { account = initialConfig.fints.account
            , blz = initialConfig.fints.blz
            , endpoint = initialConfig.fints.endpoint
            , password = initialConfig.fints.password
            , journalFile = initialConfig.ledger.journalFile
            }

  updatedForm <- main (formApp configDirectory) (FormAppState initialForm False)
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

-- Create a form with all configuration fields
mkForm :: UiConfig -> Form UiConfig e Fields.Field
mkForm =
  newForm
    [ textInput Fields.account #account
    , textInput Fields.blz #blz
    , textInput Fields.endpoint #endpoint
    , passwordInput Fields.password (#password . maybePasswordToPassword . #get)
    , textInput Fields.journalFile (#journalFile . defaultValueToMaybe "" . textToString)
    ]

data FormAppState e = FormAppState
  { form :: Form UiConfig e Fields.Field
  , aborted :: Bool
  }
  deriving (Generic)

-- Main UI layout drawing function
draw :: ConfigDirectory -> FormAppState e -> [Widget Fields.Field]
draw configDirectory state = [mainLayer]
 where
  mainLayer =
    (formSection <+> vBorder <+> helpSection)
      <=> infoSection
      <=> controlsSection

  -- Core sections of the UI
  formSection = renderForm state.form
  helpSection = hLimitPercent 30 $ renderHelpText currentFocus
  infoSection = renderConfigFilePath configDirectory
  controlsSection = renderControlsBar

  currentFocus = focusGetCurrent $ formFocus state.form

-- Renders help text for the currently focused field
renderHelpText :: Maybe Fields.Field -> Widget Fields.Field
renderHelpText focus = (txtWrap . (.helpText) <$> focus) ?? emptyWidget

-- Shows the config file path
renderConfigFilePath :: ConfigDirectory -> Widget Fields.Field
renderConfigFilePath configDir =
  str $
    "The full config with more options is available at:\n"
      <> getConfigFilePath configDir

-- Renders the controls bar at the bottom of the UI
renderControlsBar :: Widget Fields.Field
renderControlsBar =
  primaryCtrl (str " Ctrl+C ")
    <+> secondaryCtrl (str " Quit without saving ")
    <+> primaryCtrl (str " Esc ")
    <+> secondaryCtrl (str " Save and quit ")
 where
  primaryCtrl = withAttr $ attrName "primary"
  secondaryCtrl = withAttr $ attrName "secondary"

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
    vLimit 1 (hLimit 15 $ txt field.label <+> fill ' ') <+> widgetToModify

textInput :: Fields.Field -> TextLens -> UiConfig -> FintsFormFieldState e
textInput field lens = label field @@= editTextField lens field (Just 1)

passwordInput :: Fields.Field -> TextLens -> UiConfig -> FintsFormFieldState e
passwordInput field lens = label field @@= editPasswordField lens field

type FintsFormFieldState e = FormFieldState UiConfig e Fields.Field
type TextLens = Lens' UiConfig Text
