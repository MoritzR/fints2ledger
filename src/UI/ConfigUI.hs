{-# LANGUAGE OverloadedLabels #-}

module UI.ConfigUI (runConfigUI) where

import Brick (AttrMap, BrickEvent (VtyEvent), Padding (Pad), Widget, attrMap, customMain, emptyWidget, fill, hLimit, halt, on, padBottom, str, strWrap, vLimit, zoom, (<+>), (<=>))
import Brick.Focus (focusGetCurrent, focusRingCursor)
import Brick.Forms (Form (formFocus, formState), FormFieldState, editPasswordField, editTextField, focusedFormInputAttr, handleFormEvent, invalidFormInputAttr, newForm, renderForm, (@@=))
import Brick.Main (App (..))
import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Edit qualified as E
import Config.Files (ConfigDirectory, getConfigFilePath)
import Config.YamlConfig (FintsConfig (..), YamlConfig (..))
import Control.Lens (Iso', Lens', iso)
import Data.Generics.Labels ()
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import UI.ConfigFields qualified as Fields
import Utils ((??))

runConfigUI :: YamlConfig -> ConfigDirectory -> IO FintsConfig
runConfigUI initialConfig configDirectory = do
  initialVty <- buildVty

  let initialForm = mkForm initialConfig.fints

  updatedForm <-
    customMain
      initialVty
      buildVty
      Nothing
      (formApp configDirectory)
      (FormAppState initialForm)
  let newFintsConfig = formState updatedForm.form

  return newFintsConfig

type Name = Fields.Fields

newtype FormAppState e = FormAppState
  { form :: Form FintsConfig e Name
  }
  deriving (Generic)

draw :: ConfigDirectory -> FormAppState e -> [Widget Name]
draw configDirectory state =
  [ form
      <+> vBorder
      <+> hLimit 20 help
        <=> configText
  ]
 where
  form = renderForm state.form
  help = (strWrap . helpText <$> currentFocus) ?? emptyWidget
  configText =
    str $
      "The full config is available with more options is available at:\n"
        <> getConfigFilePath configDirectory
  currentFocus = focusGetCurrent $ formFocus state.form

showFieldForUser :: Fields.Fields -> String
showFieldForUser field = case field of
  Fields.Account -> "Account"
  Fields.Blz -> "BLZ"
  Fields.Password -> "Password"
  Fields.Endpoint -> "FinTS Endpoint"
  _ -> "<missing field name>"

helpText :: Name -> String
helpText field = case field of
  Fields.Account -> "The account number you use to log into your banking account."
  Fields.Blz -> "Your banks BLZ"
  Fields.Password -> "Your banking password.\nLeave empty if you don't want to store it."
  Fields.Endpoint -> "Your banks FinTS endpoint.\nFor example for ING this is: https://fints.ing.de/fints"
  _ -> "<missing help text>"

attributeMap :: AttrMap
attributeMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.black)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    ]

formApp :: ConfigDirectory -> App (FormAppState e) e Name
formApp configDirectory =
  App
    { appDraw = draw configDirectory
    , appHandleEvent = \event -> do
        case event of
          VtyEvent (V.EvResize{}) -> return ()
          VtyEvent (V.EvKey V.KEsc []) -> halt
          _ -> zoom #form $ handleFormEvent event
    , appChooseCursor = focusRingCursor $ formFocus . (.form)
    , appStartEvent = return ()
    , appAttrMap = const attributeMap
    }

buildVty :: IO V.Vty
buildVty = do
  v <- V.mkVty =<< V.standardIOConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

lazyTextConverter :: Iso' TL.Text T.Text
lazyTextConverter = iso TL.toStrict TL.fromStrict

mkForm :: FintsConfig -> Form FintsConfig e Name
mkForm =
  newForm
    [ textInput Fields.Account #account
    , textInput Fields.Blz #blz
    , textInput Fields.Endpoint #endpoint
    , passwordInput Fields.Password (#password . #get)
    ]

label :: Fields.Fields -> Widget n -> Widget n
label field widgetToModify =
  padBottom (Pad 1) $
    vLimit 1 (hLimit 15 $ str (showFieldForUser field) <+> fill ' ') <+> widgetToModify

textInput :: Fields.Fields -> TextLens -> FintsConfig -> FintsFormFieldState e
textInput field lens = label field @@= editTextField (lens . lazyTextConverter) field (Just 1)

passwordInput :: Fields.Fields -> TextLens -> FintsConfig -> FintsFormFieldState e
passwordInput field lens = label field @@= editPasswordField (lens . lazyTextConverter) field

type FintsFormFieldState e = FormFieldState FintsConfig e Fields.Fields
type TextLens = Lens' FintsConfig TL.Text
