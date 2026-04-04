module UI.Helper (main, attributeMap) where

import Brick (attrMap, attrName, customMain)
import Brick.AttrMap (AttrMap)
import Brick.Forms (focusedFormInputAttr, invalidFormInputAttr)
import Brick.Main qualified as Brick (App (..))
import Brick.Util (on)
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List (listSelectedFocusedAttr)
import Graphics.Vty qualified as V
import Graphics.Vty.Config qualified
import Graphics.Vty.CrossPlatform qualified

main :: (Ord n) => Brick.App s e n -> s -> IO s
main app state = do
  initialVty <- buildVty
  customMain
    initialVty
    buildVty
    Nothing
    app
    state

buildVty :: IO V.Vty
buildVty = do
  v <- Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

-- Define color themes for the UI
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
    , (listSelectedFocusedAttr, V.black `on` V.yellow)
    ]
