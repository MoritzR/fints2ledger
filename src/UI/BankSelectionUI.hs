{-# LANGUAGE OverloadedLabels #-}

module UI.BankSelectionUI (runBankSelectionUI) where

import Brick (AttrMap, BrickEvent (VtyEvent), Padding (Pad), Widget, attrMap, customMain, hLimit, halt, on, padAll, padTop, str, vLimit, zoom, (<=>))
import Brick.Main qualified as Brick (App (..))
import Brick.Widgets.Center (center)
import Brick.Widgets.List (List, handleListEvent, list, listSelectedElement, listSelectedFocusedAttr, renderList)
import Config.Banks (Bank (..))
import Control.Lens ((.=))
import Data.Generics.Labels ()
import Data.Vector qualified as Vec
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Graphics.Vty.Config qualified
import Graphics.Vty.CrossPlatform qualified

data BankSelectionState = BankSelectionState
  { bankList :: List () Bank
  , aborted :: Bool
  }
  deriving (Generic)

allBanks :: [Bank]
allBanks = enumerate

displayName :: Bank -> String
displayName ING = "ING"
displayName GLS = "GLS"
displayName OTHER = "Other (enter endpoint manually)"

runBankSelectionUI :: IO (Maybe Bank)
runBankSelectionUI = do
  let initialState =
        BankSelectionState
          { bankList = list () (Vec.fromList allBanks) 1
          , aborted = False
          }
  initialVty <- buildVty
  finalState <-
    customMain
      initialVty
      buildVty
      Nothing
      bankSelectionApp
      initialState
  if finalState.aborted
    then return Nothing
    else return $ snd <$> listSelectedElement finalState.bankList

draw :: BankSelectionState -> [Widget ()]
draw state =
  [ center $
      hLimit 50 $
        padAll 1 $
          str "Select your bank:"
            <=> vLimit (length allBanks) (renderList renderItem True state.bankList)
            <=> padTop (Pad 1) (str "[↑↓] Navigate  [Enter] Select  [Ctrl+C] Quit")
  ]
 where
  renderItem _isSelected bank = str (displayName bank)

attributeMap :: AttrMap
attributeMap =
  attrMap
    V.defAttr
    [(listSelectedFocusedAttr, V.black `on` V.yellow)]

bankSelectionApp :: Brick.App BankSelectionState e ()
bankSelectionApp =
  Brick.App
    { appDraw = draw
    , appHandleEvent = \case
        VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> do
          #aborted .= True
          halt
        VtyEvent (V.EvKey V.KEnter []) -> halt
        VtyEvent vtyEvent -> zoom #bankList $ handleListEvent vtyEvent
        _otherwise -> return ()
    , appChooseCursor = \_ _ -> Nothing
    , appStartEvent = return ()
    , appAttrMap = const attributeMap
    }

buildVty :: IO V.Vty
buildVty = do
  v <- Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

-- can be removed once on base-4.22.0
enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]
