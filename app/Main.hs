module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Themes
import Brick.Types
import Control.Monad (replicateM, void)
import Graphics.Vty
import System.Random (randomRIO)

getNumbers :: Int -> IO [Int]
getNumbers n = replicateM n $ randomRIO (1000, 9999 :: Int)

data MyAppState n = MyAppState {
  numbers :: [Int]
, total :: Int
}

drawUI :: MyAppState () -> [Widget ()]
drawUI p = [ui]
  where
    ui :: Widget ()
    ui =
      withBorderStyle unicode
        (borderWithLabel
          (str "暗算") (center (str display)))

    display :: String
    display
      | null (numbers p) = "正解?\n\n" ++ show (total p)
      | otherwise = (show . head) (numbers p)

appEvent :: MyAppState () ->  BrickEvent () e -> EventM () (Next (MyAppState ()))
appEvent p (VtyEvent e)
  = case numbers p of
    [] -> halt p
    (x : xs) -> continue $ p { numbers = xs }

initialState :: [Int] -> MyAppState ()
initialState xs = MyAppState xs (sum xs)

theme :: Theme
theme = newTheme (white `on` black) []

myApp :: App (MyAppState ()) e ()
myApp = App {
  appDraw = drawUI
, appChooseCursor = neverShowCursor
, appHandleEvent = appEvent
, appStartEvent = return
, appAttrMap = \_ -> themeToAttrMap theme
}

main :: IO ()
main = do
  ns <- getNumbers 5
  void $ defaultMain myApp (initialState ns)
