{-# LANGUAGE OverloadedStrings #-}
module Draw where

import Agent
import Grid
import Types

import Graphics.Svg
import qualified Data.Text as T

svg :: Int -> Int -> Element -> Element
svg w h content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- (T.pack $ show w), Height_ <<- (T.pack $ show h)]

drawGrid :: Grid Cell -> Element
drawGrid g = svg w h (foldMap id (evalState (traverse f g) 0))
    where f :: Cell -> State Int Element
          f (Empty _i) = State (\i -> (square i "white", i+1))
          f (Full agent)
            | agentGroup agent == 1 = State (\i -> (square i "red", i+1))
            | agentGroup agent == 2 = State (\i -> (square i "blue", i+1))
            | otherwise = error "group not defined"

          w = maximum (fmap length (unGrid g))
          h = length $ unGrid g
          square i fill = rect_ [ Width_ <<- "1"
                                , Height_ <<- "1"
                                , X_ <<- (T.pack $ show (mod i w))
                                , Y_ <<- (T.pack $ show (div i h))
                                , Fill_ <<- fill ]


save :: String -> Grid Cell -> IO ()
save filename g = do
    let filename' = filename <> ".svg"
    writeFile filename' (show $ drawGrid g)
    putStrLn ("Write " <> filename')
