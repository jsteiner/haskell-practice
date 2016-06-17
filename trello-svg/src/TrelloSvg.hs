{-# LANGUAGE OverloadedStrings #-}
module TrelloSvg where

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg.Renderer.Text as S (renderSvg)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import qualified Data.Text.Lazy.IO as T

writeSvg :: IO ()
writeSvg = do
    let svg = S.renderSvg $ trelloSvg "#0079BF"
    T.writeFile "trello.svg" svg

trelloSvg :: S.AttributeValue -> S.Svg
trelloSvg color =
    S.docTypeSvg ! A.version "1.1" ! A.width "100" ! A.height "100" $
        S.path ! A.d svgPathD ! A.fill color

svgPathD :: S.AttributeValue
svgPathD = "M25.0013814,0 C11.1934997,0 0,11.1994381 0,25.0013814 L0,174.998619 C0,188.8065 11.1994381,200 25.0013814,200 L174.998619,200 C188.8065,200 200,188.800562 200,174.998619 L200,25.0013814 C200,11.1934997 188.800562,0 174.998619,0 L25.0013814,0 L25.0013814,0 Z M124.997036,26 C118.371256,26 113,31.3745986 113,38.0059087 L113,101.494091 C113,108.124772 118.373226,113.5 124.997036,113.5 L162.002964,113.5 C168.628744,113.5 174,108.125401 174,101.494091 L174,38.0059087 C174,31.3752284 168.626774,26 162.002964,26 L124.997036,26 L124.997036,26 Z M37.997036,26 C31.371256,26 26,31.3690439 26,37.9916439 L26,151.508356 C26,158.131158 31.3732256,163.5 37.997036,163.5 L75.002964,163.5 C81.628744,163.5 87,158.130956 87,151.508356 L87,37.9916439 C87,31.3688418 81.6267744,26 75.002964,26 L37.997036,26 L37.997036,26 Z"
