{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Network.HTTP.Conduit
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Network (withSocketsDo)

(email, name) = ("alagaster@yandex.ru", encodeUtf8 "Пыркин Д.В.") -- адрес почты и фамилия с инициалами

pascal :: Int -> Int -> Int
pascal 0 _ = 1
pascal c r  
    | c == r = 1
    | otherwise = pascal (c - 1) (r - 1) + pascal c (r - 1) -- а тут решение

printIt :: Int -> C.ByteString
printIt n = C.pack $ show $ [pascal y x | x <- [0..n], y <- [0..x]]

main :: IO()
main = 
  withSocketsDo $ do
  initReq <- parseUrl "http://91.239.142.110:13666/lab0"
  let req = urlEncodedBody [("email", email), ("name", name), ("content", printIt 20)] $ initReq { method = "POST" }
  response <- withManager $ httpLbs req
  L.putStr $ responseBody response
