{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import System.Directory
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import Network.HTTP.Conduit
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, ($//), (&|), (&//), (&/), (>=>), descendant, attribute, check) 
import Network (withSocketsDo)
import Data.String.Utils(replace)
import Data.List(sortBy)
import Data.Ord(comparing)


-- почтовый адрес
email = "alagaster@yandex.ru"

-- для перехода по гиперссылкам
mainUrl :: String
mainUrl = "http://wikimipt.org"

-- первая страница со списком преподавателей
startUrl :: String
startUrl = "http://wikimipt.org/wiki/%D0%9A%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D1%8F:%D0%9F%D1%80%D0%B5%D0%BF%D0%BE%D0%B4%D0%B0%D0%B2%D0%B0%D1%82%D0%B5%D0%BB%D0%B8_%D0%BF%D0%BE_%D0%B0%D0%BB%D1%84%D0%B0%D0%B2%D0%B8%D1%82%D1%83"

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- withSocketsDo $ simpleHttp u
     return $ fromDocument $ parseLBS page

getName :: Cursor -> T.Text
getName = T.concat . (descendant >=> element "h1" >=> attributeIs "id" "firstHeading" >=> child >=> content )

getCommentsNumber :: Cursor -> Int
getCommentsNumber = length . (descendant >=> element "div" >=> attributeIs "id" "allcomments" >=> child >=> element "div")

getProfessorData :: Cursor -> (T.Text, Int)
getProfessorData cursor = (getName cursor, getCommentsNumber cursor)

-- получение ссылок на страницы преподавателей с одной страницы оглавления
getProfessorsUrls :: Cursor -> [String]
getProfessorsUrls = map (((++) mainUrl) . T.unpack . T.concat) . (descendant >=> element "div" >=> attributeIs "class" "mw-category" >=> descendant >=> element "a" &| attribute "href")

checkUrlDest :: Cursor -> Bool
checkUrlDest = ((==) "Следующая страница") . T.concat . (child >=> content)

-- если следующей страницы нет — вернёт mainUrl
getNextPageUrl :: Cursor -> String
getNextPageUrl = (replace "amp;" "") . ((++) mainUrl) . T.unpack . T.concat . head . tail . (descendant >=> element "div" >=> attributeIs "id" "mw-pages" >=> child >=> element "a" &| check checkUrlDest >=> attribute "href")

cutProfessors :: [(T.Text, Int)] -> [(T.Text, Int)]
cutProfessors = (take 50) . sortBy (comparing (negate . snd)) 

-- получение ссылок на страницы всех преподавателей
getAllProfessorsUrls :: IO [String]
getAllProfessorsUrls = getAllProfessorsUrls' startUrl
    where getAllProfessorsUrls' url = do
            cursor <- cursorFor url
            let urls = getProfessorsUrls cursor
                nextUrl = getNextPageUrl cursor
            if nextUrl == mainUrl then return urls
                                  else do
                                      tail <- getAllProfessorsUrls' nextUrl
                                      return $ urls ++ tail

-- получение пары (ФИО, количество комментариев) для всех преподавателей
getAllProfessorsData :: IO [(T.Text, Int)]
getAllProfessorsData = do
        professorsPages <- getAllProfessorsUrls
        sequence $ map (fmap getProfessorData . cursorFor) professorsPages

-- собственно, задача
getTop50 :: IO [(T.Text, Int)]
getTop50 = do
        professorsData <- getAllProfessorsData
        return $ cutProfessors professorsData

toTextProfessorData :: (Integer, (T.Text, Int)) -> T.Text
toTextProfessorData (num, (name, score)) = T.concat [snum, T.pack ") ", name, T.pack ", ", sscore, T.pack " comments;\n"]
    where snum = T.pack $ show num
          sscore = T.pack $ show score 

toTextTop50 :: [(T.Text, Int)] -> T.Text
toTextTop50 top50 = T.concat $ map toTextProfessorData (zip [1..50] top50)

main :: IO()
main = withSocketsDo $ do
  top50 <- getTop50
  let textTop50 = toTextTop50 top50
  dir <- getCurrentDirectory
  initReq <- parseUrl "http://91.239.142.110:13666/lab3"
  handle <- openFile (dir ++ "/Lab3.hs") ReadMode
  hSetEncoding handle utf8_bom
  content <- hGetContents handle
  let req = urlEncodedBody [("email", email), ("result", encodeUtf8 textTop50), ("content", encodeUtf8 $ T.pack content) ] $ initReq { method = "POST" }
  response <- withManager $ httpLbs req
  hClose handle
  L.putStrLn $ responseBody response
