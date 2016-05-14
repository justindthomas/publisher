{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Web.Scotty
import           Database.Persist ((==.), (=.))
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import           Database.Persist.TH
import           Data.Aeson
import           GHC.Generics
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Internal as I
import qualified Data.List as L
import           Data.Maybe
import           Text.Pandoc
import           Text.Pandoc.Error
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as B (pack,unpack,ByteString)
import qualified Data.ByteString.Lazy as BL (unpack,toStrict)
import           System.IO

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Article
    json
    reference String Eq
    title String
    content String
    summary String
    UniqueName reference
    deriving Show
    deriving Generic
|]

markdownToHtml :: String -> T.Text
markdownToHtml = pandocToHtml . markdownToPandoc

markdownToPandoc :: String -> Pandoc
markdownToPandoc = handleError . readMarkdown def

pandocToHtml :: Pandoc -> T.Text
pandocToHtml = T.pack . writeHtmlString def

readDbConfig :: IO B.ByteString
readDbConfig = do
  c <- readFile "devel.cfg"
  return $ B.pack $ T.unpack $ buildString $ T.lines $ T.pack c

buildString :: [T.Text] -> T.Text
buildString = T.unwords

main :: IO ()
main = do
  putStrLn "Starting Server..."
  cs <- readDbConfig
  putStrLn $ show cs

  scotty 3000 $ do
    get "/" $ html "Hello World!"
    get "/articles" $ do
      articles <- inHandlerDb cs $ DB.selectList [] []
      html ("Articles: " <> (T.pack $ show $ length (articles :: [DB.Entity Article])))
    get "/articles/:ref" $ do
      articleReference <- param "ref"
      findArticle <- inHandlerDb cs $ DB.getBy $ UniqueName articleReference
      case findArticle of
        Nothing -> html $ (markdownToHtml $ articleContent (Article "Invalid Entry" "Invalid Entry" "No such article exists." "No summary."))
        Just (DB.Entity _ a) ->
          html $ (markdownToHtml $ articleContent a)
    get "/articles/:ref/raw" $ do
      articleReference <- param "ref"
      findArticle <- inHandlerDb cs $ DB.getBy $ UniqueName articleReference
      case findArticle of
        Nothing -> html $ (markdownToHtml $ articleContent (Article "Invalid Entry" "Invalid Entry" "No such article exists." "No summary."))
        Just (DB.Entity _ a) ->
          html $ (T.pack $ articleContent a)
    put "/articles/:id" $ do
      article <- jsonData
      articleId <- param "id"
      inHandlerDb cs $ do
        DB.replace (DB.toSqlKey (read articleId)) $ (article :: Article)
      text (T.pack $ "updated")
    put "/articles/:id/content" $ do
      article <- body
      articleId <- param "id"
      inHandlerDb cs $ do
        DB.update (DB.toSqlKey (read articleId)) [ArticleContent =. (B.unpack $ BL.toStrict article)]
      text (T.pack $ "updated")
    post "/articles" $ do
      article <- jsonData
      articleId <- inHandlerDb cs $ DB.insert (article :: Article)
      text (T.pack $ "inserted: " <> show articleId)
    delete "/articles/:id" $ do
      articleId <- param "id"
      inHandlerDb cs $ DB.deleteWhere [ArticleId ==. (DB.toSqlKey (read articleId))]
      text (T.pack $ "deleted: " <> show articleId)

instance FromJSON Article

inHandlerDb conn query = liftIO $ dbFunction conn query

dbFunction conn query = runStderrLoggingT $ 
        DB.withPostgresqlPool conn 10 $ 
        \pool -> liftIO $ DB.runSqlPersistMPool query pool
