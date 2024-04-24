


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib(startApp) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import  Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp 
import Servant
import Servant.API
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Persist.Sql
import qualified Data.ByteString.Char8 as BS
import Data.String (fromString)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger ( runStdoutLoggingT, LoggingT, runStderrLoggingT )
import Data.Int (Int64)
import Data.Text ( Text )
-- import Control.Monad.Logger

import Schema
startApp :: IO()
startApp = do 
   let connStr = "host=127.0.0.1 dbname=rest_api user=postgres password=123 port=5432"
   pool <- runStderrLoggingT $ createPostgresqlPool connStr 10 
   print("server up and running at 8080")
   runSqlPool (runMigration migrateAll) pool 
   run 8080 (app pool)

-- sqlPersistT transformer Monad read documentation for more details
-- runDb :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
-- runDb connStr action = runStdoutLoggingT $  withPostgresqlConn connStr $ (\ conn -> runReaderT action conn)

-- migrateDB :: ConnectionString -> IO()
-- migrateDB connStr = runDb connStr (runMigration migrateAll)



type UserAPI1 =
  "hello" :> Get '[PlainText] Text 
  :<|> "register" :> ReqBody '[JSON] Users :> Post '[PlainText] Text  
  :<|> "movies" :> Get '[JSON] [Entity Movies]
  :<|> "movies" :> "add" :> ReqBody '[JSON] Movies :> Post '[JSON] (Entity Movies)
  :<|> "movies" :> "update" :> Capture "id" Int64 :> ReqBody '[JSON] Movies :> Put '[JSON] (Entity Movies)
  :<|> "movies" :> "delete" :> Capture "id" Int64 :> Delete '[PlainText] String
  :<|> "users"  :> "login" :> ReqBody '[JSON] ReqUser :> Post '[JSON] Text
  :<|> "users" :> "logout" :> Delete '[JSON] Text
  :<|> "users" :> "addFav" :> Capture "id" MoviesId :> Post '[PlainText] Text 


helloHandler :: Handler Text 
helloHandler = return "connection done"



-- Define handler for registering a new user
registerHandler :: ConnectionPool -> Users -> Handler Text
registerHandler pool regUser = liftIO $ do
  _ <- liftIO $ runSqlPool (insert regUser) pool
  return "successful"

userLoginHandler :: ConnectionPool -> ReqUser -> Handler Text 
userLoginHandler pool (ReqUser username password) = do 
  activeUser <- liftIO $ runSqlPool (selectFirst [] []) pool :: Handler (Maybe (Entity Activeuser)) 
  case activeUser of 
    Just _ -> throwError err404 { errBody = "Already Looged in" }
    Nothing -> do
      userAvailable <- liftIO $ runSqlPool (selectFirst [UsersUsername ==. username] []) pool
      case userAvailable of 
        Nothing -> throwError err404 { errBody = "User Not found" }
        Just(Entity _ user) -> do
          if usersPassword user == password
            then do
              _ <- liftIO $ runSqlPool (insert_ (Activeuser username)) pool
              return "Login Successfully."
            else 
              throwError err404 { errBody = "Incorrect Password." }

userLogoutHandler :: ConnectionPool -> Handler Text 
userLogoutHandler pool = do
  _ <- liftIO $ runSqlPool (deleteWhere ([] :: [Filter Activeuser])) pool
  return "Logged out Successfully." 
 
userFavHandler :: ConnectionPool -> MoviesId -> Handler Text
userFavHandler pool movieId = do
    maybeActiveUser <- liftIO $ runSqlPool (selectFirst [] []) pool :: Handler (Maybe (Entity Activeuser))
    case maybeActiveUser of
        Nothing -> throwError err404 { errBody = "User not logged in" }
        Just (Entity _ activeUser) -> do
            maybeUser <- liftIO $ runSqlPool (selectFirst [UsersUsername ==. activeuserUsername activeUser] []) pool :: Handler (Maybe (Entity Users))
            case maybeUser of
                Nothing -> throwError err404 { errBody = "User not found" }
                Just (Entity userId user) -> do
                    let updatedFav = movieId : usersFavMovies user
                    _ <- liftIO $ runSqlPool (update userId [UsersFavMovies =. updatedFav]) pool
                    return "Movie added to favorite list"


-- userLoginHandler :: ConnectionPool ->


-- userLogoutHandler :: ConnectionPool -> 

getMoviesHandler :: ConnectionPool -> Handler [Entity Movies]
getMoviesHandler pool = liftIO $ runSqlPool (selectList [] []) pool 

-- write add movie handler here 
addMoviesHandler :: ConnectionPool -> Movies -> Handler (Entity Movies)
addMoviesHandler pool newMovie = liftIO $ do
    movieId <- runSqlPool (insert newMovie) pool
    return $ Entity movieId newMovie

updateMovieHandler :: ConnectionPool -> Int64 -> Movies -> Handler (Entity Movies)
updateMovieHandler pool movieIdToUpdate updatedMovie = liftIO $ do
    runSqlPool (replace (toSqlKey movieIdToUpdate) updatedMovie) pool
    return $ Entity (toSqlKey movieIdToUpdate) updatedMovie


deleteMovieHandler :: ConnectionPool -> Int64 -> Handler String 
deleteMovieHandler pool movieIdToDelete = liftIO $ do
  _ <- runSqlPool (delete (toSqlKey movieIdToDelete :: MoviesId)) pool
  return "True"








userServer :: ConnectionPool -> Server UserAPI1 
userServer pool = 
  helloHandler
  :<|>
  registerHandler pool
  :<|>
  getMoviesHandler pool 
  :<|>
  addMoviesHandler pool 
  :<|>
  updateMovieHandler pool
  :<|>
  deleteMovieHandler pool
  :<|>
  userLoginHandler pool 
  :<|>
  userLogoutHandler pool 
  :<|>
  userFavHandler pool


app :: ConnectionPool -> Application
app pool  =
   serve (Proxy :: Proxy UserAPI1) (userServer pool)