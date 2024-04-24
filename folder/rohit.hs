{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Rohit where 
where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics
import           Servant
import           Network.Wai.Handler.Warp
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Aeson
import           Data.Text
import           Data.Maybe 
import           DB 
import           Database.Esqueleto (ConnectionPool, Entity (Entity))
import Servant (Delete, StdMethod (DELETE))
import Data.Maybe (fromMaybe)

type API = "register" :> ReqBody '[JSON] UserRegistration :> Post '[PlainText] Text 
      :<|> "addmovie" :> ReqBody '[JSON] MovieAdd :> Post '[PlainText] Text 
      :<|> "login" :> ReqBody '[JSON] UserLogin :> Post '[PlainText] Text 
      :<|> "logout" :> Delete '[PlainText] Text 
      :<|> "updatemovie" :> Capture "movieId" MovieId :> ReqBody '[JSON] MovieUpdate :> Patch '[JSON] (Entity Movie) 
      :<|> "addfav" :> Capture "movieId" MovieId :> Post '[PlainText] Text  
      :<|> "users" :> Get '[JSON] [Entity User]
      :<|> "movies" :> Get '[JSON] [Entity Movie]

startApp :: IO ()
startApp = do
    let connStr = "dbname=cred user=postgres password=Himanshu_manuj_@69_Arya"
    pool <- runStderrLoggingT $ createPostgresqlPool connStr 10
    runSqlPool (runMigration migrateAll) pool
    run 8080 (app pool)

data UserRegistration = UserRegistration
  { regUsername :: String
  , regPassword :: String
  , regEmail :: String 
  } deriving (Eq, Show, Generic, FromJSON) 

data MovieAdd = MovieAdd
  { newMoviename :: String
  , newGenre :: String
  , newRating :: Int  
  } deriving (Eq, Show, Generic, FromJSON) 

data UserLogin = UserLogin
  { loginUsername :: String
  , loginPassword :: String 
  } deriving (Eq, Show, Generic, FromJSON)

data MovieUpdate = MovieUpdate
  { updatedMoviename :: Maybe String
  , updatedMoviegenre :: Maybe String 
  , updatedMovierating :: Maybe Int 
  } deriving (Eq, Show, Generic, FromJSON) 

registerUser :: ConnectionPool -> UserRegistration -> Handler Text
registerUser pool userReg = do
  let newUser = User
        { userUsername = regUsername userReg
        , userPassword = regPassword userReg
        , userEmail = regEmail userReg  
        , userFav = []
        }
  _ <- liftIO $ runSqlPool (insert_ newUser) pool
  return "User registered successfully."

movieAdd :: ConnectionPool -> MovieAdd -> Handler Text
movieAdd pool userMov = do
  let newMovie = Movie
        { movieMoviename = newMoviename userMov
        , movieGenre = newGenre userMov
        , movieRating = newRating userMov   
        }
  _ <- liftIO $ runSqlPool (insert_ newMovie) pool
  return "Movie add successfully."

loginUser :: ConnectionPool -> UserLogin -> Handler Text
loginUser pool (UserLogin username password) = do
  activeUser <- liftIO $ runSqlPool (selectFirst [] []) pool :: Handler (Maybe (Entity ActiveUser)) 
  case activeUser of 
    Just _ -> throwError err404 { errBody = "Someone already Looged in" }
    Nothing -> do
      userAvailable <- liftIO $ runSqlPool (selectFirst [UserUsername ==. username] []) pool
      case userAvailable of 
        Nothing -> throwError err404 { errBody = "User Not found" }
        Just(Entity userId user) -> do
          if userPassword user == password
            then do
              _ <- liftIO $ runSqlPool (insert_ (ActiveUser username)) pool
              return "Login Successfully."
            else 
              throwError err404 { errBody = "Incorrect Password." }

logoutUser :: ConnectionPool -> Handler Text
logoutUser pool = do
  _ <- liftIO $ runSqlPool (deleteWhere ([] :: [Filter ActiveUser])) pool
  return "Logged out Successfully." 
 
updateMovie :: ConnectionPool -> MovieId -> MovieUpdate -> Handler (Entity Movie)
updateMovie pool movieId newMovie = do
  movieAvailable <- liftIO $ runSqlPool (get movieId) pool
  case movieAvailable of
    Nothing -> throwError err404
    Just prevmovie -> do
      let updatedMovie = prevmovie{
        movieMoviename = fromMaybe (movieMoviename prevmovie) (updatedMoviename newMovie) ,
        movieGenre = fromMaybe (movieGenre prevmovie) (updatedMoviegenre newMovie) ,
        movieRating = fromMaybe (movieRating prevmovie) (updatedMovierating newMovie) 
      }
      _ <- liftIO $ runSqlPool (Database.Persist.Postgresql.replace movieId updatedMovie) pool
      return $ Entity movieId updatedMovie 

favMovie :: ConnectionPool -> MovieId  -> Handler Text
favMovie pool movieId = do
  activeUser <- liftIO $ runSqlPool (selectFirst [] []) pool :: Handler (Maybe (Entity ActiveUser)) 
  case activeUser of 
    Nothing -> throwError err404 { errBody = "User Not looged in" }
    Just (Entity _ activeUser') -> do
      maybeUser <- liftIO $ runSqlPool (selectFirst [UserUsername ==. activeUserUsername activeUser'] []) pool :: Handler (Maybe (Entity User))
      case maybeUser of 
        Nothing -> throwError err404 { errBody = "User Not looged in" }
        Just (Entity userId user) -> do
          let updateFav = movieId : userFav user
          _ <- liftIO $ runSqlPool (update userId [UserFav =. updateFav]) pool
          return "Movie added to favourite list"


getUsers :: ConnectionPool -> Handler [Entity User]
getUsers pool = liftIO (runSqlPool (selectList [] []) pool)

getMovies ::  ConnectionPool -> Handler [Entity Movie]
getMovies pool = liftIO (runSqlPool (selectList [] []) pool)

app :: ConnectionPool -> Application 
app pool = serve api (server pool)

api :: Proxy API
api = Proxy

server :: ConnectionPool -> Server API
server pool = registerUser pool :<|> movieAdd pool :<|> loginUser pool :<|> logoutUser pool :<|> updateMovie pool :<|> favMovie pool :<|> getUsers pool :<|> getMovies pool


{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module DB where
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics
import           Data.Aeson
import           Data.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User 
    username String
    password String 
    email String 
    UniqueUsername username
    fav [MovieId]
    deriving Show Generic

Movie
    moviename String 
    genre String 
    rating Int
    deriving Show Generic

ActiveUser
    username String
    UniqueActiveUsername username  
|]

instance ToJSON User 
instance ToJSON (Entity User) where
  toJSON (Entity userId user) = object ["id" .= userId, "user" .= user]

instance ToJSON Movie 
instance ToJSON (Entity Movie) where
  toJSON (Entity movieId movie) = object ["id" .= movieId, "movie" .= movie]

instance FromJSON User
instance FromJSON Movie