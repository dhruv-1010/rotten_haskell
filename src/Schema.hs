

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
{-# LANGUAGE  DeriveAnyClass #-}
module Schema where

import  Data.Aeson
import Database.Persist.Postgresql
import Database.Persist.TH
import GHC.Generics
-- import Control.Monad.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Users
    username  String     
    password  String
    firstName String
    lastName  String
    favMovies [MoviesId]
    deriving Show Generic
Movies 
    movieId String
    movieTitle String
    movieRating Int 
    movieGenre String 
    deriving Show Generic
Activeuser 
    username String
    deriving Show Generic 
|]
instance FromJSON Users
instance ToJSON Users

instance FromJSON Movies 
instance ToJSON Movies 

instance ToJSON (Entity Movies) where
    toJSON (Entity movieDBId movie) =
        object [ "id" .= movieDBId
                ,"movieId" .= moviesMovieId movie
               , "title" .= moviesMovieTitle movie
               , "rating" .= moviesMovieRating movie
               , "genre" .= moviesMovieGenre movie
               ]


instance ToJSON (Entity Users) where
    toJSON (Entity userId user) =
        object [ "id" .= userId
               , "username" .= usersUsername user
               , "firstName" .= usersFirstName user
               , "lastName" .= usersLastName user
               ]


data ReqUser = ReqUser
  { loginUsername :: String
  , loginPassword :: String 
  } deriving (Eq, Show, Generic, FromJSON)
