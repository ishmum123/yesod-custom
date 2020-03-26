{-# LANGUAGE OverloadedStrings #-}

module Handler.LibraryUser where

import           Import

postLibraryUsersR :: Handler Value
postLibraryUsersR = do
  _ <- commonHeaders
  libraryUser <- requireCheckJsonBody :: Handler LibraryUser
  insertedLibraryUser <- runDB $ insertEntity libraryUser
  returnJson insertedLibraryUser

getLibraryUsersR :: Handler Value
getLibraryUsersR = do
  _ <- commonHeaders
  libraryUsers <- runDB $ selectList ([] :: [Filter LibraryUser]) []
  returnJson libraryUsers

getLibraryUserR :: Key LibraryUser -> Handler Value
getLibraryUserR libraryUserId = do
  libraryUser <- runDB $ selectFirst [LibraryUserId ==. libraryUserId] []
  returnJson libraryUser

putLibraryUserR :: Key LibraryUser -> Handler Value
putLibraryUserR libraryUserId = do
  libraryUser <- requireCheckJsonBody :: Handler LibraryUser
  _ <- runDB $ replace libraryUserId libraryUser
  returnJson libraryUser

deleteLibraryUserR :: Key LibraryUser -> Handler Value
deleteLibraryUserR libraryUserId = do
  _ <- runDB $ delete libraryUserId
  returnJson ("" :: Text)

commonHeaders :: Handler ()
commonHeaders = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader
    "Access-Control-Allow-Headers"
    "Origin, X-Requested-With, Content-Type, Accept, Authorization, Access-Control-Allow-Origin, Token"
  addHeader "Accept" "application/json, multipart/form-data"

commonOptions :: Handler ()
commonOptions = do
  _ <- commonHeaders
  addHeader "Content-Type" "application/json"
  addHeader "Allow" "GET POST"
  addHeader "Access-Control-Allow-Methods" "GET POSTs"

optionsLibraryUserR :: Key LibraryUser -> Handler Value
optionsLibraryUserR _ = do
  _ <- commonOptions
  returnJson ("" :: Text)

optionsLibraryUsersR :: Handler Value
optionsLibraryUsersR = do
  _ <- commonOptions
  returnJson ("" :: Text)
