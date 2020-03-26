{-# LANGUAGE OverloadedStrings #-}

module Handler.Book where

import           Import

postBookR :: Handler Value
postBookR = do
  _ <- commonHeaders
  book <- requireCheckJsonBody :: Handler Book
  insertedBook <- runDB $ insertEntity book
  returnJson insertedBook

getBookR :: Handler Value
getBookR = do
  _ <- commonHeaders
  books <- runDB $ selectList ([] :: [Filter Book]) []
  returnJson books

getBookSingleR :: Key Book -> Handler Value
getBookSingleR bookId = do
  book <- runDB $ selectFirst [BookId ==. bookId] []
  returnJson book

putBookSingleR :: Key Book -> Handler Value
putBookSingleR bookId = do
  book <- requireCheckJsonBody :: Handler Book
  _ <- runDB $ replace bookId book
  returnJson book

deleteBookSingleR :: Key Book -> Handler Value
deleteBookSingleR bookId = do
  _ <- runDB $ delete bookId
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

optionsBookSingleR :: Key Book -> Handler Value
optionsBookSingleR _ = do
  _ <- commonOptions
  returnJson ("" :: Text)

optionsBookR :: Handler Value
optionsBookR = do
  _ <- commonOptions
  returnJson ("" :: Text)
