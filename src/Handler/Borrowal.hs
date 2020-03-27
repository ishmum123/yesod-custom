{-# LANGUAGE OverloadedStrings #-}

module Handler.Borrowal where

import qualified Database.Esqueleto as E (from, isNothing, select, val, where_,
                                          (==.), (^.))
import           Import

postBorrowalsR :: Handler Value
postBorrowalsR = do
  _ <- commonHeaders
  borrowal <- requireCheckJsonBody :: Handler Borrowal
  insertedBorrowal <- runDB $ insertEntity borrowal
  returnJson insertedBorrowal

getBorrowalsR :: Handler Value
getBorrowalsR = do
  _ <- commonHeaders
  borrowals <- runDB $ selectList ([] :: [Filter Borrowal]) []
  returnJson borrowals

getBorrowalR :: Key Borrowal -> Handler Value
getBorrowalR borrowalId = do
  borrowal <- runDB $ selectFirst [BorrowalId ==. borrowalId] []
  returnJson borrowal

getBorrowalsUserR :: Key LibraryUser -> Handler Value
getBorrowalsUserR userId = do
  borrowals <- runDB $ selectList [BorrowalUserId ==. userId] []
  returnJson borrowals

getUnreturnedBorrowalsR :: Key LibraryUser -> Handler Value
getUnreturnedBorrowalsR userId = do
  borrowals <-
    runDB $
    E.select $
    E.from $ \(br, b) -> do
      E.where_ ((br E.^. BorrowalUserId) E.==. E.val userId)
      E.where_ (E.isNothing (br E.^. BorrowalReturnDate))
      E.where_ (b E.^. BookId E.==. br E.^. BorrowalBookId)
      return b
  returnJson borrowals

getBorrowalsBookR :: Key Book -> Handler Value
getBorrowalsBookR bookId = do
  borrowals <- runDB $ selectList [BorrowalBookId ==. bookId] []
  returnJson borrowals

putBorrowalR :: Key Borrowal -> Handler Value
putBorrowalR borrowalId = do
  borrowal <- requireCheckJsonBody :: Handler Borrowal
  _ <- runDB $ replace borrowalId borrowal
  returnJson borrowal

deleteBorrowalR :: Key Borrowal -> Handler Value
deleteBorrowalR borrowalId = do
  _ <- runDB $ delete borrowalId
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

optionsBorrowalR :: Key Borrowal -> Handler Value
optionsBorrowalR _ = do
  _ <- commonOptions
  returnJson ("" :: Text)

optionsBorrowalsR :: Handler Value
optionsBorrowalsR = do
  _ <- commonOptions
  returnJson ("" :: Text)

optionsBorrowalsUserR :: Key LibraryUser -> Handler Value
optionsBorrowalsUserR _ = do
  _ <- commonOptions
  returnJson ("" :: Text)

optionsBorrowalsBookR :: Key Book -> Handler Value
optionsBorrowalsBookR _ = do
  _ <- commonOptions
  returnJson ("" :: Text)

optionsUnreturnedBorrowalsR :: Key LibraryUser -> Handler Value
optionsUnreturnedBorrowalsR _ = do
  _ <- commonOptions
  returnJson ("" :: Text)
