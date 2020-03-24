module Handler.Book where

import           Import

postBookR :: Handler Value
postBookR = do
  book <- requireCheckJsonBody :: Handler Book
  insertedBook <- runDB $ insertEntity book
  returnJson insertedBook

getBookR :: Handler Value
getBookR = do
  books <- runDB $ selectList ([] :: [Filter Book]) []
  returnJson books

getBookSingleR :: Key Book -> Handler Value
getBookSingleR bookId = do
  book <- runDB $ selectFirst [BookId ==. bookId] []
  returnJson book

putBookSingleR :: Key Book -> Handler Value
putBookSingleR _ = do
  let x = selectList ([] :: [Filter Book]) []
  books <- runDB x
  returnJson books

deleteBookSingleR :: Key Book -> Handler Value
deleteBookSingleR bookId = do
  _ <- runDB $ delete bookId
  returnJson ""
