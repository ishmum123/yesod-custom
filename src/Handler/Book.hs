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
putBookSingleR bookId = do
  book <- requireCheckJsonBody :: Handler Book
  _ <- runDB $ replace bookId book
  returnJson book

deleteBookSingleR :: Key Book -> Handler Value
deleteBookSingleR bookId = do
  _ <- runDB $ delete bookId
  returnJson ""
