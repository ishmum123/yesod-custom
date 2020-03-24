module Handler.Book where

import           Import

postBookR :: Handler Value
postBookR = do
  book <- requireCheckJsonBody :: Handler Book
  insertedBook <- runDB $ insertEntity book
  returnJson insertedBook

getBookR :: Handler Value
getBookR = do
  let x = selectList ([] :: [Filter Book]) []
  books <- runDB x
  returnJson books
