{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.BookSpec
  ( spec
  ) where

import           Data.Aeson
import           Prelude    as P
import           TestImport

spec :: Spec
spec =
  withApp $ do
    creationSpec
    bulkRetrievalSpec
    singleRetrievalSpec
    singleDeletionSpec
    invalidRequestSpec

creationSpec :: SpecWith (TestApp App)
creationSpec =
  describe "creation request" $
  it "creates a book" $ do
    get HomeR
    statusIs 200
    let name = "My name" :: Text
        author = "Some Author" :: Text
        isPirated = False
        body = object [("name" .= name), ("author" .= author), ("isPirated" .= isPirated)]
        encoded = encode body
    request $ do
      setMethod "POST"
      setUrl BookR
      setRequestBody encoded
      addRequestHeader ("Content-Type", "application/json")
    statusIs 200
    printBody
    books <- runDB $ selectList [BookName ==. name] []
    Entity _id book <-
      case books of
        [ent] -> pure ent
        _     -> error "needed 1 entity"
    assertEq "Should have " book (Book name author isPirated Nothing)

bulkRetrievalSpec :: SpecWith (TestApp App)
bulkRetrievalSpec =
  describe "bulk retrieval request" $
  it "gets all the books" $ do
    let book1 = Book "Introduction..." "Coremen" False Nothing
        book2 = Book "Calculus" "Anton" False Nothing
    _ <- runDB $ insertEntity book1
    _ <- runDB $ insertEntity book2
    get BookR
    statusIs 200
    printBody
    (books :: [Book]) <- requireJSONResponse
    assertEq "Should have 2 book" 2 $ P.length books
    assertEq "Should be the same book" book1 $ P.head books

singleRetrievalSpec :: SpecWith (TestApp App)
singleRetrievalSpec =
  describe "single retrieval request" $
  it "gets book with id" $ do
    let book1 = Book "Introduction..." "Coremen" False Nothing
        book2 = Book "Calculus" "Anton" False Nothing
    _ <- runDB $ insertEntity book1
    _ <- runDB $ insertEntity book2
    get $ BookSingleR $ BookKey 2
    statusIs 200
    printBody
    (book :: Book) <- requireJSONResponse
    assertEq "Should be the same book" book2 book

singleDeletionSpec :: SpecWith (TestApp App)
singleDeletionSpec =
  describe "single deletion request" $
  it "should remove book with id" $ do
    let book1 = Book "Introduction..." "Coremen" False Nothing
        book2 = Book "Calculus" "Anton" False Nothing
        book3 = Book "Pattern...." "Bishop" False Nothing
    _ <- runDB $ insertEntity book1
    _ <- runDB $ insertEntity book2
    _ <- runDB $ insertEntity book3
    performMethod "DELETE" $ BookSingleR $ BookKey 1
    statusIs 200
    printBody
    books <- runDB $ selectList ([] :: [Filter Book]) []
    assertEq "Should decrease count" 2 $ P.length books

-- TODO: PUT REQUEST

invalidRequestSpec :: SpecWith (TestApp App)
invalidRequestSpec =
  describe "invalid request" $
  it "400s when the JSON body is invalid" $ do
    get HomeR
    let body = object ["foo" .= ("My name" :: Value)]
    request $ do
      setMethod "POST"
      setUrl BookR
      setRequestBody $ encode body
      addRequestHeader ("Content-Type", "application/json")
    statusIs 400
