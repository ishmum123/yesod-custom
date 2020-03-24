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
    retrievalSpec
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
    books <- runDB $ selectList [BookName ==. name] []
    Entity _id book <-
      case books of
        [ent] -> pure ent
        _     -> error "needed 1 entity"
    assertEq "Should have " book (Book name author isPirated Nothing)

retrievalSpec :: SpecWith (TestApp App)
retrievalSpec =
  describe "retrieval request" $
  it "gets all the books" $ do
    let book1 = Book "Introduction..." "Coremen" False Nothing
        book2 = Book "Calculus" "Anton" False Nothing
    _ <- runDB $ insertEntity book1
    _ <- runDB $ insertEntity book2
    get BookR
    statusIs 200
    (books :: [Book]) <- requireJSONResponse
    assertEq "Should have 2 book" 2 $ P.length books
    assertEq "Should be the same book" book1 $ P.head books

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
