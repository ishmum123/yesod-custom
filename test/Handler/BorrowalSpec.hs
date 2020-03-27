{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.BorrowalSpec
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
    getByUserSpec
    getByBookSpec
    getUnreturnedBooksByUserSpec
    singleDeletionSpec
    singleUpdationSpec
    invalidRequestSpec

creationSpec :: SpecWith (TestApp App)
creationSpec =
  describe "creation request" $
  it "creates a borrowal" $ do
    get HomeR
    statusIs 200
    let userId = LibraryUserKey 1
        bookId = BookKey 1
        issueDate = randomTime
        body = object [("userId" .= userId), ("bookId" .= bookId), ("issueDate" .= randomTime)]
        encoded = encode body
    _ <- runDB $ insertEntity $ LibraryUser "User1" "user@mail.com" "Very Secret Password" randomTime randomTime
    _ <- runDB $ insertEntity $ Book "Introduction..." "Coremen" False Nothing
    request $ do
      setMethod "POST"
      setUrl BorrowalsR
      setRequestBody encoded
      addRequestHeader ("Content-Type", "application/json")
    statusIs 200
    printBody
    borrowals <- runDB $ selectList [BorrowalUserId ==. userId] []
    Entity _id borrowal <-
      case borrowals of
        [ent] -> pure ent
        _     -> error "needed 1 entity"
    assertEq "Should have " borrowal (Borrowal userId bookId issueDate Nothing)

randomTime :: UTCTime
randomTime = read "2012-09-15 00:07:31.874712 UTC"

bulkRetrievalSpec :: SpecWith (TestApp App)
bulkRetrievalSpec =
  describe "bulk retrieval request" $
  it "gets all the borrowals" $ do
    let borrowal1 = Borrowal (LibraryUserKey 1) (BookKey 1) randomTime Nothing
        borrowal2 = Borrowal (LibraryUserKey 1) (BookKey 2) randomTime Nothing
    _ <- runDB $ insertEntity $ LibraryUser "User1" "user@mail.com" "Very Secret Password" randomTime randomTime
    _ <- runDB $ insertEntity $ Book "Introduction..." "Coremen" False Nothing
    _ <- runDB $ insertEntity $ Book "Calculus" "Anton" False Nothing
    _ <- runDB $ insertEntity borrowal1
    _ <- runDB $ insertEntity borrowal2
    get BorrowalsR
    statusIs 200
    printBody
    (borrowals :: [Borrowal]) <- requireJSONResponse
    assertEq "Should have 2 borrowal" 2 $ P.length borrowals
    assertEq "Should be the same borrowal" borrowal1 $ P.head borrowals

createLibraryUser :: SIO (YesodExampleData App) ()
createLibraryUser = do
  _ <- runDB $ insertEntity $ LibraryUser "User1" "user@mail.com" "Very Secret Password" randomTime randomTime
  return ()

createBook :: SIO (YesodExampleData App) ()
createBook = do
  _ <- runDB $ insertEntity $ Book "Calculus" "Anton" False Nothing
  return ()

singleRetrievalSpec :: SpecWith (TestApp App)
singleRetrievalSpec =
  describe "single retrieval request" $
  it "gets borrowal with id" $ do
    let borrowal1 = Borrowal (LibraryUserKey 1) (BookKey 1) randomTime Nothing
        borrowal2 = Borrowal (LibraryUserKey 1) (BookKey 2) randomTime Nothing
    _ <- createBook
    _ <- createBook
    _ <- createLibraryUser
    _ <- runDB $ insertEntity borrowal1
    _ <- runDB $ insertEntity borrowal2
    get $ BorrowalR $ BorrowalKey 2
    statusIs 200
    printBody
    (borrowal :: Borrowal) <- requireJSONResponse
    assertEq "Should be the same borrowal" borrowal2 borrowal

getByUserSpec :: SpecWith (TestApp App)
getByUserSpec =
  describe "retrieval request" $
  it "gets borrowals by user id" $ do
    let borrowal1 = Borrowal (LibraryUserKey 1) (BookKey 1) randomTime Nothing
        borrowal2 = Borrowal (LibraryUserKey 1) (BookKey 2) randomTime Nothing
    _ <- createBook
    _ <- createBook
    _ <- createLibraryUser
    _ <- runDB $ insertEntity borrowal1
    _ <- runDB $ insertEntity borrowal2
    get $ BorrowalsUserR $ LibraryUserKey 1
    statusIs 200
    printBody
    (borrowals :: [Borrowal]) <- requireJSONResponse
    assertEq "Should contain 2 borrowals" 2 $ P.length borrowals

getUnreturnedBooksByUserSpec :: SpecWith (TestApp App)
getUnreturnedBooksByUserSpec =
  describe "retrieval request" $
  it "gets unreturned borrowals by user id" $ do
    let borrowal1 = Borrowal (LibraryUserKey 1) (BookKey 1) randomTime Nothing
        borrowal2 = Borrowal (LibraryUserKey 1) (BookKey 2) randomTime Nothing
    _ <- createBook
    _ <- createBook
    _ <- createLibraryUser
    _ <- runDB $ insertEntity borrowal1
    _ <- runDB $ insertEntity borrowal2
    get $ UnreturnedBorrowalsR $ LibraryUserKey 1
    statusIs 200
    printBody
    (books :: [Book]) <- requireJSONResponse
    assertEq "Should contain 2 books" 2 $ P.length books

getByBookSpec :: SpecWith (TestApp App)
getByBookSpec =
  describe "retrieval request" $
  it "gets borrowals by user id" $ do
    let borrowal1 = Borrowal (LibraryUserKey 1) (BookKey 1) randomTime Nothing
        borrowal2 = Borrowal (LibraryUserKey 1) (BookKey 2) randomTime Nothing
    _ <- createBook
    _ <- createBook
    _ <- createLibraryUser
    _ <- runDB $ insertEntity borrowal1
    _ <- runDB $ insertEntity borrowal2
    get $ BorrowalsBookR $ BookKey 1
    statusIs 200
    printBody
    (borrowals :: [Borrowal]) <- requireJSONResponse
    assertEq "Should contain 1 borrowal" 1 $ P.length borrowals

singleDeletionSpec :: SpecWith (TestApp App)
singleDeletionSpec =
  describe "single deletion request" $
  it "should remove borrowal with id" $ do
    let borrowal1 = Borrowal (LibraryUserKey 1) (BookKey 1) randomTime Nothing
        borrowal2 = Borrowal (LibraryUserKey 1) (BookKey 2) randomTime Nothing
        borrowal3 = Borrowal (LibraryUserKey 1) (BookKey 3) randomTime Nothing
    _ <- createBook
    _ <- createBook
    _ <- createBook
    _ <- createLibraryUser
    _ <- runDB $ insertEntity borrowal1
    _ <- runDB $ insertEntity borrowal2
    _ <- runDB $ insertEntity borrowal3
    performMethod "DELETE" $ BorrowalR $ BorrowalKey 1
    statusIs 200
    printBody
    borrowals <- runDB $ selectList ([] :: [Filter Borrowal]) []
    assertEq "Should decrease count" 2 $ P.length borrowals

singleUpdationSpec :: SpecWith (TestApp App)
singleUpdationSpec =
  describe "single updation request" $
  it "should update borrowal with id" $ do
    let borrowal1 = Borrowal (LibraryUserKey 1) (BookKey 1) randomTime Nothing
        borrowal2 = Borrowal (LibraryUserKey 1) (BookKey 2) randomTime Nothing
        borrowal3 = Borrowal (LibraryUserKey 1) (BookKey 3) randomTime Nothing
        updatedBorrowal = Borrowal (LibraryUserKey 1) (BookKey 4) randomTime Nothing
    _ <- createBook
    _ <- createBook
    _ <- createBook
    _ <- createBook
    _ <- createLibraryUser
    _ <- runDB $ insertEntity borrowal1
    _ <- runDB $ insertEntity borrowal2
    _ <- runDB $ insertEntity borrowal3
    request $ do
      setMethod "PUT"
      setUrl $ BorrowalR $ BorrowalKey 2
      setRequestBody $ encode updatedBorrowal
      addRequestHeader ("Content-Type", "application/json")
    statusIs 200
    printBody
    cnt <- runDB $ count ([] :: [Filter Borrowal])
    (borrowal :: Borrowal) <- requireJSONResponse
    assertNotEq "Should not be the same borrowal" borrowal2 borrowal
    assertEq "Should not insert new row" 3 cnt

invalidRequestSpec :: SpecWith (TestApp App)
invalidRequestSpec =
  describe "invalid request" $
  it "400s when the JSON body is invalid" $ do
    get HomeR
    let body = object ["foo" .= ("My userId" :: Value)]
    request $ do
      setMethod "POST"
      setUrl BorrowalsR
      setRequestBody $ encode body
      addRequestHeader ("Content-Type", "application/json")
    statusIs 400
