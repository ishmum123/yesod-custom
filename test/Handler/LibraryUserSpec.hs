{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.LibraryUserSpec
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
    singleUpdationSpec
    singleDeletionSpec
    invalidRequestSpec

creationSpec :: SpecWith (TestApp App)
creationSpec =
  describe "creation request" $
  it "creates a libraryUser" $ do
    get HomeR
    statusIs 200
    let name = "My name" :: Text
        email = "someone@somemail.com" :: Text
        password = "Secret Key"
        body =
          object
            [ ("name" .= name)
            , ("email" .= email)
            , ("password" .= password)
            , ("birthDate" .= randomTime)
            , ("registrationDate" .= randomTime)
            ]
        encoded = encode body
    request $ do
      setMethod "POST"
      setUrl LibraryUsersR
      setRequestBody encoded
      addRequestHeader ("Content-Type", "application/json")
    statusIs 200
    printBody
    libraryUsers <- runDB $ selectList [LibraryUserName ==. name] []
    Entity _id libraryUser <-
      case libraryUsers of
        [ent] -> pure ent
        _     -> error "needed 1 entity"
    assertEq "Should have " libraryUser (LibraryUser name email password randomTime randomTime)

randomTime :: UTCTime
randomTime = read "2012-09-15 00:07:31.874712 UTC"

bulkRetrievalSpec :: SpecWith (TestApp App)
bulkRetrievalSpec =
  describe "bulk retrieval request" $
  it "gets all the libraryUsers" $ do
    let libraryUser1 = LibraryUser "User1" "user@mail.com" "Very Secret Password" randomTime randomTime
        libraryUser2 = LibraryUser "Calculus" "Anton" "Very Secret Password" randomTime randomTime
    _ <- runDB $ insertEntity libraryUser1
    _ <- runDB $ insertEntity libraryUser2
    get LibraryUsersR
    statusIs 200
    printBody
    (libraryUsers :: [LibraryUser]) <- requireJSONResponse
    assertEq "Should have 2 libraryUser" 2 $ P.length libraryUsers
    assertEq "Should be the same libraryUser" libraryUser1 $ P.head libraryUsers

singleRetrievalSpec :: SpecWith (TestApp App)
singleRetrievalSpec =
  describe "single retrieval request" $
  it "gets libraryUser with id" $ do
    let libraryUser1 = LibraryUser "User1" "user@mail.com" "Very Secret Password" randomTime randomTime
        libraryUser2 = LibraryUser "Calculus" "Anton" "Very Secret Password" randomTime randomTime
    _ <- runDB $ insertEntity libraryUser1
    _ <- runDB $ insertEntity libraryUser2
    get $ LibraryUserR $ LibraryUserKey 2
    statusIs 200
    printBody
    (libraryUser :: LibraryUser) <- requireJSONResponse
    assertEq "Should be the same libraryUser" libraryUser2 libraryUser

singleDeletionSpec :: SpecWith (TestApp App)
singleDeletionSpec =
  describe "single deletion request" $
  it "should remove libraryUser with id" $ do
    let libraryUser1 = LibraryUser "User1" "user@mail.com" "Very Secret Password" randomTime randomTime
        libraryUser2 = LibraryUser "Calculus" "Anton" "Very Secret Password" randomTime randomTime
        libraryUser3 = LibraryUser "Pattern...." "Bishop" "Very Secret Password" randomTime randomTime
    _ <- runDB $ insertEntity libraryUser1
    _ <- runDB $ insertEntity libraryUser2
    _ <- runDB $ insertEntity libraryUser3
    performMethod "DELETE" $ LibraryUserR $ LibraryUserKey 1
    statusIs 200
    printBody
    libraryUsers <- runDB $ selectList ([] :: [Filter LibraryUser]) []
    assertEq "Should decrease count" 2 $ P.length libraryUsers

singleUpdationSpec :: SpecWith (TestApp App)
singleUpdationSpec =
  describe "single updation request" $
  it "should update libraryUser with id" $ do
    let libraryUser1 = LibraryUser "User1" "user@mail.com" "Very Secret Password" randomTime randomTime
        libraryUser2 = LibraryUser "Calculus" "Anton" "Very Secret Password" randomTime randomTime
        libraryUser3 = LibraryUser "Pattern...." "Bishop" "Very Secret Password" randomTime randomTime
        updatedLibraryUser = LibraryUser "Some" "Thing" "Very Secret Password" randomTime randomTime
    _ <- runDB $ insertEntity libraryUser1
    _ <- runDB $ insertEntity libraryUser2
    _ <- runDB $ insertEntity libraryUser3
    request $ do
      setMethod "PUT"
      setUrl $ LibraryUserR $ LibraryUserKey 2
      setRequestBody $ encode updatedLibraryUser
      addRequestHeader ("Content-Type", "application/json")
    statusIs 200
    printBody
    cnt <- runDB $ count ([] :: [Filter LibraryUser])
    (libraryUser :: LibraryUser) <- requireJSONResponse
    assertNotEq "Should not be the same libraryUser" libraryUser2 libraryUser
    assertEq "Should not insert new row" 3 cnt

invalidRequestSpec :: SpecWith (TestApp App)
invalidRequestSpec =
  describe "invalid request" $
  it "400s when the JSON body is invalid" $ do
    get HomeR
    let body = object ["foo" .= ("My name" :: Value)]
    request $ do
      setMethod "POST"
      setUrl LibraryUsersR
      setRequestBody $ encode body
      addRequestHeader ("Content-Type", "application/json")
    statusIs 400
