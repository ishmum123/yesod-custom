{-# LANGUAGE OverloadedStrings #-}

module Handler.Comment where

import Import

postCommentR :: Handler Value
postCommentR = do
    -- this will try to parse the request body into a json object first
    -- if that fails it will return a 422 status code
    -- then it tries to parse it into the appropriate type, or return a 400 status code if that fails
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    comment <- requireJson :: Handler Comment

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    maybeCurrentUserId <- maybeAuthId
    let comment' = comment { commentUserId = maybeCurrentUserId }

    insertedComment <- runDB $ insertEntity comment'
    returnJson insertedComment