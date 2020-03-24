module Types where

import           Import

type Selected a = ReaderT SqlBackend Handler [Entity a]
