module Bucketchain.SimpleAPI.Auth where

-- | The type for request handlers with authentication.
-- |
-- | example:
-- |
-- | ```purescript
-- | -- This request handler is run with authentication.
-- | getItems :: Auth User -> Proc AppContext (JSON Item)
-- | ```
newtype Auth a = Auth a
