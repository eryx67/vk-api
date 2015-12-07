-- |

module Network.API.Builder.Queryable where

import           Network.API.Builder.Routes (URLParam)

class Queryable a where
  toURLParams :: a -> [URLParam]
