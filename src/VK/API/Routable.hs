{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
-- |

module VK.API.Routable where

import           Control.Monad.IO.Class        (MonadIO)


import           Network.API.Builder
import           Network.API.Builder.Queryable


class (Queryable q, Receivable a) => Routable q a | q -> a where
  toRoute :: q -> Route
  toAPI :: (MonadIO m, ErrorReceivable e) => q -> APIT s e m a
  toAPI = runRoute . toRoute
