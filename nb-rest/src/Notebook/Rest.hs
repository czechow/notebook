{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Notebook.Rest where

import Prelude hiding (id)
import Servant.API

import Notebook.Model

-- FIXME: perhaps name without 'Notebook'
type NotebookApi =
       "rest" :> Get '[JSON] [User]
  :<|> "rest" :> ReqBody '[JSON] NewUser :> PostCreated '[JSON] User
  :<|> "rest" :> Capture "id" Id :> DeleteNoContent '[JSON] NoContent
