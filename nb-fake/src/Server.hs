module Server where

import Prelude hiding (id)
import Servant
import qualified Data.Map.Strict as M
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Data.String (fromString)

import qualified Network.Wai.Handler.Warp as Warp


import Notebook.Rest
import Notebook.Model

notebookAPI :: Proxy NotebookApi
notebookAPI = Proxy

type Users = M.Map Id User

data State = State { nid2 :: TVar Id
                   , users2 :: TVar Users
                   , nid :: IORef Int
                   , users :: IORef Users }


server :: State -> Server NotebookApi
server state =
  listUsers state
  :<|> addUser state
  :<|> delUser state
  where
    listUsers _ = do
      us <- liftIO $ readIORef $ users state
      return $ sort $ M.elems us

    -- TVars here.. :-)
    addUser _ (NewUser fn sn) = do
      liftIO $ atomically $ change (nid2 state) (users2 state)
      where
        change :: TVar Id -> TVar Users -> STM Id
        change nid2'tv users2'tv = do
          nid2' <- succ <$> readTVar nid2'tv
          modifyTVar' users2'tv $ \m -> M.insert nid2' (User nid2' fn sn) m
          writeTVar nid2'tv nid2'
          return nid2'


    delUser _ id' = do
     liftIO $ atomicModifyIORef' (users state) $ \m ->
       (M.delete id' m, ())
     return NoContent

mkState :: IO State
mkState = do
  r1 <- newIORef 0
  r2 <- newIORef M.empty
  return $ State undefined undefined r1 r2

runServer :: IO ()
runServer = do
  state <- mkState
  Warp.runSettings settings $ serve notebookAPI $ server state
  where
    settings = Warp.setHost (fromString "0.0.0.0") $
               Warp.setPort 8998 $
               Warp.defaultSettings
