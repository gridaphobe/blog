{-# LANGUAGE TemplateHaskell #-}

{-

This module defines our application's state type and an alias for its handler
monad.

-}

module Application where

import Data.Lens.Template
import Data.Map (Map)

import Snap.Snaplet

import Post

data App = App
    { _posts :: Map String Post
    }

type AppHandler = Handler App App

makeLens ''App
