{-# LANGUAGE DeriveFunctor #-}

module Jatek.Message where

data ServerMessage v =
  ViewChanged v | NeedAction v | AcceptAction | RejectAction | Final v
  deriving (Eq, Ord, Show, Functor)

data ClientMessage t = TryAction t | Ok deriving (Eq, Ord, Show, Functor)

class ServerSendable f where
  toServerMessage :: f v -> ServerMessage v

class ClientSendable f where
  toClientMessage :: f t -> ClientMessage t
