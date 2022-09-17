{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Internet address families.
module System.Posix.Socket.Inet
  ( AF_INET
  , pattern AF_INET
  , AF_INET6
  , pattern AF_INET6
  ) where

import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Network.IP.Addr
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Foreign.C.Types (CSize)
import Foreign.Storable (Storable(..))
import System.Posix.Socket

#include <sys/socket.h>
#include <netinet/ip.h>
#include <netinet/ip6.h>

-- | IPv4 socket family.
data AF_INET deriving Typeable

pattern AF_INET ∷ Proxy AF_INET
pattern AF_INET = Proxy

instance SockAddr Inet4Addr where
  sockAddrMaxSize _ = #{size struct sockaddr_in}
  sockAddrSize    _ = #{size struct sockaddr_in}
  peekSockAddr p sz =
    if sz /= #{size struct sockaddr_in}
      then ioError $ userError $
             "peekSockAddr(Inet4Addr): invalid size " ++ show sz ++
             " (expected " ++ show (#{size struct sockaddr_in} ∷ CSize) ++ ")"
      else InetAddr <$> #{peek struct sockaddr_in, sin_addr} p
                    <*> #{peek struct sockaddr_in, sin_port} p
  pokeSockAddr p (InetAddr addr port) = do
    #{poke struct sockaddr_in, sin_port} p port
    #{poke struct sockaddr_in, sin_addr} p addr

instance SockFamily AF_INET where
  type SockFamilyAddr AF_INET = Inet4Addr
  sockFamilyCode _ = #const AF_INET

-- | IPv6 socket family.
data AF_INET6 deriving Typeable

pattern AF_INET6 ∷ Proxy AF_INET6
pattern AF_INET6 = Proxy

instance SockAddr Inet6Addr where
  sockAddrMaxSize _ = #{size struct sockaddr_in6}
  sockAddrSize    _ = #{size struct sockaddr_in6}
  peekSockAddr p sz =
    if sz /= #{size struct sockaddr_in6}
      then ioError $ userError $
             "peekSockAddr(Inet6Addr): invalid size " ++ show sz ++
             " (expected " ++ show (#{size struct sockaddr_in6} ∷ CSize) ++ ")"
      else InetAddr <$> #{peek struct sockaddr_in6, sin6_addr} p
                    <*> #{peek struct sockaddr_in6, sin6_port} p
  pokeSockAddr p (InetAddr addr port) = do
    #{poke struct sockaddr_in6, sin6_port} p port
    #{poke struct sockaddr_in6, sin6_addr} p addr

instance SockFamily AF_INET6 where
  type SockFamilyAddr AF_INET6 = Inet6Addr
  sockFamilyCode _ = #const AF_INET6

