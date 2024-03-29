{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- | Local address family.
module System.Posix.Socket.Local
  ( LocalAddr(..)
  , aLocalAddr
  , AF_LOCAL
  , pattern AF_LOCAL
  ) where

import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import System.Posix.Socket

#include <sys/socket.h>
#include <sys/un.h>

-- | Local socket family.
data AF_LOCAL deriving Typeable

pattern AF_LOCAL ∷ Proxy AF_LOCAL
pattern AF_LOCAL = Proxy

-- | Local socket address.
data LocalAddr = LocalAddr ByteString
               | NoLocalAddr
               deriving (Typeable, Eq, Ord, Show)

-- | 'LocalAddr' proxy value.
aLocalAddr ∷ Proxy LocalAddr
aLocalAddr = Proxy

instance SockAddr LocalAddr where
  sockAddrMaxSize _ = #{size struct sockaddr_un}
  sockAddrSize (LocalAddr path) = #{offset struct sockaddr_un, sun_path}
                                + BS.length path + 1
  sockAddrSize NoLocalAddr = #{size sa_family_t}
  peekSockAddr p sz = do
    let offset = #{offset struct sockaddr_un, sun_path}
    if sz < offset + 1
      then return NoLocalAddr
      else LocalAddr <$> BS.packCStringLen
                           (castPtr $ plusPtr p offset, sz - offset - 1)
  pokeSockAddr p (LocalAddr path) = do
    let offset = #{offset struct sockaddr_un, sun_path}
    BS.unsafeUseAsCStringLen path $ \(pBs, len) → do
      BS.memcpy (castPtr $ plusPtr p offset) (castPtr pBs) (fromIntegral len)
      poke (castPtr $ plusPtr p $ offset + len) (0 ∷ Word8)
  pokeSockAddr _ NoLocalAddr = return ()

instance SockFamily AF_LOCAL where
  type SockFamilyAddr AF_LOCAL = LocalAddr
  sockFamilyCode _ = #const AF_LOCAL

