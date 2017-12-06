{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- | POSIX sockets.
module System.Posix.Socket
  ( Socket
  , withSocketFd
  , unsafeSocketFd
  , unsafeSocketFromFd
  , SockFamily(..)
  , SockAddr(..)
  , SockType(.., SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_RDM, SOCK_SEQPACKET)
  , SockProto(..)
  , defaultSockProto
  , SockOpt(..)
  , SO_ERROR(..)
  , SO_KEEPALIVE(..)
  , SO_REUSEADDR(..)
  , SockOps
  , sendSockOp
  , recvSockOp
  , MsgFlags
  , peekMsgFlag
  , truncMsgFlag
  , oobMsgFlag
  , dontRouteMsgFlag

  , socket
  , getSockOpt
  , setSockOpt
  , bind
  , connect
  , tryConnect
  , listen
  , accept
  , tryAccept
  , getLocalAddr
  , getRemoteAddr
  , hasOobData
  , recvBufs
  , recvBuf
  , recv'
  , recv
  , recvBufsFrom
  , recvBufFrom
  , recvFrom'
  , recvFrom
  , sendBufs
  , sendMany'
  , sendMany
  , sendBuf
  , send'
  , send
  , sendBufsTo
  , sendManyTo'
  , sendManyTo
  , sendBufTo
  , sendTo'
  , sendTo
  , shutdown
  , close
  ) where

import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Data.Word
import Data.Bits ((.|.))
import Data.Default.Class
import Data.List (partition)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Flags (Flags(..), (.>=.))
import Data.Flags.TH (bitmaskWrapper)
import Data.Foldable (forM_)
import Control.Applicative ((<$>))
import Control.Monad (void, when, foldM)
import Control.Monad.Base
import Control.Exception (throwIO)
import Control.Concurrent (threadWaitRead, threadWaitWrite)
import Control.Concurrent.MVar
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytesAligned)
import Foreign.Marshal.Utils (with)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CInt(..), CSize(..))
import System.Posix.Types (CSsize(..))
#else
import Foreign.C.Types (CInt, CSize)
import System.Posix.Types (CSsize)
#endif
import Foreign.C.Error (Errno(..), eOK, eINVAL, eMSGSIZE, eINPROGRESS, eAGAIN,
                        eWOULDBLOCK, getErrno, errnoToIOError,
                        throwErrno, throwErrnoIfMinus1, throwErrnoIfMinus1_)
import System.IO.Error (eofErrorType, mkIOError)
import System.Posix.Types (Fd(..))
import System.Posix.IO (closeFd)
import GHC.Conc (closeFdWith)

#ifdef __linux__
# include <linux/version.h>
# if LINUX_VERSION_CODE >= KERNEL_VERSION (2, 6, 27)
#   define HAVE_SOCKET_WITH_FLAGS
# endif
# if LINUX_VERSION_CODE >= KERNEL_VERSION (2, 6, 28)
#   define HAVE_ACCEPT_WITH_FLAGS
# endif
#endif

#if !defined (HAVE_SOCKET_WITH_FLAGS) || !defined(HAVE_ACCEPT_WITH_FLAGS)
import System.Posix.Internals (setNonBlockingFD)
#endif

#include <sys/socket.h>
#include <sys/uio.h>
#include <posix-socket.macros.h>

-- | Socket of a particular family.
newtype Socket f = Socket (MVar Fd) deriving (Typeable, Eq)

-- | Lock the socket and pass the underlying file descriptor to the given
--   action.
withSocketFd ∷ MonadBase IO μ ⇒ Socket f → (Fd → IO α) → μ α
withSocketFd (Socket v) f = liftBase $ withMVar v f
{-# INLINE withSocketFd #-}

-- | Get the underlying file descriptor.
unsafeSocketFd ∷ MonadBase IO μ ⇒ Socket f → μ Fd
unsafeSocketFd (Socket v) = liftBase $ readMVar v
{-# INLINE unsafeSocketFd #-}

-- | Use file descriptor as a socket.
unsafeSocketFromFd ∷ MonadBase IO μ ⇒ Fd → μ (Socket f)
unsafeSocketFromFd = liftBase . fmap Socket . newMVar
{-# INLINE unsafeSocketFromFd #-}

-- | Socket address.
class SockAddr a where
  -- | Maximum size of a socket address.
  sockAddrMaxSize ∷ Proxy a → Int
  -- | Size of a particular socket address.
  sockAddrSize    ∷ a → Int
  -- | Read socket address from a memory buffer.
  peekSockAddr    ∷ Ptr a -- ^ Buffer
                  → Int   -- ^ Buffer size
                  → IO a
  -- | Write socket address to a memory buffer.
  pokeSockAddr    ∷ Ptr a -- ^ Buffer of sufficient size
                  → a     -- ^ The address to poke
                  → IO ()

-- | Socket family.
class SockAddr (SockFamilyAddr f) ⇒ SockFamily f where
  type SockFamilyAddr f
  -- | Socket family code.
  sockFamilyCode ∷ f → CInt

-- | Socket type.
newtype SockType = SockType CInt deriving (Typeable, Eq, Ord, Show, Storable)

-- | See /socket(2)/.
pattern SOCK_STREAM ∷ SockType
pattern SOCK_STREAM = SockType #const SOCK_STREAM

-- | See /socket(2)/.
pattern SOCK_DGRAM ∷ SockType
pattern SOCK_DGRAM = SockType #const SOCK_DGRAM

-- | See /socket(2)/.
pattern SOCK_RAW ∷ SockType
pattern SOCK_RAW = SockType #const SOCK_RAW

-- | See /socket(2)/.
pattern SOCK_RDM ∷ SockType
pattern SOCK_RDM = SockType #const SOCK_RDM

-- | See /socket(2)/.
pattern SOCK_SEQPACKET ∷ SockType
pattern SOCK_SEQPACKET = SockType #const SOCK_SEQPACKET

-- | Socket protocol.
newtype SockProto = SockProto CInt deriving (Typeable, Eq, Ord, Show, Storable)

-- | Default socket protocol (corresponds to @0@).
defaultSockProto ∷ SockProto
defaultSockProto = SockProto 0

instance Default SockProto where
  def = defaultSockProto

-- | Socket option.
class Storable (SockOptRaw o) ⇒ SockOpt o where
  -- | Option value type
  type SockOptValue o
  -- | FFI-level option value type
  type SockOptRaw o
  -- | Whether option is readable
  type SockOptReadable o ∷ Bool
  -- | Whether option is writable
  type SockOptWritable o ∷ Bool
  -- | Convert to FFI-level value
  sockOptRaw   ∷ o → SockOptValue o → SockOptRaw o
  -- | Convert from FFI-level value
  sockOptValue ∷ o → SockOptRaw o → SockOptValue o
  -- | Option protocol level
  sockOptLevel ∷ o → CInt
  -- | Option code
  sockOptCode  ∷ o → CInt

data SO_ERROR = SO_ERROR deriving (Typeable, Eq, Show)

instance SockOpt SO_ERROR where
  type SockOptValue    SO_ERROR = Errno
  type SockOptRaw      SO_ERROR = CInt
  type SockOptReadable SO_ERROR = 'True
  type SockOptWritable SO_ERROR = 'False
  sockOptRaw   _ (Errno e) = e
  sockOptValue _ = Errno
  sockOptLevel _ = #const SOL_SOCKET
  sockOptCode  _ = #const SO_ERROR

data SO_KEEPALIVE = SO_KEEPALIVE deriving (Typeable, Eq, Show)

instance SockOpt SO_KEEPALIVE where
  type SockOptValue    SO_KEEPALIVE = Bool
  type SockOptRaw      SO_KEEPALIVE = CInt
  type SockOptReadable SO_KEEPALIVE = 'True
  type SockOptWritable SO_KEEPALIVE = 'True
  sockOptRaw _ False = 0
  sockOptRaw _ True  = 1
  sockOptValue _ = (/= 0)
  sockOptLevel _ = #const SOL_SOCKET
  sockOptCode  _ = #const SO_KEEPALIVE

data SO_REUSEADDR = SO_REUSEADDR deriving (Typeable, Eq, Show)

instance SockOpt SO_REUSEADDR where
  type SockOptValue    SO_REUSEADDR = Bool
  type SockOptRaw      SO_REUSEADDR = CInt
  type SockOptReadable SO_REUSEADDR = 'True
  type SockOptWritable SO_REUSEADDR = 'True
  sockOptRaw _ False = 0
  sockOptRaw _ True  = 1
  sockOptValue _ = (/= 0)
  sockOptLevel _ = #const SOL_SOCKET
  sockOptCode  _ = #const SO_REUSEADDR

-- | Socket operations. Used by 'shutdown'.
$(bitmaskWrapper "SockOps" ''Int []
    [("sendSockOp", 1),
     ("recvSockOp", 2)])

-- | Message flags.
newtype MsgFlags = MsgFlags CInt deriving (Typeable, Eq, Show, Storable, Flags)

-- | See /MSG_PEEK/.
peekMsgFlag ∷ MsgFlags
peekMsgFlag = MsgFlags #const MSG_PEEK

-- | See /MSG_TRUNC/.
truncMsgFlag ∷ MsgFlags
truncMsgFlag = MsgFlags #const MSG_TRUNC

-- | See /MSG_OOB/.
oobMsgFlag ∷ MsgFlags
oobMsgFlag = MsgFlags #const MSG_OOB

-- | See /MSG_DONTROUTE/.
dontRouteMsgFlag ∷ MsgFlags
dontRouteMsgFlag = MsgFlags #const MSG_DONTROUTE

allocaMaxAddr ∷ SockAddr a ⇒ Proxy a → (Ptr a → #{itype socklen_t} → IO α) → IO α
allocaMaxAddr addrProxy f =
    allocaBytesAligned size #{alignment struct sockaddr} $
      (`f` (fromIntegral size))
  where size = sockAddrMaxSize addrProxy

allocaAddr ∷ SockAddr a ⇒ a → (Ptr a → #{itype socklen_t} → IO α) → IO α
allocaAddr addr f =
    allocaBytesAligned size #{alignment struct sockaddr} $
      (`f` (fromIntegral size))
  where size = sockAddrSize addr

peekAddrOfSize ∷ SockFamily f
               ⇒ f → Ptr (SockFamilyAddr f) → Ptr #{itype socklen_t}
               → IO (SockFamilyAddr f)
peekAddrOfSize fam p pSize = do
  outSize ← fromIntegral <$> peek pSize
  famCode ∷ #{itype sa_family_t} ← #{peek struct sockaddr, sa_family} p
  when (fromIntegral famCode /= sockFamilyCode fam) $
    ioError $ userError "Invalid socket address family"
  peekSockAddr p outSize

withAddr ∷ SockFamily f
         ⇒ f
         → SockFamilyAddr f
         → (Ptr (SockFamilyAddr f) → #{itype socklen_t} → IO α)
         → IO α
withAddr fam addr f =
    allocaAddr addr $ \p size → do
      pokeSockAddr p addr
      #{poke struct sockaddr, sa_family} p famCode
      f p size
  where famCode ∷ #{itype sa_family_t}
        famCode = fromIntegral $ sockFamilyCode fam

-- | Create a socket. The underlying file descriptor is non-blocking. All
-- blocking operations are done via the GHC event manager. See /socket(2)/.
socket ∷ (SockFamily f, MonadBase IO μ)
       ⇒ f → SockType → SockProto → μ (Socket f)
socket f (SockType t) p = liftBase $ do
  fd ← throwErrnoIfMinus1 "socket" $
         c_socket (sockFamilyCode f)
#ifdef HAVE_SOCKET_WITH_FLAGS
           (t .|. #{const SOCK_NONBLOCK}) p
#else
           t p
  onException (setNonBlockingFD fd True) (closeFd fd)
#endif
  fmap Socket $ newMVar $ Fd fd

getFdOpt ∷ ∀ o . (SockOpt o, SockOptReadable o ~ 'True)
         ⇒ Fd → o → IO (SockOptValue o)
getFdOpt fd o =
  alloca $ \p →
    with (fromIntegral $ sizeOf (undefined ∷ SockOptRaw o)) $ \pSize → do
      throwErrnoIfMinus1_ "getSockOpt" $
        c_getsockopt fd (sockOptLevel o) (sockOptCode o) p pSize
      sockOptValue o <$> peek p

-- | Get socket option value. See /getsockopt(2)/.
getSockOpt ∷ (SockOpt o, SockOptReadable o ~ 'True, MonadBase IO μ)
           ⇒ Socket f → o → μ (SockOptValue o)
getSockOpt s o = withSocketFd s $ \fd → getFdOpt fd o

-- | Set socket option value. See /setsockopt(2)/.
setSockOpt ∷ (SockOpt o, SockOptWritable o ~ 'True, MonadBase IO μ)
           ⇒ Socket f → o → SockOptValue o → μ ()
setSockOpt s o v = withSocketFd s $ \fd →
    with raw $ \p →
      throwErrnoIfMinus1_ "setSockOpt" $
        c_setsockopt fd (sockOptLevel o) (sockOptCode o) p $
          fromIntegral (sizeOf raw)
  where raw = sockOptRaw o v

-- | Bind socket to the specified address. See /bind(2)/.
bind ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
     ⇒ Socket f → SockFamilyAddr f → μ ()
bind s addr = withSocketFd s $ \fd →
  withAddr (undefined ∷ f) addr $ \p size →
    throwErrnoIfMinus1_ "bind" $ c_bind fd p $ fromIntegral size

-- | Connect socket to the specified address. This operation blocks.
-- See /connect(2)/.
connect ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
        ⇒ Socket f → SockFamilyAddr f → μ ()
connect s addr = withSocketFd s $ \fd →
    withAddr (undefined ∷ f) addr $ \p size →
      doConnect fd p $ fromIntegral size
  where doConnect fd p size = do
          r ← c_connect fd p size
          if r == -1 then do
            errno ← getErrno
            case errno of
              e | e == eINPROGRESS → do
                threadWaitWrite fd
                errno' ← getFdOpt fd SO_ERROR
                when (errno' /= eOK) $
                  throwIO $ errnoToIOError "connect" errno' Nothing Nothing
              _ → throwErrno "connect"
          else
            return ()

-- | Try to connect socket without blocking. On success 'True' is returned.
-- If the connection did not succeed immediately, 'False' is returned.
-- See /connect(2)/.
tryConnect ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
           ⇒ Socket f → SockFamilyAddr f → μ Bool
tryConnect s addr = withSocketFd s $ \fd →
  withAddr (undefined ∷ f) addr $ \p size → do
    r ← c_connect fd p $ fromIntegral size
    if r == -1
      then do
        errno ← getErrno
        if errno == eINPROGRESS
          then return False
          else throwErrno "connect"
      else
        return True

-- | Listen for connections on the given socket. See /listen(2)/.
listen ∷ MonadBase IO μ ⇒ Socket f → Int → μ ()
listen s backlog = withSocketFd s $ \fd →
  throwErrnoIfMinus1_ "listen" $ c_listen fd $ fromIntegral backlog

-- | Accept a connection on the given socket. This operation blocks.
-- See /accept(2)/.
accept ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
       ⇒ Socket f → μ (Socket f, SockFamilyAddr f)
accept s = withSocketFd s $ \fd →
    allocaMaxAddr (Proxy ∷ Proxy (SockFamilyAddr f)) $ \p size →
      with size $ \pSize → doAccept fd p pSize
  where doAccept fd p pSize = do
          cfd ← c_accept fd p pSize
#ifdef HAVE_ACCEPT_WITH_FLAGS
                  #{const SOCK_NONBLOCK}
#endif
          if cfd == -1 then do
            errno ← getErrno
            case errno of
              e | e == eAGAIN || e == eWOULDBLOCK → do
                threadWaitRead fd
                doAccept fd p pSize
              _ → throwErrno "accept"
          else do
            addr ← peekAddrOfSize (undefined ∷ f) p pSize
            let accFd = Fd cfd
#ifndef HAVE_ACCEPT_WITH_FLAGS
            onException (setNonBlockingFD accFd True) (closeFd accFd)
#endif
            (, addr) <$> unsafeSocketFromFd accFd

-- | Try to accept a connection on the given socket without blocking.
-- On success the accepted socket and the peer address are returned.
-- See /accept(2)/.
tryAccept ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
          ⇒ Socket f → μ (Maybe (Socket f, SockFamilyAddr f))
tryAccept s = withSocketFd s $ \fd →
    allocaMaxAddr (Proxy ∷ Proxy (SockFamilyAddr f)) $ \p size →
      with size $ \pSize → do
        cfd ← c_accept fd p pSize
#ifdef HAVE_ACCEPT_WITH_FLAGS
                #{const SOCK_NONBLOCK}
#endif
        if cfd == -1 then do
          errno ← getErrno
          case errno of
            e | e == eAGAIN || e == eWOULDBLOCK → return Nothing
            _ → throwErrno "accept"
        else do
          addr ← peekAddrOfSize (undefined ∷ f) p pSize
          let accFd = Fd cfd
#ifndef HAVE_ACCEPT_WITH_FLAGS
          onException (setNonBlockingFD accFd True) (closeFd accFd)
#endif
          Just . (, addr) <$> unsafeSocketFromFd accFd

-- | Get the local address. See /getsockname(2)/.
getLocalAddr ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
             ⇒ Socket f → μ (SockFamilyAddr f)
getLocalAddr s = withSocketFd s $ \fd →
  allocaMaxAddr (Proxy ∷ Proxy (SockFamilyAddr f)) $ \p size →
    with size $ \pSize → do
      throwErrnoIfMinus1_ "getLocalAddr" $ c_getsockname fd p pSize
      peekAddrOfSize (undefined ∷ f) p pSize

-- | Get the remote address. See /getpeername(2)/.
getRemoteAddr ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
              ⇒ Socket f → μ (SockFamilyAddr f)
getRemoteAddr s = withSocketFd s $ \fd →
  allocaMaxAddr (Proxy ∷ Proxy (SockFamilyAddr f)) $ \p size →
    with size $ \pSize → do
      throwErrnoIfMinus1_ "getRemoteAddr" $ c_getpeername fd p pSize
      peekAddrOfSize (undefined ∷ f) p pSize

-- | Check if socket has out-of-band data. See /sockatmark(3)/.
hasOobData ∷ MonadBase IO μ ⇒ Socket f → μ Bool
hasOobData s = withSocketFd s $ \fd →
  fmap (== 1) $ throwErrnoIfMinus1 "hasOOBData" $ c_sockatmark fd

throwCustomErrno ∷ String → Errno → IO α
throwCustomErrno loc errno =
  throwIO $ errnoToIOError loc errno Nothing Nothing

throwInval ∷ String → IO α
throwInval loc = throwCustomErrno loc eINVAL

recvBufsFromFd ∷ ∀ f . SockFamily f
               ⇒ f → Fd → [(Ptr Word8, Int)] → MsgFlags
               → IO (Maybe (SockFamilyAddr f), Int, MsgFlags)
recvBufsFromFd _ fd bufs' flags = do
  let (bufs, bufs'') = partition ((> 0) . snd) bufs'
      nn             = length bufs
  when (any ((< 0) . snd) bufs'' || nn == 0) $ throwInval "recv"
  when (nn > #{const UIO_MAXIOV}) $ throwCustomErrno "recv" eMSGSIZE
  allocaBytesAligned #{size struct msghdr}
                     #{alignment struct msghdr} $ \pHdr →
    allocaMaxAddr (Proxy ∷ Proxy (SockFamilyAddr f)) $ \pAddr addrLen → do
      #{poke struct msghdr, msg_name}    pHdr pAddr
      #{poke struct msghdr, msg_namelen} pHdr addrLen
      let doRecv = do
            r ← c_recvmsg fd pHdr flags
            case r of
              (-1) → do
                errno ← getErrno
                case errno of
                  e | e == eAGAIN || e == eWOULDBLOCK → do
                    threadWaitRead fd
                    doRecv
                  _ → throwErrno "recv"
              0 → throwIO $ mkIOError eofErrorType "recv" Nothing Nothing
              _ → do
                addrLen' ← #{peek struct msghdr, msg_namelen} pHdr ∷
                              IO #{itype socklen_t}
                addr     ← if addrLen' == 0
                             then return Nothing
                             else fmap Just $ peekSockAddr pAddr $
                                    fromIntegral addrLen'
                flags'   ← #{peek struct msghdr, msg_flags} pHdr
                return (addr, fromIntegral r, flags')
      allocaBytesAligned (nn * #{size struct iovec})
                         #{alignment struct iovec} $ \pIoVs → do
        void . ($ bufs) . ($ pIoVs) . foldM $ \pIoV (p, n) → do
          #{poke struct iovec, iov_base} pIoV p
          #{poke struct iovec, iov_len}  pIoV (fromIntegral n ∷ CSize)
          return $ plusPtr pIoV #{size struct iovec}
        #{poke struct msghdr, msg_iov}        pHdr pIoVs
        #{poke struct msghdr, msg_iovlen}     pHdr (fromIntegral nn ∷ CSize)
        #{poke struct msghdr, msg_control}    pHdr nullPtr
        #{poke struct msghdr, msg_controllen} pHdr (0 ∷ CSize)
        #{poke struct msghdr, msg_flags}      pHdr (0 ∷ CInt)
        doRecv

recvBufsFrom' ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
              ⇒ Socket f → [(Ptr Word8, Int)] → MsgFlags
              → μ (Maybe (SockFamilyAddr f), Int, MsgFlags)
recvBufsFrom' s bufs flags = withSocketFd s $ \fd →
  recvBufsFromFd (undefined ∷ f) fd bufs flags

-- | Receive a message from a connected socket, possibly utilizing multiple
-- memory buffers. See /recvmsg(2)/.
recvBufs ∷ (SockFamily f, MonadBase IO μ)
         ⇒ Socket f           -- ^ The socket
         → [(Ptr Word8, Int)] -- ^ Memory buffers
         → MsgFlags           -- ^ Message flags
         → μ (Int, MsgFlags)  -- ^ Received message length and flags
recvBufs s bufs flags = do
  (_, r, flags') ← recvBufsFrom' s bufs flags
  return (r, flags')

-- | Receive a message from a connected socket. This operation blocks.
-- See /recvmsg(2)/.
recvBuf ∷ (SockFamily f, MonadBase IO μ)
        ⇒ Socket f          -- ^ The socket
        → Ptr α             -- ^ Buffer pointer
        → Int               -- ^ Buffer length
        → MsgFlags          -- ^ Message flags
        → μ (Int, MsgFlags) -- ^ Received message length and flags
recvBuf s p len flags = recvBufs s [(castPtr p, len)] flags

-- | Receive a message from a connected socket. This operation blocks.
-- See /recvmsg(2)/.
recv' ∷ (SockFamily f, MonadBase IO μ)
      ⇒ Socket f                 -- ^ The socket
      → Int                      -- ^ Maximum message length
      → MsgFlags                 -- ^ Message flags
      → μ (ByteString, MsgFlags) -- ^ Received message contents and flags
recv' s len flags =
  liftBase $ BS.createAndTrim' len $ \p → do
    (r, flags') ← recvBuf s p len flags
    return (0, r, flags')

-- | Receive a message from a connected socket. This operation blocks.
-- See /recvmsg(2)/.
recv ∷ (SockFamily f, MonadBase IO μ)
     ⇒ Socket f     -- ^ The socket
     → Int          -- ^ Maximum message length
     → μ ByteString -- ^ Received message contents
recv s len = fst <$> recv' s len noFlags

-- | Receive a message from an unconnected socket, possibly utilizing multiple
-- memory buffers. This operation blocks. See /recvmsg(2)/.
recvBufsFrom ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
             ⇒ Socket f                            -- ^ The socket
             → [(Ptr Word8, Int)]                  -- ^ Memory buffers
             → MsgFlags                            -- ^ Message flags
             → μ (SockFamilyAddr f, Int, MsgFlags)
             -- ^ Received message source address, length, and flags
recvBufsFrom s bufs flags = withSocketFd s $ \fd → do
  (mAddr, n, flags') ← recvBufsFromFd (undefined ∷ f) fd bufs flags
  let getpeername =
        allocaMaxAddr (Proxy ∷ Proxy (SockFamilyAddr f)) $ \p size →
          with size $ \pSize → do
            throwErrnoIfMinus1_ "recv" $ c_getpeername fd p pSize
            peekAddrOfSize (undefined ∷ f) p pSize
  (, n, flags') <$> maybe getpeername return mAddr

-- | Receive a message from an unconnected socket. This operation blocks.
-- See /recvmsg(2)/.
recvBufFrom ∷ (SockFamily f, MonadBase IO μ)
            ⇒ Socket f -- ^ The socket
            → Ptr α    -- ^ Buffer pointer
            → Int      -- ^ Buffer length
            → MsgFlags -- ^ Message flags
            → μ (SockFamilyAddr f, Int, MsgFlags)
            -- ^ Received message source address, length, and flags
recvBufFrom s p len flags = recvBufsFrom s [(castPtr p, len)] flags

-- | Receive a message from an unconnected socket. This operation blocks.
-- See /recvmsg(2)/.
recvFrom' ∷ (SockFamily f, MonadBase IO μ)
          ⇒ Socket f -- ^ The socket
          → Int      -- ^ Maximum message length
          → MsgFlags -- ^ Message flags
          → μ (SockFamilyAddr f, ByteString, MsgFlags)
          -- ^ Received message source address, contents, and flags
recvFrom' s len flags = liftBase $ do
  (bs, (addr, flags')) ← BS.createAndTrim' len $ \p → do
    (addr, len', flags') ← recvBufFrom s p len flags
    return (0, len', (addr, flags'))
  return (addr, bs, flags')

-- | Receive a message from an unconnected socket. This operation blocks.
-- See /recvmsg(2)/.
recvFrom ∷ (SockFamily f, MonadBase IO μ)
         ⇒ Socket f -- ^ The socket
         → Int      -- ^ Maximum message length
         → μ (SockFamilyAddr f, ByteString)
         -- ^ Received message source address and contents
recvFrom s len = do
  (addr, bs, _) ← recvFrom' s len noFlags
  return (addr, bs)

_sendBufs ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
          ⇒ Socket f → [(Ptr Word8, Int)] → MsgFlags
          → Maybe (SockFamilyAddr f) → μ Int
_sendBufs s bufs' flags mAddr = withSocketFd s $ \fd → do
  let (bufs, bufs'') = partition ((> 0) . snd) bufs'
      nn             = length bufs
  when (any ((< 0) . snd) bufs'') $ throwInval "send"
  when (nn > #{const UIO_MAXIOV}) $ throwCustomErrno "send" eMSGSIZE
  if nn == 0 then return 0
  else allocaBytesAligned #{size struct msghdr}
                          #{alignment struct msghdr} $ \pHdr → do
    let doSend = do
          r ← c_sendmsg fd pHdr flags
          if r == -1 then do
            errno ← getErrno
            case errno of
              e | e == eAGAIN || e == eWOULDBLOCK → do
                threadWaitWrite fd
                doSend
              _ → throwErrno "send"
          else
            return $ fromIntegral r
    let cont = allocaBytesAligned (nn * #{size struct iovec})
                                  #{alignment struct iovec} $ \pIoVs → do
          void . ($ bufs) . ($ pIoVs) . foldM $ \pIoV (p, n) → do
            #{poke struct iovec, iov_base} pIoV p
            #{poke struct iovec, iov_len}  pIoV (fromIntegral n ∷ CSize)
            return $ plusPtr pIoV #{size struct iovec}
          #{poke struct msghdr, msg_iov}        pHdr pIoVs
          #{poke struct msghdr, msg_iovlen}     pHdr (fromIntegral nn ∷ CSize)
          #{poke struct msghdr, msg_control}    pHdr nullPtr
          #{poke struct msghdr, msg_controllen} pHdr (0 ∷ CSize)
          #{poke struct msghdr, msg_flags}      pHdr (0 ∷ CInt)
          doSend
    case mAddr of
      Just addr →
        withAddr (undefined ∷ f) addr $ \pAddr addrLen → do
          #{poke struct msghdr, msg_name}    pHdr pAddr
          #{poke struct msghdr, msg_namelen} pHdr addrLen
          cont
      Nothing → do
        #{poke struct msghdr, msg_name}    pHdr nullPtr
        #{poke struct msghdr, msg_namelen} pHdr (0 ∷ #{itype socklen_t})
        cont

-- | Send a message split into several memory buffers on a connected socket.
-- This operation blocks. See /sendmsg(2)/.
sendBufs ∷ (SockFamily f, MonadBase IO μ)
         ⇒ Socket f           -- ^ The socket
         → [(Ptr Word8, Int)] -- ^ Memory buffers
         → MsgFlags           -- ^ Message flags
         → μ Int              -- ^ The number of bytes sent
sendBufs s bufs flags = _sendBufs s bufs flags Nothing

withBufs ∷ [ByteString] → ([(Ptr Word8, Int)] → IO α) → IO α
withBufs bss f = go bss []
  where go []          rbufs = f (reverse rbufs)
        go (bs : bss') rbufs = BS.unsafeUseAsCStringLen bs $ \(p, len) →
                                 go bss' ((castPtr p, len) : rbufs)

-- | Send a message split into several 'ByteString's on a connected socket.
-- This operation blocks. See /sendmsg(2)/.
sendMany' ∷ (SockFamily f, MonadBase IO μ)
          ⇒ Socket f     -- ^ The socket
          → [ByteString] -- ^ Message contents
          → MsgFlags     -- ^ Message flags
          → μ Int        -- ^ The number of bytes sent
sendMany' s bss flags =
  liftBase $ withBufs bss $ \bufs → sendBufs s bufs flags

-- | Send a message split into several 'ByteString's on a connected socket.
-- This operation blocks. See /sendmsg(2)/.
sendMany ∷ (SockFamily f, MonadBase IO μ)
         ⇒ Socket f     -- ^ The socket
         → [ByteString] -- ^ Message contents
         → μ Int        -- ^ The number of bytes sent
sendMany s bss = sendMany' s bss noFlags

-- | Send a message on a connected socket. This operation blocks.
-- See /sendmsg(2)/.
sendBuf ∷ (SockFamily f, MonadBase IO μ)
        ⇒ Socket f -- ^ The socket
        → Ptr α    -- ^ Buffer pointer
        → Int      -- ^ Buffer length
        → MsgFlags -- ^ Message flags
        → μ Int    -- ^ The number of bytes sent
sendBuf s p len flags = sendBufs s [(castPtr p, len)] flags

-- | Send a message on a connected socket. This operation blocks.
-- See /sendmsg(2)/.
send' ∷ (SockFamily f, MonadBase IO μ)
      ⇒ Socket f   -- ^ The socket
      → ByteString -- ^ Message contents
      → MsgFlags   -- ^ Message flags
      → μ Int      -- ^ The number of bytes sent
send' s bs flags = liftBase $ BS.unsafeUseAsCStringLen bs $ \(p, len) →
                     sendBuf s p len flags

-- | Send a message on a connected socket. This operation blocks.
-- See /sendmsg(2)/.
send ∷ (SockFamily f, MonadBase IO μ)
     ⇒ Socket f   -- ^ The socket
     → ByteString -- ^ Message contents
     → μ Int      -- ^ The number of bytes sent
send s bs = send' s bs noFlags

-- | Send a message split into several memory buffers on an unconnected
-- socket. This operation blocks. See /sendmsg(2)/.
sendBufsTo ∷ (SockFamily f, MonadBase IO μ)
           ⇒ Socket f           -- ^ The socket
           → [(Ptr Word8, Int)] -- ^ Memory buffers
           → MsgFlags           -- ^ Message flags
           → SockFamilyAddr f   -- ^ Message destination address
           → μ Int              -- ^ The number of bytes sent
sendBufsTo s bufs flags addr = _sendBufs s bufs flags (Just addr)

-- | Send a message split into several 'ByteString's on an unconnected socket.
-- This operation blocks. See /sendmsg(2)/.
sendManyTo' ∷ (SockFamily f, MonadBase IO μ)
            ⇒ Socket f         -- ^ The socket
            → [ByteString]     -- ^ Message contents
            → MsgFlags         -- ^ Message flags
            → SockFamilyAddr f -- ^ Message destination address
            → μ Int            -- ^ The number of bytes sent
sendManyTo' s bss flags addr = liftBase $ withBufs bss $ \bufs →
                                 sendBufsTo s bufs flags addr

-- | Send a message split into several 'ByteString's on an unconnected socket.
-- This operation blocks. See /sendmsg(2)/.
sendManyTo ∷ (SockFamily f, MonadBase IO μ)
           ⇒ Socket f         -- ^ The socket
           → [ByteString]     -- ^ Message contents
           → SockFamilyAddr f -- ^ Message destination address
           → μ Int            -- ^ The number of bytes sent
sendManyTo s bss addr = sendManyTo' s bss noFlags addr

-- | Send a message on an unconnected socket. This operation blocks.
-- See /sendmsg(2)/.
sendBufTo ∷ (SockFamily f, MonadBase IO μ)
          ⇒ Socket f         -- ^ The socket
          → Ptr α            -- ^ Buffer pointer
          → Int              -- ^ Buffer length
          → MsgFlags         -- ^ Message flags
          → SockFamilyAddr f -- ^ Message destination address
          → μ Int            -- ^ The number of bytes sent
sendBufTo s p len flags addr = sendBufsTo s [(castPtr p, len)] flags addr

-- | Send a message on an unconnected socket. This operation blocks.
-- See /sendmsg(2)/.
sendTo' ∷ (SockFamily f, MonadBase IO μ)
        ⇒ Socket f         -- ^ The socket
        → ByteString       -- ^ Message contents
        → MsgFlags         -- ^ Message flags
        → SockFamilyAddr f -- ^ Message destination address
        → μ Int            -- ^ The number of bytes sent
sendTo' s bs flags addr =
  liftBase $ BS.unsafeUseAsCStringLen bs $ \(p, len) →
    sendBufTo s p len flags addr

-- | Send a message on an unconnected socket. This operation blocks.
-- See /sendmsg(2)/.
sendTo ∷ (SockFamily f, MonadBase IO μ)
       ⇒ Socket f         -- ^ The socket
       → ByteString       -- ^ Message contents
       → SockFamilyAddr f -- ^ Message destination address
       → μ Int            -- ^ The number of bytes sent
sendTo s bs addr = sendTo' s bs noFlags addr

-- | Shut down a part of a full-duplex connection. See /shutdown(2)/.
shutdown ∷ MonadBase IO μ ⇒ Socket f → SockOps → μ ()
shutdown s dirs = withSocketFd s $ \fd →
    forM_ how $ throwErrnoIfMinus1_ "shutdown" . c_shutdown fd
  where how = if dirs .>=. sendSockOp then
                if dirs .>=. recvSockOp then
                  Just #{const SHUT_RDWR}
                else
                  Just #{const SHUT_WR}
              else
                if dirs .>=. recvSockOp then
                  Just #{const SHUT_RD}
                else
                  Nothing

-- | Close the socket. See /close(2)/.
close ∷ MonadBase IO μ ⇒ Socket f → μ ()
close (Socket v) = liftBase $ modifyMVar_ v $ \fd → do
  when (fd >= 0) $ closeFdWith closeFd fd
  return (-1)

foreign import ccall "socket"
  c_socket ∷ CInt → CInt → SockProto → IO CInt
foreign import ccall "getsockopt"
  c_getsockopt ∷ Fd → CInt → CInt → Ptr α → Ptr #{itype socklen_t} → IO CInt
foreign import ccall "setsockopt"
  c_setsockopt ∷ Fd → CInt → CInt → Ptr α → #{itype socklen_t} → IO CInt
foreign import ccall "bind"
  c_bind ∷ Fd → Ptr α → #{itype socklen_t} → IO CInt
foreign import ccall "connect"
  c_connect ∷ Fd → Ptr α → #{itype socklen_t} → IO CInt
foreign import ccall "listen"
  c_listen ∷ Fd → CInt → IO CInt
#ifdef HAVE_ACCEPT_WITH_FLAGS
foreign import ccall "accept4"
  c_accept ∷ Fd → Ptr α → Ptr #{itype socklen_t} → CInt → IO CInt
#else
foreign import ccall "accept"
  c_accept ∷ Fd → Ptr α → Ptr #{itype socklen_t} → IO CInt
#endif
foreign import ccall "getsockname"
  c_getsockname ∷ Fd → Ptr α → Ptr #{itype socklen_t} → IO CInt
foreign import ccall "getpeername"
  c_getpeername ∷ Fd → Ptr α → Ptr #{itype socklen_t} → IO CInt
foreign import ccall "sockatmark"
  c_sockatmark ∷ Fd → IO CInt
foreign import ccall "recvmsg"
  c_recvmsg ∷ Fd → Ptr β → MsgFlags → IO CSsize
foreign import ccall "sendmsg"
  c_sendmsg ∷ Fd → Ptr β → MsgFlags → IO CSsize
foreign import ccall "shutdown"
  c_shutdown ∷ Fd → CInt → IO CInt

