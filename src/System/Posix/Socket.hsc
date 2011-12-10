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

-- | POSIX sockets.
module System.Posix.Socket (
    Socket,
    withSocketFd,
    unsafeSocketFd,
    unsafeSocketFromFd,
    SockFamily(..),
    SockAddr(..),
    SockType(..),
    streamSockType,
    datagramSockType,
    seqPacketSockType,
    rawSockType,
    SockProto(..),
    defaultSockProto,
    SockOpt(..),
    SO_ERROR(..),
    SO_KEEPALIVE(..),
    SO_REUSEADDR(..),
    SockOps,
    sendSockOp,
    recvSockOp,
    MsgFlags,
    peekMsgFlag,
    truncMsgFlag,
    oobMsgFlag,
    dontRouteMsgFlag,

    socket,
    getSockOpt,
    setSockOpt,
    bind,
    connect,
    tryConnect,
    listen,
    accept,
    getLocalAddr,
    getRemoteAddr,
    hasOOBData,
    recvBufs,
    recvBuf,
    recv',
    recv,
    recvBufsFrom,
    recvBufFrom,
    recvFrom',
    recvFrom,
    sendBufs,
    sendMany',
    sendMany,
    sendBuf,
    send',
    send,
    sendBufsTo,
    sendManyTo',
    sendManyTo,
    sendBufTo,
    sendTo',
    sendTo,
    shutdown,
    close
  ) where

import Data.Typeable (Typeable)
import Data.Int
import Data.Word
import Data.Bits ((.|.))
import Data.Default
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

-- | Use file descriptor as a socket.
unsafeSocketFromFd ∷ MonadBase IO μ ⇒ Fd → μ (Socket f)
unsafeSocketFromFd = liftBase . fmap Socket . newMVar

-- | Socket address.
class SockAddr a where
  -- | Maximum size of a socket address. The argument must be ignored.
  sockAddrMaxSize ∷ a → Int
  -- | Size of a particular socket address.
  sockAddrSize    ∷ a → Int
  peekSockAddr    ∷ Bool  -- ^ Whether the peeked address is local
                  → Ptr a -- ^ Buffer
                  → Int   -- ^ Buffer size
                  → IO a
  pokeSockAddr    ∷ Bool  -- ^ Whether the poked address is local
                  → Ptr a -- ^ Buffer of sufficient size
                  → a     -- ^ The address to poke
                  → IO ()

-- | Socket family.
class SockAddr (SockFamilyAddr f) ⇒ SockFamily f where
  type SockFamilyAddr f
  sockFamilyCode ∷ f → CInt

-- | Socket type.
newtype SockType = SockType CInt deriving (Typeable, Eq, Ord, Show, Storable)

#{enum SockType, SockType
 , streamSockType    = SOCK_STREAM
 , datagramSockType  = SOCK_DGRAM
 , seqPacketSockType = SOCK_SEQPACKET
 , rawSockType       = SOCK_RAW
 }

-- | Socket protocol.
newtype SockProto = SockProto CInt deriving (Typeable, Eq, Ord, Show, Storable)

-- | Default socket protocol (corresponds to @0@).
defaultSockProto ∷ SockProto
defaultSockProto = SockProto 0

instance Default SockProto where
  def = defaultSockProto

-- | Socket option.
class Storable (SockOptRaw o) ⇒ SockOpt o where
  type SockOptValue o
  type SockOptRaw o
  type SockOptReadable o
  type SockOptWritable o
  sockOptRaw   ∷ o → SockOptValue o → SockOptRaw o
  sockOptValue ∷ o → SockOptRaw o → SockOptValue o
  sockOptLevel ∷ o → CInt
  sockOptCode  ∷ o → CInt

data SO_ERROR = SO_ERROR deriving (Typeable, Eq, Show)

instance SockOpt SO_ERROR where
  type SockOptValue    SO_ERROR = Errno
  type SockOptRaw      SO_ERROR = CInt
  type SockOptReadable SO_ERROR = SO_ERROR
  type SockOptWritable SO_ERROR = ()
  sockOptRaw   _ (Errno e) = e
  sockOptValue _ = Errno
  sockOptLevel _ = #const SOL_SOCKET
  sockOptCode  _ = #const SO_ERROR

data SO_KEEPALIVE = SO_KEEPALIVE deriving (Typeable, Eq, Show)

instance SockOpt SO_KEEPALIVE where
  type SockOptValue    SO_KEEPALIVE = Bool
  type SockOptRaw      SO_KEEPALIVE = CInt
  type SockOptReadable SO_KEEPALIVE = SO_KEEPALIVE
  type SockOptWritable SO_KEEPALIVE = SO_KEEPALIVE
  sockOptRaw _ False = 0
  sockOptRaw _ True  = 1
  sockOptValue _ = (/= 0)
  sockOptLevel _ = #const SOL_SOCKET
  sockOptCode  _ = #const SO_KEEPALIVE

data SO_REUSEADDR = SO_REUSEADDR deriving (Typeable, Eq, Show)

instance SockOpt SO_REUSEADDR where
  type SockOptValue    SO_REUSEADDR = Bool
  type SockOptRaw      SO_REUSEADDR = CInt
  type SockOptReadable SO_REUSEADDR = SO_REUSEADDR
  type SockOptWritable SO_REUSEADDR = SO_REUSEADDR
  sockOptRaw _ False = 0
  sockOptRaw _ True  = 1
  sockOptValue _ = (/= 0)
  sockOptLevel _ = #const SOL_SOCKET
  sockOptCode  _ = #const SO_REUSEADDR

-- Socket operations. Used by 'shutdown'.
$(bitmaskWrapper "SockOps" ''Int []
    [("sendSockOp", 1),
     ("recvSockOp", 2)])

-- Message flags.
newtype MsgFlags = MsgFlags CInt deriving (Typeable, Eq, Show, Storable, Flags)

#{enum MsgFlags, MsgFlags
 , peekMsgFlag      = MSG_PEEK
 , truncMsgFlag     = MSG_TRUNC
 , oobMsgFlag       = MSG_OOB
 , dontRouteMsgFlag = MSG_DONTROUTE
 }

allocaMaxAddr ∷ SockAddr a ⇒ a → (Ptr a → #{itype socklen_t} → IO α) → IO α
allocaMaxAddr addr f =
    allocaBytesAligned size #{alignment struct sockaddr} $
      (`f` (fromIntegral size))
  where size = sockAddrMaxSize addr

allocaAddr ∷ SockAddr a ⇒ a → (Ptr a → #{itype socklen_t} → IO α) → IO α
allocaAddr addr f =
    allocaBytesAligned size #{alignment struct sockaddr} $
      (`f` (fromIntegral size))
  where size = sockAddrSize addr

peekAddrOfSize ∷ SockFamily f
               ⇒ f → Bool → Ptr (SockFamilyAddr f) → Ptr #{itype socklen_t}
               → IO (SockFamilyAddr f)
peekAddrOfSize fam local p pSize = do
  outSize ← fromIntegral <$> peek pSize
  famCode ∷ #{itype sa_family_t} ← #{peek struct sockaddr, sa_family} p
  when (fromIntegral famCode /= sockFamilyCode fam) $
    ioError $ userError "Invalid socket address family"
  peekSockAddr local p outSize

withAddr ∷ SockFamily f
         ⇒ f → Bool → SockFamilyAddr f
         → (Ptr (SockFamilyAddr f) → #{itype socklen_t} → IO α)
         → IO α
withAddr fam local addr f =
    allocaAddr addr $ \p size → do
      pokeSockAddr local p addr
      #{poke struct sockaddr, sa_family} p famCode
      f p size
  where famCode ∷ #{itype sa_family_t}
        famCode = fromIntegral $ sockFamilyCode fam

-- Create a socket. See /socket(3)/.
-- The underlying file descriptor is non-blocking.
socket ∷ (SockFamily f, MonadBase IO μ)
       ⇒ f → SockType → SockProto → μ (Socket f)
socket f (SockType t) p = liftBase $ do
  fd ← throwErrnoIfMinus1 "socket" $
         c_socket (sockFamilyCode f)
#ifdef HAVE_SOCKET_WITH_FLAGS
           (t .|. #{const SOCK_NONBLOCK}) p
#else
           t p
  setNonBlockingFD fd True
#endif
  fmap Socket $ newMVar $ Fd fd

getFdOpt ∷ ∀ o . (SockOpt o, SockOptReadable o ~ o)
         ⇒ Fd → o → IO (SockOptValue o)
getFdOpt fd o =
  alloca $ \p →
    with (fromIntegral $ sizeOf (undefined ∷ SockOptRaw o)) $ \pSize → do
      throwErrnoIfMinus1_ "getSockOpt" $
        c_getsockopt fd (sockOptLevel o) (sockOptCode o) p pSize
      sockOptValue o <$> peek p

-- Get socket option value. See /getsockopt(3)/.
getSockOpt ∷ (SockOpt o, SockOptReadable o ~ o, MonadBase IO μ)
           ⇒ Socket f → o → μ (SockOptValue o)
getSockOpt s o = withSocketFd s $ \fd → getFdOpt fd o

-- Set socket option value. See /setsockopt(3)/.
setSockOpt ∷ (SockOpt o, SockOptWritable o ~ o, MonadBase IO μ)
           ⇒ Socket f → o → SockOptValue o → μ ()
setSockOpt s o v = withSocketFd s $ \fd →
    with raw $ \p →
      throwErrnoIfMinus1_ "setSockOpt" $
        c_setsockopt fd (sockOptLevel o) (sockOptCode o) p $
          fromIntegral (sizeOf raw)
  where raw = sockOptRaw o v

-- Bind socket to the specified address. See /bind(3)/.
bind ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
     ⇒ Socket f → SockFamilyAddr f → μ () 
bind s addr = withSocketFd s $ \fd →
  withAddr (undefined ∷ f) True addr $ \p size →
    throwErrnoIfMinus1_ "bind" $ c_bind fd p $ fromIntegral size

-- Connect socket to the specified address. This function blocks.
-- See /connect(3)/.
connect ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
        ⇒ Socket f → SockFamilyAddr f → μ ()
connect s addr = withSocketFd s $ \fd →
    withAddr (undefined ∷ f) False addr $ \p size →
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

-- Try to connect socket without blocking. On success 'True' is returned.
-- If the connection did not succeed immediately, 'False' is returned.
-- See /connect(3)/.
tryConnect ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
           ⇒ Socket f → SockFamilyAddr f → μ Bool
tryConnect s addr = withSocketFd s $ \fd →
  withAddr (undefined ∷ f) False addr $ \p size → do
    r ← c_connect fd p $ fromIntegral size
    if r == -1
      then do
        errno ← getErrno
        if errno == eINPROGRESS
          then return False
          else throwErrno "connect"
      else
        return True

-- Listen for connections on the given socket. See /listen(2)/.
listen ∷ MonadBase IO μ ⇒ Socket f → Int → μ ()
listen s backlog = withSocketFd s $ \fd →
  throwErrnoIfMinus1_ "listen" $ c_listen fd $ fromIntegral backlog

-- Accept a connection on the given socket. This function blocks.
-- See /accept(2)/.
accept ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
       ⇒ Socket f → μ (Socket f, SockFamilyAddr f)
accept s = withSocketFd s $ \fd →
    allocaMaxAddr (undefined ∷ SockFamilyAddr f) $ \p size →
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
            addr ← peekAddrOfSize (undefined ∷ f) False p pSize
#ifndef HAVE_ACCEPT_WITH_FLAGS
            setNonBlockingFD cfd True
#endif
            (, addr) <$> unsafeSocketFromFd (Fd cfd)

-- Get the local address. See /getsockname(3)/.
getLocalAddr ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
             ⇒ Socket f → μ (SockFamilyAddr f)
getLocalAddr s = withSocketFd s $ \fd →
  allocaMaxAddr (undefined ∷ SockFamilyAddr f) $ \p size →
    with size $ \pSize → do
      throwErrnoIfMinus1_ "getLocalAddr" $ c_getsockname fd p pSize
      peekAddrOfSize (undefined ∷ f) True p pSize

-- Get the remote address. See /getpeername(3)/.
getRemoteAddr ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
              ⇒ Socket f → μ (SockFamilyAddr f)
getRemoteAddr s = withSocketFd s $ \fd →
  allocaMaxAddr (undefined ∷ SockFamilyAddr f) $ \p size →
    with size $ \pSize → do
      throwErrnoIfMinus1_ "getRemoteAddr" $ c_getpeername fd p pSize
      peekAddrOfSize (undefined ∷ f) False p pSize

-- Check if socket has out-of-band data. See /sockatmark(3)/.
hasOOBData ∷ MonadBase IO μ ⇒ Socket f → μ Bool
hasOOBData s = withSocketFd s $ \fd →
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
    allocaMaxAddr (undefined ∷ SockFamilyAddr f) $ \pAddr addrLen → do
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
                             else fmap Just $ peekSockAddr False pAddr $
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

recvBufsFrom ∷ ∀ f μ . (SockFamily f, MonadBase IO μ)
             ⇒ Socket f → [(Ptr Word8, Int)] → MsgFlags
             → μ (SockFamilyAddr f, Int, MsgFlags)
recvBufsFrom s bufs flags = withSocketFd s $ \fd → do
  (mAddr, n, flags') ← recvBufsFromFd (undefined ∷ f) fd bufs flags
  let getpeername =
        allocaMaxAddr (undefined ∷ SockFamilyAddr f) $ \p size →
          with size $ \pSize → do
            throwErrnoIfMinus1_ "recv" $ c_getpeername fd p pSize
            peekAddrOfSize (undefined ∷ f) False p pSize
  (, n, flags') <$> maybe getpeername return mAddr

recvBufs ∷ (SockFamily f, MonadBase IO μ)
         ⇒ Socket f → [(Ptr Word8, Int)] → MsgFlags → μ (Int, MsgFlags)
recvBufs s bufs flags = do
  (_, r, flags') ← recvBufsFrom' s bufs flags
  return (r, flags')

recvBuf ∷ (SockFamily f, MonadBase IO μ)
        ⇒ Socket f → Ptr α → Int → MsgFlags → μ (Int, MsgFlags)
recvBuf s p len flags = recvBufs s [(castPtr p, len)] flags

recv' ∷ (SockFamily f, MonadBase IO μ)
      ⇒ Socket f → Int → MsgFlags → μ (ByteString, MsgFlags)
recv' s len flags =
  liftBase $ BS.createAndTrim' len $ \p → do
    (r, flags') ← recvBuf s p len flags
    return (0, r, flags')

recv ∷ (SockFamily f, MonadBase IO μ) ⇒ Socket f → Int → μ ByteString
recv s len = fst <$> recv' s len noFlags

recvBufFrom ∷ (SockFamily f, MonadBase IO μ)
            ⇒ Socket f → Ptr α → Int → MsgFlags
            → μ (SockFamilyAddr f, Int, MsgFlags)
recvBufFrom s p len flags = recvBufsFrom s [(castPtr p, len)] flags

recvFrom' ∷ (SockFamily f, MonadBase IO μ)
          ⇒ Socket f → Int → MsgFlags
          → μ (SockFamilyAddr f, ByteString, MsgFlags)
recvFrom' s len flags = liftBase $ do
  (bs, (addr, flags')) ← BS.createAndTrim' len $ \p → do
    (addr, len', flags') ← recvBufFrom s p len flags
    return (0, len', (addr, flags'))
  return (addr, bs, flags')

recvFrom ∷ (SockFamily f, MonadBase IO μ)
         ⇒ Socket f → Int → μ (SockFamilyAddr f, ByteString)
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
        withAddr (undefined ∷ f) False addr $ \pAddr addrLen → do
          #{poke struct msghdr, msg_name}    pHdr pAddr
          #{poke struct msghdr, msg_namelen} pHdr addrLen
          cont
      Nothing → do
        #{poke struct msghdr, msg_name}    pHdr nullPtr
        #{poke struct msghdr, msg_namelen} pHdr (0 ∷ #{itype socklen_t})
        cont

sendBufs ∷ (SockFamily f, MonadBase IO μ)
         ⇒ Socket f → [(Ptr Word8, Int)] → MsgFlags → μ Int
sendBufs s bufs flags = _sendBufs s bufs flags Nothing

withBufs ∷ [ByteString] → ([(Ptr Word8, Int)] → IO α) → IO α
withBufs bss f = go bss []
  where go []          rbufs = f (reverse rbufs)
        go (bs : bss') rbufs = BS.unsafeUseAsCStringLen bs $ \(p, len) →
                                 go bss' ((castPtr p, len) : rbufs)

sendMany' ∷ (SockFamily f, MonadBase IO μ)
          ⇒ Socket f → [ByteString] → MsgFlags → μ Int
sendMany' s bss flags =
  liftBase $ withBufs bss $ \bufs → sendBufs s bufs flags

sendMany ∷ (SockFamily f, MonadBase IO μ) ⇒ Socket f → [ByteString] → μ Int
sendMany s bss = sendMany' s bss noFlags

sendBuf ∷ (SockFamily f, MonadBase IO μ)
        ⇒ Socket f → Ptr α → Int → MsgFlags → μ Int
sendBuf s p len flags = sendBufs s [(castPtr p, len)] flags

send' ∷ (SockFamily f, MonadBase IO μ)
      ⇒ Socket f → ByteString → MsgFlags → μ Int
send' s bs flags = liftBase $ BS.unsafeUseAsCStringLen bs $ \(p, len) →
                     sendBuf s p len flags

send ∷ (SockFamily f, MonadBase IO μ) ⇒ Socket f → ByteString → μ Int
send s bs = send' s bs noFlags

sendBufsTo ∷ (SockFamily f, MonadBase IO μ)
           ⇒ Socket f → [(Ptr Word8, Int)] → MsgFlags → SockFamilyAddr f
           → μ Int
sendBufsTo s bufs flags addr = _sendBufs s bufs flags (Just addr)

sendManyTo' ∷ (SockFamily f, MonadBase IO μ)
            ⇒ Socket f → [ByteString] → MsgFlags → SockFamilyAddr f → μ Int
sendManyTo' s bss flags addr = liftBase $ withBufs bss $ \bufs →
                                 sendBufsTo s bufs flags addr

sendManyTo ∷ (SockFamily f, MonadBase IO μ)
           ⇒ Socket f → [ByteString] → SockFamilyAddr f → μ Int
sendManyTo s bss addr = sendManyTo' s bss noFlags addr

sendBufTo ∷ (SockFamily f, MonadBase IO μ)
          ⇒ Socket f → Ptr α → Int → MsgFlags → SockFamilyAddr f → μ Int
sendBufTo s p len flags addr = sendBufsTo s [(castPtr p, len)] flags addr

sendTo' ∷ (SockFamily f, MonadBase IO μ)
        ⇒ Socket f → ByteString → MsgFlags → SockFamilyAddr f → μ Int
sendTo' s bs flags addr =
  liftBase $ BS.unsafeUseAsCStringLen bs $ \(p, len) →
    sendBufTo s p len flags addr

sendTo ∷ (SockFamily f, MonadBase IO μ)
       ⇒ Socket f → ByteString → SockFamilyAddr f → μ Int
sendTo s bs addr = sendTo' s bs noFlags addr

-- Shut down part of a full-duplex connection. See /shutdown(3)/.
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

-- Close socket. See /close(3)/.
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

