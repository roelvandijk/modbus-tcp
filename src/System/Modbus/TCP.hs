{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PackageImports #-}

-- | An implementation of the Modbus TPC/IP protocol.
--
-- This implementation is based on the @MODBUS Application Protocol
-- Specification V1.1b@
-- (<http://www.modbus.org/docs/Modbus_Application_Protocol_V1_1b.pdf>).
module System.Modbus.TCP
  ( -- * Session Monad
    Session
  , runSession

    -- * Connections
  , Connection(..)

    -- * Types
  , TCP_ADU(..)
  , Header(..)
  , FunctionCode(..)
  , ExceptionCode(..)
  , ModbusException(..)

  , TransactionId
  , ProtocolId
  , UnitId

    -- * Commands
  , command

  , readCoils
  , readDiscreteInputs
  , readHoldingRegisters
  , readInputRegisters
  , writeSingleCoil
  , writeSingleRegister
  , writeMultipleRegisters
  ) where

import "base" Control.Exception.Base ( Exception )
import "base" Control.Monad ( replicateM, mzero )
import "base" Control.Monad.IO.Class ( MonadIO, liftIO )
import "base" Data.Functor ( void )
import "base" Data.Maybe ( fromMaybe )
import "base" Data.Word ( Word8, Word16 )
import "base" Data.Typeable ( Typeable )
import "base" System.Timeout ( timeout )
import qualified "cereal" Data.Serialize as Cereal ( encode, decode )
import "cereal" Data.Serialize
  ( Serialize, Put, put, Get, get
  , runPut, runGet
  , putWord8, putWord16be
  , getWord8, getWord16be
  , getByteString
  )
import "bytestring" Data.ByteString ( ByteString )
import qualified "bytestring" Data.ByteString as BS
import "mtl" Control.Monad.Reader ( MonadReader, ask )
import "mtl" Control.Monad.Except ( MonadError, throwError, catchError )
import "transformers" Control.Monad.Trans.Class ( lift )
import "transformers" Control.Monad.Trans.Except
    ( ExceptT(ExceptT), withExceptT )
import "transformers" Control.Monad.Trans.Reader
    ( ReaderT, runReaderT )


type TransactionId = Word16
type ProtocolId    = Word16
type UnitId        = Word8

data Connection
   = Connection
     { connWrite :: !(BS.ByteString -> IO Int)
       -- ^ Action that writes bytes.
       --
       -- You can use Network.Socket.ByteString.send applied to some
       -- socket, or some custom function.
     , connRead :: !(Int -> IO BS.ByteString)
       -- ^ Action that reads bytes.
       --
       -- You can use Network.Socket.ByteString.recv applied to some
       -- socket, or some custom function.
     , connCommandTimeout :: !Int
       -- ^ Time limit in microseconds for each command.
     , connMaxTries :: !Int
       -- ^ Maximum number of tries for each command. A command will
       -- result in a `ModbusException` if all tries fail.
       --
       -- It is an error (`NoTriesAllowed`) to set this number to a
       -- value <= 0.
     }

-- | Modbus TCP session monad.
newtype Session a
      = Session
        { runSession' :: ReaderT Connection (ExceptT ModbusException IO) a
        } deriving ( Functor
                   , Applicative
                   , Monad
                   , MonadError ModbusException
                   , MonadReader Connection
                   , MonadIO
                   )

-- | Run a session using a connection.
--
--
runSession :: Connection -> Session a -> ExceptT ModbusException IO a
runSession conn session = runReaderT (runSession' session) conn

-- | MODBUS TCP/IP Application Data Unit
--
-- See: MODBUS Application Protocol Specification V1.1b, section 4.1
data TCP_ADU
   = TCP_ADU
     { aduHeader   :: Header
     , aduFunction :: FunctionCode
     , aduData     :: ByteString
     } deriving (Eq, Show)

instance Serialize TCP_ADU where
  put (TCP_ADU header fc ws) = do
      put header
      put fc
      mapM_ putWord8 (BS.unpack ws)

  get = do
      header <- get
      fc     <- get
      ws     <- getByteString $ fromIntegral (hdrLength header) - 2
      return $ TCP_ADU header fc ws

-- | MODBUS Application Protocol Header
--
-- See: MODBUS Application Protocol Specification V1.1b, section 4.1
data Header
   = Header
     { hdrTransactionId :: TransactionId
     , hdrProtocolId    :: ProtocolId
     , hdrLength        :: Word16
     , hdrUnitId        :: UnitId
     } deriving (Eq, Show)

instance Serialize Header where
    put (Header tid pid len uid) =
      putWord16be tid >> putWord16be pid >> putWord16be len >> putWord8 uid
    get = Header <$> getWord16be <*> getWord16be <*> getWord16be <*> getWord8

-- | The function code field of a MODBUS data unit is coded in one
-- byte. Valid codes are in the range of 1 ... 255 decimal (the range
-- 128 - 255 is reserved and used for exception responses). When a
-- message is sent from a Client to a Server device the function code
-- field tells the server what kind of action to perform. Function
-- code 0 is not valid.
--
-- Sub-function codes are added to some function codes to define
-- multiple actions.
--
-- See: MODBUS Application Protocol Specification V1.1b, sections 4.1 and 5
data FunctionCode
   = -- | See: MODBUS Application Protocol Specification V1.1b, section 6.1
     ReadCoils
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.2
   | ReadDiscreteInputs
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.3
   | ReadHoldingRegisters
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.4
   | ReadInputRegisters
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.5
   | WriteSingleCoil
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.6
   | WriteSingleRegister
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.7
   | ReadExceptionStatus
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.8
   | Diagnostics
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.9
   | GetCommEventCounter
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.10
   | GetCommEventLog
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.11
   | WriteMultipleCoils
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.12
   | WriteMultipleRegisters
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.13
   | ReportSlaveID
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.14
   | ReadFileRecord
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.15
   | WriteFileRecord
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.16
   | MaskWriteRegister
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.17
   | ReadWriteMultipleRegisters
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.18
   | ReadFIFOQueue
     -- | See: MODBUS Application Protocol Specification V1.1b, section 6.19
   | EncapsulatedInterfaceTransport
     -- | See: MODBUS Application Protocol Specification V1.1b, section 5
   | UserDefinedCode Word8
     -- | See: MODBUS Application Protocol Specification V1.1b, section 5
   | ReservedCode Word8
   | OtherCode Word8
   | ExceptionCode FunctionCode
     deriving (Eq, Show)

instance Serialize FunctionCode where
  put = putWord8 . enc
    where
      enc :: FunctionCode -> Word8
      enc ReadCoils                      = 0x01
      enc ReadDiscreteInputs             = 0x02
      enc ReadHoldingRegisters           = 0x03
      enc ReadInputRegisters             = 0x04
      enc WriteSingleCoil                = 0x05
      enc WriteSingleRegister            = 0x06
      enc ReadExceptionStatus            = 0x07
      enc Diagnostics                    = 0x08
      enc GetCommEventCounter            = 0x0B
      enc GetCommEventLog                = 0x0C
      enc WriteMultipleCoils             = 0x0F
      enc WriteMultipleRegisters         = 0x10
      enc ReportSlaveID                  = 0x11
      enc ReadFileRecord                 = 0x14
      enc WriteFileRecord                = 0x15
      enc MaskWriteRegister              = 0x16
      enc ReadWriteMultipleRegisters     = 0x17
      enc ReadFIFOQueue                  = 0x18
      enc EncapsulatedInterfaceTransport = 0x2B
      enc (UserDefinedCode   code)       = code
      enc (ReservedCode      code)       = code
      enc (OtherCode         code)       = code
      enc (ExceptionCode fc)             = 0x80 + enc fc

  get = getWord8 >>= return . dec
    where
      dec :: Word8 -> FunctionCode
      dec 0x01 = ReadCoils
      dec 0x02 = ReadDiscreteInputs
      dec 0x03 = ReadHoldingRegisters
      dec 0x04 = ReadInputRegisters
      dec 0x05 = WriteSingleCoil
      dec 0x06 = WriteSingleRegister
      dec 0x07 = ReadExceptionStatus
      dec 0x08 = Diagnostics
      dec 0x0B = GetCommEventCounter
      dec 0x0C = GetCommEventLog
      dec 0x0F = WriteMultipleCoils
      dec 0x10 = WriteMultipleRegisters
      dec 0x11 = ReportSlaveID
      dec 0x14 = ReadFileRecord
      dec 0x15 = WriteFileRecord
      dec 0x16 = MaskWriteRegister
      dec 0x17 = ReadWriteMultipleRegisters
      dec 0x18 = ReadFIFOQueue
      dec 0x2B = EncapsulatedInterfaceTransport
      dec code |    (code >=  65 && code <=  72)
                 || (code >= 100 && code <= 110) = UserDefinedCode code
               | code `elem` [9, 10, 13, 14, 41, 42, 90, 91, 125, 126, 127]
                 = ReservedCode code
               | code >= 0x80 = ExceptionCode $ dec $ code - 0x80
               | otherwise = OtherCode code

-- | See: MODBUS Application Protocol Specification V1.1b, section 7
data ExceptionCode
   = -- | The function code received in the query is not an allowable
     -- action for the server (or slave). This may be because the
     -- function code is only applicable to newer devices, and was not
     -- implemented in the unit selected. It could also indicate that
     -- the server (or slave) is in the wrong state to process a
     -- request of this type, for example because it is unconfigured
     -- and is being asked to return register values.
     IllegalFunction
     -- | The data address received in the query is not an allowable
     -- address for the server (or slave). More specifically, the
     -- combination of reference number and transfer length is
     -- invalid. For a controller with 100 registers, the PDU addresses
     -- the first register as 0, and the last one as 99. If a request
     -- is submitted with a starting register address of 96 and a
     -- quantity of registers of 4, then this request will successfully
     -- operate (address-wise at least) on registers 96, 97, 98, 99. If
     -- a request is submitted with a starting register address of 96
     -- and a quantity of registers of 5, then this request will fail
     -- with Exception Code 0x02 \"Illegal Data Address\" since it
     -- attempts to operate on registers 96, 97, 98, 99 and 100, and
     -- there is no register with address 100.
   | IllegalDataAddress
     -- | A value contained in the query data field is not an allowable
     -- value for server (or slave). This indicates a fault in the
     -- structure of the remainder of a complex request, such as that
     -- the implied length is incorrect. It specifically does NOT mean
     -- that a data item submitted for storage in a register has a
     -- value outside the expectation of the application program, since
     -- the MODBUS protocol is unaware of the significance of any
     -- particular value of any particular register.
   | IllegalDataValue
     -- | An unrecoverable error occurred while the server (or slave)
     -- was attempting to perform the requested action.
   | SlaveDeviceFailure
     -- | Specialized use in conjunction with programming commands. The
     -- server (or slave) has accepted the request and is processing
     -- it, but a long duration of time will be required to do so. This
     -- response is returned to prevent a timeout error from occurring
     -- in the client (or master). The client (or master) can next
     -- issue a Poll Program Complete message to determine if
     -- processing is completed.
   | Acknowledge
     -- | Specialized use in conjunction with programming commands. The
     -- server (or slave) is engaged in processing a long–duration
     -- program command. The client (or master) should retransmit the
     -- message later when the server (or slave) is free.
   | SlaveDeviceBusy
     -- | Specialized use in conjunction with function codes
     -- 'ReadFileRecord' and 'WriteFileRecord' and reference type 6, to
     -- indicate that the extended file area failed to pass a
     -- consistency check.
   | MemoryParityError
     -- | Specialized use in conjunction with gateways, indicates that
     -- the gateway was unable to allocate an internal communication
     -- path from the input port to the output port for processing the
     -- request. Usually means that the gateway is misconfigured or
     -- overloaded.
   | GatewayPathUnavailable
     -- | Specialized use in conjunction with gateways, indicates that
     -- no response was obtained from the target device. Usually means
     -- that the device is not present on the network.
   | GatewayTargetDeviceFailedToRespond
     deriving (Eq, Show)

instance Serialize ExceptionCode where
  put = putWord8 . enc
    where
      enc IllegalFunction                    = 0x01
      enc IllegalDataAddress                 = 0x02
      enc IllegalDataValue                   = 0x03
      enc SlaveDeviceFailure                 = 0x04
      enc Acknowledge                        = 0x05
      enc SlaveDeviceBusy                    = 0x06
      enc MemoryParityError                  = 0x08
      enc GatewayPathUnavailable             = 0x0A
      enc GatewayTargetDeviceFailedToRespond = 0x0B

  get = getWord8 >>= dec
    where
      dec 0x01 = return IllegalFunction
      dec 0x02 = return IllegalDataAddress
      dec 0x03 = return IllegalDataValue
      dec 0x04 = return SlaveDeviceFailure
      dec 0x05 = return Acknowledge
      dec 0x06 = return SlaveDeviceBusy
      dec 0x08 = return MemoryParityError
      dec 0x0A = return GatewayPathUnavailable
      dec 0x0B = return GatewayTargetDeviceFailedToRespond
      dec _    = mzero

data ModbusException
   = ExceptionResponse !FunctionCode !ExceptionCode
   | DecodeException !String
   | CommandTimeout
     -- ^ A command took longer than 'connCommandTimeout'
     -- microseconds.
   | NoTriesAllowed
     -- ^ Command attempted while 'connMaxTries' <= 0. This is a
     -- configuration error.
   | OtherException !String
     deriving (Eq, Show, Typeable)

instance Exception ModbusException

-- | Sends a raw MODBUS command.
command
    :: TransactionId
    -> ProtocolId
    -> UnitId
    -> FunctionCode -- ^ PDU function code.
    -> ByteString   -- ^ PDU data.
    -> Session TCP_ADU
command tid pid uid fc fdata = do
    conn <- ask
    Session $ lift $ withConn conn
  where
    withConn :: Connection -> ExceptT ModbusException IO TCP_ADU
    withConn conn = go 0 Nothing
      where
        go :: Int -> Maybe ModbusException -> ExceptT ModbusException IO TCP_ADU
        go tries mbLastError
            | tries >= connMaxTries conn =
                throwError $ fromMaybe NoTriesAllowed mbLastError
            | otherwise =
                catchError
                  (command' conn tid pid uid fc fdata)
                  (\err -> go (tries + 1) (Just err))

command'
    :: Connection
    -> TransactionId
    -> ProtocolId
    -> UnitId
    -> FunctionCode -- ^ PDU function code.
    -> ByteString   -- ^ PDU data.
    -> ExceptT ModbusException IO TCP_ADU
command' conn tid pid uid fc fdata = do
    mbResult <- liftIO $ timeout (connCommandTimeout conn) $ do
      void $ connWrite conn (Cereal.encode cmd)
      connRead conn 512
    result <- maybe (throwError CommandTimeout) pure mbResult

    adu <- withExceptT DecodeException $ ExceptT $ pure $ Cereal.decode result
    case aduFunction adu of
      ExceptionCode rc ->
          throwError
            $ either DecodeException (ExceptionResponse rc)
            $ Cereal.decode (aduData adu)
      _ -> pure adu
  where
    cmd = TCP_ADU (Header tid pid (fromIntegral $ 2 + BS.length fdata) uid)
                  fc
                  fdata

readCoils
    :: TransactionId
    -> ProtocolId
    -> UnitId
    -> Word16
    -> Word16
    -> Session [Word8]
readCoils tid pid uid addr count =
    withAduData tid pid uid ReadCoils
                (putWord16be addr >> putWord16be count)
                decodeW8s

readDiscreteInputs
    :: TransactionId
    -> ProtocolId
    -> UnitId
    -> Word16
    -> Word16
    -> Session [Word8]
readDiscreteInputs tid pid uid addr count =
    withAduData tid pid uid ReadDiscreteInputs
                (putWord16be addr >> putWord16be count)
                decodeW8s

readHoldingRegisters
    :: TransactionId
    -> ProtocolId
    -> UnitId
    -> Word16 -- ^ Register starting address.
    -> Word16 -- ^ Quantity of registers.
    -> Session [Word16]
readHoldingRegisters tid pid uid addr count =
    withAduData tid pid uid ReadHoldingRegisters
                (putWord16be addr >> putWord16be count)
                decodeW16s

readInputRegisters
    :: TransactionId
    -> ProtocolId
    -> UnitId
    -> Word16 -- ^ Starting address.
    -> Word16 -- ^ Quantity of input registers.
    -> Session [Word16]
readInputRegisters tid pid uid addr count =
    withAduData tid pid uid ReadInputRegisters
                (putWord16be addr >> putWord16be count)
                decodeW16s

writeSingleCoil
    :: TransactionId
    -> ProtocolId
    -> UnitId
    -> Word16
    -> Bool
    -> Session ()
writeSingleCoil tid pid uid addr value =
    void $ command tid pid uid WriteSingleCoil
                   (runPut $ putWord16be addr >> putWord16be value')
  where
    value' | value     = 0xFF00
           | otherwise = 0x0000

writeSingleRegister
    :: TransactionId
    -> ProtocolId
    -> UnitId
    -> Word16 -- ^ Register address.
    -> Word16 -- ^ Register value.
    -> Session ()
writeSingleRegister tid pid uid addr value =
    void $ command tid pid uid WriteSingleRegister
                   (runPut $ putWord16be addr >> putWord16be value)

writeMultipleRegisters
    :: TransactionId
    -> ProtocolId
    -> UnitId
    -> Word16 -- ^ Register starting address
    -> [Word16] -- ^ Register values to be written
    -> Session Word16
writeMultipleRegisters tid pid uid addr values =
    withAduData tid pid uid WriteMultipleRegisters
                (do putWord16be addr
                    putWord16be $ fromIntegral numRegs
                    putWord8    $ fromIntegral numRegs
                    mapM_ putWord16be values
                )
                (getWord16be >> getWord16be)
  where
    numRegs :: Int
    numRegs = length values

--------------------------------------------------------------------------------

withAduData
    :: TransactionId
    -> ProtocolId
    -> UnitId
    -> FunctionCode
    -> Put -- ^ PDU data
    -> Get a -- ^ Parser of resulting 'aduData'
    -> Session a
withAduData tid pid uid fc fdata parser = do
    adu <- command tid pid uid fc (runPut fdata)
    Session $ lift $ withExceptT DecodeException $ ExceptT $ pure $ runGet parser $ aduData adu

decodeW8s :: Get [Word8]
decodeW8s = do n <- getWord8
               replicateM (fromIntegral n) getWord8

decodeW16s :: Get [Word16]
decodeW16s = do n <- getWord8
                replicateM (fromIntegral $ n `div` 2) getWord16be
