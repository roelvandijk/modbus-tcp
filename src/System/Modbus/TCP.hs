{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax  #-}

module System.Modbus.TCP
  ( TCP_ADU(..)
  , Header(..)
  , FunctionCode(..)
  , MB_Exception(..)
  , Exception(..)

  , command

  , readCoils
  , readDiscreteInputs
  , readHoldingRegisters
  , readInputRegisters
  , writeSingleCoil
  , writeSingleRegister
  , writeMultipleRegisters
  ) where

import "base" Control.Applicative ( (<*>) )
import "base" Control.Monad ( replicateM )
import "base" Data.Functor ( (<$>) )
import "base" Data.Word ( Word8, Word16 )
import "base-unicode-symbols" Data.Bool.Unicode     ( (∧), (∨) )
import "base-unicode-symbols" Data.List.Unicode     ( (∈) )
import "base-unicode-symbols" Data.Ord.Unicode      ( (≤), (≥) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "cereal" Data.Serialize
  ( Serialize, put, get, Get
  , encode, decode
  , runPut, runGet
  , putWord8, putWord16be
  , getWord8, getWord16be
  , getByteString
  )
import           "bytestring" Data.ByteString ( ByteString )
import qualified "bytestring" Data.ByteString as BS
import qualified "network" Network.Socket as S hiding ( send, recv )
import qualified "network" Network.Socket.ByteString as S ( send, recv )


type TransactionIdentifier = Word16
type ProtocolIdentifier    = Word16
type UnitIdentifier        = Word8

-- | MODBUS TCP/IP Application Data Unit
data TCP_ADU =
  TCP_ADU { aduHeader   ∷ Header
          , aduFunction ∷ FunctionCode
          , aduData     ∷ ByteString
          } deriving Show

instance Serialize TCP_ADU where
  put (TCP_ADU header fc ws) = put header >> put fc >> mapM_ putWord8 (BS.unpack ws)
  get = do
    header ← get
    fc     ← get
    ws     ← getByteString $ fromIntegral (hdrLength header) - 2
    return $ TCP_ADU header fc ws

-- | MODBUS Application Protocol Header
data Header =
  Header { hdrTransactionIdentifer ∷ TransactionIdentifier
         , hdrProtocolIdentifier   ∷ ProtocolIdentifier
         , hdrLength               ∷ Word16
         , hdrUnitIdentifier       ∷ UnitIdentifier
         } deriving Show

instance Serialize Header where
  put (Header tid pid len uid) =
    putWord16be tid >> putWord16be pid >> putWord16be len >> putWord8 uid
  get = Header <$> getWord16be <*> getWord16be <*> getWord16be <*> getWord8

data FunctionCode =
    ReadCoils
  | ReadDiscreteInputs
  | ReadHoldingRegisters
  | ReadInputRegisters
  | WriteSingleCoil
  | WriteSingleRegister
  | ReadExceptionStatus
  | Diagnostics
  | GetCommEventCounter
  | GetCommEventLog
  | WriteMultipleCoils
  | WriteMultipleRegisters
  | ReportSlaveID
  | ReadFileRecord
  | WriteFileRecord
  | MaskWriteRegister
  | ReadWriteMultipleRegisters
  | ReadFIFOQueue
  | EncapsulatedInterfaceTransport
  | UserDefinedCode   Word8
  | ReservedCode      Word8
  | OtherCode         Word8
  | ExceptionCode FunctionCode
    deriving Show

instance Serialize FunctionCode where
  put = putWord8 ∘ enc
    where
      enc ∷ FunctionCode → Word8
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

  get = getWord8 >>= return ∘ dec
    where
      dec ∷ Word8 → FunctionCode
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
      dec code | (code ≥  65 ∧ code ≤  72)
               ∨ (code ≥ 100 ∧ code ≤ 110) = UserDefinedCode code
               | code ∈ [9, 10, 13, 14, 41, 42, 90, 91, 125, 126, 127]
                 = ReservedCode code
               | code ≥ 0x80 = ExceptionCode $ dec $ code - 0x80
               | otherwise = OtherCode code

data MB_Exception =
    IllegalFunction
  | IllegalDataAddress
  | IllegalDataValue
  | SlaveDeviceFailure
  | Acknowledge
  | SlaveDeviceBusy
  | MemoryParityError
  | GatewayPathUnavailable
  | GatewayTargetDeviceFailedToRespond
    deriving Show

instance Serialize MB_Exception where
  put = putWord8 ∘ enc
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

  get = getWord8 >>= return ∘ dec
    where
      dec 0x01 = IllegalFunction
      dec 0x02 = IllegalDataAddress
      dec 0x03 = IllegalDataValue
      dec 0x04 = SlaveDeviceFailure
      dec 0x05 = Acknowledge
      dec 0x06 = SlaveDeviceBusy
      dec 0x08 = MemoryParityError
      dec 0x0A = GatewayPathUnavailable
      dec 0x0B = GatewayTargetDeviceFailedToRespond
      dec _    = error "TODO"

data Exception = ExceptionResponse FunctionCode MB_Exception
               | DecodeException String
               | OtherException String
                 deriving Show

command ∷ TransactionIdentifier
        → ProtocolIdentifier
        → UnitIdentifier
        → FunctionCode
        → ByteString
        → S.Socket
        → IO (Either Exception TCP_ADU)
command tid pid uid fc fdata socket = do
    _ ← S.send socket $ encode cmd
    result ← S.recv socket 512
    return $ either (Left ∘ DecodeException) checkResponse $ decode result
  where
    cmd = TCP_ADU (Header tid pid (fromIntegral $ 2 + BS.length fdata) uid)
                  fc
                  fdata

-- | Checks whether the response contains an error.
checkResponse ∷ TCP_ADU → Either Exception TCP_ADU
checkResponse adu@(TCP_ADU _ fc bs) =
    case fc of
      ExceptionCode rc → Left $ either DecodeException (ExceptionResponse rc)
                              $ decode bs
      _ → Right adu

readCoils ∷ TransactionIdentifier
          → ProtocolIdentifier
          → UnitIdentifier
          → Word16
          → Word16
          → S.Socket
          → IO (Either Exception [Word8])
readCoils tid pid uid addr count socket =
    either Left
           ( either (Left ∘ DecodeException) Right
           ∘ runGet decodeW8s ∘ aduData
           )
           <$> command tid pid uid ReadCoils
                       (runPut $ putWord16be addr >> putWord16be count)
                       socket

readDiscreteInputs ∷ TransactionIdentifier
                   → ProtocolIdentifier
                   → UnitIdentifier
                   → Word16
                   → Word16
                   → S.Socket
                   → IO (Either Exception [Word8])
readDiscreteInputs tid pid uid addr count socket =
    either Left
           ( either (Left ∘ DecodeException) Right
           ∘ runGet decodeW8s ∘ aduData
           )
           <$> command tid pid uid ReadDiscreteInputs
                       (runPut $ putWord16be addr >> putWord16be count)
                       socket

readHoldingRegisters ∷ TransactionIdentifier
                     → ProtocolIdentifier
                     → UnitIdentifier
                     → Word16 -- ^ Register starting address.
                     → Word16 -- ^ Quantity of registers.
                     → S.Socket
                     → IO (Either Exception [Word16])
readHoldingRegisters tid pid uid addr count socket =
    either Left
           ( either (Left ∘ DecodeException) Right
           ∘ runGet decodeW16s ∘ aduData
           )
           <$> command tid pid uid ReadHoldingRegisters
                       (runPut $ putWord16be addr >> putWord16be count)
                       socket

readInputRegisters ∷ TransactionIdentifier
                   → ProtocolIdentifier
                   → UnitIdentifier
                   → Word16 -- ^ Starting address.
                   → Word16 -- ^ Quantity of input registers.
                   → S.Socket
                   → IO (Either Exception [Word16])
readInputRegisters tid pid uid addr count socket =
    either Left
           ( either (Left ∘ DecodeException) Right
           ∘ runGet decodeW16s ∘ aduData
           )
           <$> command tid pid uid ReadInputRegisters
                       (runPut $ putWord16be addr >> putWord16be count)
                       socket

writeSingleCoil ∷ TransactionIdentifier
                → ProtocolIdentifier
                → UnitIdentifier
                → Word16
                → Bool
                → S.Socket
                → IO (Either Exception ())
writeSingleCoil tid pid uid addr value socket = do
    resp ← command tid pid uid WriteSingleCoil
                   (runPut $ putWord16be addr >> putWord16be (if value then 0xFF00 else 0))
                   socket
    return $ either Left (const $ Right ()) resp

writeSingleRegister ∷ TransactionIdentifier
                    → ProtocolIdentifier
                    → UnitIdentifier
                    → Word16 -- ^ Register address.
                    → Word16 -- ^ Register value.
                    → S.Socket
                    → IO (Either Exception ())
writeSingleRegister tid pid uid addr value socket = do
    resp ← command tid pid uid WriteSingleRegister
                   (runPut $ putWord16be addr >> putWord16be value)
                   socket
    return $ either Left (const $ Right ()) resp

writeMultipleRegisters ∷ TransactionIdentifier
                       → ProtocolIdentifier
                       → UnitIdentifier
                       → Word16 -- ^ Register starting address
                       → [Word16] -- ^ Register values to be written
                       → S.Socket
                       → IO (Either Exception Word16)
writeMultipleRegisters tid pid uid addr values socket =
    either Left
           ( either (Left ∘ DecodeException) Right
           ∘ runGet (getWord16be >> getWord16be) ∘ aduData
           )
           <$> command tid pid uid WriteMultipleRegisters
                       ( runPut $ do
                           putWord16be addr
                           putWord16be $ fromIntegral numRegs
                           putWord8 $ fromIntegral numRegs
                           mapM_ putWord16be values
                       )
                       socket
  where
    numRegs ∷ Int
    numRegs = length values

--------------------------------------------------------------------------------

decodeW8s ∷ Get [Word8]
decodeW8s = do n ← getWord8
               replicateM (fromIntegral n) getWord8

decodeW16s ∷ Get [Word16]
decodeW16s = do n ← getWord8
                replicateM (fromIntegral $ n `div` 2) getWord16be
