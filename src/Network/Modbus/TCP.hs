-- | An implementation of the Modbus TPC/IP protocol.
--
-- This implementation is based on the @MODBUS Application Protocol
-- Specification V1.1b@
-- (<http://www.modbus.org/docs/Modbus_Application_Protocol_V1_1b.pdf>).
module Network.Modbus.TCP
  ( -- * Sessions
    Session
  , runSession

  , Config(..)
  , RetryPredicate

    -- ** Workers
  , Worker
  , directWorker
  , batchWorker
  , BatchConfig(..)
  , BatchReadConfig(..)
  , defaultBatchConfig
  , defaultBatchReadConfig

    -- * Modbus Protocol
  , Response(..)
  , ADU(..)
  , PDU(..)
  , Header(..)
  , TPU(..)
  , FunctionCode(..)
  , ExceptionCode(..)
  , ModbusException(..)

  , TransactionId(..)
  , ProtocolId(..)
  , UnitId(..)
    -- ** Entity addresses
  , Address(..)
  , ToAddress(..)
    -- ** Entity numbers
  , CoilNumber
  , DiscreteInputNumber
  , InputRegisterNumber
  , HoldingRegisterNumber
  , mkCoilNumber
  , mkDiscreteInputNumber
  , mkInputRegisterNumber
  , mkHoldingRegisterNumber

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

import Network.Modbus.TCP.Internal.Batch
import Network.Modbus.TCP.Internal.Protocol
