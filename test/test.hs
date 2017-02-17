{-# language PackageImports #-}
{-# language ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           "base" Data.Semigroup ( (<>) )
import qualified "attoparsec" Data.Attoparsec.ByteString as AB
import qualified "bytestring" Data.ByteString.Builder as BB
import qualified "bytestring" Data.ByteString as B
import qualified "bytestring" Data.ByteString.Lazy as BL
import           "modbus-tcp" Data.Range ( Range )
import qualified "modbus-tcp" Data.Range as Range
import           "modbus-tcp" Network.Modbus.TCP
import           "modbus-tcp" Network.Modbus.TCP.Internal.Protocol
import qualified "QuickCheck" Test.QuickCheck as QC
import           "tasty" Test.Tasty
import qualified "tasty-quickcheck" Test.Tasty.QuickCheck as QC

--------------------------------------------------------------------------------

instance QC.Arbitrary B.ByteString where
    arbitrary = B.pack <$> QC.arbitrary
    shrink xs = B.pack <$> QC.shrink (B.unpack xs)

instance QC.Arbitrary TransactionId where
    arbitrary = TransactionId <$> QC.arbitrary
    shrink = map TransactionId . QC.shrink . unTransactionId

instance QC.Arbitrary ProtocolId where
    arbitrary =
        QC.oneof
          [ pure ModbusTcp
          , OtherProtocolId <$> QC.arbitrary `QC.suchThat` (/= 0)
          ]

instance QC.Arbitrary UnitId where
    arbitrary = UnitId <$> QC.arbitrary
    shrink = map UnitId . QC.shrink . unUnitId

instance QC.Arbitrary Address where
    arbitrary = Address <$> QC.arbitrary
    shrink = map Address . QC.shrink . unAddress

instance (QC.Arbitrary a, Ord a) => QC.Arbitrary (Range a) where
    arbitrary = Range.fromBounds <$> QC.arbitrary <*> QC.arbitrary
    shrink range = uncurry Range.fromBounds <$> QC.shrink (Range.begin range, Range.end range)

instance QC.Arbitrary PDU where
    arbitrary =
      PDU
      <$> QC.arbitrary
      <*> QC.arbitrary

instance QC.Arbitrary TPU where
    arbitrary =
      TPU
      <$> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary

instance QC.Arbitrary Header where
    arbitrary =
        Header
        <$> QC.arbitrary
        <*> QC.choose (0, 252)

instance QC.Arbitrary ADU where
    arbitrary = do
        header <- QC.arbitrary
        pdu <- QC.arbitrary
        pure
          ADU
          { aduHeader = header{hdrLength = fromIntegral (B.length (pduData pdu)) + 2}
          , aduPdu = pdu
          }

instance QC.Arbitrary FunctionCode where
    arbitrary = QC.oneof
        [ pure ReadCoils
        , pure ReadDiscreteInputs
        , pure ReadHoldingRegisters
        , pure ReadInputRegisters
        , pure WriteSingleCoil
        , pure WriteSingleRegister
        , pure ReadExceptionStatus
        , pure Diagnostics
        , pure GetCommEventCounter
        , pure GetCommEventLog
        , pure WriteMultipleCoils
        , pure WriteMultipleRegisters
        , pure ReportSlaveID
        , pure ReadFileRecord
        , pure WriteFileRecord
        , pure MaskWriteRegister
        , pure ReadWriteMultipleRegisters
        , pure ReadFIFOQueue
        , pure EncapsulatedInterfaceTransport
        , UserDefinedCode <$>
            QC.elements ([0x41..0x48] <> [0x64..0x6e])
        , ReservedCode <$>
            QC.elements [0x09, 0x0a, 0x0d, 0x0e, 0x29, 0x2a, 0x5a, 0x5b, 0x7d, 0x7e, 0x7f]
        , OtherCode <$>
            QC.elements
            (  [0x12 .. 0x13]
            <> [0x19 .. 0x28]
            <> [0x2c .. 0x40]
            <> [0x49 .. 0x4f]
            <> [0x5c .. 0x63]
            <> [0x6f .. 0x7c]
            )
        , ExceptionCode <$> QC.arbitrary `QC.suchThat` isNotExceptionCode
        ]

isNotExceptionCode :: FunctionCode -> Bool
isNotExceptionCode (ExceptionCode _) = False
isNotExceptionCode _                 = True

instance QC.Arbitrary ExceptionCode where
    arbitrary = QC.elements
        [ IllegalFunction
        , IllegalDataAddress
        , IllegalDataValue
        , SlaveDeviceFailure
        , Acknowledge
        , SlaveDeviceBusy
        , MemoryParityError
        , GatewayPathUnavailable
        , GatewayTargetDeviceFailedToRespond
        ]

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "modbus-tcp"
  [ testGroup "codecs"
    [ QC.testProperty "ProtocolId"    $ propCodec protocolIdParser protocolIdBuilder
    , QC.testProperty "ExceptionCode" $ propCodec exceptionCodeParser exceptionCodeBuilder
    , QC.testProperty "FunctionCode"  $ propCodec functionCodeParser functionCodeBuilder
    , QC.testProperty "Header"        $ propCodec headerParser headerBuilder
    , QC.testProperty "ADU"           $ propCodec aduParser aduBuilder
    ]
  , testGroup "AddressRange"
    [ QC.testProperty "propAddressRangeOrdered" propAddressRangeOrdered
    ]
  ]

propCodec
    :: forall a. (Eq a, Show a)
    => AB.Parser a
    -> (a -> BB.Builder)
    -> a
    -> QC.Property
propCodec parser builder x =
    either (\errMsg -> QC.counterexample errMsg False)
           (\x' -> x' QC.=== x) $
      AB.parseOnly
        (parser <* AB.endOfInput)
        xBs
  where
    xBs = BL.toStrict $ BB.toLazyByteString $ builder x

propAddressRangeOrdered :: Range Address -> Bool
propAddressRangeOrdered range = Range.begin range <= Range.end range
