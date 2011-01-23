{-# LANGUAGE TypeSynonymInstances #-}
module JavaBinary where

import Control.Applicative
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Binary.Put
import Data.ByteString.Lazy
import Data.ByteString.Lazy.UTF8
import Data.Int

class JavaBinary a where
  getJ :: Get a
  putJ :: a -> Put

instance JavaBinary Int8 where
  getJ = fromIntegral <$> getWord8
  putJ = putWord8 . fromIntegral

instance JavaBinary Int16 where
  getJ = fromIntegral <$> getWord16be
  putJ = putWord16be . fromIntegral

instance JavaBinary Int32 where
  getJ = fromIntegral <$> getWord32be
  putJ = putWord32be . fromIntegral

instance JavaBinary Int64 where
  getJ = fromIntegral <$> getWord64be
  putJ = putWord64be . fromIntegral

instance JavaBinary Double where
  getJ = getFloat64be
  putJ = putFloat64be

instance JavaBinary Float where
  getJ = getFloat32be
  putJ = putFloat32be

instance JavaBinary Bool where
  getJ = ((0 :: Int8) /=) `fmap` getJ
  putJ False = putJ (0 :: Int8)
  putJ True = putJ (1 :: Int8)

instance JavaBinary String where
  getJ = do
    len <- getWord32be
    bs  <- getLazyByteString (fromIntegral len)
    return (toString bs)
  putJ xs = do
    let bs = fromString xs
    putWord32be (fromIntegral (Data.ByteString.Lazy.length bs))
    putLazyByteString bs

instance (JavaBinary a, JavaBinary b) => JavaBinary (a,b) where
  getJ = liftA2 (,) getJ getJ
  putJ (x,y) = putJ x *> putJ y

instance (JavaBinary a, JavaBinary b, JavaBinary c) => JavaBinary (a,b,c) where
  getJ = liftA3 (,,) getJ getJ getJ
  putJ (x,y,z) = putJ x *> putJ y *> putJ z
