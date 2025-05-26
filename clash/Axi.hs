{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Axi where

import           Clash.Prelude as CP
import qualified Clash.Sized.Internal.BitVector as BV
import           Data.List as L (concatMap, map, (++))
import           Data.Proxy

-- Note: do tvalid with Maybe to comply with clash standard
-- We cannot include tready as it is of another valid
data Axi4Stream a n = Axi4Stream {
  tData :: a,
  tLast :: Bool,
  tKeep :: n
} deriving (Generic, NFDataX, Read, Show, ShowX)

instance (Eq a, Eq n) => Eq (Axi4Stream a n) where
  (Axi4Stream tdata1 tlast1 tkeep1) == (Axi4Stream tdata2 tlast2 tkeep2) =
    tdata1 == tdata2 && tlast1 == tlast2 && tkeep1 == tkeep2

instance (Lift a, Lift n) => Lift (Axi4Stream a n) where
  lift (Axi4Stream tdata tlast tkeep) =
    [| Axi4Stream $(lift tdata) $(lift tlast) $(lift tkeep) |]

instance (Num a, Num n) => Num (Axi4Stream a n) where
  (+) (Axi4Stream tdata1 tlast1 tkeep1) (Axi4Stream tdata2 _ _ ) =
    Axi4Stream (tdata1 + tdata2) tlast1 tkeep1
  (-) (Axi4Stream tdata1 tlast1 tkeep1) (Axi4Stream tdata2 _ _) =
    Axi4Stream (tdata1 - tdata2) tlast1 tkeep1
  (*) (Axi4Stream tdata1 tlast1 tkeep1) (Axi4Stream tdata2 _ _) =
    Axi4Stream (tdata1 * tdata2) tlast1 tkeep1




instance forall a n . (BitPack a, BitPack n,
  KnownNat (BitSize a), KnownNat (BitSize n))
  => BitPack (Axi4Stream a n) where

  type BitSize (Axi4Stream a n) = BitSize a + BitSize n + 1
  -- Create a BitVector
  pack (Axi4Stream { tData = tdata, tLast = tlast, tKeep = tkeep}) =
    pack tkeep ++# pack tlast ++# pack tdata

  unpack bv =
    let (axis, tdata) = BV.split# @(BitSize a) bv
        (tkeep, tlast) = BV.split# @1 axis
    in (Axi4Stream {
      tData = unpack tdata,
      tLast = unpack tlast,
      tKeep = unpack tkeep})

type family BitSize' a :: Nat where
    BitSize' (Maybe (Axi4Stream n k), Bool) = (BitSize (Axi4Stream n k) + 2)

type BITSIZE_AXIS = BitSize (Axi4Stream (Signed 32) (Unsigned 4))

getBitsizeAxis :: Proxy BITSIZE_AXIS
getBitsizeAxis = Proxy

type BITSIZE_BRAM = BitSize' (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)

getBitsizeBram :: Proxy BITSIZE_BRAM
getBitsizeBram = Proxy @BITSIZE_BRAM




keepToList :: forall n k . (KnownNat n, 1 <= n, Num k) => Axi4Stream (Vec n k) (Vec n Bool) -> [k]
keepToList axis = lst
  where
    tdata = tData axis
    idxs = iterate (SNat :: SNat n) (+1) 0
    lst = L.concatMap (\(keep,i) -> if keep then [tdata !! i] else []) (zip (tKeep axis) idxs)

keepToListTc :: [Signed 8]
keepToListTc = o
  where
    axis = Axi4Stream {
      tData  = 1 :> -2 :> 3 :> 14 :> Nil,
      tLast  = False,
      tKeep  = True :> True :> True :> False :> Nil
    } :: Axi4Stream (Vec 4 (Signed 8)) (Vec 4 Bool)
    o = keepToList axis

-- E.g. Setup the data as 4 Signed 8 items, which allows for parallel processing
-- Note that this requires 4 keep bits
axi4StreamTc0 :: Axi4Stream (Vec 4 (Signed 8)) (Vec 4 Bool)
axi4StreamTc0 = Axi4Stream {
  tData  = 1 :> -2 :> 3 :> 14 :> Nil,
  tLast  = False,
  tKeep  = True :> True :> True :> False :> Nil
}

-- or try a single item, now only 1 keep bit is required
axi4StreamTc1 :: Axi4Stream (Unsigned 32) Bool
axi4StreamTc1 = Axi4Stream {
  tData  = 31,
  tLast  = False,
  tKeep  = True
}

axi4StreamTc2 :: (Unsigned 32, Bool, Bool)
axi4StreamTc2 = o
  where
    axis = axi4StreamTc1
    o = (tData axis, tLast axis, tKeep axis)

axi4StreamTc3 :: Maybe (Axi4Stream (Unsigned 32) Bool)
axi4StreamTc3 = Just Axi4Stream {
  tData  = 31,
  tLast  = False,
  tKeep  = True
}

axi4StreamBitPackTc0 :: BitVector 37
axi4StreamBitPackTc0 = pack (Axi4Stream (40 :: Unsigned 32) False (15 :: Unsigned 4))


axi4StreamBitPackTc1 :: Axi4Stream (Unsigned 32) (Unsigned 4)
axi4StreamBitPackTc1 = unpack axi4StreamBitPackTc0


axi4StreamBitPackTc2 :: [Maybe (Axi4Stream (Unsigned 32) (Unsigned 4))]
axi4StreamBitPackTc2 = o
  where
    bv = L.map pack [Nothing, Just (Axi4Stream (40 :: Unsigned 32) False (15 :: Unsigned 4))]
    o = L.map unpack bv

