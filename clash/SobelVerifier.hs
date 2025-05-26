{-# LANGUAGE CPP #-}
module SobelVerifier where

import           Data.Maybe (fromJust, isJust)
--import Data.Text.IO  (writeFile)

import           Clash.Prelude hiding (writeFile)

import           Axi
import           Clash.Explicit.Testbench
import           Clash.Prelude.Testbench as CPT (assert)
import           Common
import           ConvTbData
import           SobelTbData

-- Note: assuming no reuse & convFeaturesCols = 362 thus about 372 is enough to test whole system
-- Cycles:
-- * Reset: 1 cycle
-- * Kernel cycles: kernelRows * kernelCols = 3 * 3 = 9
-- * Image cycles: getImageLen False 372 = 3348
-- * Fill Up: 1 cycle
-- ----------------------------------------------------- +
-- Total cycles: 3359
rSobelVerifier :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom (Maybe (Axi4Stream (Signed 32) (Unsigned 4))) -- AXIS Slave
  -> Signal dom Bool
rSobelVerifier s_axis = match
  where
    vld = mux (isJust <$> s_axis) (pure True) (pure False)
    vld' = register False vld
    s_axis_tdata = mux (isJust <$> s_axis) (tData <$> (fromJust <$> s_axis)) (pure 0)
    s_axis_tdata' = register (0 :: Signed 32) s_axis_tdata

    bram = $(memBlobTH (Just 0) (sobelFilteredExp convFeaturesSubset False))
    -- Read only Block RAM
    bramOut = blockRamBlob bram rd (pure Nothing)
    rd = register (0 :: Index CONV_FEATURES_SUBSET) rd'
    -- DO NOT read the first cycle as it is not unpackable
    running = register (False :: Bool) (pure True)

    bramOut' = mux running (fromJust <$> unpack <$> bramOut) (pure 0)

    -- Note: assert doesnt seems to work in GTKWave, so disable if outside of clashi
#ifdef WAVE_SIM
    err = mux (running .&&. vld') (mux (s_axis_tdata' ./=. bramOut') (pure True) (pure False)) (pure False)
#else
    err = mux (running .&&. vld') (CPT.assert "Error s_axis_tdata' != bramOut'" s_axis_tdata' bramOut' (pure False)) (pure False)
#endif

    -- Take the successor of the read address if input valid;
    -- Become exception if under/overflow
    rd' = mux (running .&&. vld .&&. rd ./=. pure (maxBound :: Index CONV_FEATURES_SUBSET)) (satSucc SatError <$> rd) rd
    match = mux (rd .==. pure (maxBound :: Index CONV_FEATURES_SUBSET) .&&. not <$> err) (pure True) err

-- rSobelVerifierDump :: IO ()
-- rSobelVerifierDump = do
--   let out = exposeClockResetEnable rSobelVerifier systemClockGen systemResetGen enableGen
--   vcd <- dumpVCD (0,100) out ["rd"]
--   case vcd of
--     Left msg ->
--       error msg
--     Right contents ->
--       writeFile "rSobelVerifier.vcd" contents


{-# NOINLINE sobelVerifier #-}
{-# ANN sobelVerifier
  (Synthesize
    { t_name   = "sobelVerifier"
    , t_inputs = [
      PortName "aclk",
      PortName "nrst",
      PortProduct "s_axis"
        [ PortName "tvalid",
          PortProduct ""
          [ PortName "tdata",
            PortName "tlast",
            PortName "tkeep"
          ]
        ]
    ]
    , t_output = PortName "match"
    }) #-}
sobelVerifier
  :: Clock ConvAccelSystem
  -> Reset ConvAccelSystem
  -> Signal ConvAccelSystem (Maybe (Axi4Stream (Signed 32) (Unsigned 4)))
  -> Signal ConvAccelSystem Bool
sobelVerifier clk rst = exposeClockResetEnable rSobelVerifier clk rst enableGen

