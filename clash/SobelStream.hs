module SobelStream where

import           Clash.Prelude

import           Axi
import           Common

import           SobelTbData

axisBlob :: IO ()
axisBlob = do
  let content0 =
        $(memBlobTH
            (Just 0)
            [ (Nothing, True)
            , (Just (Axi4Stream (2 :: Signed 32) True (0 :: Unsigned 4)), False)
            ])
  let pr = mapM_ (putStrLn . show)
  pr $ unpackMemBlob content0

axisBlob' :: MemBlob 5 39
axisBlob' =
  $(memBlobTH
      (Just 0)
      [ (Just (Axi4Stream (125 :: Signed 32) False (15 :: Unsigned 4)), False)
      , (Nothing, True)
      , (Just (Axi4Stream (-273 :: Signed 32) False (15 :: Unsigned 4)), False)
      , (Nothing, True)
      , (Just (Axi4Stream (12 :: Signed 32) True (15 :: Unsigned 4)), False)
      ])

axisBlobStream :: HiddenClockResetEnable dom => Signal dom (BitVector 39)
axisBlobStream = bram
  where
    -- Read only Block RAM; Set initial content to the axisBlob'
    bram = blockRamBlob axisBlob' rd (pure Nothing)
    rd = register (0 :: Index 3) (pure 0)

-- -- Note: this is how to pass dom along to avoid the errors with wrong dom5 etc
axiStream ::
     forall dom. (HiddenClockResetEnable dom)
  => Signal dom (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)
axiStream = o
  where
    -- Read only Block RAM
    bram = blockRamBlob axisBlob' rd (pure Nothing)
    rd = register (0 :: Index 5) rd'
    -- DO NOT read the first cycle as it is not unpackable
    running = register (False :: Bool) (pure True)
    -- Take the successor of the read address; Become expecption if under/overflow
    rd' = mux running (satSucc SatError <$> rd) rd
    o = mux running (unpack <$> bram) (pure (Nothing, True))

axisBlobStreamTc :: IO ()
axisBlobStreamTc = mapM_ print $ sampleN @System 5 axisBlobStream

axiStreamTc :: IO ()
axiStreamTc = mapM_ print $ sampleN @System 8 axiStream

-- Note: no use in making this function generic, as the sobelSeq already constraints the input
sobelStream ::
     HiddenClockResetEnable dom
  => Signal dom Bool -- m_axis_tready
  -> Signal
       dom
       ( Maybe (Axi4Stream (Signed 32) (Unsigned 4)) -- AXIS Master,
       , Bool -- s_axis_tready
         )
    --Index CONV_FEATURES_SUBSET, -- rd
sobelStream m_axis_tready =
  bundle
  --rd,
    (m_axis, s_axis_tready)
  where
    bram = $(memBlobTH (Just 0) (getSER $ sobelSeq SIMPLE_NO_REUSE seqSubset False))
    -- Read only Block RAM
    bramOut = blockRamBlob bram rd (pure Nothing)
    -- Delay read pointer
    -- In case m_axis_tready is False we need to retransmit as previous value, as BRAM data lags 1 behind
    rd = register (0 :: Index SEQ_SUBSET) rd'
    rd' = register (0 :: Index SEQ_SUBSET) rd''
    -- Cycle    rd    BRAM(rd)
    -- 0        0     unpackable
    -- 1        0     BRAM(0)
    -- 2        1     BRAM(0)
    -- 3        2     BRAM(1)
    -- 4        3     BRAM(2)
    -- DO NOT read the first cycle as it is not unpackable
    running = register (False :: Bool) (pure True)
    -- DO NOT sent the 2nd cycle as it is duplicated on the 3rd cycle
    block = register (False :: Bool) running
    -- External slave is ready to receive
    -- Take the successor of the read address; Become expecption if under/overflow
    rd'' = mux (running .&&. m_axis_tready) (satSucc SatError <$> rd) rd
    -- If both delayed read pointer equals current read pointer, don't sent as it is a duplicate
    m_axis_s_axis_tready =
      mux
        (block .&&. rd ./=. rd' .&&. rd ./=. pure maxBound)
        (unpack <$> bramOut)
        (pure (Nothing, True))
    (m_axis, s_axis_tready) = unbundle m_axis_s_axis_tready

{-# NOINLINE sobelStreamer #-}
{-# ANN sobelStreamer
  (Synthesize
   { t_name   = "sobelStreamer"
    , t_inputs = [
      PortName "aclk",
      PortName "nrst",
      PortName "m_axis_tready"
    ]
   , t_output = PortProduct ""
        [ PortProduct "m_axis"
          [ PortName "tvalid",
            PortProduct ""
            [
              PortName "tdata",
              PortName "tlast",
              PortName "tkeep"
           ]
          ]
        , PortName "s_axis_tready"
        ]
    }) #-}
sobelStreamer ::
     Clock ConvAccelSystem
  -> Reset ConvAccelSystem
  -> Signal ConvAccelSystem Bool -- s_axis_tready
  -> Signal
       ConvAccelSystem
       ( Maybe (Axi4Stream (Signed 32) (Unsigned 4)) -- AXIS Master
       , Bool)
    --Index CONV_FEATURES_SUBSET, -- rd
sobelStreamer clk rst = exposeClockResetEnable sobelStream clk rst enableGen

sobelStreamTc :: Int -> IO ()
sobelStreamTc x =
  mapM_ print
    $ simulateN @System
        x
        sobelStream
        [ True :: Bool
        , True
        , True
        , False
        , True
        , True
        , True
        , True
        , True
        , True
        , True
        , True
        , False
        , True
        , True
        , True
        , True
        , True
        ]
