{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ConvStudent where

import Axi
import Clash.Explicit.Testbench
import Clash.Prelude
import Common
import Control.Monad (when)
import ConvTbData
import Data.List qualified as L
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromJust, isJust)
import Debug.Trace
import SobelStream
import SobelTbData
import SobelVerifier
import Prelude qualified (zip)
import Clash.Explicit.Prelude (Applicative(liftA2))

-----------------------------------------------------------------------------------------
-- Clash Q1: First conv to AXIS
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- The conv function

conv ::
  (KnownNat a, SaturatingNum n) =>
  Vec a n -> -- Image
  Vec a n -> -- Kernel
  n -- Convolved Feature
conv vec1 vec2 = foldl (+) 0 (zipWith (*) vec1 vec2)

-----------------------------------------------------------------------------------------
-- Clocked conv function called rConv

rConv ::
  forall dom a n.
  (HiddenClockResetEnable dom, KnownNat a, NFDataX n, SaturatingNum n) =>
  Signal dom (Vec a n) ->
  Signal dom (Vec a n) ->
  Signal dom n
rConv vec1 vec2 = register 0 (conv <$> vec1 <*> vec2)

-----------------------------------------------------------------------------------------
-- Simulation of rConv

rConvTb ::
  forall dom a n.
  (HiddenClockResetEnable dom, KnownNat a, NFDataX n, SaturatingNum n) =>
  (Signal dom (Vec a n, Vec a n)) ->
  Signal dom n
rConvTb bvec =
  let (vec1, vec2) = unbundle bvec
   in rConv vec1 vec2

simRConvTb = simulate @System rConvTb testInput

testInput =
  [ (1 :> 2 :> Nil, 3 :> 4 :> Nil),
    (5 :> 6 :> Nil, 7 :> 8 :> Nil),
    (9 :> 10 :> Nil, 11 :> 12 :> Nil)
  ]

-----------------------------------------------------------------------------------------
-- State machine to handle input streams

conv1D ::
  (KnownNat a, SaturatingNum n) =>
  (Vec a n, Vec a n) -> -- STATE: Current kernel and image
  (ConvState, n) -> -- INPUT: ConvState and value that must be streamed in
  ( (Vec a n, Vec a n), -- STATE': New kernel and image
    n -- OUTPUT: Convolved feature
  )
conv1D (kernel, subImg) (state, input) =
  case state of
    LOAD_KERNEL ->
      let newKernel = input +>> kernel
       in ((newKernel, subImg), 0)
    LOAD_SUBIMG ->
      let newSubImg = input +>> subImg
       in ((kernel, newSubImg), 0)
    CONV ->
      let newSubImg = input +>> subImg
          result = conv kernel subImg
       in ((kernel, newSubImg), result)
    _ -> ((kernel, subImg), 0) -- default for future states

-----------------------------------------------------------------------------------------
-- Testing the conv1D function

conv1D' ::
  (KnownNat a, SaturatingNum n) =>
  (ConvState, Index a, Vec a n, Vec a n) -> -- STATE: current ConvState, counter, kernel and image
  n -> -- INPUT: New item either for the kernel or image
  ( (ConvState, Index a, Vec a n, Vec a n), -- STATE': new ConvState, counter, kernel and image
    n -- OUTPUT: Convolved feature
  )
conv1D' (state, counter, kernel, subImg) input =
  let ((newKernel, newSubImg), out) = conv1D (kernel, subImg) (state, input)
      counter' = succ counter

      -- check for end of 8-cycle period
      (nextState, nextCounter) =
        case state of
          LOAD_KERNEL -> if counter == maxBound then (LOAD_SUBIMG, 0) else (state, counter')
          LOAD_SUBIMG -> if counter == maxBound then (CONV, 0) else (state, counter')
          CONV -> (LOAD_SUBIMG, 1)
          _ -> (LOAD_KERNEL, 0)
   in ((nextState, nextCounter, newKernel, newSubImg), out)

-----------------------------------------------------------------------------------------
-- You can use the simulation function simConv1DTbPrint' to print out all the iner stages of the states
simConv1D' :: (Show a, SaturatingNum a) => [a] -> [Char]
simConv1D' = sim conv1D' (LOAD_KERNEL, 0, replicate d9 0, replicate d9 0)

simConv1DTb' :: String
simConv1DTb' = simConv1D' rockyFirstNoReuseInps

simConv1DTbPrint' = putStrLn simConv1DTb'

-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- Mealy machine and simulation

mConv1D' ::
  (HiddenClockResetEnable dom, KnownNat a, SaturatingNum n, NFDataX n) =>
  SNat a -> -- Depth vector
  Signal dom n -> -- New item either for the kernel or image
  Signal dom n -- Convolved feature
mConv1D' snat = mealy conv1D' (LOAD_KERNEL, 0, replicate snat 0, replicate snat 0)

input = [1, 2, 3, 4]

simMConv1DTb :: [Signed 16]
simMConv1DTb = simulate @System (mConv1D' d9) input

simMConv1DTbPrint = mapM_ print $ L.zip [1 ..] rockyFirstNoReuseInps

-----------------------------------------------------------------------------------------
-- Making an AXIS version of the conv1D function

axisConv1D ::
  forall a n k.
  (KnownNat a, SaturatingNum n, Num k) =>
  (ConvState, Index a, Vec a n, Vec a n, k) -> -- STATE: Current convState, counter, kernel, image, keep
  (Maybe (Axi4Stream n k), Bool) -> -- INPUT: AXIS slave, m_axis_tready
  ( (ConvState, Index a, Vec a n, Vec a n, k), -- STATE': New  convState, counter, kernel, image, keep
    (Maybe (Axi4Stream n k), Bool) -- OUTPUT: AXIS master, s_axis_tready
  )
axisConv1D (state, counter, kernel, subImg, keep) (s_axis, m_axis_tready) =
  let 
      counter' = succ counter

      vld =
        case (s_axis, s_axis_tready) of
          (Just _, True) -> True -- Incoming packet is valid & local slave is ready
          _ -> False

      (s_axis_tdata, s_axis_tlast, s_axis_tkeep) =
        case s_axis of
          (Just x) -> (tData x, tLast x, tKeep x) -- extract data from axi record
          _ -> (0, False, 0) -- default data

      s_axis_tready = not (state == CONV) && m_axis_tready

      m_axis_tvalid
        | state == CONV = True -- Convolutional feature available
        | otherwise = False -- Convolutional feature not available

      m_axis
        | m_axis_tvalid =
          Just
            Axi4Stream
              {tData = cf, tLast = s_axis_tlast, tKeep = s_axis_tkeep}
        | otherwise = Nothing

      ((newKernel, newSubImg), cf) = if vld || (m_axis_tvalid && m_axis_tready)
        then conv1D (kernel, subImg) (state, s_axis_tdata)
        else ((kernel, subImg), 0)

      (nextState, nextCounter) =
        case state of
          LOAD_KERNEL -> if vld then
              if counter == maxBound then (LOAD_SUBIMG, 0) else (state, counter')
            else (state, counter)
          LOAD_SUBIMG -> if vld then
            if counter == maxBound then (CONV, 0) else (state, counter')
            else (state, counter)
          CONV -> if m_axis_tready then
            (LOAD_SUBIMG, 1)
            else (state, counter)
          _ -> (LOAD_KERNEL, 0)

   in ((nextState, nextCounter, newKernel, newSubImg, s_axis_tkeep), (m_axis, s_axis_tready))

-----------------------------------------------------------------------------------------
-- You can use the simulation function simAxisConv1DTbPrint to print out all the iner stages of the states
simAxisConv1D :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)] -> String
simAxisConv1D = sim axisConv1D (LOAD_KERNEL, 0, replicate d9 0, replicate d9 0, 0)

simAxisConv1DTb :: String
simAxisConv1DTb = simAxisConv1D mAxisConv1DTbNoReuseInp

simAxisConv1DTbPrint = putStrLn simAxisConv1DTb

-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- The AXIS version in a mealy machine

mAxisConv1D ::
  (SaturatingNum n, Num k, NFDataX n, HiddenClockResetEnable dom, KnownNat p, NFDataX k) =>
  SNat p ->
  Signal dom (Maybe (Axi4Stream n k)) ->
  Signal dom Bool ->
  Signal dom (Maybe (Axi4Stream n k), Bool)
mAxisConv1D snat stream ready = mealy axisConv1D (LOAD_KERNEL, 0, replicate snat 0, replicate snat 0, 0) (bundle (stream, ready))

mAxisConv1DTb ::
  (HiddenClockResetEnable dom) =>
  Signal dom (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool) ->
  Signal dom (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)
mAxisConv1DTb s_axis = o
  where
    (s_axis', m_axis_tready) = unbundle s_axis
    o = mAxisConv1D d9 s_axis' m_axis_tready

simMAxisConv1DTb :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)]
simMAxisConv1DTb = simulate @System mAxisConv1DTb mAxisConv1DTbNoReuseInp

simMAxisConv1DTbPrint = mapM_ print $ L.zip [1 ..] simMAxisConv1DTb

-----------------------------------------------------------------------------------------
-- Clash Q2, Convolution over time
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
--  Implementation of serConv1D

serConv1D ::
  (KnownNat a, SaturatingNum n) =>
  (Vec a n, n, Index a) -> -- STATE: Current kernel, accumulator and counter
  (ConvState, n) -> -- INPUT: ConvState and value that must be streamed in
  ( (Vec a n, n, Index a), -- STATE': New kernel, accumulator and counter
    n -- OUTPUT: Convolved feature
  )
serConv1D (kernel, acc, counter) (state, input) =
  case state of
    LOAD_KERNEL ->
      let newKernel = kernel <<+ input
       in ((newKernel, acc, counter), -1)
    CONV ->
      let 
          newAcc = if counter == 0 
            then input * kernel !! 0
            else acc + input * kernel !! counter

       in ((kernel, newAcc, counter), newAcc)
    _ -> ((kernel, acc, counter), 0) -- default for future states

-----------------------------------------------------------------------------------------
--  Implementation of serConv1D'

serConv1D' ::
  (KnownNat a, SaturatingNum n) =>
  (ConvState, Vec a n, n, Index a) -> -- STATE: current ConvState, kernel, accumulator and counter
  n -> -- INPUT: New item either for the kernel or image
  ( (ConvState, Vec a n, n, Index a), -- STATE': new ConvState, kernel, accumulator and counter
    n -- OUTPUT: Convolved feature
  )
serConv1D' (state, kernel, acc, counter) input =
  let ((nextKernel, nextAcc, _), out) = serConv1D (kernel, acc, counter) (state, input)
      counter' = succ counter
      
      -- check for end of 8-cycle period
      (nextState, nextCounter) =
        case state of
          LOAD_KERNEL -> if counter == maxBound then (CONV, 0) else (state, counter')
          CONV -> if counter == maxBound then (CONV, 0) else (state, counter')
          _ -> (LOAD_KERNEL, 0)
   in ((nextState, nextKernel, nextAcc, nextCounter), out)

-----------------------------------------------------------------------------------------
-- You can use the simulation function simSerConv1DTbPrint' to print out all the iner stages of the states
simSerConv1D' :: (Show a, SaturatingNum a) => [a] -> [Char]
simSerConv1D' = sim serConv1D' (LOAD_KERNEL, replicate d9 0, 0, 0)

simSerConv1DTb' :: String
simSerConv1DTb' = simSerConv1D' rockyFirstNoReuseInps

simSerConv1DTbPrint' = putStrLn simSerConv1DTb'
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
--  Testing the serConv1D' using mSerConv1D'

-- Use mealy below:
mSerConv1D' ::
  (HiddenClockResetEnable dom, SaturatingNum n, NFDataX n) =>
  Signal dom n -> -- New item either for the kernel or image
  Signal dom n -- Convolved feature
mSerConv1D' = mealy serConv1D' (LOAD_KERNEL, replicate d9 0, 0, 0)

-- Simulation for the mSerConv1D' below
simMSerConv1DTb' :: [Signed 16]
simMSerConv1DTb' = simulate @System mSerConv1D' rockyFirstNoReuseInps

simMSerConv1DTbPrint' = mapM_ print $ L.zip [1 ..] simMSerConv1DTb'

-----------------------------------------------------------------------------------------
-- Implementation of axisSerConv1D
axisSerConv1D ::
  forall a n k.
  (KnownNat a, SaturatingNum n, Num k) =>
  (ConvState, Vec a n, n, Index a, k) -> -- STATE: Current ConvState, kernel, accumulator, counter and keep
  (Maybe (Axi4Stream n k), Bool) -> -- INPUT: AXIS slave, m_axis_tready
  ( (ConvState, Vec a n, n, Index a, k), -- STATE': Current ConvState, kernel, accumulator, counter and keep
    (Maybe (Axi4Stream n k), Bool) -- OUTPUT: AXIS master, s_axis_tready
  )
axisSerConv1D (state, kernel, acc, counter, keep) (s_axis, m_axis_tready) =
  let 
      counter' = succ counter

      vld =
        case (s_axis, s_axis_tready) of
          (Just _, True) -> True -- Incoming packet is valid & local slave is ready
          _ -> False

      (s_axis_tdata, s_axis_tlast, s_axis_tkeep) =
        case s_axis of
          (Just x) -> (tData x, tLast x, tKeep x) -- extract data from axi record
          _ -> (0, False, 0) -- default data

      s_axis_tready = not (state == CONV) && m_axis_tready

      m_axis_tvalid
        | state == CONV = True -- Convolutional feature available
        | otherwise = False -- Convolutional feature not available

      m_axis
        | m_axis_tvalid =
          Just
            Axi4Stream
              {tData = cf, tLast = s_axis_tlast, tKeep = s_axis_tkeep}
        | otherwise = Nothing

      ((nextKernel, nextAcc, _), cf) = if vld || (m_axis_tvalid && m_axis_tready)
        then serConv1D (kernel, acc, counter) (state, s_axis_tdata)
        else ((kernel, acc, counter), 0)

      (nextState, nextCounter) =
        case state of
          LOAD_KERNEL -> if vld
            then if counter == maxBound then (CONV, 0) else (state, counter')
            else (state, counter)
          CONV -> if (m_axis_tvalid && m_axis_tready)
            then if counter == maxBound then (CONV, 0) else (state, counter')
            else (state, counter)
          _ -> (LOAD_KERNEL, 0)

   in ((nextState, nextKernel, nextAcc, nextCounter, s_axis_tkeep), (m_axis, s_axis_tready))

-----------------------------------------------------------------------------------------
-- You can use the simulation function simSerAxisConv1DTbPrint to print out all the iner stages of the states
simSerAxisConv1D :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)] -> String
simSerAxisConv1D = sim axisSerConv1D (LOAD_KERNEL, replicate d9 0, 0, 0, 0)

simSerAxisConv1DTb :: String
simSerAxisConv1DTb = simSerAxisConv1D mAxisConv1DTbNoReuseInp

simSerAxisConv1DTbPrint = putStrLn simSerAxisConv1DTb
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- The AXIS version of axisSerConv1D in a mealy machine and simulation

mAxisSerConv1D ::
  (SaturatingNum n, Num k, NFDataX n, NFDataX k, HiddenClockResetEnable dom) =>
  Signal dom (Maybe (Axi4Stream n k)) ->
  Signal dom Bool ->
  Signal dom (Maybe (Axi4Stream n k), Bool)
mAxisSerConv1D stream ready = mealy axisSerConv1D (LOAD_KERNEL, replicate d9 0, 0, 0, 0) (bundle (stream, ready))

mAxisSerConv1DTb ::
  (HiddenClockResetEnable dom) =>
  Signal dom (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool) ->
  Signal dom (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)
mAxisSerConv1DTb s_axis = o
  where
    (s_axis', m_axis_tready) = unbundle s_axis
    o = mAxisSerConv1D s_axis' m_axis_tready

simMAxisSerConv1DTb :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)]
simMAxisSerConv1DTb = simulate @System mAxisSerConv1DTb mAxisConv1DTbNoReuseInp

-- NOTE: the function simMAxisConv1DTbPrint simulates the mealy machine, and hence hides the internal state.
simMAxisSerConv1DTbPrint = mapM_ print $ L.zip [1 ..] simMAxisSerConv1DTb

-----------------------------------------------------------------------------------------
-- Clash Q3: RTL schematics
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- convAccel RTL schematic

{-# NOINLINE convAccel #-}
{-# ANN
  convAccel
  ( Synthesize
      { t_name = "convAccel",
        t_inputs =
          [ PortName "aclk",
            PortName "nrst",
            PortProduct
              ""
              [ PortProduct
                  "s_axis"
                  [ PortName "tvalid",
                    PortProduct
                      ""
                      [ PortName "tdata",
                        PortName "tlast",
                        PortName "tkeep"
                      ]
                  ],
                PortName "m_axis_tready"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct
                "m_axis"
                [ PortName "tvalid",
                  PortProduct
                    ""
                    [ PortName "tdata",
                      PortName "tlast",
                      PortName "tkeep"
                    ]
                ],
              PortName "s_axis_tready"
            ]
      }
  )
  #-}
convAccel ::
  Clock ConvAccelSystem -> -- aclk
  Reset ConvAccelSystem -> -- nrst
  Signal ConvAccelSystem (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool) ->
  Signal ConvAccelSystem (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)
convAccel clk rst s_axis = o
  where
    (s_axis', m_axis_tready) = unbundle s_axis
    o = exposeClockResetEnable mAxisConv1D clk rst enableGen d9 s_axis' m_axis_tready

-----------------------------------------------------------------------------------------
-- serConvAccel RTL schematic

{-# NOINLINE serConvAccel #-}
{-# ANN
  serConvAccel
  ( Synthesize
      { t_name = "serConvAccel",
        t_inputs =
          [ PortName "aclk",
            PortName "nrst",
            PortProduct
              ""
              [ PortProduct
                  "s_axis"
                  [ PortName "tvalid",
                    PortProduct
                      ""
                      [ PortName "tdata",
                        PortName "tlast",
                        PortName "tkeep"
                      ]
                  ],
                PortName "m_axis_tready"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct
                "m_axis"
                [ PortName "tvalid",
                  PortProduct
                    ""
                    [ PortName "tdata",
                      PortName "tlast",
                      PortName "tkeep"
                    ]
                ],
              PortName "s_axis_tready"
            ]
      }
  )
  #-}
serConvAccel ::
  Clock ConvAccelSystem -> -- aclk
  Reset ConvAccelSystem -> -- nrst
  Signal ConvAccelSystem (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool) ->
  Signal ConvAccelSystem (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)
serConvAccel clk rst s_axis = o
  where
    (s_axis', m_axis_tready) = unbundle s_axis
    o = exposeClockResetEnable mAxisSerConv1D clk rst enableGen s_axis' m_axis_tready

-----------------------------------------------------------------------------------------
-- Clash Q4: Convolution with reusing sub images
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- conv1DReuse function

conv1DReuse ::
  (KnownNat a, SaturatingNum n) =>
  (ConvState, Index a, Vec a n, Vec a n) -> -- STATE: current ConvState, counter, kernel, subImg
  n -> -- INPUT: incoming pixel (kernel or image stream)
  ( (ConvState, Index a, Vec a n, Vec a n), -- STATE': next state, counter, kernel, subImg
    n -- OUTPUT: convolved result (or 0 if not ready)
  )
conv1DReuse (state, counter, kernel, subImg) input =
  let
    -- Update counter
    counter' = succ counter

    -- Transition logic
    (nextState, nextCounter) =
      case state of
        LOAD_KERNEL -> if counter == maxBound then (LOAD_SUBIMG, 0) else (LOAD_KERNEL, counter')
        LOAD_SUBIMG -> if counter == maxBound then (CONV, 0) else (LOAD_SUBIMG, counter')
        CONV        -> if counter == maxBound then (CONV, 0) else (CONV, counter')  -- Reuse: stay in CONV state

    -- Kernel update (only during LOAD_KERNEL)
    newKernel =
      case state of
        LOAD_KERNEL -> kernel <<+ input
        _           -> kernel

    -- Sub-image update (shift for LOAD_SUBIMG and CONV)
    newSubImg =
      case state of
        LOAD_SUBIMG -> subImg <<+ input
        CONV        -> subImg <<+ input
        _           -> subImg

    -- Only compute convolution in CONV state
    out =
      case state of
        CONV -> conv newKernel newSubImg
        _    -> 0
  in
    ((nextState, nextCounter, newKernel, newSubImg), out)

-----------------------------------------------------------------------------------------
-- You can use the simulation function simConv1DReuseTbPrint to print out all the iner stages of the states
simConv1DReuse :: (Show a, SaturatingNum a) => [a] -> [Char]
simConv1DReuse = sim conv1DReuse (LOAD_KERNEL, 0, replicate d9 0, replicate d9 0)

simConv1DReuseTb :: String
simConv1DReuseTb = simConv1DReuse rockyFirstReuseInps

simConv1DReuseTbPrint' = putStrLn simConv1DReuseTb
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- conv1DReuse in a mealy machine and simulation

mConv1DReuse = mealy conv1DReuse (LOAD_KERNEL, 0, replicate d9 0, replicate d9 0)

simMConv1DReuse :: [Signed 16]
simMConv1DReuse = simulate @System mConv1DReuse rockyFirstReuseInps

simMConv1DReusePrint = mapM_ print $ L.zip [1 ..] simMConv1DReuse

-----------------------------------------------------------------------------------------
-- conv1DReuse with axis

axisConv1DReuse = undefined

-----------------------------------------------------------------------------------------
-- You can use the simulation function simSerAxisConv1DTbPrint to print out all the iner stages of the states
-- simAxisConv1DReuse :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)] -> String
-- simAxisConv1DReuse = sim axisConv1DReuse undefined -- DEFINE Initial state

-- simAxisConv1DReuseTb :: String
-- simAxisConv1DReuseTb = simAxisConv1DReuse mAxisConv1DTbReuseInp

-- simAxisConv1DReuseTbPrint = putStrLn simAxisConv1DReuseTb
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- axisConv1DReuse in a mealy machine and simulation

mAxisConv1DReuse = undefined

mAxisConv1DReuseTb = undefined

simMAxisConv1DReuseTb :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)]
simMAxisConv1DReuseTb = simulate @System mAxisConv1DReuseTb mAxisConv1DTbReuseInp

-- NOTE: the function simMAxisConv1DTbPrint simulates the mealy machine, and hence hides the internal state.
simMAxisConv1DReuseTbPrint = mapM_ print $ L.zip [1 ..] simMAxisConv1DReuseTb

-----------------------------------------------------------------------------------------
-- Sim functions and input
-----------------------------------------------------------------------------------------

sim f s [] = []
sim f s (i : is) = h L.++ sim f s' is
  where
    (s', o) = f s i
    h =
      ""
        L.++ "\ni: "
        L.++ (show i)
        L.++ "\ns:  "
        L.++ (show s)
        L.++ "\ns': "
        L.++ (show s')
        L.++ "\no: "
        L.++ (show o)

-- L.++ "\n"

rockyFirstNoReuseInps =
  [(1 :: Signed 16), 2, 1, 0, 0, 0, -1, -2, -1] -- kernel
    L.++ [70, 70, 73, 75, 74, 76, 82, 80, 80] -- image1, output should be -39
    L.++ [75, 74, 76, 82, 80, 80, 83, 82, 81] -- image2, output should be -29
    L.++ [82, 80, 80, 83, 82, 81, 77, 77, 78] -- image3, output should be 13
    L.++ [83, 82, 81, 77, 77, 78, 69, 69, 71] -- image4, output should be 50
    L.++ [77, 77, 78, 69, 69, 71, 65, 65, 68] -- image5, output should be 46
    L.++ [69, 69, 71, 65, 65, 68, 66, 65, 67] -- image6, output should be 15
    L.++ [65, 65, 68, 66, 65, 67, 66, 66, 65] -- image7, output should be 0
    L.++ [66, 65, 67, 66, 66, 65, 67, 66, 64] -- image8, output should be 0
    L.++ [66, 66, 65, 67, 66, 64, 68, 66, 62] -- image9, output should be 1
    L.++ [67, 66, 64, 68, 66, 62, 68, 66, 62] -- image10, output should be 1

rockyFirstReuseInps =
  [(1 :: Signed 16), 2, 1, 0, 0, 0, -1, -2, -1] -- kernel
    L.++ [70, 70, 73, 75, 74, 76, 82, 80, 80]
    L.++ [83, 82, 81, 77, 77, 78, 69, 69, 71]
    L.++ [65, 65, 68, 66, 65, 67, 66, 66, 65]
    L.++ [67, 66, 64, 68, 66, 62, 68, 66, 62]
    L.++ [67, 66, 63, 64, 64, 62, 61, 62, 61]
    L.++ [59, 60, 60, 65, 63, 60, 69, 67, 64]

{-
-----------------------------------------------------------------------------------------
-- Clash: pipelined version, for if you  are bored:
-----------------------------------------------------------------------------------------

convPipelined = undefined

conv1DPipelined = undefined

conv1DPipelined' = undefined

mConv1DPipelined' :: (HiddenClockResetEnable dom, KnownNat n,  SaturatingNum a, NFDataX a, Show a)
  => SNat n      -- Depth vector
  -> Signal dom a -- New item either for the kernel or image
  -> Signal dom a -- Convolved feature
mConv1DPipelined' = undefined

axisConv1DPipelined = undefined

mAxisConv1DPipelined :: (SaturatingNum n, Num k, NFDataX n, HiddenClockResetEnable dom, KnownNat p, NFDataX k, Show n)
  => SNat p
  -> Signal dom (Maybe (Axi4Stream n k))
  -> Signal dom Bool
  -> Signal dom (Maybe (Axi4Stream n k), Bool)
mAxisConv1DPipelined = undefined

mAxisConv1DPipelinedTb :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)
  -> Signal dom (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)
mAxisConv1DPipelinedTb s_axis = o
  where
    (s_axis', m_axis_tready) = unbundle s_axis
    o = mAxisConv1DPipelined d9 s_axis' m_axis_tready

-- Simulation function
simMAxisConv1DPipelinedTb :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)]
simMAxisConv1DPipelinedTb = simulate @System mAxisConv1DPipelinedTb mAxisConv1DTbNoReuseInp
--NOTE: the function simMAxisConv1DTbPrint simulates the mealy machine, and hence hides the internal state.
simMAxisConv1DPipelinedTbPrint = mapM_ print $ L.zip [1..] simMAxisConv1DPipelinedTb

-- -- some helper functions for print statements below that shows internal state:
-- simAxisConv1DPipelined :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)] -> String
-- simAxisConv1DPipelined = sim axisConv1DPipelined undefined -- NOTE: Change this to initial state

-- simAxisConv1DPipelinedTb :: String
-- simAxisConv1DPipelinedTb = simAxisConv1DPipelined mAxisConv1DTbNoReuseInp

-- -- NOTE: the simAxisConv1DTbPrint simulates the non-mealy function and shows internal state.
-- simAxisConv1DPipelinedTbPrint = putStrLn simAxisConv1DPipelinedTb

-----------------------------------------------------------------------------------------
-- code if you want to use the Vivado testbench:
-----------------------------------------------------------------------------------------

convAccelNoReuseTc :: IO ()
convAccelNoReuseTc = o
  where
    rst = (Nothing :: Maybe (Axi4Stream (Signed 32) (Unsigned 4)), False)
    o = mapM_ print $ simulateWithResetN @ConvAccelSystem d1 rst (L.length mAxisConv1DTbReuseInp) mAxisConv1DTb mAxisConv1DTbReuseInp

convAccelReuseTc :: IO ()
convAccelReuseTc = o
  where
    rst = (Nothing :: Maybe (Axi4Stream (Signed 32) (Unsigned 4)), False)
    o = mapM_ print $ simulateWithResetN @ConvAccelSystem d1 rst (L.length mAxisConv1DTbReuseInp) mAxisConv1DTb mAxisConv1DTbReuseInp

{-# NOINLINE convAccelTb #-}
{-# ANN convAccelTb (TestBench 'convAccel) #-}
convAccelTb :: Signal ConvAccelSystem (
  Maybe (Axi4Stream (Signed 32) (Unsigned 4)),
  Bool
  )
convAccelTb = bundle (m_axis', match)
  where
    clk = tbConvAccelSystemClockGen (not <$> match) -- Enable clock unless match
    rst = convAccelSystemResetGen

    m_axis_s_axis_tready = sobelStreamer clk rst s_axis_tready'
    (m_axis, _) = unbundle m_axis_s_axis_tready

    -- SobelVerifier slave is always ready
    m_axis_s_axis_tready' = convAccel clk rst (bundle (m_axis, pure True))
    (m_axis', s_axis_tready') = unbundle m_axis_s_axis_tready'

    match = unbundle $ sobelVerifier clk rst m_axis' -- Compare expected versus result

convAccelTbSample :: Int -> IO ()
convAccelTbSample x = mapM_ print $ sampleN @ConvAccelSystem x convAccelTb

serConvAccelNoReuseTc :: IO ()
serConvAccelNoReuseTc = o
  where
    rst = (Nothing, False) :: (Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)
    o = mapM_ print $ simulateWithResetN @ConvAccelSystem d1 rst (L.length mAxisConv1DTbNoReuseInp) mAxisSerConv1DTb mAxisConv1DTbNoReuseInp

{-# NOINLINE serConvAccelTb #-}
{-# ANN serConvAccelTb (TestBench 'serConvAccel) #-}
serConvAccelTb :: Signal ConvAccelSystem Bool
serConvAccelTb = match
  where
    clk = tbConvAccelSystemClockGen (not <$> match) -- Enable clock unless match
    rst = convAccelSystemResetGen

    (m_axis, _) = unbundle $ sobelStreamer clk rst s_axis_tready

    (m_axis', s_axis_tready) = unbundle $ serConvAccel clk rst (bundle (m_axis, pure True))

    match = sobelVerifier clk rst m_axis' -- Compare expected versus result

serConvAccelTbSample :: Int -> IO ()
serConvAccelTbSample cycles = do
    let res = sampleN @ConvAccelSystem cycles serConvAccelTb
    print res
    cyclesTillMatch res
-}