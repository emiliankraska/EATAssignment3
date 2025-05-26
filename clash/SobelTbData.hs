{-# LANGUAGE UndecidableInstances #-}

module SobelTbData where

import           Axi
import           Clash.Prelude as CP
import           Common
import           Data.List as L (concat, map, replicate, take, (++))
import           System.IO.Unsafe (unsafePerformIO)

-- BRAM loads row-wise, others load col-wise
data TestState = SIMPLE_NO_REUSE | SIMPLE_REUSE | PARALLEL_NO_REUSE |
    PARALLEL_REUSE | BRAM | SERIAL_NO_REUSE
    deriving (Show, Generic, NFDataX, Eq)

sobelXFP :: String
sobelXFP = wsPath L.++ "clash/RockyTestSobelX.hs"

parSobelXFP :: String
parSobelXFP = wsPath L.++ "clash/RockyTestParSobelX.hs"

bramSobelXFP :: String
bramSobelXFP = wsPath L.++ "clash/RockyTestBramSobelX.hs"


sobelYFP :: String
sobelYFP = wsPath L.++ "clash/RockyTestSobelY.hs"

parSobelYFP :: String
parSobelYFP = wsPath L.++ "clash/RockyTestParSobelY.hs"

bramSobelYFP :: String
bramSobelYFP = wsPath L.++ "clash/RockyTestBramSobelY.hs"


imageNoReuseFP :: String
imageNoReuseFP = wsPath L.++ "clash/RockyTestNoReuseImage.hs"

parImageNoReuseFP :: String
parImageNoReuseFP = wsPath L.++ "clash/RockyTestParNoReuseImage.hs"

imageReuseFP :: String
imageReuseFP = wsPath L.++ "clash/RockyTestReuseImage.hs"

parImageReuseFP :: String
parImageReuseFP = wsPath L.++ "clash/RockyTestParReuseImage.hs"

bramImageFP :: String
bramImageFP = wsPath L.++ "clash/RockyTestBramImage.hs"

rockyEdgesSimpleNoReuseFP :: String
rockyEdgesSimpleNoReuseFP = wsPath L.++ "img/rockyEdgesSimpleNoReuse.pgm"

rockyEdgesSimpleReuseFP :: String
rockyEdgesSimpleReuseFP = wsPath L.++ "img/rockyEdgesSimpleReuse.pgm"

rockyEdgesSerialNoReuseFP :: String
rockyEdgesSerialNoReuseFP = wsPath L.++ "img/rockyEdgesSerialNoReuse.pgm"

sobelXReuseExpFP :: String
sobelXReuseExpFP = wsPath L.++ "clash/RockyTestReuseConvFeaturesSobelX.hs"

parSobelXReuseExpFP :: String
parSobelXReuseExpFP = wsPath L.++ "clash/RockyTestParReuseConvFeaturesSobelX.hs"

sobelYReuseExpFP :: String
sobelYReuseExpFP = wsPath L.++ "clash/RockyTestReuseConvFeaturesSobelY.hs"

parSobelYReuseExpFP :: String
parSobelYReuseExpFP = wsPath L.++ "clash/RockyTestParReuseConvFeaturesSobelY.hs"

sobelXExpFP :: String
sobelXExpFP = wsPath L.++ "clash/RockyTestNoReuseConvFeaturesSobelX.hs"

parSobelXExpFP :: String
parSobelXExpFP = wsPath L.++ "clash/RockyTestParNoReuseConvFeaturesSobelX.hs"

sobelYExpFP :: String
sobelYExpFP = wsPath L.++ "clash/RockyTestNoReuseConvFeaturesSobelY.hs"

parSobelYExpFP :: String
parSobelYExpFP = wsPath L.++ "clash/RockyTestParNoReuseConvFeaturesSobelY.hs"

sobelXFilteredExpFP :: String
sobelXFilteredExpFP = wsPath L.++ "img/sobel_x_filtered.txt"

sobelYFilteredExpFP :: String
sobelYFilteredExpFP = wsPath L.++ "img/sobel_y_filtered.txt"


-- Note: no reuse/ reuse here, this is a list of only tValid high combined results
sobelNormExpFP :: String
sobelNormExpFP = wsPath L.++ "img/sobel_norm.txt"


sobel :: TestState -> Bool -> RetType
sobel state sobelY = o
  where
    fp = case state of
      BRAM              -> if sobelY then bramSobelYFP else bramSobelXFP
      PARALLEL_NO_REUSE -> if sobelY then parSobelYFP else parSobelXFP
      PARALLEL_REUSE    -> if sobelY then parSobelYFP else parSobelXFP
      _                 -> if sobelY then sobelYFP else sobelXFP

    o = case state of
      PARALLEL_NO_REUSE -> PAR (unsafePerformIO $ parReadRocky fp)
      PARALLEL_REUSE    -> PAR (unsafePerformIO $ parReadRocky fp)
      _                 -> SER (unsafePerformIO $ readRocky fp)

image :: TestState -> Int -> RetType
image state subset = o
  where
    fp = case state of
      BRAM              -> bramImageFP
      PARALLEL_NO_REUSE -> parImageNoReuseFP
      PARALLEL_REUSE    -> parImageReuseFP
      SIMPLE_REUSE      -> imageReuseFP
      _                 -> imageNoReuseFP

    o = case state of
      PARALLEL_NO_REUSE -> PAR (L.take subset (unsafePerformIO $ parReadRocky fp))
      PARALLEL_REUSE    -> PAR (L.take subset (unsafePerformIO $ parReadRocky fp))
      _                 -> SER (L.take subset (unsafePerformIO $ readRocky fp))

sobelExp :: TestState -> Int -> Bool -> RetType
sobelExp state subset sobelY = o
  where
    fp = case state of
      BRAM              -> if sobelY then bramSobelYFP else bramSobelXFP
      PARALLEL_NO_REUSE -> if sobelY then parSobelYExpFP else parSobelXExpFP
      PARALLEL_REUSE    -> if sobelY then parSobelYReuseExpFP else parSobelXReuseExpFP
      SIMPLE_REUSE      -> if sobelY then sobelYReuseExpFP else sobelXReuseExpFP
      _                 -> if sobelY then sobelYExpFP else sobelXExpFP

    o = case state of
      PARALLEL_NO_REUSE -> PAR (L.take subset (unsafePerformIO $ parReadRocky fp))
      PARALLEL_REUSE    -> PAR (L.take subset (unsafePerformIO $ parReadRocky fp))
      _                 -> SER (L.take subset (unsafePerformIO $ readRocky fp))

sobelFilteredExp :: Int -> Bool -> [Maybe (Signed 32)]
sobelFilteredExp subset sobelY = o'
  where
    fp = if sobelY then sobelYFilteredExpFP else sobelXFilteredExpFP
    s = unsafePerformIO $ readFromFile fp
    o = L.take subset s
    o' = L.map (\x -> Just x) o


sobelNormExp :: Int -> [Unsigned 8]
sobelNormExp subset = o
  where
    s = unsafePerformIO $ readFromFile sobelNormExpFP
    o = if subset == 0 then s else L.take subset s

-- Create input sequence by combining the reset cycle, sobel kernel, image, fill up cycle
-- (Axi4Stream s_axis_tdata   Input data
--             s_axis_tlast   End of input packet flag
--             s_axis_tkeep   Input bytes to keep
--             s_axis_tvalid  Input packet is valid/not valid
--             m_axis_tready) Receiving external master is ready
sobelSeq
  :: TestState
  -> Int  -- Subset of convolved features
  -> Bool -- Sobel kernel of choice (Sobel-X or Sobel-Y)
  -> RetType -- Sobel Input Sequence
sobelSeq state subset sobelY = o
  where
    par = case state of
      PARALLEL_NO_REUSE -> True
      PARALLEL_REUSE    -> True
      _                 -> False

    reuse = case state of
      PARALLEL_REUSE -> True
      SIMPLE_REUSE   -> True
      _              -> False

    rst | par       = PAR [(Nothing, True)]
        | otherwise = SER [(Nothing, True)]

    fillUp = rst

    pars | par       = 4
         | otherwise = 1

    kernel' = sobel state sobelY
    image' =  image state (getImageLen pars reuse subset)

    -- Note: <> does something simular to concat
    -- The instance allows us to concat on PAR SER level
    o = rst <> kernel' <> image' <> fillUp

-- Create output sequence by combining the reset cycle, sobel conv features & fill up cycle
-- (Axi4Stream m_axis_tdata m_axis_tlast m_axis_tkeep m_axis_tvalid s_axis_tready)
sobelOutSeq
  :: TestState
  -> Int  -- Convolved features subset
  -> Bool -- Sobel kernel of choice (Sobel-X or Sobel-Y)
  -> RetType -- Sobel Output Sequence
sobelOutSeq state subset sobelY = o
  where

    par = case state of
      PARALLEL_NO_REUSE -> True
      PARALLEL_REUSE    -> True
      _                 -> False

    reuse = case state of
      PARALLEL_REUSE -> True
      SIMPLE_REUSE   -> True
      _              -> False

    pars | par       = 4
         | otherwise = 1

    rst | par       = PAR [(Nothing, True)]
        | otherwise = SER [(Nothing, True)]

    kernel | par       = PAR (L.concat $ L.replicate 9 [(Nothing, True)])
           | otherwise = SER (L.concat $ L.replicate 9 [(Nothing, True)])

    sobel' = sobelExp state (getImageLen pars reuse subset) sobelY

    -- Note: no need to add a fillup cycle here, the outputVerifier' returns
    -- 'matching' if the output matches the input, so we can already match before
    -- thus if output is N items than input is N + 1 items
    o = rst <> kernel <> sobel'
