{-# LANGUAGE CPP #-}
module Rocky where

import           Control.Monad (when)
import qualified Data.List as L
import           Data.List.Split (chunksOf)
import           Data.Maybe (catMaybes, fromJust, isJust)
import           GHC.Float.RealFracMethods (ceilingFloatInt, floorFloatInt)

import           Clash.Explicit.Testbench
import           Clash.Prelude

import           Axi
import           Common
import           ConvStudent
import           SobelTbData

-- Note: it is a MUST to use txt files here to store the test sequences
-- a haskell file cannot be compiled because the test sequence is way to long
-- this way we avoid compiling a haskell file ;)
tcRocky
  :: TestState
  -> Int   -- Subset of convolved features
  -> Bool  -- Save convolved features as .pgm image (requires all features)
  -> String -- Store image path
  -> IO () -- Console
tcRocky state subset saveImage fp = do
    -- Convolved Features to print to the console
    let thres = 60

    let sobelSeqX' = getSER $ sobelSeq state subset False
    let sobelSeqY' = getSER $ sobelSeq state subset True
    let sobelXExp' = getSER $ sobelOutSeq state subset False
    let sobelYExp' = getSER $ sobelOutSeq state subset True

    -- Simulate for N cycles
    -- Note: simulateN runs L cyles + reset cycle, thus we want L = N - 1
    let len = L.length sobelSeqX'
    putStrLn $ L.concat ["Simulating for ", show len, " cycles ..."]
    let sobelXOut = case state of
                      SIMPLE_NO_REUSE -> simulateWithResetN @ConvAccelSystem d1 (Nothing, True) len mAxisConv1DTb sobelSeqX'
                      SIMPLE_REUSE    -> simulateWithResetN @ConvAccelSystem d1 (Nothing, True) len mAxisConv1DReuseTb sobelSeqX'
                      SERIAL_NO_REUSE -> simulateWithResetN @ConvAccelSystem d1 (Nothing, True) len mAxisSerConv1DTb sobelSeqX'
                      _               -> []

    let sobelYOut = case state of
                  SIMPLE_NO_REUSE -> simulateWithResetN @ConvAccelSystem d1 (Nothing, True) len mAxisConv1DTb sobelSeqY'
                  SIMPLE_REUSE    -> simulateWithResetN @ConvAccelSystem d1 (Nothing, True) len mAxisConv1DReuseTb sobelSeqY'
                  SERIAL_NO_REUSE -> simulateWithResetN @ConvAccelSystem d1 (Nothing, True) len mAxisSerConv1DTb sobelSeqY'
                  _               -> []


    -- Grep tdata from packets where tvalid is high in out stream
    -- [(Just (32, False 0), True)]
    -- [(Nothing, True)]
    let sobelXVld = L.map tData (catMaybes $ L.map fst sobelXOut)
    let sobelYVld = L.map tData (catMaybes $ L.map fst sobelYOut)

    -- Print subresults to console
    putStrLn $ L.concat ["Sobel X Output:\n", show $ if L.length(sobelXVld) > thres then L.take thres sobelXVld else sobelXVld, "\n"]
    putStrLn $ L.concat ["Sobel Y Output:\n", show $ if L.length(sobelYVld) > thres then L.take thres sobelYVld else sobelYVld, "\n"]

    -- Compare Sobel X result to expected Sobel X
    --let sobelXMatch = cmpr sobelXOut sobelXExp'
    --putStrLn $ L.concat ["Sobel X Match: ", show sobelXMatch]

    -- Compare Sobel Y result to expected Sobel Y
    --let sobelYMatch = cmpr sobelYOut sobelYExp'
    --putStrLn $ L.concat ["Sobel Y Match: ", show sobelYMatch]

    sobelCombine subset saveImage thres sobelXVld sobelYVld fp

sobelCombine :: (Real k, Bits k) => Int -> Bool -> Int -> [k] -> [k] -> String -> IO ()
sobelCombine subset saveImage thres sobelXVld sobelYVld fp = do
    putStrLn $ L.concat ["Convolved Features: ", show subset]
    let sobelNorm' = sobelNormExp subset

    -- Combine Sobel-X & Y results
    let out = L.map (\(x,y) -> sqrt ((realToFrac x :: Float) ** 2 + (realToFrac y :: Float) ** 2)) (L.zip sobelXVld sobelYVld)

    -- Note: Only use L.maximum out if no subset is taken
    -- However a constant precomputed value might be more suiteable to save on runtime
    --let maxVal = if subset == 0 then L.maximum out
    --           else 582.5083690385916
    let maxVal = 580.0034482656116

    -- Apply normalization
    let normOut = L.map (\o -> floor $ (realToFrac (maxBound :: Unsigned 8)) * o / maxVal :: Unsigned 8) out

    putStrLn $ L.concat [show $ L.length(normOut), " normalized Sobel Results:\n", show $ if L.length(normOut) > thres then L.take thres normOut else normOut, "\n"]
    putStrLn $ L.concat [show $ L.length(sobelNorm'), " normalized Sobel Expected Results:\n", show $ if L.length(sobelNorm') > thres then L.take thres sobelNorm' else sobelNorm', "\n"]

    -- Compare result with expectation
    let match = cmpr normOut sobelNorm'
    putStrLn $ L.concat ["Normalized Combined Sobel Match: ", show match]

    -- Convert 1D list to 2D list of lists of chunks with convFeaturesCols length
    let normOut' = Data.List.Split.chunksOf convFeaturesCols normOut

    when saveImage $ do
        -- Create header for Portable Gray Map (.pgm)
        let mx = show $ L.maximum $ L.map L.maximum normOut'
        let pre = L.concat["P2\r\n", show convFeaturesCols, " ", show convFeaturesRows, "\r\n", mx, "\r\n"]

        -- Save convolvedFeatures to .pgm
        writeFile fp (pre L.++ printimage normOut')
        putStrLn $ "Sobel Normalized Results written to: " L.++ show fp

    putStrLn "Done!"

-- Run through all test cases
tcRockyFullImage :: IO ()
tcRockyFullImage = do
  -- putStrLn "Test case SIMPLE_NO_REUSE"
  -- tcRocky SIMPLE_NO_REUSE (convFeaturesRows * convFeaturesCols) True rockyEdgesSimpleNoReuseFP
  -- putStrLn "Test case SERIAL_NO_REUSE"
  -- tcRocky SERIAL_NO_REUSE (convFeaturesRows * convFeaturesCols) True rockyEdgesSerialNoReuseFP
  putStrLn "Test case SIMPLE_REUSE"
  tcRocky SIMPLE_REUSE (convFeaturesRows * convFeaturesCols) True rockyEdgesSimpleReuseFP
