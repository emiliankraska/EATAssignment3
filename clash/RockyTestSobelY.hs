module RockyTestSobelY where
import Clash.Prelude
import Axi

rockyTestSobelY :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)]
rockyTestSobelY = [
  (Just (Axi4Stream (1) False 15), True),
  (Just (Axi4Stream (0) False 15), True),
  (Just (Axi4Stream (-1) False 15), True),
  (Just (Axi4Stream (2) False 15), True),
  (Just (Axi4Stream (0) False 15), True),
  (Just (Axi4Stream (-2) False 15), True),
  (Just (Axi4Stream (1) False 15), True),
  (Just (Axi4Stream (0) False 15), True),
  (Just (Axi4Stream (-1) True 15), True)]