
module ConvTbData where

import           Axi
import           Clash.Prelude as CP
import           Common

mAxisConv1DTbNoReuseInp :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)]
mAxisConv1DTbNoReuseInp =
  [ (Nothing, True)    -- Reset
  , (Nothing, True)
  , (Nothing, False)
  -- Kernel
  , (Just (Axi4Stream 9 False 15), True)
  , (Just (Axi4Stream 6 False 15), True)
  , (Just (Axi4Stream 3 False 15), True)
  , (Nothing, True)
  , (Nothing, False)
  , (Nothing, True)
  , (Just (Axi4Stream 8 False 15), True)
  , (Just (Axi4Stream 5 False 15), True)
  , (Just (Axi4Stream 2 False 15), True)
  , (Just (Axi4Stream 7 False 15), True)
  , (Just (Axi4Stream 4 False 15), True)
  , (Just (Axi4Stream 1 True 15), True)       -- End Of Packet
  -- Image 1
  , (Just (Axi4Stream 1 False 15), True)
  , (Just (Axi4Stream 6 False 15), True)
  , (Just (Axi4Stream 11 False 15), True)
  , (Just (Axi4Stream 2 False 15), True)
  , (Nothing, True) -- External master is not transmitting valid content
  , (Just (Axi4Stream 7 False 15), True)
  , (Just (Axi4Stream 12 False 15), True)
  , (Just (Axi4Stream 3 False 15), True)
  , (Nothing, True)
  , (Just (Axi4Stream 8 False 15), True)
  , (Just (Axi4Stream 13 True 15), True)  -- EOF
   -- Image 2
  , (Just (Axi4Stream 2 False 15), False) -- The external slave is not ready; Tell external master to retransmit
  , (Just (Axi4Stream 2 False 15), False) -- The external slave is not ready; Tell external master to retransmit
  , (Just (Axi4Stream 2 False 15), True)  -- Make external master retransmit here
  , (Just (Axi4Stream 7 False 15), True)
  , (Just (Axi4Stream 12 False 15), True)
  , (Just (Axi4Stream 3 False 15), True)
  , (Just (Axi4Stream 8 False 15), True)
  , (Just (Axi4Stream 13 False 15), True)
  , (Nothing, False)
  , (Just (Axi4Stream 4 False 15), True)
  , (Just (Axi4Stream 9 False 15), True)
  , (Just (Axi4Stream 14 True 15), True)       -- EOF
  -- Image 3
  , (Just (Axi4Stream 3 False 15), True) -- Pipeline here by already loading the newest
  , (Just (Axi4Stream 8 False 15), True)
  , (Just (Axi4Stream 13 False 15), True)
  , (Just (Axi4Stream 4 False 15), True)
  , (Just (Axi4Stream 9 False 15), True)
  , (Just (Axi4Stream 14 False 15), True)
  , (Just (Axi4Stream 5 False 15), True)
  , (Just (Axi4Stream 10 False 15), True)
  , (Just (Axi4Stream 15 True 15), True)
  , (Nothing, False) -- No pipeline; External slave not ready, let external master retransmit
  , (Nothing, False)
  , (Nothing, True)
  , (Nothing, True)
  , (Nothing, False) -- External slave not ready, however local regs can buffer here, because we are not in CONV state, so external master can transmit
  , (Nothing, False)
  , (Nothing, False)
  , (Nothing, False)
  , (Just (Axi4Stream 3 False 15), True) -- Pipeline here by already loading the newest
  , (Just (Axi4Stream 8 False 15), True)
  , (Just (Axi4Stream 13 False 15), True)
  , (Just (Axi4Stream 4 False 15), True)
  , (Just (Axi4Stream 9 False 15), True)
  , (Just (Axi4Stream 14 False 15), True)
  , (Just (Axi4Stream 5 False 15), True)
  , (Just (Axi4Stream 10 False 15), True)
  , (Just (Axi4Stream 15 True 15), True)
  , (Nothing, True)
  , (Nothing, True)
  ]



mAxisConv1DTbReuseInp :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)]
mAxisConv1DTbReuseInp = [
  (Nothing, True),    -- Reset
  (Just (Axi4Stream 1 False 15), True),     -- Kernel
  (Just (Axi4Stream 2 False 15), True),
  (Just (Axi4Stream 3 False 15), True),
  (Just (Axi4Stream 4 False 15), True),
  (Just (Axi4Stream 5 False 15), True),
  (Nothing, True),
  (Just (Axi4Stream 6 False 15), False),
  (Just (Axi4Stream 7 False 15), True),
  (Just (Axi4Stream 8 False 15), True),
  (Just (Axi4Stream 9 True 15), True),
  (Just (Axi4Stream 1 False 15), True),   -- Image 1
  (Just (Axi4Stream 2 False 15), True),
  (Nothing, False),
  (Nothing, True),
  (Just (Axi4Stream 3 False 15), True),
  (Just (Axi4Stream 4 False 15), True),
  (Just (Axi4Stream 5 False 15), True),
  (Just (Axi4Stream 6 False 15), True),
  (Just (Axi4Stream 7 False 15), True),
  (Just (Axi4Stream 8 False 15), True),
  (Just (Axi4Stream 9 True 15), True),
  (Just (Axi4Stream 10 False 15), True),  -- Convolution execution and load new item 2nd image at same time
  (Just (Axi4Stream 11 False 15), True),
  (Just (Axi4Stream 12 True 15), True),   -- Image 2 only 3 new items
  (Nothing, False), -- Fill up the pipeline should break here because no 1st item of 3rd sub image
  (Nothing, True),
  (Nothing, True),
  (Nothing, False), -- A "double valid result" needs to be inserted here
  (Nothing, False), -- The external slave could not take the 1st valid result
  (Nothing, False) -- , because it was not ready, so we need to reinsert here!
  ]

mAxisConv1DTbNoReuseExp :: [(Maybe (Axi4Stream (Signed 32) (Unsigned 4)), Bool)]
mAxisConv1DTbNoReuseExp = [
  (Nothing, True),  -- Reset
  (Nothing, True),  -- Kernel
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True), -- End Of Packet
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Just (Axi4Stream 285 True 0), True),  -- Convolved feature image 1 & 1st item image 2
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Nothing, True),
  (Just (Axi4Stream 285 True 0), True), -- Convolved feature image 2
  (Nothing, True),
  (Nothing, True),
  (Nothing, True)
  ]

rAxisParConv1DTbNoReuseInp :: [(Maybe (Axi4Stream (Vec 4 ParElm) (Vec 4 Bool)), Bool)]
rAxisParConv1DTbNoReuseInp =
  [ (Nothing, True)    -- Reset
  , (Just (Axi4Stream (1:>1:>1:>1:>Nil) False (True:>True:>True:>True:>Nil)), True)     -- Kernel
  , (Just (Axi4Stream (2:>2:>2:>2:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (3:>3:>3:>3:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (4:>4:>4:>4:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (5:>5:>5:>5:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (6:>6:>6:>6:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (7:>7:>7:>7:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (8:>8:>8:>8:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (9:>9:>9:>9:>Nil) True (True:>True:>True:>True:>Nil)), True)  -- End of kernel
  -- Image 1,2,3 & 4
  , (Just (Axi4Stream (1:>9:>1:>1:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (2:>8:>2:>2:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (3:>7:>3:>3:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (4:>6:>4:>4:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (5:>5:>5:>5:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (6:>4:>6:>6:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (7:>3:>7:>7:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (8:>2:>8:>8:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (9:>1:>9:>9:>Nil) True (True:>True:>True:>True:>Nil)), True)  -- End of images
  -- Image 5 & 6
  , (Just (Axi4Stream (1:>9:>1:>1:>Nil) False (True:>True:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (2:>8:>2:>2:>Nil) False (True:>True:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (3:>7:>3:>3:>Nil) False (True:>True:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (4:>6:>4:>4:>Nil) False (True:>True:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (5:>5:>5:>5:>Nil) False (True:>True:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (6:>4:>6:>6:>Nil) False (True:>True:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (7:>3:>7:>7:>Nil) False (True:>True:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (8:>2:>8:>8:>Nil) False (True:>True:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (9:>1:>9:>9:>Nil) True (True:>True:>False:>False:>Nil)), True)  -- End of images
  -- Image 7, 8, 9 & 10
  , (Just (Axi4Stream (1:>9:>1:>1:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (2:>8:>2:>2:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (3:>7:>3:>3:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (4:>6:>4:>4:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (5:>5:>5:>5:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (6:>4:>6:>6:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (7:>3:>7:>7:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (8:>2:>8:>8:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (9:>1:>9:>9:>Nil) True (True:>True:>True:>True:>Nil)), True)  -- End of images
  , (Nothing, False)     -- Fill up the pipeline should break here because no 1st item of 3rd sub image
  , (Nothing, False)
  , (Nothing, False)
  ]


rAxisParConv1DTbReuseInp :: [(Maybe (Axi4Stream (Vec 4 ParElm) (Vec 4 Bool)), Bool)]
rAxisParConv1DTbReuseInp =
  [ (Nothing, True)    -- Reset
  , (Just (Axi4Stream (1:>1:>1:>1:>Nil) False (True:>True:>True:>True:>Nil)), True)     -- Kernel
  , (Just (Axi4Stream (2:>2:>2:>2:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (3:>3:>3:>3:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (4:>4:>4:>4:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (5:>5:>5:>5:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (6:>6:>6:>6:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (7:>7:>7:>7:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (8:>8:>8:>8:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (9:>9:>9:>9:>Nil) True (True:>True:>True:>True:>Nil)), True)  -- End of kernel
  -- Image 1
  , (Just (Axi4Stream (1:>1:>1:>1:>Nil) False (True:>False:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (2:>2:>2:>2:>Nil) False (True:>False:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (3:>3:>3:>3:>Nil) False (True:>False:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (4:>4:>4:>4:>Nil) False (True:>False:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (5:>5:>5:>5:>Nil) False (True:>False:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (6:>6:>6:>6:>Nil) False (True:>False:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (7:>7:>7:>7:>Nil) False (True:>False:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (8:>8:>8:>8:>Nil) False (True:>False:>False:>False:>Nil)), True)
  , (Just (Axi4Stream (9:>9:>9:>9:>Nil) True (True:>False:>False:>False:>Nil)), True)  -- End of images
  -- Image 2, 3, 4 & 5
  , (Just (Axi4Stream (10:>1:>7:>7:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (11:>2:>8:>8:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (12:>1:>9:>9:>Nil) True (True:>True:>True:>True:>Nil)), True)  -- End of images
  -- Image 6, 7, 8 & 9
  , (Just (Axi4Stream (10:>3:>7:>7:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (11:>2:>8:>8:>Nil) False (True:>True:>True:>True:>Nil)), True)
  , (Just (Axi4Stream (12:>1:>9:>9:>Nil) True (True:>True:>True:>True:>Nil)), True)  -- End of images
  , (Nothing, False)     -- Fill up the pipeline should break here because no 1st item of 3rd sub image
  , (Nothing, False)
  , (Nothing, False)
  ]
