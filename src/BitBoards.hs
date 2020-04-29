{-# Language BinaryLiterals #-}
{-# Language TemplateHaskell #-}

module BitBoards where

import qualified Data.Vector.Unboxed as V
import           Numeric                ( showHex )
import           Data.Word              ( Word64 )
import           Data.List.Split        ( chunksOf )
import           Data.Bits              ( (.&.)
                                        , (.|.)
                                        , shiftR
                                        , shiftL
                                        , complement )

type QBitBoards = V.Vector Word64
type BitBoards = Word64

data Piece = Pawn
           | Knight
           | Bishop
           | Rook
           | Queen
           | King

data Side = White
          | Black

defaultBoard :: QBitBoards
defaultBoard = V.fromList [ 0xffff000000000000   -- Black
                          , 0x2CFF00000000FF2C   -- PBQ
                          , 0x7600000000000076   -- NBK
                          , 0x9900000000000099 ] -- RQK

black :: QBitBoards -> BitBoards
black =  (V.! 0)
pbq   :: QBitBoards -> BitBoards
pbq   =  (V.! 1)
nbk   :: QBitBoards -> BitBoards
nbk   =  (V.! 2)
rqk   :: QBitBoards -> BitBoards
rqk   =  (V.! 3)

getBoard                :: Side -> Piece -> QBitBoards -> BitBoards
getBoard White Pawn   b =  complement (black b) .&. pbq b .&. complement (nbk b .|. rqk b)
getBoard White Knight b =  complement (black b) .&. nbk b .&. complement (pbq b .|. rqk b)
getBoard White Bishop b =  complement (black b) .&. pbq b .&. nbk b
getBoard White Rook   b =  complement (black b) .&. rqk b .&. complement (pbq b .|. nbk b)
getBoard White Queen  b =  complement (black b) .&. pbq b .&. rqk b
getBoard White King   b =  complement (black b) .&. nbk b .&. rqk b
getBoard Black Pawn   b =  black b .&. pbq b .&. complement (nbk b .|. rqk b)
getBoard Black Knight b =  black b .&. nbk b .&. complement (pbq b .|. rqk b)
getBoard Black Bishop b =  black b .&. pbq b .&. nbk b
getBoard Black Rook   b =  black b .&. rqk b .&. complement (pbq b .|. nbk b)
getBoard Black Queen  b =  black b .&. pbq b .&. rqk b
getBoard Black King   b =  black b .&. nbk b .&. rqk b

showBoardVisual   :: QBitBoards -> [BitBoards]
showBoardVisual b =  wh ++ bl ++ [allw, allb, allp]
                     where wh   = [ getBoard White Pawn   b
                                  , getBoard White Knight b
                                  , getBoard White Bishop b
                                  , getBoard White Rook   b
                                  , getBoard White Queen  b
                                  , getBoard White King   b ]
                           bl   = [ getBoard Black Pawn   b
                                  , getBoard Black Knight b
                                  , getBoard Black Bishop b
                                  , getBoard Black Rook   b
                                  , getBoard Black Queen  b
                                  , getBoard Black King   b ]
                           allw = foldr1 (.|.) wh
                           allb = foldr1 (.|.) bl
                           allp = allw .|. allb

genQBitBoards     :: String -> QBitBoards
genQBitBoards str =  go 64
                        (V.fromList [0, 0, 0, 0])
                        $ (foldr1 (++) . map reverse . chunksOf 8)
                        (str ++ replicate (8 - (length str `mod` 8)) 'O')
                     where go :: Int -> QBitBoards -> String -> QBitBoards
                           go 0 r _ =  r
                           go i b [] =  V.map (`shiftL` i) b
                           go i b ('P' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [        (1, 1)                ]) str
                           go i b ('N' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [                (2, 1)        ]) str
                           go i b ('B' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [        (1, 1), (2, 1)        ]) str
                           go i b ('R' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [                        (3, 1)]) str
                           go i b ('Q' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [        (1, 1),         (3, 1)]) str
                           go i b ('K' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [                (2, 1), (3, 1)]) str
                           go i b ('p' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [(0, 1), (1, 1)                ]) str
                           go i b ('n' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [(0, 1),         (2, 1)        ]) str
                           go i b ('b' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [(0, 1), (1, 1), (2, 1)        ]) str
                           go i b ('r' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [(0, 1),                 (3, 1)]) str
                           go i b ('q' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [(0, 1), (1, 1),         (3, 1)]) str
                           go i b ('k' : str) =  go (i - 1) (V.accum (.|.) (V.map (`shiftL` 1) b) [(0, 1),         (2, 1), (3, 1)]) str
                           go i b ('O' : str) =  go (i - 1)                (V.map (`shiftL` 1) b)                                   str

bitcheck   :: BitBoards -> [Int]
bitcheck w |  w == 0    = []
           |  otherwise = go 0 [] mask w
                          where go :: Int -> [Int] -> [Word64] -> Word64 -> [Int]
                                go _ r [] _       =  r
                                go n r (m : ms) w |  m .&. w == m = go (n+1) (n : r) ms w
                                                  |  otherwise = go (n+1) r ms w

attack     :: Piece -> BitBoards -> BitBoards
attack p b =  foldr1 (.|.) $ map (\x -> atkls p V.! x) $ bitcheck b
              where atkls        :: Piece -> V.Vector BitBoards
                    atkls Knight =  natkls
                    atkls King   =  katkls

attackPos      :: Side -> QBitBoards -> BitBoards
attackPos s qb =  attack Knight (getBoard s Knight qb) .|.
                  attack King   (getBoard s King   qb)

f = bitcheck 0b10010

showBin :: BitBoards -> String
showBin =  go "" 0
           where go :: String -> Word -> Word64 -> String
                 go r 64 _ =  r
                 go r n  w =  go (show (w `mod` 2) ++ r) (n + 1) (w `div` 2)

showBoard   :: BitBoards -> String
showBoard w =  unlines $ map reverse $ chunksOf 8 (showBin w)

getbMove   :: Int -> String
getbMove i =  showBoard (bmagics V.! i) ++ "\n" ++ showBoard (bmask V.! i)

bmagics :: V.Vector BitBoards
bmagics =  V.fromList [ 0x0002020202020200, 0x0002020202020000, 0x0004010202000000, 0x0004040080000000
                      , 0x0001104000000000, 0x0000821040000000, 0x0000410410400000, 0x0000104104104000
                      , 0x0000040404040400, 0x0000020202020200, 0x0000040102020000, 0x0000040400800000
                      , 0x0000011040000000, 0x0000008210400000, 0x0000004104104000, 0x0000002082082000
                      , 0x0004000808080800, 0x0002000404040400, 0x0001000202020200, 0x0000800802004000
                      , 0x0000800400A00000, 0x0000200100884000, 0x0000400082082000, 0x0000200041041000
                      , 0x0002080010101000, 0x0001040008080800, 0x0000208004010400, 0x0000404004010200
                      , 0x0000840000802000, 0x0000404002011000, 0x0000808001041000, 0x0000404000820800
                      , 0x0001041000202000, 0x0000820800101000, 0x0000104400080800, 0x0000020080080080
                      , 0x0000404040040100, 0x0000808100020100, 0x0001010100020800, 0x0000808080010400
                      , 0x0000820820004000, 0x0000410410002000, 0x0000082088001000, 0x0000002011000800
                      , 0x0000080100400400, 0x0001010101000200, 0x0002020202000400, 0x0001010101000200
                      , 0x0000410410400000, 0x0000208208200000, 0x0000002084100000, 0x0000000020880000
                      , 0x0000001002020000, 0x0000040408020000, 0x0004040404040000, 0x0002020202020000
                      , 0x0000104104104000, 0x0000002082082000, 0x0000000020841000, 0x0000000000208800
                      , 0x0000000010020200, 0x0000000404080200, 0x0000040404040400, 0x0002020202020200 ]

bmask :: V.Vector BitBoards
bmask =  V.fromList [ 0x0040201008040200, 0x0000402010080400, 0x0000004020100A00, 0x0000000040221400
                    , 0x0000000002442800, 0x0000000204085000, 0x0000020408102000, 0x0002040810204000
                    , 0x0020100804020000, 0x0040201008040000, 0x00004020100A0000, 0x0000004022140000
                    , 0x0000000244280000, 0x0000020408500000, 0x0002040810200000, 0x0004081020400000
                    , 0x0010080402000200, 0x0020100804000400, 0x004020100A000A00, 0x0000402214001400
                    , 0x0000024428002800, 0x0002040850005000, 0x0004081020002000, 0x0008102040004000
                    , 0x0008040200020400, 0x0010080400040800, 0x0020100A000A1000, 0x0040221400142200
                    , 0x0002442800284400, 0x0004085000500800, 0x0008102000201000, 0x0010204000402000
                    , 0x0004020002040800, 0x0008040004081000, 0x00100A000A102000, 0x0022140014224000
                    , 0x0044280028440200, 0x0008500050080400, 0x0010200020100800, 0x0020400040201000
                    , 0x0002000204081000, 0x0004000408102000, 0x000A000A10204000, 0x0014001422400000
                    , 0x0028002844020000, 0x0050005008040200, 0x0020002010080400, 0x0040004020100800
                    , 0x0000020408102000, 0x0000040810204000, 0x00000A1020400000, 0x0000142240000000
                    , 0x0000284402000000, 0x0000500804020000, 0x0000201008040200, 0x0000402010080400
                    , 0x0002040810204000, 0x0004081020400000, 0x000A102040000000, 0x0014224000000000
                    , 0x0028440200000000, 0x0050080402000000, 0x0020100804020000, 0x0040201008040200 ]


mask :: [BitBoards]
mask =  [ 1
        , 2
        , 4
        , 8
        , 16
        , 32
        , 64
        , 128
        , 256
        , 512
        , 1024
        , 2048
        , 4096
        , 8192
        , 16384
        , 32768
        , 65536
        , 131072
        , 262144
        , 524288
        , 1048576
        , 2097152
        , 4194304
        , 8388608
        , 16777216
        , 33554432
        , 67108864
        , 134217728
        , 268435456
        , 536870912
        , 1073741824
        , 2147483648
        , 4294967296
        , 8589934592
        , 17179869184
        , 34359738368
        , 68719476736
        , 137438953472
        , 274877906944
        , 549755813888
        , 1099511627776
        , 2199023255552
        , 4398046511104
        , 8796093022208
        , 17592186044416
        , 35184372088832
        , 70368744177664
        , 140737488355328
        , 281474976710656
        , 562949953421312
        , 1125899906842624
        , 2251799813685248
        , 4503599627370496
        , 9007199254740992
        , 18014398509481984
        , 36028797018963968
        , 72057594037927936
        , 144115188075855872
        , 288230376151711744
        , 576460752303423488
        , 1152921504606846976
        , 2305843009213693952
        , 4611686018427387904
        , 9223372036854775808
        ]



natkls :: V.Vector BitBoards
natkls =  V.fromList [ 0x0000000000020400 ,0x0000000000050800 ,0x00000000000a1100 ,0x0000000000142200
                     , 0x0000000000284400 ,0x0000000000508800 ,0x0000000000a01000 ,0x0000000000402000
                     , 0x0000000002040004 ,0x0000000005080008 ,0x000000000a110011 ,0x0000000014220022
                     , 0x0000000028440044 ,0x0000000050880088 ,0x00000000a0100010 ,0x0000000040200020
                     , 0x0000000204000402 ,0x0000000508000805 ,0x0000000a1100110a ,0x0000001422002214
                     , 0x0000002844004428 ,0x0000005088008850 ,0x000000a0100010a0 ,0x0000004020002040
                     , 0x0000020400040200 ,0x0000050800080500 ,0x00000a1100110a00 ,0x0000142200221400
                     , 0x0000284400442800 ,0x0000508800885000 ,0x0000a0100010a000 ,0x0000402000204000
                     , 0x0002040004020000 ,0x0005080008050000 ,0x000a1100110a0000 ,0x0014220022140000
                     , 0x0028440044280000 ,0x0050880088500000 ,0x00a0100010a00000 ,0x0040200020400000
                     , 0x0204000402000000 ,0x0508000805000000 ,0x0a1100110a000000 ,0x1422002214000000
                     , 0x2844004428000000 ,0x5088008850000000 ,0xa0100010a0000000 ,0x4020002040000000
                     , 0x0400040200000000 ,0x0800080500000000 ,0x1100110a00000000 ,0x2200221400000000
                     , 0x4400442800000000 ,0x8800885000000000 ,0x100010a000000000 ,0x2000204000000000
                     , 0x0004020000000000 ,0x0008050000000000 ,0x00110a0000000000 ,0x0022140000000000
                     , 0x0044280000000000 ,0x0088500000000000 ,0x0010a00000000000 ,0x0020400000000000 ]

katkls :: V.Vector BitBoards
katkls =  V.fromList [ 0x0000000000000302 ,0x0000000000000705 ,0x0000000000000e0a ,0x0000000000001c14
                     , 0x0000000000003828 ,0x0000000000007050 ,0x000000000000e0a0 ,0x000000000000c040
                     , 0x0000000000030203 ,0x0000000000070507 ,0x00000000000e0a0e ,0x00000000001c141c
                     , 0x0000000000382838 ,0x0000000000705070 ,0x0000000000e0a0e0 ,0x0000000000c040c0
                     , 0x0000000003020300 ,0x0000000007050700 ,0x000000000e0a0e00 ,0x000000001c141c00
                     , 0x0000000038283800 ,0x0000000070507000 ,0x00000000e0a0e000 ,0x00000000c040c000
                     , 0x0000000302030000 ,0x0000000705070000 ,0x0000000e0a0e0000 ,0x0000001c141c0000
                     , 0x0000003828380000 ,0x0000007050700000 ,0x000000e0a0e00000 ,0x000000c040c00000
                     , 0x0000030203000000 ,0x0000070507000000 ,0x00000e0a0e000000 ,0x00001c141c000000
                     , 0x0000382838000000 ,0x0000705070000000 ,0x0000e0a0e0000000 ,0x0000c040c0000000
                     , 0x0003020300000000 ,0x0007050700000000 ,0x000e0a0e00000000 ,0x001c141c00000000
                     , 0x0038283800000000 ,0x0070507000000000 ,0x00e0a0e000000000 ,0x00c040c000000000
                     , 0x0302030000000000 ,0x0705070000000000 ,0x0e0a0e0000000000 ,0x1c141c0000000000
                     , 0x3828380000000000 ,0x7050700000000000 ,0xe0a0e00000000000 ,0xc040c00000000000
                     , 0x0203000000000000 ,0x0507000000000000 ,0x0a0e000000000000 ,0x141c000000000000
                     , 0x2838000000000000 ,0x5070000000000000 ,0xa0e0000000000000 ,0x40c0000000000000 ]
