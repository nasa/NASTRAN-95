      BLOCK DATA TABFBD
CTABFBD
C TABFTX - BLOCK DATA PROGRAM FOR MODULE TABPRT
C
      INTEGER HX, RE
C
      INTEGER HX01(32)
      INTEGER HX02(32)
      INTEGER HX03(32)
      INTEGER HX04(32)
      INTEGER HX05(32)
      INTEGER HX06(32)
      INTEGER HX07(32)
      INTEGER HX08(32)
      INTEGER HX09(32)
      INTEGER HX10(32)
      INTEGER HX11(32)
      INTEGER HX12(32)
      INTEGER HX13(32)
      INTEGER HX14(32)
      INTEGER HX15(32)
      INTEGER HX16(32)
      INTEGER HX17(32)
      INTEGER HX18(32)
      INTEGER HX19(32)
      INTEGER HX20(32)
      INTEGER HX21(32)
      INTEGER HX22(32)
C
      COMMON /TABFTX/ LA,NA(2,21)  ,  HX(32,40)  , RE(21)
C
      EQUIVALENCE (HX01(1),HX(1, 1))
      EQUIVALENCE (HX02(1),HX(1, 2))
      EQUIVALENCE (HX03(1),HX(1, 3))
      EQUIVALENCE (HX04(1),HX(1, 4))
      EQUIVALENCE (HX05(1),HX(1, 5))
      EQUIVALENCE (HX06(1),HX(1, 6))
      EQUIVALENCE (HX07(1),HX(1, 7))
      EQUIVALENCE (HX08(1),HX(1, 8))
      EQUIVALENCE (HX09(1),HX(1, 9))
      EQUIVALENCE (HX10(1),HX(1,10))
      EQUIVALENCE (HX11(1),HX(1,11))
      EQUIVALENCE (HX12(1),HX(1,12))
      EQUIVALENCE (HX13(1),HX(1,13))
      EQUIVALENCE (HX14(1),HX(1,14))
      EQUIVALENCE (HX15(1),HX(1,15))
      EQUIVALENCE (HX16(1),HX(1,16))
      EQUIVALENCE (HX17(1),HX(1,17))
      EQUIVALENCE (HX18(1),HX(1,18))
      EQUIVALENCE (HX19(1),HX(1,19))
      EQUIVALENCE (HX20(1),HX(1,20))
      EQUIVALENCE (HX21(1),HX(1,21))
      EQUIVALENCE (HX22(1),HX(1,22))
C
C-----------------------------------------------------------------------
C
      DATA LA / 9 /
      DATA RE /1,1,1,1,1,1,1,0,1,1
     1        ,1,1,1,1,1,1,1,1,1,1
     2        ,1                    /
C
      DATA NA / 4HBGPD,4HT     ,  4HGPL ,4H      ,  4HCSTM,4H
     4        , 4HGPLD,4H      ,  4HEQEX,4HIN    ,  4HEQDY,4HN
     7        , 4HGPDT,4H      ,  4HGPTT,4H      ,  4HGPCT,4H
     X        , 4H*10*,4H****  ,  4H*11*,4H****  ,  4H*12*,4H****
     3        , 4H*13*,4H****  ,  4H*14*,4H****  ,  4H*15*,4H****
     6        , 4H*16*,4H****  ,  4H*17*,4H****  ,  4H*18*,4H****
     9        , 4H*19*,4H****  ,  4H*20*,4H****  ,  4H*21*,4H****
     Z        /
C
C
      DATA HX01/4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     1         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     2         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     3         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX02/4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     1         ,4H    ,4HFORM,4HATTE,4HD LI,4HST O,4HF TA,4HBLE ,4HDATA
     2         ,4H BLO,4HCK  ,4H****,4H****,4H    ,4H( RE,4HCORD,4H****
     3         ,4H  ) ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX03/4H    ,4H    ,4H    ,4H    ,4HINTE,4HRNAL,4H    ,4H COO
     1         ,4HRDIN,4HATE ,4H    ,4H    ,4H    ,4H COO,4HRDIN,4HATES
     2         ,4H IN ,4HBASI,4HC CO,4HORDI,4HNATE,4H SYS,4HTEM ,4H
     3         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX04/4H    ,4H    ,4H    ,4H    ,4H   I,4HD   ,4H    ,4H SYS
     1         ,4HTEM ,4HID  ,4H    ,4H    ,4H   X,4H    ,4H    ,4H
     2         ,4H    ,4H   Y,4H    ,4H    ,4H    ,4H    ,4H   Z,4H
     3         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX05/4H    ,4H  IN,4HTERN,4HAL  ,4H    ,4H EXT,4HERNA,4HL GR
     1         ,4HID  ,4H    ,4H EXT,4HERNA,4HL GR,4HID  ,4H    ,4H EXT
     2         ,4HERNA,4HL GR,4HID  ,4H    ,4H EXT,4HERNA,4HL GR,4HID
     3         ,4H    ,4H EXT,4HERNA,4HL GR,4HID  ,4H    ,4H    ,4H    /
C
      DATA HX06/4H    ,4H    ,4H ID ,4H    ,4H    ,4H OR ,4HSCAL,4HAR I
     1         ,4HD   ,4H    ,4H OR ,4HSCAL,4HAR I,4HD   ,4H    ,4H OR
     2         ,4HSCAL,4HAR I,4HD   ,4H    ,4H OR ,4HSCAL,4HAR I,4HD
     3         ,4H    ,4H OR ,4HSCAL,4HAR I,4HD   ,4H    ,4H    ,4H    /
C
      DATA HX07/4H    ,4H  IN,4HTERN,4HAL  ,4H    ,4H   E,4HXTER,4HNAL
     1         ,4HGRID,4H   S,4HEQUE,4HNCE ,4H    ,4H    ,4HEXTE,4HRNAL
     2         ,4H GRI,4HD   ,4HSEQU,4HENCE,4H    ,4H    ,4H EXT,4HERNA
     3         ,4HL GR,4HID  ,4H SEQ,4HUENC,4HE   ,4H    ,4H    ,4H    /
C
      DATA HX08/4H    ,4H    ,4H ID ,4H    ,4H    ,4H   O,4HR SC,4HALAR
     1         ,4H ID ,4H    ,4HNUMB,4HER  ,4H    ,4H    ,4HOR S,4HCALA
     2         ,4HR ID,4H    ,4H NUM,4HBER ,4H    ,4H    ,4H OR ,4HSCAL
     3         ,4HAR I,4HD   ,4H  NU,4HMBER,4H    ,4H    ,4H    ,4H    /
C
      DATA HX09/4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     1         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     2         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     3         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX10/4H    ,4H    ,4H    ,4H  N ,4H    ,4H   I,4HD   ,4H
     1         ,4HTYPE,4H    ,4H    ,4H R(I,4H,1) ,4H    ,4H    ,4H
     2         ,4H R(I,4H,2) ,4H    ,4H    ,4H    ,4H R(I,4H,3) ,4H
     3         ,4H    ,4H    ,4H    ,4H    ,4H    ,4HT(I),4H    ,4H    /
C
      DATA HX11/4H   E,4HXTER,4HNAL ,4H    ,4HEXTE,4HRNAL,4H GRI,4HD
     1         ,4HINTE,4HRNAL,4H    ,4H EXT,4HERNA,4HL GR,4HID  ,4H INT
     2         ,4HERNA,4HL   ,4H  EX,4HTERN,4HAL G,4HRID ,4H  IN,4HTERN
     3         ,4HAL  ,4H   E,4HXTER,4HNAL ,4HGRID,4H   I,4HNTER,4HNAL /
C
      DATA HX12/4H   S,4HORT ,4HID  ,4H    ,4HOR S,4HCALA,4HR ID,4H
     1         ,4H NUM,4HBER ,4H    ,4H OR ,4HSCAL,4HAR I,4HD   ,4H  NU
     2         ,4HMBER,4H    ,4H  OR,4H SCA,4HLAR ,4HID  ,4H   N,4HUMBE
     3         ,4HR   ,4H   O,4HR SC,4HALAR,4H ID ,4H    ,4HNUMB,4HER  /
C
      DATA HX13/4H   I,4HNTER,4HNAL ,4H    ,4H    ,4HCOOR,4HDINA,4HTE
     1         ,4H    ,4H    ,4H COO,4HRDIN,4HATES,4H IN ,4HDEFI,4HNING
     2         ,4H COO,4HRDIN,4HATE ,4HSYST,4HEM  ,4H    ,4H  DI,4HSPLA
     3         ,4HCEME,4HNT C,4HOOR-,4H    ,4HCONS,4HTRAI,4HNT  ,4H    /
C
      DATA HX14/4H    ,4H  ID,4H    ,4H    ,4H    ,4HSYST,4HEM  ,4H
     1         ,4H    ,4H    ,4H  X ,4H    ,4H    ,4H    ,4H    ,4HY
     2         ,4H    ,4H    ,4H    ,4H Z  ,4H    ,4H    ,4H  DI,4HNATE
     3         ,4H SYS,4HTEM ,4HID  ,4H    ,4H   C,4HODE ,4H    ,4H    /
C
      DATA HX15/4H   I,4HTERN,4HAL  ,4H    ,4H   T,4HEMPE,4HRATU,4HRE
     1         ,4H    ,4H    ,4HDEFA,4HULT ,4HTEMP,4HERAT,4HURE ,4H
     2         ,4H    ,4H   R,4HECOR,4HD NU,4HMBER,4H FOR,4H    ,4H
     3         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX16/4H    ,4HINDE,4HX   ,4H    ,4H    ,4H SET,4H ID ,4H
     1         ,4H    ,4H    ,4H    ,4H  OR,4H FLA,4HG   ,4H    ,4H
     2         ,4H    ,4H ADD,4HITIO,4HNAL ,4HTEMP,4H. DA,4HTA  ,4H
     3         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX17/4H  SU,4HBSEQ,4HUENT,4H REC,4HORDS,4H OF ,4H G P,4H T T
     1         ,4H  TE,4HMPER,4HATUR,4HE DA,4HTA A,4HRE L,4HISTE,4HD UN
     2         ,4HDER ,4HSET ,4HID A,4HND E,4HLEME,4HNT T,4HYPE ,4HBY E
     3         ,4HLEME,4HNT I,4HD   ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX18/4H   R,4HECOR,4HD NU,4HMBER,4H   T,4HEMPE,4HRATU,4HRE S
     1         ,4HET I,4HD   ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     2         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     3         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX19/4H    ,4H    ,4H    ,4H  F ,4HO R ,4HM A ,4HT T ,4HE D
     1         ,4H  L ,4HI S ,4HT   ,4HO F ,4H  T ,4HA B ,4HL E ,4H  D
     2         ,4HA T ,4HA   ,4HB L ,4HO C ,4HK   ,4HG P ,4HC T ,4H
     3         ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX20/4H  RE,4HCORD,4H    ,4HPIVO,4HT  C,4HONNE,4HCTIN,4HG
     1         ,4H    ,4H    ,4H    ,4H    ,4H SOR,4HTED ,4HLIST,4H OF
     2         ,4H S I,4H L  ,4HNUMB,4HERS ,4HOF C,4HONNE,4HCTED,4H POI
     3         ,4HNTS ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
      DATA HX21/4H  NU,4HMBER,4H    ,4HS I ,4HL   ,4H NUM,4HBER ,4H
     1         ,4H( 1 ,4H)   ,4H  ( ,4H2 ) ,4H    ,4H( 3 ,4H)   ,4H  (
     2         ,4H4 ) ,4H    ,4H( 5 ,4H)   ,4H  ( ,4H6 ) ,4H    ,4H( 7
     3         ,4H)   ,4H  ( ,4H8 ) ,4H    ,4H( 9 ,4H)   ,4H ( 1,4H0 ) /
C
      DATA HX22/4H   S,4HORT ,4HID  ,4H    ,4HOR S,4HCALA,4HR ID,4H   C
     1         ,4HODED,4H SIL,4H    ,4H OR ,4HSCAL,4HAR I,4HD   ,4HCODE
     2         ,4HD SI,4HL   ,4H  OR,4H SCA,4HLAR ,4HID  ,4H COD,4HED S
     3         ,4HIL  ,4H   O,4HR SC,4HALAR,4H ID ,4H  CO,4HDED ,4HSIL /
C
      END
