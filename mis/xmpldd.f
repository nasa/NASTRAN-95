      SUBROUTINE XMPLDD
C
C
C     MPL    = MODULE PROPERTIES TABLE
C     LMPL   = LENGTH OF MPL TABLE
C     MPLPNT = POINTER TO AN MPL ENTRY
C
C     DESCRIPTION OF VARIABLES EQUIVALENCED TO /XGPI2/ ENTRIES
C     EQUIVALENCE (LMPL,LORDNL),(MPLPNT,IORBOT),(MPL,IORDNL)
C
C     IORDNL = TABLE USED TO COMPUTE FILE ORDNALS AND NUT VALUES
C     LORDNL = LENGTH OF IORDNL TABLE
C     IORBOT = POINTER TO LAST ENTRY MADE IN IORDNL TABLE
C
C     ==================================================================
C
C     NOTE DATA ITEMS HAVE BLANK WORDS TO FACILITATE ADDITIONS
C     CHANGE DIMENSIONS ONLY IF THE BLANKS ARE DEPLETED BY ADDITIONS
C
C     ANY FOLLOWING DATA LINE ENDS WITH BCD BLANKS SHOULD BE FOLLOWED BY
C     COMMA, SO THAT A STRIPPING ROUTINE WOULD NOT STRIP OFF THOSE BLANK
C
C     ==================================================================
C
C                        LOAD /XGPI2/
C                   MODULE PROPTERIES LIST (MPL)
C
      REAL             X(2,20)
      DOUBLE PRECISION XX(20)    , XXX(20)
      DIMENSION        MPL01( 68), MPL02(161), MPL03(135), MPL04(152),
     5                 MPL05(138), MPL06(162), MPL07(200), MPL08(137),
     9                 MPL09(173), MPL10( 93), MPL11(116), MPL12(135),
     3                 MPL13(150), MPL14(151), MPL15(135), MPL16( 53),
     7                 MPL17(144), MPL18(169), MPL19(193), MPL20(186),
     1                 MPL21(196), MPL22(119), MPL( 3166)
      COMMON /XGPI2 /  LMPL, MPLPNT    , IMP( 3166)
      COMMON /XGPI2X/  XXX
      EQUIVALENCE      (XX(1),X(1,1))
      EQUIVALENCE      (MPL(   1),MPL01(1)) ,(MPL(  69),MPL02(1)) ,
     3                 (MPL( 230),MPL03(1)) ,(MPL( 365),MPL04(1)) ,
     5                 (MPL( 517),MPL05(1)) ,(MPL( 655),MPL06(1)) ,
     7                 (MPL( 817),MPL07(1)) ,(MPL(1017),MPL08(1)) ,
     9                 (MPL(1154),MPL09(1)) ,(MPL(1327),MPL10(1)) ,
     1                 (MPL(1420),MPL11(1)) ,(MPL(1536),MPL12(1)) ,
     3                 (MPL(1671),MPL13(1)) ,(MPL(1821),MPL14(1)) ,
     5                 (MPL(1972),MPL15(1)) ,(MPL(2107),MPL16(1)) ,
     7                 (MPL(2160),MPL17(1)) ,(MPL(2304),MPL18(1)) ,
     9                 (MPL(2473),MPL19(1)) ,(MPL(2666),MPL20(1)) ,
     1                 (MPL(2852),MPL21(1)) ,(MPL(3048),MPL22(1))
C
      DATA    LMPLX          /    3166      /
C
      DATA    X(1,1)         /    -1.0      /
      DATA    XX(2)          /    -1.0D+0   /
      DATA    X(1,3),X(2,3)  /  2*-1.0      /
      DATA    XX(4),XX(5)    /  2*-1.0D+0   /
      DATA    X(1,6)         /     1.0      /
      DATA    X(1,7),X(2,7)  / 1.0,0.0      /
      DATA    XX(8)          /     0.0D+0   /
      DATA    X(1,9),X(2,9)  /     2*0.0    /
      DATA    X(1,10)        /     30.0     /
      DATA    X(1,11)        /     0.001    /
      DATA    X(1,12)        /     0.55     /
      DATA    X(1,13)        /     0.01     /
      DATA    X(1,14)        /     0.00001  /
      DATA    X(1,15)        /     1.01     /
      DATA    X(1,16)        /     0.80     /
      DATA    XX(17)         /     1.1D+37  /
      DATA    X(1,18),X(2,18)/   2*1.1E+37  /
C
C
      DATA    MPL01          /
     1   4,   4HFILE,4H    , 5
     2,  4,   4HBEGI,4HN   , 5
     3,  4,   4HCHKP,4HNT  , 4
     4,  4,   4HLABE,4HL   , 5
     5,  4,   4HREPT,4H    , 3
     6,  4,   4HJUMP,4H    , 3
     7,  4,   4HCOND,4H    , 3
     8,  4,   4HSAVE,4H    , 4
     9,  4,   4HPURG,4HE   , 4
     X,  4,   4HEQUI,4HV   , 4
     1,  4,   4HEND ,4H    , 3
     2,  4,   4HEXIT,4H    , 3
     M, 20,   19*0
     Z/
C
C      IN NEXT 12 LINES, '1' MAY MEAN NUMERIC ONE, OR A VERTICAL BAR
C
C      NO. OF WORDS        1  I  O  S   ---PARAMETERS
C      OF THIS DMAP           N  U  C   1  NEGATIVE FOR NO DEFAULT
C      LINE               OR  P  T  R   1  POSITIVE INDICATES DEFAULT TO
C        1                    U  P  A   1   1 = INTEGER    NEXT VALUE(S)
C        1   DAMP NAME     2  T  U  T   1   2 = RSP
C        1   1                1  T  C   1   3 = BCD    (NEXT WORD(S)
C        1   1     1. I/O DB  1  1  H   1   4 = RDP    AFTER 2,4,5,6 ARE
C        1   1     2. NO OUT- 1  1  1   1   5 = CSP    POINTER TO DEF-
C        1   1        PUT DB  1  1  1   1   6 = CDP    AULT VALUE(S) IN
C        1   1             1  1  1  1   1              X OR XX ARRAYS)
C        1   1             1  1  1  1   1   REF. PROG. MAN. SEC 2.4.2.2
      DATA  MPL02 /
     1  25, 4HADD ,4H    , 1, 2, 1, 0,  5,  2*18, 5, 2*18, 6,  4*17
     2                               ,  6,  4*17, 1, 0
     2, 22, 4HADD5,4H    , 1, 5, 1, 0,  5,  7, 7, 5, 7, 7, 5,7,7, 5,7,7
     *                               ,  5,  7, 7
     3, 10, 4HAMG ,4H    , 1, 2, 4, 5,  3* -1
     4, 11, 4HAMP ,4H    , 1,10, 3,14,  2* -1, 1,-1
     5, 12, 4HAPD ,4H    , 1, 8,12, 5,  3* -1, 2, 9
     6, 10, 4HBMG ,4H    , 1, 4, 1, 1, -1, -1,-5
     7, 12, 4HCASE,4H    , 1, 2, 1, 0, -3,  1, 1, 1,-1
     9, 15, 4HCYCT,4H1   , 1, 1, 2, 3, -3, -3,-1,-1, 1, 1,1, 1
     X, 16, 4HCYCT,4H2   , 1, 6, 5, 6, -3, -1,-1, 1,-1, 1,1, 1,1
     8, 10, 4HCEAD,4H    , 1, 5, 4,12, -1,  1, 1
     M, 11, 4HCURV,4H    , 1, 6, 2, 5,  1, -1, 1, 0
     N,  7, 6*0
     Z/
C
      DATA  MPL03 /
     1  10, 4HDDR ,4H    , 1, 1, 1, 0, -3,-3,-3
     2,  7, 4HDDR1,4H    , 1, 2, 1, 1
     3, 14, 4HDDR2,4H    , 1, 9, 3, 6, -3, 1,-1, 1,-1,   1,-1
     4,  7, 4HDDRM,4HM   , 1,11, 5, 7
     5, 21, 4HDECO,4HMP  , 1, 1, 2, 4,  1, 0, 1, 0, 4,   8, 8, 5, 9, 9
     *                               ,  1, 0, 1, 0
     6, 12, 4HDIAG,4HONAL, 1, 1, 1, 0,  3,4HCOLU,4HMN  , 2, 6
     7, 19, 4HDPD ,4H    , 1, 4,11, 4,  9*-1, 1, 1,-1
     8, 16, 4HDSCH,4HK   , 1, 3, 0, 3,  2*-2, 7*-1
     9,  8, 4HDSMG,4H1   , 1,10, 1, 1, -1
     X, 11, 4HDSMG,4H2   , 1,11, 7, 0,  1, 0,-1,-1
     M, 10, 9*0
     Z/
C
      DATA  MPL04 /
     1  33, 4HDUMM,4HOD1 , 1, 1, 2, 3,  1,-1, 1,-1, 1,-1, 1,-1, 2,1, 2,1
     *                               ,  3, 4HABCD , 4HEFGH , 4, 2,2, 5,3
     *                               ,  3, 6, 4, 4, 5, 5
     2, 33, 4HDUMM,4HOD2 , 1, 8, 8,10,  1,-1, 1,-1, 1,-1, 1,-1, 2,1, 2,1
     *                               ,  3, 4HABCD , 4HEFGH,  4, 2,2, 5,3
     *                               ,  3, 6, 4, 4, 5, 5
     3, 33, 4HDUMM,4HOD3 , 1, 8, 8,10,  1,-1, 1,-1, 1,-1, 1,-1, 2,1, 2,1
     *                               ,  3, 4HABCD , 4HEFGH , 4, 2,2, 5,3
     *                               ,  3, 6, 4, 4, 5, 5
     4, 33, 4HDUMM,4HOD4 , 1, 8, 8,10,  1,-1, 1,-1, 1,-1, 1,-1, 2,1, 2,1
     *                               ,  3, 4HABCD , 4HEFGH,  4, 2,2, 5,3
     *                               ,  3, 6, 4, 4, 5, 5
     M, 20, 19*0
     Z/
C
      DATA  MPL05 /
     1  11, 4HEMA1,4H    , 1, 5, 1, 2,  1,-1, 2, 6
     2, 43, 4HEMG ,4H    , 1, 6, 7, 4,  1,-1, 1,-1, 1,-1, 1,-1, 1,-1
     *                               ,  1,-1, 1,-1, 1,-1, 1,-1, 1,-1
     *                               ,  1,-1, 1,-1, 1,-1, 1,-1, 1,-1
     *                               ,  1,-1, 2, 9, 2, 9
     3, 11, 4HFA1 ,4H    , 1, 6, 4, 6,  2*-1, 1, 0
     4, 12, 4HFA2 ,4H    , 1, 3, 4, 0, -1,-2, 3, 4HYES   ,4H     ,
     5  15, 4HFBS ,4H    , 1, 3, 1, 1,  1, 0, 1, 1, 1, 0, 1, 0
     6, 13, 4HFRLG,4H    , 1, 8, 5, 4, -3, 1,-1, 3, 4HFREQ,4H    ,
     7  17, 4HFRRD,4H    , 1,11, 4, 8, -3,-3,-1,-1,-1,-1,-1,-1
     *                               ,  1, 1
     M, 16, 15*0
     Z/
C
      DATA  MPL06 /
     1   9, 4HGI  ,4H    , 1, 8, 1, 6,  2*-1
     2, 24, 4HGKAD,4H    , 1,10, 8, 6, -3,-3,-3,-2,-2,-2, 11*-1
     3, 21, 4HGKAM,4H    , 1, 9, 4, 4, -1,-1, 2, 9, 2, 1,  4*-1
     *                   , 1, 1, 1,-1
     4, 11, 4HGP1 ,4H    , 1, 3, 6, 2, -1,-1, 1, 1
     5,  7, 4HGP2 ,4H    , 1, 2, 1, 4
     6, 12, 4HGP3 ,4H    , 1, 3, 2, 2, -1, 1, 1, 1, 1
     7, 24, 4HGP4 ,4H    , 1, 7, 5, 2,  9*-1, 1, 1, 1,-1, 1,0,  1,0
     8, 10, 4HGPCY,4HC   , 1, 3, 1, 2, -3, 1, 1
     A,  8, 4HGPFD,4HR   , 1, 9, 2, 4, -3
     9, 20, 4HDUMM,4HOD5 , 1, 5, 5, 0, -1, 1, 0, 1, 0, 1, 0,1,0,1,0,1,0
     X, 11, 4HGPWG,4H    , 1, 4, 1, 4,  1,-1, 2, 6
     M,  5, 4*0
     Z/
C
      DATA  MPL07 /
     1  13, 4HINPU,4HT   , 1, 5, 5, 0,  1,-1, 1, 0, 1, 0
     2, 17, 4HINPU,4HTT1 , 1, 0, 5, 0,  1, 0, 1, 0, 3, 4HXXXX,4HXXXX
     *                               ,  3, 4H     , 4H       ,
     3  21, 4HINPU,4HTT2 , 1, 0, 5, 0,  1, 0, 1,14, 3, 4HXXXX,4HXXXX
     *                               ,  1, 0, 1, 0, 3, 4H    ,4H    ,
     4  13, 4HINPU,4HTT3 , 1, 5, 5, 0,  1,-11, 1, 0, 1, 0
     5, 16, 4HINPU,4HTT4 , 1, 0, 5, 0,  1, 1, 1,14, 3, 4HXXXX,4HXXXX
     *                               ,  1, 0
     6, 29, 4HMATG,4HEN  , 1, 1, 1, 0,  1, 0, 1, 0, 1, 0, 1, 0, 1, 0
     *                            , 1,  0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0
     7, 18, 4HMATG,4HPR  , 2, 4, 0, 0, -3, 3,  4H    , 4H     , 3
     *                               ,  4HALL ,4H    , 2, 9, 1, 0
     8, 19, 4HMATP,4HRN  , 2, 5, 0, 0,  1, 0, 1, 0, 1, 0, 1, 0, 1, 0
     *                               ,  1, 0
     9, 11, 4HMATP,4HRT  , 2, 1, 0, 0,  1, 0, 1, 0
     X,  7, 4HMCE1,4H    , 1, 2, 1, 7
     1,  7, 4HMCE2,4H    , 1, 6, 4, 6
     2, 17, 4HMERG,4HE   , 1, 6, 1, 0,  1,-1, 1, 0, 1, 0, 1, 0, 1, 0
     M, 12, 11*0  /
C
      DATA  MPL08 /
     1  20, 4HMODA,4H    , 1, 0, 4, 0,  5*-2, 5* -1, -2, -1, -1
     2, 10, 4HMODA,4HCC  , 1, 6, 5, 0,  3,4HTRAN,4H        ,
     3  18, 4HMODB,4H    , 1, 3, 4, 0,  4*-2, 3* -1, -2,  3* -1
     4,  8, 4HMODC,4H    , 1, 2, 0, 0, -1
     5, 14, 4HMPYA,4HD   , 1, 3, 1, 1, -1, 1,  1, 1,  1,  1,  0
     6, 14, 4HMTRX,4HIN  , 1, 5, 3, 7, -1, 1, -1, 1, -1,  1, -1
     7, 11, 4HOFP ,4H    , 2, 6, 0, 0,  1, 0,  1,-1
     8, 10, 4HOPTP,4HR1  , 1, 5, 1, 1,  3*-1
     9, 12, 4HOPTP,4HR2  , 1, 3, 2, 0,  3*-1,  1, 0
     M, 20, 19*0
     Z/
C
      DATA  MPL09 /
     1   9, 4HOUTP,4HUT  , 2, 1, 0, 0,  1,-1
     2, 14, 4HOUTP,4HUT1 , 2, 5, 0, 0,  1, 0, 1, 0,3,4HXXXX,4HXXXX
     3, 21, 4HOUTP,4HUT2 , 2, 5, 0, 0,  1, 0, 1,14,3,4HXXXX,4HXXXX,
     *                                  1, 0, 1, 0,3,4H    ,4H    ,
     4  22, 4HOUTP,4HUT3 , 2, 5, 0, 0,  1, 0,-3,   3,3HXXX,1H ,3,3HXXX
     *                               ,  1H  , 3,3HXXX,1H ,3,3HXXX,1H ,
     5  13, 4HOUTP,4HUT4 , 2, 5, 0, 0,  1,-1, 1,14,1, 1
     6, 14, 4HPARA,4HM   , 1, 0, 0, 0, -3, 1, 1, 1,1, 1,1
     7, 30, 4HPARA,4HML  , 2, 1, 0, 0, -3, 1, 1, 1,1, 2,9, 1,0,   4,8,8
     *                               ,  3, 4H(VOI ,4HD)  , 5,9,9, 6,4*8
     8, 25, 4HPARA,4HMR  , 2, 0, 0, 0, -3, 2, 9, 2,9, 2,9, 5,9,9, 5,9,9
     *                               ,  5, 9, 9, 1,0
     9, 19, 4HPART,4HN   , 1, 3, 4, 0,  1,-1, 1, 0,1, 0,1, 0,1,0, 1,0
     M, 6, 5*0
     Z/
C
      DATA  MPL10 /
     1  17, 4HMRED,4H1   , 1, 4, 4, 1, -3,-1,-1,-1, -1,-3, 1,0, 2,9
     2, 14, 4HMRED,4H2   , 1,12, 6,11, -1,-1, 3,4H    ,4H     , 1,0
     3, 12, 4HCMRE,4HD2  , 1,11, 6,11, -1,-1, 3,4H    ,4H     ,
     4  13, 4HPLA1,4H    , 1, 7, 4, 0,  5*-1,-5
     5,  8, 4HPLA2,4H    , 1, 3, 3, 0, -1
     6,  9, 4HPLA3,4H    , 1, 6, 2, 1, -1,-1
     7, 10, 4HPLA4,4H    , 1, 6, 2, 1, -1,-1,-5
     M, 10, 9*0
     Z/
C
      DATA  MPL11 /
     1  14, 4HPLOT,4H    , 1,13, 1, 4,  3*-1, 1, 1, 1, 0
     2, 10, 4HPLTS,4HET  , 1, 4, 4, 2, -1, 1,-1
     3, 11, 4HPLTT,4HRAN , 1, 2, 2, 0,  1, 0, 1, 0
     4,  7, 4HPRTM,4HSG  , 2, 1, 0, 0
     5, 13, 4HPRTP,4HARM , 2, 0, 0, 0, -1, 3, 4HXXXX ,4HXXXX, 1,0
     6,  9, 4HRAND,4HOM  , 1, 9, 2, 0,  1,-1
     7,  7, 4HRBMG,4H1   , 1, 3, 6, 1
     8, 11, 4HRBMG,4H2   , 1, 1, 1, 4,  1, 1, 2, 6
     9,  7, 4HRBMG,4H3   , 1, 3, 1, 2
     X,  7, 4HRBMG,4H4   , 1, 4, 1, 3
     M, 20, 19*0
     Z/
C
      DATA  MPL12 /
     1  13, 4HREAD,4H    , 1, 7, 4,10, -3,-1, 1, 1, 2,  6
     2, 14, 4HRMG ,4H    , 1, 4, 3, 6,  2, 9, 2, 9, 1, -1, -1
     3, 24, 4HSCAL,4HAR  , 2, 1, 0, 0,  1, 1, 1, 1, 2,  9,  4,8,8
     *                               ,  5, 9, 9, 6, 4*8
     4,  7, 4HSCE1,4H    , 1, 5, 6, 1
     5,  9, 4HSDR1,4H    , 1,11, 3, 6, -1,-3
     6, 16, 4HSDR2,4H    , 1,16, 8, 3, -3, 1, 1, 1,-1,  1,-1, 1,1
     7,  7, 4HSDR3,4H    , 1, 6, 6, 8
     8, 11, 4HSDRH,4HT   , 1,10, 1, 3,  2, 9, 1,-1
     9, 23, 4HSEEM,4HAT  , 2, 5, 0, 0,  3,4HPRIN,4HT   , 1,0, 1,100
     *                               ,  3,4HM   ,4H    , 1,1, 2,9, 2,9
     M, 11, 10*0
     Z/
C
      DATA  MPL13 /
     1  26, 4HSETV,4HAL  , 2, 0, 0, 0, -1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1
     *                               ,  1,-1, 1,-1, 1,-1, 1,-1
     2, 11, 4HSMA1,4H    , 1, 5, 3, 2, -1,-1, 1,-1
     3, 32, 4HSMA2,4H    , 1, 5, 2, 2, -2,-1,-1, 1,-1, 1,-1, 1,-1
     *                               ,  1,-1, 1,-1, 1,-1, 1,-1, 1,-1
     *                               ,  1,-1, 1,-1, 1,-1
     4, 10, 4HSMA3,4H    , 1, 2, 1, 7, -1,-1,-1
     5,  7, 4HSMP1,4H    , 1, 5, 9, 7
     6,  7, 4HSMP2,4H    , 1, 3, 1, 6
     7, 22, 4HSMPY,4HAD  , 1, 6, 1, 2, -1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0
     *                               ,  1, 0, 1, 0
     8, 15, 4HSOLV,4HE   , 1, 2, 1, 5,  1, 0, 1, 1, 1, 0, 1, 0
     M, 20, 19*0
     Z/
C
      DATA  MPL14 /
     1  13, 4HSSG1,4H    , 1,12, 5, 7, -1,-1, 1, 0, 2,14
     2,  7, 4HSSG2,4H    , 1, 7, 4, 4
     3, 13, 4HSSG3,4H    , 1, 6, 4, 2, -1,-1, 1, 1, 1, 1
     4,  8, 4HSSG4,4H    , 1,11, 2, 5, -1
     5, 23, 4HSSGH,4HT   , 1,17, 3, 5,  1,-1, 1,-1, 2,11, 2, 9, 1, 4
     *                               ,  1,-1, 1, 0, 1, 0
     6, 15, 4HTA1 ,4H    , 1, 8, 8, 4, -1,-1, 1, 1,-1,-1, 1, 1
     7, 22, 4HTABP,4HCH  , 2, 5, 0, 0,  3,     4HAA  ,4H      ,3
     *                               ,  4HAB  ,4H    ,3,4HAC  ,4H    ,3
     *                               ,  4HAD  ,4H    ,3,4HAE  ,4H    ,
     8  13, 12*0
     9, 12, 4HTABP,4HRT  , 2, 1, 0, 0, -3, 1, 0, 1, 0
     X,  7, 4HTABP,4HT   , 2, 5, 0, 0
     M, 18, 17*0
     Z/
C
      DATA  MPL15 /
     1  17, 4HTIME,4HTEST, 1, 0, 0, 2,  1,50, 1,50, 1, 2, 1, 1, 1, 511
     2, 13, 4HTRD ,4H    , 1, 8, 3, 9, -3, 3*-1, 1,-1
     3, 17, 4HTRHT,4H    , 1,10, 2, 7,  2,12, 2, 9, 1,-1, 1,-1, 2, 9
     4, 11, 4HTRLG,4H    , 1,15, 6, 9,  1,-1, 1, 0
     5,  9, 4HTRNS,4HP   , 1, 1, 1, 8,  1, 0
     6, 10, 4HUMER,4HGE  , 1, 3, 1, 1, -3,-3,-3
     7, 10, 4HUPAR,4HTN  , 1, 2, 4, 1, -3,-3,-3
     8, 14, 4HVDR ,4H    , 1, 7, 2, 2, -3,-3,-1, 1, 0,-1,-1
     9, 16, 4HVEC ,4H    , 1, 1, 1, 0, -3, 3, 4HCOMP ,1H   , 3
     *                               ,        4HCOMP, 1H   , 1, 0
     M, 18, 17*0
     Z/
C
      DATA  MPL16 /
     1   7, 4HXYPL,4HOT  , 2, 1, 0, 2
     2,  7, 4HXYPR,4HNPLT, 2, 1, 0, 0
     3, 19, 4HXYTR,4HAN  , 1, 6, 1, 5,  3,4HTRAN,4HS   ,3,4HSOL ,4H    ,
     *                                  1, 0, 1,0, 1,1
     M, 20, 19*0
     Z/
C
      DATA  MPL17 /
     1  13, 4HCOMB,4H1    , 1, 2, 1,10,  1,     0,-1,  3,4H    ,4H    ,
     2  35, 4HCOMB,4H2    , 1, 7, 1, 7, -1,    -3,     3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,1,0
     3, 36, 4HEXIO,4H     , 2, 0, 0, 2,  2*-1,  5*-3,  3
     *                                ,  4HALL ,4H    ,3,4HWHOL,4HESOF,3
     *                                ,  4HXXXX,4HXXXX,3,4HXXXX,4HXXXX,3
     *                                ,  4HXXXX,4HXXXX,3,4HXXXX,4HXXXX
     *                                ,  1, 0,  1, 0
     4, 39, 4HRCOV,4HR    , 1,11, 8, 9,  3*-1, -3,-1,  1, 0,    1,0,   3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,1,-1,2,9,2,9, 2,9
C
C     THE BCD PARAMETER   IN THE NEXT MODULE IS A DUMMY SINCE WE NEED
C     11 WORDS IN THIS SPACE
C
     5, 11, 4HEMFL,4HD    , 1,10, 1, 1, -1,   3,4H    ,4H    ,
     M  10, 9*0
     Z/
C
      DATA  MPL18 /
     1  11, 4HRCOV,4HR3   , 1, 4, 7, 3, -1, -3, 1,-1
     2, 16, 4HREDU,4HCE   , 1, 2, 3, 2,  1,  0, 1, 0,  3,4H    ,4H    ,1
     *                                ,  0
     3, 11, 4HSGEN,4H     , 1, 4,10, 0, -1, -3,-1,-1
     4, 25, 4HSOFI,4H     , 1, 0, 5, 0,  1, -1,-3,     3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,
     5  25, 4HSOFO,4H     , 2, 5, 0, 0,  1, -1,-3,     3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,
     6  37, 4HSOFU,4HT    , 2, 0, 0, 1,  1, -1, 2*-3,  1,0     ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,3
     *                                ,  4H    ,4H    ,3,4H    ,4H    ,
     7  15, 4HSUBP,4HH1   , 2, 7, 0, 1,  1,0,-3,1,0,   3,4H    ,4H    ,
     8  11, 4HPLTM,4HRG   , 1, 2, 6, 1, -3,3*-1,
     M  18, 17*0
     Z/
C
      DATA  MPL19 /
     1   9, 4HCOPY,4H    , 1, 1, 1, 0,  1, -1
     2,  9, 4HSWIT,4HCH  , 2, 2, 0, 0,  1, -1
     3, 11, 4HMPY3,4H    , 1, 3, 1, 3,  1,  0, 1, 0
     4, 33, 4HSDCM,4HPS  , 1, 4, 2, 6,  1,  0, 1, 0, 1,20, 1,0,   1,0
     *                               ,  3, 1HL,1H  , 1, 0, 5,9,9, 4,8,8
     *                               ,  1,  0, 3,4H NON,  1HE
     5,  9, 4HLODA,4HPP  , 2, 2, 0, 8, -3, -1
     6,  7, 4HGPST,4HGEN , 1, 2, 1, 0
     7, 15, 4HEQMC,4HK   , 1,12, 1, 7,  1,  0, 1,-1, -1, 3,4H NON,1HE
     8, 11, 4HADR ,4H    , 1, 7, 1, 5, -2,  2, 9,-3
     9, 12, 4HFRRD,4H2   , 1, 6, 1, 9, -2,  2, 9, 2, 9
     X, 14, 4HGUST,4H    , 1,10, 1, 7, -1,  2, 9, 2, 9, 2, 9
     1,  9, 4HIFT ,4H    , 1, 4, 2, 0,  1,  1
     2,  9, 4HLAMX,4H    , 1, 2, 1, 0,  1,  0
     3, 11, 4HEMA ,4H    , 1, 3, 1, 2,  1, -1, 2, 6
     4,  9, 4HANIS,4HOP  , 1, 5, 1, 0,  1,  1
     M, 25, 24*0
     Z/
      DATA  MPL20 /
     1  11, 4HGENC,4HOS  , 1, 2, 1, 0, -1, -1,-1,-1
     2,  8, 4HDDAM,4HAT  , 1, 2, 1, 0, -2
     3,  9, 4HDDAM,4HPG  , 1, 2, 1, 0, -1, -1
     4, 11, 4HNRLS,4HUM  , 1, 2, 2, 3, -1, -1,-1,-1
     5,  9, 4HGENP,4HART , 1, 1, 4, 0, -1, -1
     6, 10, 4HCASE,4HGEN , 1, 1, 1, 0, -1, -1,-1
     7, 21, 4HDESV,4HEL  , 1, 2, 5, 0,  14*-2
C
C     3 DUMMY PARAMETERS IN PROLATE SO THAT AXLOOP CAN HAVE A PARAMETER
C     IN THE SAME POSITION IN BOTH SSG1 AND PROLATE
C
     8, 15, 4HPROL,4HATE , 1,10, 1, 2,  1, -1, 1,-1, 1,-1, 2,14
     9,  8, 4HMAGB,4HDY  , 1, 2, 1, 0, -1
     X,  9, 4HCOMB,4HUGV , 1, 1, 5, 0, -1, -1
     1, 14, 4HFLBM,4HG   , 1, 9, 4, 7, -1, -1, 5, 7, 7, 1, 0
     2, 17, 4HGFSM,4HA   , 1,14, 5, 8, -1, -1, 2, 6, 1,-1, 1,-1, 1,-1
     3, 10, 4HTRAI,4HLER , 2, 1, 0, 0, -3, -1,-1
CRLBR 12/29/93  SPR 93010 & 93011
C    4, 24, 4HSCAN,4H    , 1, 3, 1, 1,  3,4H    , 4H     , 1, 0, 1,20
     4, 24, 4HSCAN,4H    , 1, 5, 2, 1,  3,4H    , 4H     , 1, 0, 1,20
     *                               ,  2,  9, 2, 9, 1, 0, 1, 0, 1, 0
     M, 10, 9*0
     Z/
      DATA  MPL21 /
     1   9, 4HPLTH,4HBDY , 1, 6, 4, 3, -1, -3
     2,  9, 4HVARI,4HAN  , 1, 5, 5, 3, -3, -2
     3, 21, 4HFVRS,4HTR1 , 1, 8, 8,10,  13*-1,-2
     4, 15, 4HFVRS,4HTR2 , 1, 8, 8,10,  8* -1
     5, 29, 4HALG ,4H    , 1, 7, 2, 4,  1, -1, 1, -1, 1, -1, 1, -1, 1, 0
     *                               ,  1,  0, 2,  6, 2,  9, 2,  6, 2, 6
     *                               ,  2,  6
     6, 20, 4HAPDB,4H    , 1, 7, 5, 5, -1, -1, 2, 15, 2, 16, 1, -1
     *                               ,  3,  4HCOSI,4HNE    ,-1, -1
     7, 27, 4HPROM,4HPT1 , 2, 0, 0, 0,  1,  0, 1,  0, 1,  0, 1,  0, 1, 0
     *                               ,  1,  0, 1,  0, 1,  0, 1,  0, 1, 0
     8,  7, 4HSITE,4HPLOT, 2, 0, 0, 0
     9, 16, 4HINPU,4HTT5 , 1, 0, 5, 0,  1,  0, 1, 11, 3,4HXXXX,4HXXXX
     *                               ,  1,  0
     X, 36, 4HOUTP,4HUT5 , 2, 5, 0, 0,  1,  0, 1, 11, 3,4HXXXX,4HXXXX
     *                               ,  1,  0, 1,  0, 1,  0, 1,  0, 1, 0
     *                               ,  1,  0, 1,  0, 1,  0, 1,  0, 1, 0
     *                               ,  1,  0
     M,  7, 6*0  /
      DATA  MPL22 /
     1  34, 4HPARA,4HMD  , 2, 0, 0, 0, -3,  4,8,8, 4,8,8, 4,8,8, 6,4*8
     *                               ,  6,  4*8,   6,4*8, 1,0
     2, 12, 4HGINO,4HFILE, 1, 0, 1, 1, -1,  1,0,   1,999999
     3, 13, 4HDATA,4HBASE, 2, 7, 0, 1,  1, 11,  1, 0,  1, 0
     4, 16, 4HNORM,4H    , 1, 1, 1, 0,  1,  0,  1, 0,  2, 9, 3
     *                               ,  4HMAX , 4H      ,
     5  13, 4HVECG,4HRB  , 1, 3, 1, 0,  1,  0,  1, 0,  1, 0
     6, 21, 4HAUTO,4HASET, 1, 6, 2, 1,  1, -1,  1, -1, 1, -1, 1, -1
     *                               ,  1, -1,  1, -1, 1, -1
     M, 10, 9*0  /
C
C     INITIALIZE /XGPI2/
C
      LMPL = LMPLX
      DO 10 I = 1,LMPL
   10 IMP(I) = MPL(I)
C
C     INITIALIZE /XGPI2X/
C
      DO 20 I = 1,20
   20 XXX(I) = XX(I)
C
      RETURN
      END
