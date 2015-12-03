      SUBROUTINE ASCM07 (NAME,IPHASE,ISOL,NOGO)
C
C     BRECOVER COMMAND DMAP DATA
C
      INTEGER         COMND(6,1),SUBNAM(2),RDMAP(18,21),RDMAP1(18,9),
     1                RDMAP2(18,9),RDMAP3(18,3),OCT(3,13),OCT1(3,13),
     2                PTBS(7,26),PTBS1(7,18),PTBS2(7,8)
      COMMON /PHAS37/ IPAS37(6)
      COMMON /ASDBD / IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1                IPH,NPH,IDAT(605)
      EQUIVALENCE     (RDMAP1(1,1),RDMAP(1, 1)),(OCT1(1,1),OCT(1,1)),
     1                (RDMAP2(1,1),RDMAP(1,10)),(PTBS1(1,1),PTBS(1,1)),
     2                (RDMAP3(1,1),RDMAP(1,19)),(PTBS2(1,1),PTBS(1,19))
      DATA COMND    /
     1                4HBREC  , 21    ,  0    , 13    , 26    ,  6  /
      DATA SLASH    / 1H/     /
      DATA RDMAP 1  /
     1 4HALTE,4HR   ,4H  (S,4HOLVE,4H) $ ,13*4H    ,
     2 4HPARA,4HM   ,4H  //,4H*NOP,4H*/AL,4HWAYS,4H=-1 ,4H$   ,4H    ,
     * 4H    , 8*4H    ,
     3 4HSSG1,4H    ,4H  SL,4HT,BG,4HPDT,,4HCSTM,4H,SIL,4H,EST,4H,MPT,
     * 4H,GPT,4HT,ED,4HT,MG,4HG,CA,4HSECC,4H,DIT,4H,/PG,4H,,,,,4H/   ,
     4 4H    ,4H    ,4H  LU,4HSET/,4HNSKI,4HP $ ,12*4H    ,
     5 4HSSG2,4H    ,4H  US,4HET,G,4HM,YS,4H,KFS,4H,GO,,4H,PG/,4HQR,P,
     * 4HO,PS,4H,PL ,4H$   , 6*4H    ,
     6 4HRCOV,4HR3  ,4H  ,P,4HG,PS,4H,PO,,4HYS/U,4HAS ,,4HQAS,,4HPGS,,
     * 4HPSS,,4HPOS,,4HYSS,,4HLAMA,4H/SOL,4HN!*N,4HAME ,4H   *,4H/   ,
     7 4H    ,4H    ,4H  NO,4HUE $,14*4H    ,
     8 4HEQUI,4HV   ,4H  PG,4HS,PG,4H/ALW,4HAYS ,4H$   ,4H    ,4H    ,
     * 4H    , 8*4H    ,
     9 4HEQUI,4HV   ,4H  PS,4HS,PS,4H/ALW,4HAYS ,4H$   ,4H    ,4H    ,
     * 4H    , 8*4H        /
      DATA RDMAP 2 /
     O 4HEQUI,4HV   ,4H  PO,4HS,PO,4H/ALW,4HAYS ,4H$   ,4H    ,4H    ,
     * 4H    , 8*4H    ,
     1 4HEQUI,4HV   ,4H  YS,4HS,YS,4H/ALW,4HAYS ,4H$   ,4H    ,4H    ,
     * 4H    , 8*4H    ,
     2 4HCOND,4H    ,4H  LB,4HSSTP,4H,OMI,4HT $ ,12*4H    ,
     3 4HFBS ,4H    ,4H  LO,4HO,,P,4HOS/U,4HOOV/,4H1/1/,4HPREC,4H/0 $,
     * 4H    , 8*4H    ,
     4 4HLABE,4HL   ,4H  LB,4HSSTP,4H $  ,13*4H    ,
     5 4HOFP ,4H    ,4H  LA,4HMA,,,4H,,,/,4H/CAR,4HDNO ,4H$   ,4H    ,
     * 4H    , 8*4H    ,
     6 4HALTE,4HR   ,4H  (S,4HDR1),4H $  ,13*4H    ,
     7 4HUMER,4HGE  ,4H  US,4HET,Q,4HAS,/,4HQGS/,4H*G*/,4H*A*/,4H*O* ,
     * 4H$   , 8*4H    ,
     8 4HADD ,4H    ,4H  QG,4H ,QG,4HS/QG,4HT/  ,4H(1.0,4H,0.0,4H)/(1,
     * 4H.0,0,4H.0) ,4H$   ,6*4H      /
      DATA RDMAP 3 /
     9 4HEQUI,4HV   ,4H  QG,4HT,QG,4H /AL,4HWAYS,4H $  ,4H    ,4H    ,
     * 4H    , 8*4H    ,
     O 4HEQUI,4HV   ,4H  CA,4HSECC,4H,CAS,4HEXX/,4HALWA,4HYS $,4H    ,
     * 4H    , 8*4H    ,
     1 4HALTE,4HR   ,4H  (R,4HEPT),4H $  ,13*4H        /
      DATA OCT 1   /
     1                    3    ,    983040    ,        12  ,
     2                    4    ,    983040    ,        12  ,
     3                    5    ,    524288    ,        12  ,
     4                    8    ,   1835008    ,        12  ,
     5                    9    ,   1835008    ,        12  ,
     6                   10    ,   1835008    ,        12  ,
     7                   11    ,   1835008    ,        12  ,
     8                   12    ,   1835008    ,        12  ,
     9                   13    ,   1835008    ,        12  ,
     O                   14    ,   1835008    ,        12  ,
     1                   15    ,   1769472    ,         0  ,
     2                   20    ,    458752    ,         0  ,
     3                   21    ,    458752    ,         0  /
      DATA PTBS 1  /
     1          1  , 11  , 11  ,  0  ,     1  ,         0  ,  0  ,
     2          5  ,  1  ,  1  ,  0  ,4HNAME  ,         0  ,  0  ,
     3          5  , 19  , 21  ,  0  ,4HNAME  ,   1048576  ,  0  ,
     4          5  , 33  , 35  ,  0  ,4HNAME  ,         0  ,  0  ,
     5          5  , 36  , 38  ,  0  ,4HNAME  ,         0  ,  0  ,
     6          5  , 42  , 44  ,  0  ,4HNAME  ,         0  ,  0  ,
     7          6  , 12  , 14  ,  0  ,4HNAME  ,    524300  ,  0  ,
     8          6  , 15  , 17  ,  0  ,4HNAME  ,    524300  ,  0  ,
     9          6  , 18  , 20  ,  0  ,4HNAME  ,   1572876  ,  0  ,
     O          6  , 21  , 23  ,  0  ,4HNAME  ,   1572876  ,  0  ,
     1          6  , 24  , 24  ,  4  ,4HUAPH  ,         0  ,  0  ,
     2          6  , 33  , 33  ,  3  ,4HPGVC  ,         0  ,  0  ,
     3          6  , 37  , 37  ,  3  ,4HPSVC  ,         0  ,  0  ,
     4          6  , 41  , 44  ,  0  ,4HNAME  ,   1572876  ,  0  ,
     5          6  , 45  , 48  ,  0  ,4HNAME  ,   1572876  ,  0  ,
     6          6  , 49  , 49  ,  4  ,4HDYNT  ,    196608  ,  0  ,
     7          6  , 54  , 54  ,  4  ,4HSOL   ,         0  ,  0  ,
     8          6  , 60  , 60  ,  8  ,4HNAME  ,         0  ,  0  /
      DATA PTBS 2  /
     1          7  , 11  , 15  ,  0  ,4HNAME  ,    458752  ,  0  ,
     2         12  , 14  , 14  ,  3  ,4HSTEP  ,         0  ,  0  ,
     3         13  , 29  , 29  ,  4  ,4HPREC  ,         0  ,  0  ,
     4         14  , 14  , 14  ,  3  ,4HSTEP  ,         0  ,  0  ,
     5         16  , 11  , 11  ,  0  ,     2  ,         0  ,  0  ,
     6         18  , 11  , 11  ,  3  ,4HQVEC  ,         0  ,  0  ,
     7         19  , 15  , 15  ,  3  ,4HQVEC  ,         0  ,  0  ,
     8         21  , 11  , 11  ,  0  ,     3  ,         0  ,  0  /
      DATA SUBNAM  / 4HASCM,2H07  /
C
C     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
C     (SEE ASCM01 FOR EXPLANATION))
C
      RDMAP(15,6) = KHRFN1(RDMAP(15,6),2,SLASH,1)
C
C     VALIDATE COMMAND AND SET POINTERS
C
      IF (NAME .NE. COMND(1,1)) GO TO 1000
      ICOMND = 1
      IRDM   = 1
      NRDM   = COMND(2,ICOMND)
      IXTRA  = IRDM  + 18*NRDM
      NXTRA  = COMND(3,ICOMND)
      IOCT   = IXTRA + NXTRA
      NOCT   = COMND(4,ICOMND)
      IPTBS  = IOCT  + 3*NOCT
      NPTBS  = COMND(5,ICOMND)
      IPH    = IPTBS + 7*NPTBS
      NPH    = COMND(6,ICOMND)
C
C     MOVE RDMAP DATA
C
      K = 0
      IF (NRDM .EQ. 0) GO TO 35
      DO 30 J = 1,NRDM
      DO 30 I = 1,18
      K = K + 1
   30 IDAT(K) = RDMAP(I,J)
   35 CONTINUE
C
C     MOVE OCT DATA
C
      IF (NOCT .EQ. 0) GO TO 55
      DO 50 J = 1,NOCT
      DO 50 I = 1,3
      K = K + 1
   50 IDAT(K) = OCT(I,J)
   55 CONTINUE
C
C     MOVE PTBS DATA
C
      IF (NPTBS .EQ. 0) GO TO 65
      DO 60 J = 1,NPTBS
      DO 60 I = 1,7
      K = K + 1
   60 IDAT(K) = PTBS(I,J)
   65 CONTINUE
C
C     MOVE PHASE 3 DATA
C
      IF (IPHASE.NE.3 .OR. NPH.EQ.0) GO TO 200
      DO 110 I = 1,NPH
      K = K + 1
  110 IDAT(K) = IPAS37(I)
C
  200 RETURN
C
C     INPUT ERROR
C
 1000 CALL MESAGE (7,0,SUBNAM)
      NOGO = 1
      RETURN
C
      END
