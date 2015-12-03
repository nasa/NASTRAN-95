      SUBROUTINE ASCM06 (NAME,IPHASE,ISOL,NOGO)
C
C     RECOVER, MRECOVER COMMAND DMAP DATA
C
      INTEGER        COMND(6,2),XTRA(15),SUBNAM(2),RDMAP(18,17),
     1               RDMAP1(18,9),RDMAP2(18,8),OCT(3,1),OCT1(3,1),
     2               PTBS(7,31),PTBS1(7,18),PTBS2(7,13)
      COMMON /ASDBD/ IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1               IPH,NPH,IDAT(543)
      EQUIVALENCE    (RDMAP1(1,1),RDMAP(1, 1)),(PTBS1(1,1),PTBS(1,1)),
     1               (RDMAP2(1,1),RDMAP(1,10)),(PTBS2(1,1),PTBS(1,19)),
     2               (OCT1(1,1),OCT(1,1))
      DATA COMND   /
     1               4HRECO    , 17    , 15    ,  1    , 31    ,  0  ,
     2               4HMREC    , 17    , 15    ,  1    , 31    ,  0  /
      DATA SLASH   / 1H/       /
      DATA RDMAP 1 /
     1 4HFILE,4H    ,4H  U1,4H=APP,4HEND/,4HU2=A,4HPPEN,4HD/U3,4H=APP,
     * 4HEND/,4HU4=A,4HPPEN,4HD/U5,4H=APP,4HEND ,4H$   ,4H    ,4H    ,
     2 4HPARA,4HM   ,4H  //,4H*ADD,4H*/IL,4HOOP/,4H0/0 ,4H$   ,4H    ,
     * 4H    , 8*4H    ,
     3 4HLABE,4HL   ,4H  LB,4HSTP ,4H$   ,13*4H    ,
     4 4HRCOV,4HR   ,4H  CA,4HSESS,4H,GEO,4HM4,K,4HGG,M,4HGG,P,4HGG,U,
     * 4HGV ,,4HDIT,,4HDLT,,4HBGG,,4HK4GG,4H,PPF,4H/OUG,4HV1 ,,4H    ,
     5 4H    ,4H    ,4H  OP,4HG1,O,4HQG1,,4HU1,U,4H2,U3,4H,U4,,4HU5/S,
     * 4H,N,D,4HRY/S,4H,N,I,4HLOOP,4H/STP,4H!*NA,4HMEFS,4HS */,4H    ,
     6 4H    ,4H    ,4H  NS,4HOL/N,4HEIGV,4H/S,N,4H,LUI,4H/S,N,4H,U1N,
     * 4H/S,N,4H,U2N,4H/S,N,4H,U3N,4H/S,N,4H,U4N,4H/S,N,4H,U5N,4H/   ,
     7 4H    ,4H    ,4H  S,,4HN,NO,4HSORT,4H2/V,,4HY,UT,4HHRES,4HH/V,,
     * 4HY,PT,4HHRES,4HH/V,,4HY,QT,4HHRES,4HH $ , 3*4H    ,
     8 4HEQUI,4HV   ,4H  OU,4HGV1 ,4H,OUG,4HV /N,4HOSOR,4HT2/O,4HQG1,,
     * 4HOQG/,4HNOSO,4HRT2 ,4H$   , 5*4H    ,
     9 4HEQUI,4HV   ,4H  OP,4HG1,O,4HPG/N,4HOSOR,4HT2 $,4H    ,4H    ,
     * 4H    , 8*4H        /
      DATA RDMAP 2 /
     O 4HCOND,4H    ,4H  NS,4HT2ST,4HP,NO,4HSORT,4H2 $ ,4H    ,4H    ,
     * 4H    , 8*4H    ,
     1 4HSDR3,4H    ,4H  OU,4HGV1 ,4H,OPG,4H1,OQ,4HG1,,,4H,/OU,4HGV ,,
     * 4HOPG,,4HOQG,,4H,, $, 6*4H    ,
     2 4HLABE,4HL   ,4H  NS,4HT2ST,4HP $ ,13*4H    ,
     3 4HOFP ,4H    ,4H  OU,4HGV ,,4HOPG,,4HOQG,,4H,,//,4HS,N,,4HCARD,
     * 4HNO $, 8*4H    ,
     4 4HCOND,4H    ,4H  LB,4HBSTP,4H,ILO,4HOP $,12*4H    ,
     5 4HREPT,4H    ,4H  LB,4HSTP,,4H100 ,4H$   ,12*4H    ,
     6 4HLABE,4HL   ,4H  LB,4HBSTP,4H $  ,13*4H    ,
     7 4HSOFO,4H    ,4H  ,U,4H1,U2,4H,U3,,4HU4,U,4H5//-,4H1!*X,4HXXXX,
     * 4HXXX*,4H $  , 7*4H        /
      DATA XTRA    /
     1               4HPRIN,4HSAVE,4HDISP,4HOLOA,4HSPCF,4HMODE,4HRANG ,
     2               4HSUBC,4HSORT,4HBASI,4HVELO,4HACCE,4HENER,4HUIMP ,
     3               4HSTEP       /
      DATA OCT  1  /
     1                    9    ,    262144    ,         0     /
      DATA PTBS 1  /
     1          3  , 13  , 13  ,  3  ,4HSTEP  ,         0  ,  0  ,
     2          4  , 11  , 15  ,  2  ,4HCASE  ,         0  ,  0  ,
     3          4  , 18  , 18  ,  5  ,4HGORL  ,         0  ,  0  ,
     4          4  , 24  , 27  ,  0  ,4HNAME  ,         1  ,  0  ,
     5          4  , 28  , 31  ,  0  ,4HNAME  ,         2  ,  0  ,
     6          4  , 32  , 32  ,  3  ,4HPVEC  ,         0  ,  0  ,
     7          4  , 36  , 36  ,  4  ,4HUVEC  ,         0  ,  0  ,
     8          4  , 41  , 44  ,  0  ,4HNAME  ,    458752  ,  0  ,
     9          4  , 45  , 48  ,  0  ,4HNAME  ,    458752  ,  0  ,
     O          4  , 49  , 52  ,  0  ,4HNAME  ,    458768  ,  0  ,
     1          4  , 53  , 57  ,  0  ,4HNAME  ,    458784  ,  0  ,
     2          4  , 58  , 58  ,  3  ,4HPFTL  ,         0  ,  0  ,
     3          4  , 62  , 62  ,  6  ,4HOVEC  ,         0  ,  0  ,
     4          5  , 11  , 15  ,  0  ,4HNAME  ,    262144  ,  0  ,
     5          5  , 54  , 54  ,  3  ,4HSTEP  ,         0  ,  0  ,
     6          5  , 57  , 59  ,  8  ,4HNAME  ,         0  ,  0  ,
     7          6  , 11  , 11  ,  4  ,4HSOL   ,         0  ,  0  ,
     8          6  , 16  , 21  ,  0  ,4HNAME  ,   1769472  ,  0  /
      DATA PTBS 2  /
     1          8  , 11  , 11  ,  6  ,4HOVEC  ,         0  ,  0  ,
     2          8  , 18  , 18  ,  5  ,4HOVC2  ,         0  ,  0  ,
     3         10  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     4         11  , 11  , 11  ,  6  ,4HOVEC  ,         0  ,  0  ,
     5         11  , 18  , 22  ,  0  ,4HNAME  ,    262144  ,  0  ,
     6         11  , 31  , 31  ,  5  ,4HOVC2  ,         0  ,  0  ,
     7         11  , 37  , 40  ,  0  ,4HNAME  ,    262144  ,  0  ,
     8         12  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     9         13  , 11  , 11  ,  5  ,4HOVC2  ,         0  ,  0  ,
     O         13  , 17  , 20  ,  0  ,4HNAME  ,    262144  ,  0  ,
     1         14  , 14  , 14  ,  3  ,4HSTEP  ,         0  ,  0  ,
     2         15  , 13  , 13  ,  3  ,4HSTEP  ,         0  ,  0  ,
     3         16  , 14  , 14  ,  3  ,4HSTEP  ,         0  ,  0  /
      DATA SUBNAM  / 4HASCM,2H06  /
C
C     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
C     (SEE ASCM01 FOR EXPLANATION))
C
      RDMAP(15,5) = KHRFN1(RDMAP(15,5),1,SLASH,1)
      RDMAP(8,17) = KHRFN1(RDMAP(8,17),2,SLASH,1)
C
C     VALIDATE COMMAND AND SET POINTERS
C
      DO 10 I = 1,2
      IF (NAME .EQ. COMND(1,I)) GO TO 20
   10 CONTINUE
      GO TO 70
   20 ICOMND = I
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
C     MOVE XTRA DATA
C
      IF (NXTRA .EQ. 0) GO TO 45
      DO 40 I = 1,NXTRA
      K = K + 1
   40 IDAT(K) = XTRA(I)
   45 CONTINUE
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
      RETURN
C
C     INPUT ERROR
C
   70 CALL MESAGE (7,0,SUBNAM)
      NOGO = 1
      RETURN
C
      END
