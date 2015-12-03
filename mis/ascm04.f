      SUBROUTINE ASCM04 (NAME,IPHASE,ISOL,NOGO)
C
C     REDUCE COMMAND DMAP DATA
C
      INTEGER        COMND(6,1),XTRA(4),SUBNAM(2),ISAVE(54),
     1               RDMAP(18,23),RDMAP1(18,9),RDMAP2(18,9),
     2               RDMAP3(18,5),OCT(3,16),OCT1(3,16),PTBS(7,67),
     3               PTBS1(7,18),PTBS2(7,18),PTBS3(7,18),PTBS4(7,13)
      COMMON /ASDBD/ IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1               IPH,NPH,IDAT(935)
      EQUIVALENCE    (RDMAP1(1,1),RDMAP(1,1)),(RDMAP2(1,1),RDMAP(1,10)),
     1               (RDMAP3(1,1),RDMAP(1,19)),(OCT1(1,1),OCT(1,1)),
     2               (PTBS 1(1,1),PTBS(1, 1)),(PTBS2(1,1),PTBS(1,19)),
     3               (PTBS 3(1,1),PTBS(1,37)),(PTBS4(1,1),PTBS(1,55))
      DATA COMND   /
     1               4HREDU    , 23    ,  4    , 16    , 67    ,  0  /
      DATA SLASH   / 1H/       /
      DATA ISAVE   /
     1    1,14,1,  3,12,1,  3,16,3,  4, 5,1,  8, 8,2,  8,11,1,  9, 7,1,
     2   10,10,3, 12, 7,1, 14, 7,1, 16, 7,2, 16,10,1, 19, 7,2, 19,10,1,
     3   20, 7,2, 20,10,1, 21, 7,1, 23, 6,2    /
      DATA RDMAP 1 /
     1 4HREDU,4HCE  ,4H  CA,4HSECC,4H,GEO,4HM4/P,4HVNOA,4H,USS,4HTP,I,
     * 4HNSTP,4H/STP,4H/S,N,4H,DRY,4H!*PV,4HEC* ,4H$   ,4H    ,4H    ,
     2 4HCOND,4H    ,4H  LB,4HRSTP,4H,DRY,4H $  ,12*4H    ,
     3 4HSOFI,4H    ,4H  /K,4HNOA,,4HMNOA,4H,PNO,4HA,BN,4HOA,K,4H4NOA,
     * 4H/S,N,4H,DRY,4H!*NA,4HME00,4H0A*/,4H*KMT,4HX*!*,4HMMTX,4H*/  ,
     4 4H    ,4H    ,4H  *P,4HVEC*,4H!*BM,4HTX*/,4H*K4M,4HX* $,4H    ,
     * 4H    , 8*4H    ,
     5 4HCOND,4H    ,4H  LB,4HRSTP,4H,DRY,4H $  ,12*4H    ,
     6 4HSMP1,4H    ,4H  US,4HSTP,,4HKNOA,4H,,,/,4HGONO,4HA,KN,4HOB,K,
     * 4HONOA,4H,LON,4HOA,,,4H,,, ,4H$   , 4*4H    ,
     7 4HMERG,4HE   ,4H  GO,4HNOA,,4HINST,4HP,,,,4H,PVN,4HOA/G,4HNOA/,
     * 4H1/TY,4HP/2 ,4H$   , 6*4H    ,
     8 4HSOFO,4H    ,4H  ,G,4HNOA,,4HLONO,4HA,,,,4H//DR,4HY!*N,4HAME0,
     * 4H00A*,4H!*HO,4HRG*/,4H*LMT,4HX* $, 4*4H    ,
     9 4HSOFO,4H    ,4H  ,K,4HNOB,,4H,,,/,4H/DRY,4H!*NA,4HME00,4H0B*/,
     * 4H*KMT,4HX* $, 7*4H       /
      DATA RDMAP 2 /
     O 4HSOFI,4H    ,4H  /G,4HNOA,,4H,,,/,4HS,N,,4HDRY/,4H*NAM,4HE000,
     * 4HA*!*,4HHORG,4H* $ , 6*4H    ,
     1 4HMPY3,4H    ,4H  GN,4HOA,M,4HNOA,,4H/MNO,4HB/0/,4H0 $ ,4H    ,
     * 4H    , 8*4H    ,
     2 4HSOFO,4H    ,4H  ,M,4HNOB,,4H,,,/,4H/DRY,4H!*NA,4HME00,4H0B*/,
     * 4H*MMT,4HX* $, 7*4H    ,
     3 4HMPY3,4H    ,4H  GN,4HOA,B,4HNOA,,4H/BNO,4HB/0/,4H0 $ ,4H    ,
     * 4H    , 8*4H    ,
     4 4HSOFO,4H    ,4H  ,B,4HNOB,,4H,,,/,4H/DRY,4H!*NA,4HME00,4H0B*/,
     * 4H*BMT,4HX* $, 7*4H    ,
     5 4HMPY3,4H    ,4H  GN,4HOA,K,4H4NOA,4H,/K4,4HNOB/,4H0/0 ,4H$   ,
     * 4H    , 8*4H    ,
     6 4HSOFO,4H    ,4H  ,K,4H4NOB,4H,,,,,4H//DR,4HY!*N,4HAME0,4H00B*,
     * 4H!*K4,4HMX* ,4H$   , 6*4H    ,
     7 4HPART,4HN   ,4H  PN,4HOA,,,4HPVNO,4HA/PO,4HNOA,,4H,,/1,4H/1/2,
     * 4H $  , 8*4H    ,
     8 4HMPYA,4HD   ,4H  GN,4HOA,P,4HNOA,,4H/PNO,4HB/1/,4H1/0/,4H1 $ ,
     * 4H    , 8*4H        /
      DATA RDMAP 3 /
     9 4HSOFO,4H    ,4H  ,P,4HONOA,4H,,,,,4H//DR,4HY!*N,4HAME0,4H00A*,
     * 4H!*PO,4HVE* ,4H$   , 6*4H    ,
     O 4HSOFO,4H    ,4H  ,P,4HVNOA,4H,,,,,4H//DR,4HY!*N,4HAME0,4H00A*,
     * 4H!*UP,4HRT* ,4H$   , 6*4H    ,
     1 4HSOFO,4H    ,4H  ,P,4HNOB,,4H,,,/,4H/DRY,4H!*NA,4HME00,4H0B*/,
     * 4H*PVE,4HC* $, 7*4H    ,
     2 4HLABE,4HL   ,4H  LB,4HRSTP,4H $  ,13*4H    ,
     3 4HLODA,4HPP  ,4H  PN,4HOB,P,4HONOA,4H/!*N,4HAME0,4H00B*,4H/S,N,
     * 4H,DRY,4H $  , 7*4H      /
      DATA XTRA    /
     1               4HOUTP,4HNAME,4HBOUN,4HRSAV        /
      DATA OCT 1   /
     1                    6    ,         0    ,         1  ,
     2                    7    ,         0    ,         1  ,
     3                    8    ,         0    ,         1  ,
     4                    9    ,         0    ,         1  ,
     5                   10    ,         1    ,        62  ,
     6                   11    ,         0    ,         2  ,
     7                   12    ,         0    ,         2  ,
     8                   13    ,         0    ,        16  ,
     9                   14    ,         0    ,        16  ,
     O                   15    ,         0    ,        32  ,
     1                   16    ,         0    ,        32  ,
     2                   17    ,         0    ,        12  ,
     3                   18    ,         0    ,        12  ,
     4                   19    ,         0    ,        12  ,
     5                   21    ,         0    ,        12  ,
     6                   23    ,         0    ,         8  /
      DATA PTBS 1  /
     1          1  , 24  , 26  ,  3  ,4HNONA  ,         0  ,  0  ,
     2          1  , 32  , 32  ,  3  ,4HSTEP  ,         0  ,  0  ,
     3          1  , 38  , 38  ,  3  ,4HSTEP  ,         0  ,  0  ,
     4          1  , 41  , 42  ,  3  ,4HSTEP  ,         0  ,  0  ,
     5          1  , 53  , 55  ,  4  ,4HPITM  ,         0  ,  0  ,
     6          2  , 14  , 14  ,  3  ,4HSTEP  ,         0  ,  0  ,
     7          3  , 12  , 13  ,  3  ,4HNONA  ,         1  , -1  ,
     8          3  , 17  , 18  ,  3  ,4HNONA  ,         2  , -1  ,
     9          3  , 22  , 23  ,  3  ,4HNONA  ,        12  , -1  ,
     O          3  , 27  , 28  ,  3  ,4HNONA  ,        16  , -1  ,
     1          3  , 32  , 34  ,  3  ,4HNONA  ,        32  , -1  ,
     2          3  , 45  , 47  ,  8  ,4HNAMA  ,         0  ,  0  ,
     3          4  , 11  , 12  ,  4  ,4HPITM  ,         0  ,  0  ,
     4          5  , 14  , 14  ,  3  ,4HSTEP  ,         0  ,  0  ,
     5          6  , 11  , 13  ,  3  ,4HSTEP  ,         0  ,  0  ,
     6          6  , 17  , 18  ,  3  ,4HNONA  ,         0  ,  0  ,
     7          6  , 25  , 27  ,  3  ,4HNONA  ,         0  , -1  ,
     8          6  , 31  , 32  ,  3  ,4HNONB  ,         0  , -1  /
      DATA PTBS 2  /
     1          6  , 36  , 38  ,  3  ,4HNONA  ,         0  ,  0  ,
     2          6  , 42  , 44  ,  3  ,4HNONA  ,         0  ,  0  ,
     3          7  , 11  , 13  ,  3  ,4HNONA  ,         0  ,  0  ,
     4          7  , 17  , 19  ,  3  ,4HSTEP  ,         0  ,  0  ,
     5          7  , 26  , 28  ,  3  ,4HNONA  ,         0  ,  0  ,
     6          7  , 32  , 33  ,  3  ,4HNONA  ,         0  , -1  ,
     7          7  , 38  , 39  ,  3  ,4HPREC  ,         0  ,  0  ,
     8          8  , 12  , 13  ,  3  ,4HNONA  ,         0  ,  0  ,
     9          8  , 17  , 19  ,  3  ,4HNONA  ,         0  ,  0  ,
     O          8  , 17  , 21  ,  0  ,4HRSAV  ,         0  ,  0  ,
     1          8  , 30  , 32  ,  8  ,4HNAMA  ,         0  ,  0  ,
     2          8  , 48  , 54  ,  0  ,4HRSAV  ,         0  ,  0  ,
     3          9  , 12  , 13  ,  3  ,4HNONB  ,         0  ,  0  ,
     4          9  , 25  , 27  ,  8  ,4HNAMB  ,         0  ,  0  ,
     5         10  , 12  , 13  ,  3  ,4HNONA  ,         0  , -1  ,
     6         10  , 28  , 30  ,  8  ,4HNAMA  ,         0  ,  0  ,
     7         11  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     8         11  , 16  , 17  ,  3  ,4HNONA  ,         0  ,  0  /
      DATA PTBS 3  /
     1         11  , 22  , 23  ,  3  ,4HNONB  ,         0  , -1  ,
     2         12  , 12  , 13  ,  3  ,4HNONB  ,         0  ,  0  ,
     3         12  , 25  , 27  ,  8  ,4HNAMB  ,         0  ,  0  ,
     4         13  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     5         13  , 16  , 17  ,  3  ,4HNONA  ,         0  ,  0  ,
     6         13  , 22  , 23  ,  3  ,4HNONB  ,         0  , -1  ,
     7         14  , 12  , 13  ,  3  ,4HNONB  ,         0  ,  0  ,
     8         14  , 25  , 27  ,  8  ,4HNAMB  ,         0  ,  0  ,
     9         15  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     O         15  , 16  , 18  ,  3  ,4HNONA  ,         0  ,  0  ,
     1         15  , 23  , 25  ,  3  ,4HNONB  ,         0  , -1  ,
     2         16  , 12  , 14  ,  3  ,4HNONB  ,         0  ,  0  ,
     3         16  , 26  , 28  ,  8  ,4HNAMB  ,         0  ,  0  ,
     4         17  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     5         17  , 17  , 19  ,  3  ,4HNONA  ,         0  ,  0  ,
     6         17  , 23  , 25  ,  3  ,4HNONA  ,         0  ,  0  ,
     7         18  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     8         18  , 16  , 17  ,  3  ,4HNONA  ,         0  ,  0  /
      DATA PTBS 4  /
     1         18  , 22  , 23  ,  3  ,4HNONB  ,         0  , -1  ,
     2         19  , 12  , 14  ,  3  ,4HNONA  ,         0  ,  0  ,
     3         19  , 26  , 28  ,  8  ,4HNAMA  ,         0  ,  0  ,
     4         19  , 37  , 39  ,  4  ,4HPOIT  ,         0  ,  0  ,
     5         20  , 12  , 14  ,  3  ,4HNONA  ,         0  ,  0  ,
     6         20  , 26  , 28  ,  8  ,4HNAMA  ,         0  ,  0  ,
     7         21  , 12  , 13  ,  3  ,4HNONB  ,         0  ,  1  ,
     8         21  , 25  , 27  ,  8  ,4HNAMB  ,         0  ,  0  ,
     9         21  , 36  , 38  ,  4  ,4HPITM  ,         0  ,  0  ,
     O         22  , 14  , 14  ,  3  ,4HSTEP  ,         0  ,  0  ,
     1         23  , 11  , 12  ,  3  ,4HNONB  ,         0  ,  1  ,
     2         23  , 16  , 18  ,  3  ,4HNONA  ,         0  ,  1  ,
     3         23  , 22  , 24  ,  8  ,4HNAMB  ,         0  ,  0  /
C
      DATA SUBNAM  / 4HASCM,2H04  /
C
C     RESTORE TO ORIGINAL DATA BY REPLACEING !* BY /* IN RDMAP ARRAY
C     (SEE ASCM01 FOR EXPLANATION)
C
      DO 20 L = 1,51,3
      I = ISAVE(L+1)
      J = ISAVE(L  )
      K = ISAVE(L+2)
      RDMAP(I,J) = KHRFN1(RDMAP(I,J),K,SLASH,1)
   20 CONTINUE
C
C     VALIDATE COMMAND AND SET POINTERS
C
      IF (NAME .NE. COMND(1,1)) GO TO 70
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
