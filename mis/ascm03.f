      SUBROUTINE ASCM03 (NAME,IPHASE,ISOL,NOGO)
C
C     COMBINE COMMAND DMAP DATA
C
      INTEGER        COMND(6,1),XTRA(10),SUBNAM(2),ISAVE(111),
     1               RDMAP(18,24),RDMAP1(18,9),RDMAP2(18,9),
     2               RDMAP3(18,6),OCT(3,21),OCT1(3,18),OCT2(3,3),
     3               PTBS(7,93),PTBS1(7,18),PTBS2(7,18),PTBS3(7,18),
     4               PTBS4(7,18),PTBS5(7,18),PTBS6(7,3)
      COMMON /ASDBD/ IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1               IPH,NPH,IDAT(1156)
      EQUIVALENCE    (RDMAP1(1,1),RDMAP(1, 1)),(OCT1(1,1),OCT(1, 1)),
     1               (RDMAP2(1,1),RDMAP(1,10)),(OCT2(1,1),OCT(1,19)),
     2               (RDMAP3(1,1),RDMAP(1,19)),
     3               (PTBS 1(1,1),PTBS (1, 1)),(PTBS2(1,1),PTBS(1,19)),
     4               (PTBS 3(1,1),PTBS(1, 37)),(PTBS4(1,1),PTBS(1,55)),
     5               (PTBS 5(1,1),PTBS(1, 73)),(PTBS6(1,1),PTBS(1,91))
      DATA COMND   /
     1               4HCOMB    , 24    , 10    , 21    , 93    ,  0  /
      DATA SLASH   / 1H/       /
      DATA ISAVE   /
     1    3,15,3,  3,16,3,  4, 6,1,  4,11,3,  4,14,2,  5, 6,1,  6, 8,1,
     2    7,15,3,  7,16,3,  8, 6,1,  8,11,3,  8,14,2,  9, 6,1, 10, 8,1,
     3   11,15,3, 11,16,3, 12, 6,1, 12,11,3, 12,14,2, 13, 6,1, 14, 8,1,
     4   15,15,3, 15,16,3, 16, 6,1, 16,11,3, 16,14,2, 17, 6,1, 18, 8,1,
     5   19,17,3, 20, 5,1, 20,10,3, 20,13,2, 20,16,1, 21, 6,1, 22, 8,2,
     6   22,11,1, 24, 5,1  /
      DATA RDMAP 1 /
     1 4HCOMB,4H1   ,4H  CA,4HSECC,4H,GEO,4HM4//,4HSTP/,4HS,N,,4HDRY/,
     * 4H*PVE,4HC* $, 7*4H    ,
     2 4HCOND,4H    ,4H  LB,4HSTP,,4HDRY ,4H$   ,12*4H    ,
     3 4HCOMB,4H2   ,4H  ,K,4HN01,,4HKN02,4H,KN0,4H3,KN,4H04,K,4HN05,,
     * 4HKN06,4H,KN0,4H7/KN,4HSC/S,4H,N,D,4HRY!*,4HK*!*,4H    ,4H*/  ,
     4 4H    ,4H    ,4H  *N,4HAME0,4H001*,4H!*NA,4HME00,4H02*/,4H*NAM,
     * 4HE000,4H3*!*,4HNAME,4H0004,4H*!*N,4HAME0,4H005*,4H/   ,4H    ,
     5 4H    ,4H    ,4H  *N,4HAME0,4H006*,4H!*NA,4HME00,4H07* ,4H$   ,
     * 4H    , 8*4H    ,
     6 4HSOFO,4H    ,4H  ,K,4HNSC,,4H,,,/,4H/S,N,4H,DRY,4H!*NA,4HMEC ,
     * 4H  */,4H*KMT,4HX* $, 6*4H    ,
     7 4HCOMB,4H2   ,4H  ,M,4HN01,,4HMN02,4H,MN0,4H3,MN,4H04,M,4HN05,,
     * 4HMN06,4H,MN0,4H7/MN,4HSC/S,4H,N,D,4HRY!*,4HM*!*,4H    ,4H*/  ,
     8 4H    ,4H    ,4H  *N,4HAME0,4H001*,4H!*NA,4HME00,4H02*/,4H*NAM,
     * 4HE000,4H3*!*,4HNAME,4H0004,4H*!*N,4HAME0,4H005*,4H/   ,4H    ,
     9 4H    ,4H    ,4H  *N,4HAME0,4H006*,4H!*NA,4HME00,4H07* ,4H$   ,
     * 4H    , 8*4H    /
      DATA RDMAP 2 /
     O 4HSOFO,4H    ,4H  ,M,4HNSC,,4H,,,/,4H/S,N,4H,DRY,4H!*NA,4HMEC ,
     * 4H  */,4H*MMT,4HX* $, 6*4H    ,
     1 4HCOMB,4H2   ,4H  ,P,4HN01,,4HPN02,4H,PN0,4H3,PN,4H04,P,4HN05,,
     * 4HPN06,4H,PN0,4H7/PN,4HSC/S,4H,N,D,4HRY!*,4HP*!*,4HPVEC,4H*/  ,
     2 4H    ,4H    ,4H  *N,4HAME0,4H001*,4H!*NA,4HME00,4H02*/,4H*NAM,
     * 4HE000,4H3*!*,4HNAME,4H0004,4H*!*N,4HAME0,4H005*,4H/   ,4H    ,
     3 4H    ,4H    ,4H  *N,4HAME0,4H006*,4H!*NA,4HME00,4H07* ,4H$   ,
     * 4H    , 8*4H    ,
     4 4HSOFO,4H    ,4H  ,P,4HNSC,,4H,,,/,4H/S,N,4H,DRY,4H!*NA,4HMEC ,
     * 4H  */,4H*PVE,4HC* $, 6*4H    ,
     5 4HCOMB,4H2   ,4H  ,B,4HN01,,4HBN02,4H,BN0,4H3,BN,4H04,B,4HN05,,
     * 4HBN06,4H,BN0,4H7/BN,4HSC/S,4H,N,D,4HRY!*,4HB*!*,4H    ,4H*/  ,
     6 4H    ,4H    ,4H  *N,4HAME0,4H001*,4H!*NA,4HME00,4H02*/,4H*NAM,
     * 4HE000,4H3*!*,4HNAME,4H0004,4H*!*N,4HAME0,4H005*,4H/   ,4H    ,
     7 4H    ,4H    ,4H  *N,4HAME0,4H006*,4H!*NA,4HME00,4H07* ,4H$   ,
     * 4H    , 8*4H    ,
     8 4HSOFO,4H    ,4H  ,B,4HNSC,,4H,,,/,4H/S,N,4H,DRY,4H!*NA,4HMEC ,
     * 4H  */,4H*BMT,4HX* $, 6*4H    /
      DATA RDMAP 3 /
     9 4HCOMB,4H2   ,4H  ,K,4H4N01,4H,K4N,4H02,K,4H4N03,4H,K4N,4H04,K,
     * 4H4N05,4H,K4N,4H06,K,4H4N07,4H/K4N,4HSC/S,4H,N,D,4HRY!*,4HK4*/,
     O 4H    ,4H    ,4H  * ,4H   *,4H!*NA,4HME00,4H01*/,4H*NAM,4HE000,
     * 4H2*!*,4HNAME,4H0003,4H*!*N,4HAME0,4H004*,4H!*NA,4HME00,4H05*/,
     1 4H    ,4H    ,4H  *N,4HAME0,4H006*,4H!*NA,4HME00,4H07* ,4H$   ,
     * 4H    , 8*4H    ,
     2 4HSOFO,4H    ,4H  ,K,4H4NSC,4H,,,,,4H//S,,4HN,DR,4HY!*N,4HAMEC,
     * 4H   *,4H!*K4,4HMX* ,4H$   , 5*4H    ,
     3 4HLABE,4HL   ,4H  LB,4HSTP ,4H$   ,13*4H    ,
     4 4HLODA,4HPP  ,4H  PN,4HSC,/,4H!*NA,4HMEC ,4H  */,4HS,N,,4HDRY ,
     * 4H$   , 8*4H    /
      DATA XTRA /
     1             4HSORT,4HNAME,4HNAMS,4HTOLE,4HCONN,4HCOMP,4HTRAN  ,
     2             4HSYMT,4HSEAR,4HOUTP/
      DATA OCT 1 /
     1                    3    ,         0    ,         1     ,
     2                    4    ,         0    ,         1     ,
     3                    5    ,         0    ,         1     ,
     4                    6    ,         0    ,         1     ,
     5                    7    ,         0    ,         2     ,
     6                    8    ,         0    ,         2     ,
     7                    9    ,         0    ,         2     ,
     8                   10    ,         0    ,         2     ,
     9                   11    ,         0    ,        12     ,
     O                   12    ,         0    ,        12     ,
     1                   13    ,         0    ,        12     ,
     2                   14    ,         0    ,        12     ,
     3                   15    ,         0    ,        16     ,
     4                   16    ,         0    ,        16     ,
     5                   17    ,         0    ,        16     ,
     6                   18    ,         0    ,        16     ,
     7                   19    ,         0    ,        32     ,
     8                   20    ,         0    ,        32     /
      DATA OCT 2 /
     1                   21    ,         0    ,        32     ,
     2                   22    ,         0    ,        32     ,
     3                   24    ,         0    ,         8     /
      DATA PTBS 1 /
     1          1  , 24  , 25  ,  3  ,4HNSTP  ,         0  ,  0  ,
     2          1  , 36  , 38  ,  4  ,4HPITM  ,        12  ,  0  ,
     3          2  , 13  , 13  ,  3  ,4HNSTP  ,         0  ,  0  ,
     4          3  , 12  , 13  ,  3  ,4HN1    ,         0  ,  1  ,
     5          3  , 17  , 18  ,  3  ,4HN2    ,         0  ,  1  ,
     6          3  , 22  , 23  ,  3  ,4HN3    ,         0  ,  1  ,
     7          3  , 27  , 28  ,  3  ,4HN4    ,         0  ,  1  ,
     8          3  , 32  , 33  ,  3  ,4HN5    ,         0  ,  1  ,
     9          3  , 37  , 38  ,  3  ,4HN6    ,         0  ,  1  ,
     O          3  , 42  , 43  ,  3  ,4HN7    ,         0  ,  1  ,
     1          3  , 47  , 48  ,  3  ,4HNCNO  ,         1  , -1  ,
     2          4  , 11  , 12  ,  8  ,4HNA1   ,         0  ,  0  ,
     3          4  , 21  , 23  ,  8  ,4HNA2   ,         0  ,  0  ,
     4          4  , 32  , 34  ,  8  ,4HNA3   ,         0  ,  0  ,
     5          4  , 43  , 45  ,  8  ,4HNA4   ,         0  ,  0  ,
     6          4  , 54  , 56  ,  8  ,4HNA5   ,         0  ,  0  ,
     7          5  , 11  , 12  ,  8  ,4HNA6   ,         0  ,  0  ,
     8          5  , 21  , 23  ,  8  ,4HNA7   ,         0  ,  0  /
      DATA PTBS 2 /
     1          6  , 12  , 13  ,  3  ,4HNCNO  ,         1  ,  1  ,
     2          6  , 29  , 31  ,  8  ,4HNAMC  ,         0  ,  0  ,
     3          7  , 12  , 13  ,  3  ,4HN1    ,         0  ,  1  ,
     4          7  , 17  , 18  ,  3  ,4HN2    ,         0  ,  1  ,
     5          7  , 22  , 23  ,  3  ,4HN3    ,         0  ,  1  ,
     6          7  , 27  , 28  ,  3  ,4HN4    ,         0  ,  1  ,
     7          7  , 32  , 33  ,  3  ,4HN5    ,         0  ,  1  ,
     8          7  , 37  , 38  ,  3  ,4HN6    ,         0  ,  1  ,
     9          7  , 42  , 43  ,  3  ,4HN7    ,         0  ,  1  ,
     O          7  , 47  , 48  ,  3  ,4HNCNO  ,         2  , -1  ,
     1          8  , 11  , 12  ,  8  ,4HNA1   ,         0  ,  0  ,
     2          8  , 21  , 23  ,  8  ,4HNA2   ,         0  ,  0  ,
     3          8  , 32  , 34  ,  8  ,4HNA3   ,         0  ,  0  ,
     4          8  , 43  , 45  ,  8  ,4HNA4   ,         0  ,  0  ,
     5          8  , 54  , 56  ,  8  ,4HNA5   ,         0  ,  0  ,
     6          9  , 11  , 12  ,  8  ,4HNA6   ,         0  ,  0  ,
     7          9  , 21  , 23  ,  8  ,4HNA7   ,         0  ,  0  ,
     8         10  , 12  , 13  ,  3  ,4HNCNO  ,         2  ,  1  /
      DATA PTBS 3 /
     1         10  , 29  , 31  ,  8  ,4HNAMC  ,         0  ,  0  ,
     2         11  , 12  , 13  ,  3  ,4HN1    ,         0  ,  1  ,
     3         11  , 17  , 18  ,  3  ,4HN2    ,         0  ,  1  ,
     4         11  , 22  , 23  ,  3  ,4HN3    ,         0  ,  1  ,
     5         11  , 27  , 28  ,  3  ,4HN4    ,         0  ,  1  ,
     6         11  , 32  , 33  ,  3  ,4HN5    ,         0  ,  1  ,
     7         11  , 37  , 38  ,  3  ,4HN6    ,         0  ,  1  ,
     8         11  , 42  , 43  ,  3  ,4HN7    ,         0  ,  1  ,
     9         11  , 47  , 48  ,  3  ,4HNCNO  ,        12  , -1  ,
     O         11  , 63  , 65  ,  4  ,4HPITM  ,         0  ,  0  ,
     1         12  , 11  , 12  ,  8  ,4HNA1   ,         0  ,  0  ,
     2         12  , 21  , 23  ,  8  ,4HNA2   ,         0  ,  0  ,
     3         12  , 32  , 34  ,  8  ,4HNA3   ,         0  ,  0  ,
     4         12  , 43  , 45  ,  8  ,4HNA4   ,         0  ,  0  ,
     5         12  , 54  , 56  ,  8  ,4HNA5   ,         0  ,  0  ,
     6         13  , 11  , 12  ,  8  ,4HNA6   ,         0  ,  0  ,
     7         13  , 21  , 23  ,  8  ,4HNA7   ,         0  ,  0  ,
     8         14  , 12  , 13  ,  3  ,4HNCNO  ,        12  ,  1  /
      DATA PTBS 4 /
     1         14  , 29  , 31  ,  8  ,4HNAMC  ,         0  ,  0  ,
     2         14  , 40  , 42  ,  4  ,4HPITM  ,         0  ,  0  ,
     3         15  , 12  , 13  ,  3  ,4HN1    ,         0  ,  1  ,
     4         15  , 17  , 18  ,  3  ,4HN2    ,         0  ,  1  ,
     5         15  , 22  , 23  ,  3  ,4HN3    ,         0  ,  1  ,
     6         15  , 27  , 28  ,  3  ,4HN4    ,         0  ,  1  ,
     7         15  , 32  , 33  ,  3  ,4HN5    ,         0  ,  1  ,
     8         15  , 37  , 38  ,  3  ,4HN6    ,         0  ,  1  ,
     9         15  , 42  , 43  ,  3  ,4HN7    ,         0  ,  1  ,
     O         15  , 47  , 48  ,  3  ,4HNCNO  ,        16  , -1  ,
     1         16  , 11  , 12  ,  8  ,4HNA1   ,         0  ,  0  ,
     2         16  , 21  , 23  ,  8  ,4HNA2   ,         0  ,  0  ,
     3         16  , 32  , 34  ,  8  ,4HNA3   ,         0  ,  0  ,
     4         16  , 43  , 45  ,  8  ,4HNA4   ,         0  ,  0  ,
     5         16  , 54  , 56  ,  8  ,4HNA5   ,         0  ,  0  ,
     6         17  , 11  , 12  ,  8  ,4HNA6   ,         0  ,  0  ,
     7         17  , 21  , 23  ,  8  ,4HNA7   ,         0  ,  0  ,
     8         18  , 12  , 13  ,  3  ,4HNCNO  ,        16  ,  1  /
      DATA PTBS 5 /
     1         18  , 29  , 31  ,  8  ,4HNAMC  ,         0  ,  0  ,
     2         19  , 12  , 14  ,  3  ,4HN1    ,         0  ,  1  ,
     3         19  , 18  , 20  ,  3  ,4HN2    ,         0  ,  1  ,
     4         19  , 24  , 26  ,  3  ,4HN3    ,         0  ,  1  ,
     5         19  , 30  , 32  ,  3  ,4HN4    ,         0  ,  1  ,
     6         19  , 36  , 38  ,  3  ,4HN5    ,         0  ,  1  ,
     7         19  , 42  , 44  ,  3  ,4HN6    ,         0  ,  1  ,
     8         19  , 48  , 50  ,  3  ,4HN7    ,         0  ,  1  ,
     9         19  , 54  , 56  ,  3  ,4HNCNO  ,        32  , -1  ,
     O         20  , 17  , 19  ,  8  ,4HNA1   ,         0  ,  0  ,
     1         20  , 28  , 30  ,  8  ,4HNA2   ,         0  ,  0  ,
     2         20  , 39  , 41  ,  8  ,4HNA3   ,         0  ,  0  ,
     3         20  , 50  , 52  ,  8  ,4HNA4   ,         0  ,  0  ,
     4         20  , 61  , 63  ,  8  ,4HNA5   ,         0  ,  0  ,
     5         21  , 11  , 12  ,  8  ,4HNA6   ,         0  ,  0  ,
     6         21  , 21  , 23  ,  8  ,4HNA7   ,         0  ,  0  ,
     7         22  , 12  , 14  ,  3  ,4HNCNO  ,        32  ,  1  ,
     8         22  , 30  , 32  ,  8  ,4HNAMC  ,         0  ,  0  /
      DATA PTBS 6 /
     1         23  , 11  , 13  ,  3  ,4HNSTP  ,         0  ,  0  ,
     2         24  , 11  , 12  ,  3  ,4HNCNO  ,         0  ,  1  ,
     3         24  , 17  , 19  ,  8  ,4HNAMC  ,         0  ,  0  /
      DATA SUBNAM / 4HASCM,2H03 /
C
C     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
C     (SEE ASCM01 FOR EXPLANATION))
C
      DO 20 L = 1,111,3
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
