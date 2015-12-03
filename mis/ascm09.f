      SUBROUTINE ASCM09 (NAME,IPHASE,ISOL,NOGO)
C
C     MREDUCE COMMAND DMAP DATA
C
      INTEGER        COMND(6,1),XTRA(13),SUBNAM(2),ISAVE(30),
     1               RDMAP(18,25),RDMAP1(18,9),RDMAP2(18,9),
     2               RDMAP3(18,7),OCT(3,16),OCT1(3,16),PTBS(7,53),
     3               PTBS1(7,18),PTBS2(7,18),PTBS3(7,17)
      COMMON /ASDBD/ IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1               IPH,NPH,IDAT(882)
      EQUIVALENCE    (RDMAP1(1,1),RDMAP(1, 1)),(OCT1(1,1),OCT(1,1)),
     1               (RDMAP2(1,1),RDMAP(1,10)),(PTBS1(1,1),PTBS(1,1)),
     2               (RDMAP3(1,1),RDMAP(1,19)),(PTBS2(1,1),PTBS(1,19)),
     3               (PTBS3(1,1),PTBS(1,37))
      DATA COMND   /
     1                4HMRED    , 25    , 13    , 16    , 53    ,  0  /
      DATA SLASH   /  1H/       /
      DATA ISAVE   /
     1   1,15,1,  2,11,2,  4,12,1,  4,16,3,  5, 5,1, 19, 7,3, 19, 8,3,
     2  19, 9,3, 22,15,2, 24, 6,2    /
      DATA RDMAP 1 /
     1 4HMRED,4H1   ,4H  CA,4HSECC,4H,GEO,4HM4,D,4HYNAM,4HICS,,4HCSTM,
     * 4H/USE,4HTR,E,4HEDR,,4HEQST,4H,DMR,4H!*NA,4HMEA ,4H  */,4H    ,
     2 4H    ,4H    ,4H  S,,4HN,DR,4HY/ST,4HP/S,,4HN,NO,4HFIX/,4HS,N,,
     * 4HSKIP,4HM!*R,4HEAL*,4H $  , 5*4H    ,
     3 4HCOND,4H    ,4H  LB,4HM3ST,4HP,DR,4HY $ ,12*4H    ,
     4 4HSOFI,4H    ,4H  /K,4HNOA,,4HMNOA,4H,PNO,4HA,BN,4HOA,K,4H4NOA,
     * 4H/S,N,4H,DRY,4H!*NA,4HMEA ,4H  */,4H*KMT,4HX*!*,4HMMTX,4H*/  ,
     5 4H    ,4H    ,4H  *P,4HVEC*,4H!*BM,4HTX*/,4H*K4M,4HX* $,4H    ,
     * 4H    , 8*4H    ,
     6 4HCOND,4H    ,4H  LB,4HM2ST,4HP,SK,4HIPM ,4H$   ,4H    ,4H    ,
     * 4H    , 8*4H    ,
     7 4HEQUI,4HV   ,4H  KN,4HOA,K,4HFFX/,4HNOFI,4HX $ ,4H    ,4H    ,
     * 4H    , 8*4H    ,
     8 4HEQUI,4HV   ,4H  MN,4HOA,M,4HFFX/,4HNOFI,4HX $ ,4H    ,4H    ,
     * 4H    , 8*4H    ,
     9 4HEQUI,4HV   ,4H  BN,4HOA,B,4HFFX/,4HNOFI,4HX $ ,4H    ,4H    ,
     * 4H    , 8*4H        /
      DATA RDMAP 2 /
     O 4HEQUI,4HV   ,4H  K4,4HNOA,,4HK4FF,4HX/NO,4HFIX ,4H$   ,4H    ,
     * 4H    , 8*4H    ,
     1 4HCOND,4H    ,4H  LB,4HM1ST,4HP,NO,4HFIX ,4H$   ,4H    ,4H    ,
     * 4H    , 8*4H    ,
     2 4HSCE1,4H    ,4H  US,4HETR,,4HKNOA,4H,MNO,4HA,BN,4HOA,K,4H4NOA,
     * 4H/KFF,4HX,KF,4HSX,K,4HSSX,,4HMFFX,4H,BFF,4HX,K4,4HFFX ,4H$   ,
     3 4HLABE,4HL   ,4H  LB,4HM1ST,4HP $ ,13*4H    ,
     4 4HREAD,4H    ,4H  KF,4HFX,M,4HFFX,,4HBFFX,4H,K4F,4HFX,E,4HEDR,,
     * 4HUSET,4HR,/L,4HAMAR,4H,PHI,4HR,MI,4HR,OE,4HIGR/,4H*MOD,4HES*/,
     5 4H    ,4H    ,4H  NE,4HIGVS,4H $  ,13*4H    ,
     6 4HOFP ,4H    ,4H  LA,4HMAR,,4HOEIG,4HR,,,,4H,// ,4H$   ,4H    ,
     * 4H    , 8*4H    ,
     7 4HEQUI,4HV   ,4H  PH,4HIR,P,4HHIS/,4HNOFI,4HX $ ,4H    ,4H    ,
     * 4H    , 8*4H    ,
     8 4HCOND,4H    ,4H  LB,4HM2ST,4HP,NO,4HFIX ,4H$   ,4H    ,4H    ,
     * 4H    , 8*4H        /
      DATA RDMAP 3 /
     9 4HUMER,4HGE  ,4H  US,4HETR,,4HPHIR,4H,/PH,4HIS!*,4HN*!*,4HF*!*,
     * 4HS* $, 8*4H    ,
     O 4HLABE,4HL   ,4H  LB,4HM2ST,4HP $ ,13*4H    ,
     1 4HMRED,4H2   ,4H  CA,4HSECC,4H,LAM,4HAR,P,4HHIS,,4HEQST,4H,USE,
     * 4HTR,K,4HNOA,,4HMNOA,4H,BNO,4HA,K4,4HNOA,,4HPNOA,4H,DMR,4H,   ,
     2 4H    ,4H    ,4H  QS,4HM/KN,4HOB,M,4HNOB,,4HBNOB,4H,K4N,4HOB,P,
     * 4HNOB,,4HPONO,4HB/ST,4HP/S,,4HN,DR,4HY!*P,4HVEC*,4H $  ,4H    ,
     3 4HLABE,4HL   ,4H  LB,4HM3ST,4HP $ ,13*4H    ,
     4 4HLODA,4HPP  ,4H  PN,4HOB,P,4HONOB,4H/!*N,4HAMEB,4H   *,4H/S,N,
     * 4H,DRY,4H $  , 7*4H    ,
     5 4HCOND,4H    ,4H  FI,4HNIS,,4HDRY ,4H$   ,12*4H        /
      DATA XTRA   /
     1              4HNAME,4HBOUN,4HFIXE,4HMETH,4HRANG,4HNMAX,4HOLDM ,
     2              4HOLDB,4HUSER,4HOUTP,4HRGRI,4HRNAM,4HRSAV        /
      DATA OCT 1  /
     1                    6    ,         8    ,         0  ,
     2                    7    ,         8    ,         1  ,
     3                    8    ,         8    ,         2  ,
     4                    9    ,         8    ,        16  ,
     5                   10    ,         8    ,        32  ,
     6                   11    ,         8    ,         0  ,
     7                   12    ,         8    ,         0  ,
     8                   13    ,         8    ,         0  ,
     9                   14    ,         8    ,         0  ,
     O                   15    ,         8    ,         0  ,
     1                   16    ,         8    ,         0  ,
     2                   17    ,         8    ,         0  ,
     3                   18    ,         8    ,         0  ,
     4                   19    ,         8    ,         0  ,
     5                   20    ,         8    ,         0  ,
     6                   24    ,         0    ,         8  /
      DATA PTBS 1  /
     1          1  , 59  , 59  ,  8  ,4HNAMA  ,         0  ,  0  ,
     2          2  , 19  , 19  ,  3  ,4HSTEP  ,         0  ,  0  ,
     3          3  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     4          4  , 12  , 13  ,  3  ,4HNONA  ,         1  , -1  ,
     5          4  , 17  , 18  ,  3  ,4HNONA  ,         2  , -1  ,
     6          4  , 22  , 23  ,  3  ,4HNONA  ,        12  , -1  ,
     7          4  , 27  , 28  ,  3  ,4HNONA  ,        16  , -1  ,
     8          4  , 32  , 34  ,  3  ,4HNONA  ,        32  , -1  ,
     9          4  , 47  , 47  ,  8  ,4HNAMA  ,         0  ,  0  ,
     O          5  , 12  , 12  ,  4  ,4HPITM  ,         0  ,  0  ,
     1          6  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     2          7  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     3          8  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     4          9  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     5         10  , 11  , 13  ,  3  ,4HNONA  ,         0  ,  0  ,
     6         11  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     7         12  , 17  , 18  ,  3  ,4HNONA  ,         1  ,  0  ,
     8         12  , 22  , 23  ,  3  ,4HNONA  ,         2  ,  0  /
      DATA PTBS 2  /
     1         12  , 27  , 28  ,  3  ,4HNONA  ,        16  ,  0  ,
     2         12  , 32  , 34  ,  3  ,4HNONA  ,        32  ,  0  ,
     3         12  , 38  , 42  ,  0  ,4HNAMA  ,         1  ,  0  ,
     4         12  , 43  , 47  ,  0  ,4HNAMA  ,         1  ,  0  ,
     5         12  , 48  , 52  ,  0  ,4HNAMA  ,         1  ,  0  ,
     6         12  , 53  , 57  ,  0  ,4HNAMA  ,         2  ,  0  ,
     7         12  , 58  , 62  ,  0  ,4HNAMA  ,        16  ,  0  ,
     8         12  , 63  , 68  ,  0  ,4HNAMA  ,        32  ,  0  ,
     9         13  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     O         14  , 11  , 15  ,  0  ,4HNAMA  ,         1  ,  0  ,
     1         14  , 16  , 20  ,  0  ,4HNAMA  ,         2  ,  0  ,
     2         14  , 21  , 25  ,  0  ,4HNAMA  ,        16  ,  0  ,
     3         14  , 26  , 31  ,  0  ,4HNAMA  ,        32  ,  0  ,
     4         18  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     5         20  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     6         21  , 18  , 23  ,  0  ,4HNAMA  ,        55  ,  0  ,
     7         21  , 24  , 28  ,  0  ,4HNAMA  ,        55  ,  0  ,
     8         21  , 40  , 41  ,  3  ,4HNONA  ,         1  ,  0  /
      DATA PTBS 3  /
     1         21  , 45  , 46  ,  3  ,4HNONA  ,         2  ,  0  ,
     2         21  , 50  , 51  ,  3  ,4HNONA  ,        16  ,  0  ,
     3         21  , 55  , 57  ,  3  ,4HNONA  ,        32  ,  0  ,
     4         21  , 61  , 62  ,  3  ,4HNONA  ,        12  ,  0  ,
     5         22  , 11  , 14  ,  0  ,4HNAMA  ,    131072  ,  0  ,
     6         22  , 15  , 16  ,  3  ,4HNONB  ,         1  , -1  ,
     7         22  , 20  , 21  ,  3  ,4HNONB  ,         2  , -1  ,
     8         22  , 25  , 26  ,  3  ,4HNONB  ,        16  , -1  ,
     9         22  , 30  , 32  ,  3  ,4HNONB  ,        32  , -1  ,
     O         22  , 36  , 37  ,  3  ,4HNONB  ,        12  , -1  ,
     1         22  , 41  , 43  ,  3  ,4HNONB  ,        12  , -1  ,
     2         22  , 47  , 47  ,  3  ,4HSTEP  ,         0  ,  0  ,
     3         22  , 60  , 60  ,  4  ,4HPITM  ,        12  ,  0  ,
     4         23  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     5         24  , 11  , 12  ,  3  ,4HNONB  ,         0  ,  0  ,
     6         24  , 16  , 18  ,  3  ,4HNONB  ,         0  ,  0  ,
     7         24  , 24  , 24  ,  8  ,4HNAMB  ,         0  ,  0  /
      DATA SUBNAM  / 4HASCM,2H09  /
C
C     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
C     (SEE ASCM01 FOR EXPLANATION))
C
      DO 20 L = 1,30,3
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
