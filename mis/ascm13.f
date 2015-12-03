      SUBROUTINE ASCM13 (NAME,IPHASE,ISOL,NOGO)
C
C     CREDUCE COMMAND DMAP DATA
C
      INTEGER        COMND(6,1),XTRA(11),SUBNAM(2),ISAVE(39),
     1               RDMAP(18,30),RDMAP1(18,9),RDMAP2(18,9),
     2               RDMAP3(18,9),RDMAP4(18,3),OCT(3,20),OCT1(3,18),
     3               OCT2(3,2),PTBS(7,53),PTBS1(7,18),PTBS2(7,18),
     4               PTBS3(7,17)
      COMMON /ASDBD/ IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1               IPH,NPH,IDAT( 982)
      EQUIVALENCE    (RDMAP1(1,1),RDMAP(1, 1)),(OCT1(1,1),OCT(1, 1)),
     1               (RDMAP2(1,1),RDMAP(1,10)),(OCT2(1,1),OCT(1,19)),
     2               (RDMAP3(1,1),RDMAP(1,19)),(PTBS1(1,1),PTBS(1,1)),
     3               (RDMAP4(1,1),RDMAP(1,28)),(PTBS2(1,1),PTBS(1,19)),
     4               (PTBS3 (1,1),PTBS (1,37))
      DATA COMND   /
     1               4HCRED    , 30    , 11    , 20    , 53    ,  0  /
      DATA SLASH   / 1H/       /
      DATA ISAVE   /
     1   2,15,1,  3,11,2,  5,12,1,  5,16,3,  6, 5,1, 23,8,1, 23,9,1,
     2  23,10,1, 24, 8,1, 24, 9,1, 24,10,1, 27,14,2, 29, 6,2         /
      DATA RDMAP 1 /
     1 4HPARA,4HM   ,4H  //,4H*NOP,4H*/AL,4HWAYS,4H=-1 ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     2 4HMRED,4H1   ,4H  CA,4HSECC,4H,GEO,4HM4,D,4HYNAM,4HICS,,4HCSTM,
     * 4H/USE,4HTR,E,4HEDR,,4HEQST,4H,DMR,4H!*NA,4HMEA ,4H  */,4H    ,
     3 4H    ,4H    ,4H  S,,4HN,DR,4HY/ST,4HP/S,,4HN,NO,4HFIX/,4HS,N,,
     * 4HSKIP,4HM!*C,4HOMPL,4HEX* ,4H$   , 4*4H    ,
     4 4HCOND,4H    ,4H  LB,4HM3ST,4HP,DR,4HY $ ,12*4H    ,
     5 4HSOFI,4H    ,4H  /K,4HNOA,,4HMNOA,4H,PNO,4HA,BN,4HOA,K,4H4NOA,
     * 4H/S,N,4H,DRY,4H!*NA,4HMEA ,4H  */,4H*KMT,4HX*!*,4HMMTX,4H*/  ,
     6 4H    ,4H    ,4H  *P,4HVEC*,4H!*BM,4HTX*/,4H*K4M,4HX* $,4H    ,
     * 4H    ,8*4H    ,
     7 4HCOND,4H    ,4H  LB,4HM2ST,4HP,SK,4HIPM ,4H$   ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     8 4HEQUI,4HV   ,4H  KN,4HOA,K,4HFFX/,4HNOFI,4HX $ ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     9 4HEQUI,4HV   ,4H  MN,4HOA,M,4HFFX/,4HNOFI,4HX $ ,4H    ,4H    ,
     * 4H    ,8*4H         /
      DATA RDMAP 2 /
     O 4HEQUI,4HV   ,4H  BN,4HOA,B,4HFFX/,4HNOFI,4HX $ ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     1 4HEQUI,4HV   ,4H  K4,4HNOA,,4HK4FF,4HX/NO,4HFIX ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     2 4HCOND,4H    ,4H  LB,4HM1ST,4HP,NO,4HFIX ,4H$   ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     3 4HSCE1,4H    ,4H  US,4HETR,,4HKNOA,4H,MNO,4HA,BN,4HOA,K,4H4NOA,
     * 4H/KFF,4HX,KF,4HSX,K,4HSSX,,4HMFFX,4H,BFF,4HX,K4,4HFFX ,4H$   ,
     4 4HLABE,4HL   ,4H  LB,4HM1ST,4HP $ ,13*4H    ,
     5 4HPARA,4HMR  ,4H  //,4H*COM,4HPLEX,4H*//1,4H.0/G,4HPARA,4HM  /,
     * 4HG $ ,8*4H    ,
     6 4HADD ,4H    ,4H  KF,4HFX,K,4H4FFX,4H/KDD,4H/G/(,4H0.0,,4H1.0),
     * 4H/(1.,4H0,0.,4H0)  ,4H$   ,5*4H    ,
     7 4HEQUI,4HV   ,4H  KD,4HD,KF,4HFX/A,4HLWAY,4HS $ ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     8 4HCEAD,4H    ,4H  KF,4HFX,B,4HFFX,,4HMFFX,4H,EED,4HR,/P,4HHIDR,
     * 4H,CLA,4HMA,O,4HCEIG,4HS,PH,4HIDL/,4HNEIG,4HVS $,4H    ,4H    /
      DATA RDMAP 3 /
     9 4HOFP ,4H    ,4H  CL,4HAMA,,4HOCEI,4HGS,,,4H,,//,4H $  ,4H    ,
     * 4H    ,8*4H    ,
     O 4HEQUI,4HV   ,4H  PH,4HIDR,,4HPHIF,4HR/NO,4HFIX ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     1 4HEQUI,4HV   ,4H  PH,4HIDL,,4HPHIF,4HL/NO,4HFIX ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     2 4HCOND,4H    ,4H  LB,4HM2ST,4HP,NO,4HFIX ,4H$   ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     3 4HUMER,4HGE  ,4H  US,4HETR,,4HPHID,4HR,/P,4HHIFR,4H!*N*,4H!*F*,
     * 4H!*S*,4H $  , 7*4H    ,
     4 4HUMER,4HGE  ,4H  US,4HETR,,4HPHID,4HL,/P,4HHIFL,4H!*N*,4H!*F*,
     * 4H!*S*,4H $  , 7*4H    ,
     5 4HLABE,4HL   ,4H  LB,4HM2ST,4HP $ ,13*4H    ,
     6 4HCMRE,4HD2  ,4H  CA,4HSECC,4H,CLA,4HMA,P,4HHIFR,4H,PHI,4HFL,E,
     * 4HQST,,4HUSET,4HR,KN,4HOA,M,4HNOA,,4HBNOA,4H,K4N,4HOA,P,4HNOA/,
     7 4H    ,4H    ,4H  KN,4HOB,M,4HNOB,,4HBNOB,4H,K4N,4HOB,P,4HNOB,,
     * 4HPONO,4HB/ST,4HP/S,,4HN,DR,4HY!*P,4HVEC*,4H $  ,4H    ,4H    /
      DATA RDMAP 4 /
     8 4HLABE,4HL   ,4H  LB,4HM3ST,4HP $ ,13*4H    ,
     9 4HLODA,4HPP  ,4H  PN,4HOB,P,4HONOB,4H/!*N,4HAMEB,4H   *,4H/S,N,
     * 4H,DRY,4H $  ,7*4H    ,
     O 4HCOND,4H    ,4H  FI,4HNIS,,4HDRY ,4H$   ,12*4H        /
      DATA XTRA    /
     1              4HNAME,4HBOUN,4HFIXE,4HMETH,4HRANG,4HNMAX,4HUSER,
     2              4HOUTP,4HOLDM,4HGPAR,4HRSAV/
      DATA OCT 1   /
     1                    7    ,         8    ,         0  ,
     2                    8    ,         8    ,         1  ,
     3                    9    ,         8    ,         2  ,
     4                   10    ,         8    ,        16  ,
     5                   11    ,         8    ,        32  ,
     6                   12    ,         8    ,         0  ,
     7                   13    ,         8    ,         0  ,
     8                   14    ,         8    ,         0  ,
     9                   15    ,         8    ,         0  ,
     O                   16    ,         8    ,         0  ,
     1                   17    ,         8    ,         0  ,
     2                   18    ,         8    ,         0  ,
     3                   19    ,         8    ,         0  ,
     4                   20    ,         8    ,         0  ,
     5                   21    ,         8    ,         0  ,
     6                   22    ,         8    ,         0  ,
     7                   23    ,         8    ,         0  ,
     8                   24    ,         8    ,         0  /
      DATA OCT 2   /
     1                   25    ,         8    ,         0  ,
     2                   29    ,         0    ,         8  /
      DATA PTBS 1  /
     1          2  , 59  , 59  ,  8  ,4HNAMA  ,         0  ,  0  ,
     2          3  , 19  , 19  ,  3  ,4HSTEP  ,         0  ,  0  ,
     3          4  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     4          5  , 12  , 13  ,  3  ,4HNONA  ,         1  , -1  ,
     5          5  , 17  , 18  ,  3  ,4HNONA  ,         2  , -1  ,
     6          5  , 22  , 23  ,  3  ,4HNONA  ,        12  , -1  ,
     7          5  , 27  , 28  ,  3  ,4HNONA  ,        16  , -1  ,
     8          5  , 32  , 34  ,  3  ,4HNONA  ,        32  , -1  ,
     9          5  , 47  , 47  ,  8  ,4HNAMA  ,         0  ,  0  ,
     O          6  , 12  , 12  ,  4  ,4HPITM  ,         0  ,  0  ,
     1          7  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     2          8  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     3          9  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     4         10  , 11  , 12  ,  3  ,4HNONA  ,         0  ,  0  ,
     5         11  , 11  , 13  ,  3  ,4HNONA  ,         0  ,  0  ,
     6         12  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     7         13  , 17  , 18  ,  3  ,4HNONA  ,         1  ,  0  ,
     8         13  , 22  , 23  ,  3  ,4HNONA  ,         2  ,  0  /
      DATA PTBS 2  /
     1         13  , 27  , 28  ,  3  ,4HNONA  ,        16  ,  0  ,
     2         13  , 32  , 34  ,  3  ,4HNONA  ,        32  ,  0  ,
     3         13  , 38  , 42  ,  0  ,4HNAMA  ,         1  ,  0  ,
     4         13  , 43  , 47  ,  0  ,4HNAMA  ,         1  ,  0  ,
     5         13  , 48  , 52  ,  0  ,4HNAMA  ,         1  ,  0  ,
     6         13  , 53  , 57  ,  0  ,4HNAMA  ,         2  ,  0  ,
     7         13  , 58  , 62  ,  0  ,4HNAMA  ,        16  ,  0  ,
     8         13  , 63  , 68  ,  0  ,4HNAMA  ,        32  ,  0  ,
     9         14  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     O         15  , 28  , 28  ,  8  ,4HGPAR  ,         0  ,  0  ,
     1         18  , 11  , 15  ,  0  ,4HNAMA  ,         1  ,  0  ,
     2         18  , 16  , 20  ,  0  ,4HNAMA  ,        16  ,  0  ,
     3         18  , 21  , 25  ,  0  ,4HNAMA  ,         2  ,  0  ,
     4         22  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     5         25  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     6         26  , 18  , 23  ,  0  ,4HNAMA  ,        55  ,  0  ,
     7         26  , 24  , 29  ,  0  ,4HNAMA  ,        55  ,  0  ,
     8         26  , 30  , 35  ,  0  ,4HNAMA  ,        55  ,  0  /
      DATA PTBS 3  /
     1         26  , 47  , 48  ,  3  ,4HNONA  ,         1  ,  0  ,
     2         26  , 52  , 53  ,  3  ,4HNONA  ,         2  ,  0  ,
     3         26  , 57  , 58  ,  3  ,4HNONA  ,        16  ,  0  ,
     4         26  , 62  , 64  ,  3  ,4HNONA  ,        32  ,  0  ,
     5         26  , 68  , 69  ,  3  ,4HNONA  ,        12  ,  0  ,
     6         27  , 11  , 12  ,  3  ,4HNONB  ,         1  , -1  ,
     7         27  , 16  , 17  ,  3  ,4HNONB  ,         2  , -1  ,
     8         27  , 21  , 22  ,  3  ,4HNONB  ,        16  , -1  ,
     9         27  , 26  , 28  ,  3  ,4HNONB  ,        32  , -1  ,
     O         27  , 32  , 33  ,  3  ,4HNONB  ,        12  , -1  ,
     1         27  , 37  , 39  ,  3  ,4HNONB  ,        12  , -1  ,
     2         27  , 43  , 43  ,  3  ,4HSTEP  ,         0  ,  0  ,
     3         27  , 56  , 56  ,  4  ,4HPITM  ,        12  ,  0  ,
     4         28  , 15  , 15  ,  3  ,4HSTEP  ,         0  ,  0  ,
     5         29  , 11  , 12  ,  3  ,4HNONB  ,         0  ,  0  ,
     6         29  , 16  , 18  ,  3  ,4HNONB  ,         0  ,  0  ,
     7         29  , 24  , 24  ,  8  ,4HNAMB  ,         0  ,  0  /
      DATA SUBNAM  / 4HASCM,2H13  /
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
