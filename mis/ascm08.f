      SUBROUTINE ASCM08 (NAME,IPHASE,ISOL,NOGO)
C
C     SOLVE COMMAND DMAP DATA FOR DYNAMIC ANALYSIS
C
      INTEGER         COMND(6,1),SUBNAM(2),ISAVE(21),RDMAP(18,55),
     1                RDMAP1(18,9),RDMAP2(18,9),RDMAP3(18,9),
     2                RDMAP4(18,9),RDMAP5(18,9),RDMAP6(18,9),
     3                RDMAP7(18,1),OCT(3,23),OCT1(3,18),OCT2(3,5),
     4                PTBS(7,25),PTBS1(7,18),PTBS2(7,7)
      COMMON /PHAS28/ IPAS28(14)
      COMMON /ASDBD / IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1                IPH,NPH,IDAT(1248)
      EQUIVALENCE     (RDMAP1(1,1),RDMAP(1, 1)),(OCT1(1,1),OCT(1,1)),
     1                (RDMAP2(1,1),RDMAP(1,10)),(OCT2(1,1),OCT(1,19)),
     2                (RDMAP3(1,1),RDMAP(1,19)),(PTBS1(1,1),PTBS(1,1)),
     3                (RDMAP4(1,1),RDMAP(1,28)),(PTBS2(1,1),PTBS(1,19)),
     4                (RDMAP5(1,1),RDMAP(1,37)),
     5                (RDMAP6(1,1),RDMAP(1,46)),
     6                (RDMAP7(1,1),RDMAP(1,55))
      DATA COMND    /
     1                4HSOLV    , 55    ,  0    , 23    , 25    , 14 /
      DATA SLASH    / 1H/       /
      DATA ISAVE    /
     1    4,11,3, 13,10,1, 13,14,3, 13,16,2, 54,8,2, 54,9,2, 54,10,2 /
      DATA RDMAP 1  /
     1 4HALTE,4HR   ,4H  (G,4HP1) ,4H$   ,13*4H    ,
     2 4HPARA,4HM   ,4H  //,4H*NOP,4H*/AL,4HWAYS,4H=-1 ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     3 4HSGEN,4H    ,4H  CA,4HSECC,4H,GEO,4HM3,G,4HEOM4,4H,DYN,4HAMIC,
     * 4HS/CA,4HSESS,4H,CAS,4HEI,G,4HPL,E,4HQEXI,4HN,GP,4HDT, ,4H    ,
     4 4H    ,4H    ,4H  BG,4HPDT,,4HSIL,,4HGE3S,4H,GE4,4HS,DY,4HNS/S,
     * 4H,N,D,4HRY!*,4HNAME,4HSOLS,4H*/S,,4HN,LU,4HSET/,4H    ,4H    ,
     5 4H    ,4H    ,4H  S,,4HN,NO,4HGPDT,4H $  ,12*4H    ,
     6 4HPURG,4HE   ,4H  CS,4HTM $,14*4H    ,
     7 4HEQUI,4HV   ,4H  GE,4H3S,G,4HEOM3,4H/ALW,4HAYS/,4HGE4S,4H,GEO,
     * 4HM4/A,4HLWAY,4HS/CA,4HSEI,,4HCASE,4HCC/A,4HLWAY,4HS/  ,4H    ,
     8 4H    ,4H    ,4H  DY,4HNS,D,4HYNAM,4HICS/,4HALWA,4HYS $,4H    ,
     * 4H    ,8*4H    ,
     9 4HCOND,4H    ,4H  LB,4HSTP,,4HDRY ,4H$   ,12*4H     /
      DATA RDMAP 2  /
     O 4HALTE,4HR   ,4H  (P,4HLOT),4H $  ,13*4H    ,
     1 4HALTE,4HR   ,4H  (C,4HOND),4H $  ,13*4H    ,
     2 4HALTE,4HR   ,4H  (G,4HPWG),4H $  ,13*4H    ,
     3 4HSOFI,4H    ,4H  /K,4HNOS,,4HMNOS,4H,BNO,4HS,K4,4HNOS,,4H/DRY,
     * 4H!*NA,4HMESO,4HLS*/,4H*KMT,4HX*!*,4HMMTX,4H*!*B,4HMTX*,4H/   ,
     4 4H    ,4H    ,4H  *K,4H4MX*,4H $  ,13*4H    ,
     5 4HEQUI,4HV   ,4H  KN,4HOS,K,4HGG/N,4HOKGG,4HX $ ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     6 4HCOND,4H    ,4H  LB,4H2K,N,4HOKGG,4HX $ ,12*4H    ,
     7 4HADD ,4H    ,4H  KG,4HGX,K,4HNOS/,4HKGG/,4H(1.0,4H,0.0,4H)/(1,
     * 4H.0,0,4H.0) ,4H$   ,6*4H    ,
     8 4HLABE,4HL   ,4H  LB,4H2K $,14*4H       /
      DATA RDMAP 3  /
     9 4HEQUI,4HV   ,4H  MN,4HOS,M,4HGG/N,4HOMGG,4H $  ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     O 4HCOND,4H    ,4H  LB,4H2M,N,4HOMGG,4H $  ,12*4H    ,
     1 4HADD ,4H    ,4H  MG,4HG,MN,4HOS/M,4HGGX/,4H(1.0,4H,0.0,4H)/(1,
     * 4H.0,0,4H.0) ,4H$   ,6*4H    ,
     2 4HEQUI,4HV   ,4H  MG,4HGX,M,4HGG/A,4HLWAY,4HS $ ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     3 4HLABE,4HL   ,4H  LB,4H2M $,14*4H    ,
     4 4HEQUI,4HV   ,4H  BN,4HOS,B,4HGG/N,4HOBGG,4H $  ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     5 4HCOND,4H    ,4H  LB,4H2B,N,4HOBGG,4H $  ,12*4H    ,
     6 4HADD ,4H    ,4H  BG,4HG,BN,4HOS/B,4HGGX/,4H(1.0,4H,0.0,4H)/(1,
     * 4H.0,0,4H.0) ,4H$   ,6*4H    ,
     7 4HEQUI,4HV   ,4H  BG,4HGX,B,4HGG/A,4HLWAY,4HS $ ,4H    ,4H    ,
     * 4H    ,8*4H         /
      DATA RDMAP 4  /
     8 4HLABE,4HL   ,4H  LB,4H2B $,14*4H    ,
     9 4HEQUI,4HV   ,4H  K4,4HNOS,,4HK4GG,4H/NOK,4H4GG ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     O 4HCOND,4H    ,4H  LB,4H2K4,,4HNOK4,4HGG $,12*4H    ,
     1 4HADD ,4H    ,4H  K4,4HGG,K,4H4NOS,4H/K4G,4HGX/ ,4H(1.0,4H,0.0,
     * 4H)/(1,4H.0,0,4H.0) ,4H$   ,5*4H    ,
     2 4HEQUI,4HV   ,4H  K4,4HGGX,,4HK4GG,4H/ALW,4HAYS ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     3 4HLABE,4HL   ,4H  LB,4H2K4 ,4H$   ,13*4H    ,
     4 4HLABE,4HL   ,4H  LB,4HSTP ,4H$   ,13*4H    ,
     5 4HCHKP,4HNT  ,4H  MG,4HG,BG,4HG,K4,4HGG $,12*4H    ,
     6 4HALTE,4HR   ,4H  (P,4HARAM,4H) $ ,13*4H           /
      DATA RDMAP 5  /
     7 4HPARA,4HM   ,4H  //,4H*AND,4H*/MD,4HEMA/,4HNOUE,4H/NOM,4H2PP ,
     * 4H$   ,8*4H    ,
     8 4HPARA,4HM   ,4H  //,4H*ADD,4H*/KD,4HEK2/,4H1/0 ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     9 4HPARA,4HM   ,4H  //,4H*ADD,4H*/NO,4HMGG/,4H1/0 ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     O 4HPARA,4HM   ,4H  //,4H*ADD,4H*/NO,4HBGG/,4H1/0 ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     1 4HPARA,4HM   ,4H  //,4H*ADD,4H*/NO,4HK4GG,4H/1/0,4H $  ,4H    ,
     * 4H    ,8*4H    ,
     2 4HALTE,4HR   ,4H  (E,4HQUIV,4H) $ ,13*4H    ,
     3 4HEQUI,4HV   ,4H  K2,4HDD,K,4HDD/K,4HDEK2,4H $  ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     4 4HEQUI,4HV   ,4H  M2,4HDD,M,4HDD/N,4HOMGG,4H $  ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     5 4HEQUI,4HV   ,4H  B2,4HDD,B,4HDD/N,4HOBGG,4H $  ,4H    ,4H    ,
     * 4H    ,8*4H         /
      DATA RDMAP 6  /
     6 4HALTE,4HR   ,4H  (S,4HDR2),4H $  ,13*4H    ,
     7 4HEQUI,4HV   ,4H  UP,4HVF,U,4HPVC/,4HNOA ,4H$   ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     8 4HCOND,4H    ,4H  LB,4HL19,,4HNOA ,4H$   ,12*4H    ,
     9 4HSDR1,4H    ,4H  US,4HETD,,4H,UDV,4HF,,,,4HGOD,,4HGMD,,4H,,,/,
     * 4HUPVC,4H,,/1,4H/DYN,4HAMIC,4HS $ , 4*4H    ,
     O 4HLABE,4HL   ,4H  LB,4HL19 ,4H$   ,13*4H    ,
     1 4HCHKP,4HNT  ,4H  UP,4HVC $,14*4H    ,
     2 4HEQUI,4HV   ,4H  UP,4HVC,U,4HGV/N,4HOUE ,4H$   ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     3 4HCOND,4H    ,4H  LB,4HUE,N,4HOUE ,4H$   ,12*4H    ,
     4 4HUPAR,4HTN  ,4H  US,4HET,U,4HPVC/,4HUGV,,4HUEV,,4H,!*P,4H*!*G,
     * 4H*!*E,4H* $ ,7*4H       /
      DATA RDMAP 7  /
     5 4HLABE,4HL   ,4H  LB,4HUE $,14*4H      /
      DATA OCT 1    /
     1                   15    ,         0    ,         1  ,
     2                   16    ,         0    ,         1  ,
     3                   17    ,         0    ,         1  ,
     4                   18    ,         0    ,         1  ,
     5                   19    ,         0    ,         2  ,
     6                   20    ,         0    ,         2  ,
     7                   21    ,         0    ,         2  ,
     8                   22    ,         0    ,         2  ,
     9                   23    ,         0    ,         2  ,
     O                   24    ,         0    ,        16  ,
     1                   25    ,         0    ,        16  ,
     2                   26    ,         0    ,        16  ,
     3                   27    ,         0    ,        16  ,
     4                   28    ,         0    ,        16  ,
     5                   29    ,         0    ,        32  ,
     6                   30    ,         0    ,        32  ,
     7                   31    ,         0    ,        32  ,
     8                   32    ,         0    ,        32  /
      DATA OCT 2   /
     1                   33    ,         0    ,        32  ,
     2                   38    ,         0    ,         1  ,
     3                   39    ,         0    ,         2  ,
     4                   40    ,         0    ,        16  ,
     5                   41    ,         0    ,        32  /
      DATA PTBS 1  /
     1          1  , 11  , 11  ,  5  ,     1  ,         0  ,  0  ,
     2          4  , 43  , 45  ,  8  ,4HNAME  ,         0  ,  0  ,
     3          9  , 13  , 13  ,  3  ,4HSTEP  ,         0  ,  0  ,
     4         10  , 11  , 11  ,  6  ,     2  ,         0  ,  0  ,
     5         11  , 11  , 11  ,  6  ,     3  ,         0  ,  0  ,
     6         12  , 11  , 11  ,  6  ,     4  ,         0  ,  0  ,
     7         13  , 12  , 13  ,  3  ,4HNANO  ,         1  , -1  ,
     8         13  , 17  , 18  ,  3  ,4HNANO  ,         2  , -1  ,
     9         13  , 22  , 23  ,  3  ,4HNANO  ,        16  , -1  ,
     O         13  , 27  , 29  ,  3  ,4HNANO  ,        32  , -1  ,
     1         13  , 37  , 39  ,  8  ,4HNAME  ,         0  ,  0  ,
     2         15  , 11  , 12  ,  3  ,4HNANO  ,         0  ,  0  ,
     3         17  , 16  , 17  ,  3  ,4HNANO  ,         0  ,  0  ,
     4         19  , 11  , 12  ,  3  ,4HNANO  ,         0  ,  0  ,
     5         21  , 15  , 16  ,  3  ,4HNANO  ,         0  ,  0  ,
     6         24  , 11  , 12  ,  3  ,4HNANO  ,         0  ,  0  ,
     7         26  , 15  , 16  ,  3  ,4HNANO  ,         0  ,  0  ,
     8         29  , 11  , 13  ,  3  ,4HNANO  ,         0  ,  0  /
      DATA PTBS 2  /
     1         31  , 16  , 18  ,  3  ,4HNANO  ,         0  ,  0  ,
     2         34  , 13  , 13  ,  3  ,4HSTEP  ,         0  ,  0  ,
     3         36  , 11  , 11  ,  7  ,     5  ,         0  ,  0  ,
     4         42  , 11  , 11  ,  7  ,     6  ,         0  ,  0  ,
     5         46  , 11  , 11  ,  6  ,     7  ,         0  ,  0  ,
     6         47  , 11  , 11  ,  4  ,4HDVEC  ,         0  ,  0  ,
     7         49  , 18  , 18  ,  4  ,4HDVEC  ,         0  ,  0  /
      DATA SUBNAM  / 4HASCM,2H08  /
C
C     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
C     (SEE ASCM01 FOR EXPLANATION))
C
      DO 20 L = 1,21,3
      I = ISAVE(L+1)
      J = ISAVE(L  )
      K = ISAVE(L+2)
      RDMAP(I,J) = KHRFN1(RDMAP(I,J),K,SLASH,1)
   20 CONTINUE
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
C     MOVE PHASE 2 DATA
C
      IF (IPHASE.NE.2 .OR. NPH.EQ.0) GO TO 100
      DO 90 I = 1,NPH
      K = K + 1
   90 IDAT(K) = IPAS28(I)
      GO TO 200
  100 CONTINUE
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
