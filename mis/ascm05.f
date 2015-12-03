      SUBROUTINE ASCM05 (NAME,IPHASE,ISOL,NOGO)
C
C     SOLVE COMMAND DMAP DATA
C
      INTEGER         COMND(6,1),SUBNAM(2),RDMAP(18,28),RDMAP1(18,9),
     1                RDMAP2(18,9),RDMAP3(18,9),RDMAP4(18,1),OCT(3,5),
     2                OCT1(3,5),PTBS(7,20),PTBS1(7,18),PTBS2(7,2)
      COMMON /PHAS25/ IPAS25(14)
      COMMON /ASDBD / IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1                IPH,NPH,IDAT(673)
      EQUIVALENCE     (RDMAP1(1,1),RDMAP(1, 1)),(OCT1(1,1),OCT(1,1)),
     1                (RDMAP2(1,1),RDMAP(1,10)),(PTBS1(1,1),PTBS(1,1)),
     2                (RDMAP3(1,1),RDMAP(1,19)),(PTBS 2(1,1),PTBS(1,19))
     3,               (RDMAP4(1,1),RDMAP(1,28))
      DATA COMND   /
     1                4HSOLV    , 28    ,  0    ,  5    , 20    , 14  /
      DATA SLASH   /  1H/       /
      DATA RDMAP 1 /
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
      DATA RDMAP 2 /
     O 4HALTE,4HR   ,4H  (P,4HLOT),4H $  ,13*4H    ,
     1 4HALTE,4HR   ,4H  (C,4HOND),4H $  ,13*4H    ,
     2 4HCOND,4H    ,4H  LB,4HSOL,,4HNOSI,4HMP $,12*4H    ,
     3 4HALTE,4HR   ,4H  (O,4HPTP),4H $  ,13*4H    ,
     4 4HCOND,4H    ,4H  LB,4HSOL,,4HNOMG,4HG $ ,12*4H    ,
     5 4HALTE,4HR   ,4H  (S,4HMA3),4H $  ,13*4H    ,
     6 4HLABE,4HL   ,4H  LB,4HSOL ,4H$   ,13*4H    ,
     7 4HSOFI,4H    ,4H  /K,4HNOS,,4HMNOS,4H,,,/,4HDRY/,4H*NAM,4HESOL,
     * 4HS*!*,4HKMTX,4H*!*M,4HMTX*,4H $  , 4*4H    ,
     8 4HEQUI,4HV   ,4H  KN,4HOS,K,4HGG/N,4HOSIM,4HP $ ,4H    ,4H    ,
     * 4H    ,8*4H      /
      DATA RDMAP 3 /
     9 4HEQUI,4HV   ,4H  MN,4HOS,M,4HGG/N,4HOSIM,4HP $ ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     O 4HCOND,4H    ,4H  LB,4HSTP,,4HNOSI,4HMP $,12*4H    ,
     1 4HADD ,4H    ,4H  KG,4HGX,K,4HNOS/,4HKGG/,4H(1.0,4H,0.0,4H)/(1,
     * 4H.0,0,4H.0) ,4H$   ,6*4H    ,
     2 4HADD ,4H    ,4H  MG,4HG,MN,4HOS/M,4HGGX/,4H(1.0,4H,0.0,4H)/(1,
     * 4H.0,0,4H.0) ,4H$   ,6*4H    ,
     3 4HEQUI,4HV   ,4H  MG,4HGX,M,4HGG/A,4HLWAY,4HS $ ,4H    ,4H    ,
     * 4H    ,8*4H    ,
     4 4HLABE,4HL   ,4H  LB,4HSTP ,4H$   ,13*4H    ,
     5 4HCHKP,4HNT  ,4H  MG,4HG $ ,14*4H    ,
     6 4HALTE,4HR   ,4H  (G,4HP4) ,4H$   ,13*4H    ,
     7 4HCOND,4H    ,4H  LB,4HSEND,4H,DRY,4H $  ,12*4H     /
      DATA RDMAP 4 /
     8 4HALTE,4HR   ,4H  (S,4HDR2),4H $  ,13*4H            /
      DATA OCT 1   /
     1                   18    ,         0    ,         1  ,
     2                   19    ,         0    ,         2  ,
     3                   21    ,         0    ,         1  ,
     4                   22    ,         0    ,         2  ,
     5                   23    ,         0    ,         2  /
      DATA PTBS 1  /
     1          1  , 11  , 11  ,  5  ,     1  ,         0  ,  0  ,
     2          4  , 43  , 45  ,  8  ,4HNAME  ,         0  ,  0  ,
     3          9  , 13  , 13  ,  3  ,4HSTEP  ,         0  ,  0  ,
     4         10  , 11  , 11  ,  6  ,     2  ,         0  ,  0  ,
     5         11  , 11  , 11  ,  6  ,     6  ,         0  ,  0  ,
     6         12  , 50  , 50  ,  0  ,4HSOL   ,         0  ,  0  ,
     7         13  , 11  , 11  ,  6  ,     7  ,         0  ,  0  ,
     8         14  , 50  , 50  ,  0  ,4HMSKP  ,         0  ,  0  ,
     9         15  , 11  , 11  ,  6  ,     3  ,         0  ,  0  ,
     O         17  , 12  , 13  ,  3  ,4HNANO  ,         1  , -1  ,
     1         17  , 17  , 18  ,  3  ,4HNANO  ,         2  , -1  ,
     2         17  , 28  , 30  ,  8  ,4HNAME  ,         0  ,  0  ,
     3         18  , 11  , 12  ,  3  ,4HNANO  ,         0  ,  0  ,
     4         19  , 11  , 12  ,  3  ,4HNANO  ,         0  ,  0  ,
     5         20  , 13  , 13  ,  3  ,4HSTEP  ,         0  ,  0  ,
     6         21  , 16  , 17  ,  3  ,4HNANO  ,         0  ,  0  ,
     7         22  , 15  , 16  ,  3  ,4HNANO  ,         0  ,  0  ,
     8         24  , 13  , 13  ,  3  ,4HSTEP  ,         0  ,  0  /
      DATA PTBS 2  /
     1         26  , 11  , 11  ,  5  ,     4  ,         0  ,  0  ,
     2         28  , 11  , 11  ,  6  ,     5  ,         0  ,  0  /
      DATA SUBNAM  / 4HASCM,2H05  /
C
C     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
C     (SEE ASCM01 FOR EXPLANATION))
C
      RDMAP(11, 4) = KHRFN1(RDMAP(11, 4),3,SLASH,1)
      RDMAP(10,17) = KHRFN1(RDMAP(10,17),3,SLASH,1)
      RDMAP(12,17) = KHRFN1(RDMAP(12,17),2,SLASH,1)
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
      DO 70 I = 1,4
      K = K + 1
   70 IDAT(K) = IPAS25(I)
      DO 80 I = 9,14
      K = K + 1
   80 IDAT(K) = IPAS25(I)
      DO 90 I = 5,8
      K = K + 1
   90 IDAT(K) = IPAS25(I)
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
