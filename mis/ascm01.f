      SUBROUTINE ASCM01 (NAME,IPHASE,ISOL,NOGO)
C
C     SUBSTRUCTURE COMMAND DMAP DATA
C
C     COMMENTS FROM G.CHAN/UNISYS   8/1991
C     IN SOME UNIX MACHINES, SUCH AS SiliconGraphics, THE FORTRAN
C     COMPILER IS A SUBSET OF THE C COMPILER. THE SYMBOL /* IS A COMMENT
C     MARKER FOR C, AND ANYTHING AFTER /* IS NOT PASS OVER TO THE
C     FORTRAN COMPILER. THEREFORE, ALL /* SYMBOLS IN RDMAP ARRAY ARE
C     REPLACED BY
C     THE ! WILL BE CHANGED BACK TO / IN THE EXECUTABLE CODE.
C
      INTEGER         COMND(6,3),XTRA(3),SUBNAM(2),ISAVE(21),
     1                RDMAP(18,29),RDMAP1(18,9),RDMAP2(18,9),
     2                RDMAP3(18,9),RDMAP4(18,2),OCT(3,13),OCT1(3,13),
     3                PTBS(7,16),PTBS1(7,16)
      COMMON /PHAS11/ IPAS11(8)
      COMMON /PHAS31/ IPAS31(2)
      COMMON /ASDBD / IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1                IPH,NPH,IDAT(684)
      EQUIVALENCE     (RDMAP1(1,1),RDMAP(1, 1)),(OCT1(1,1),OCT(1,1)),
     1                (RDMAP2(1,1),RDMAP(1,10)),(PTBS1(1,1),PTBS(1,1)),
     2                (RDMAP3(1,1),RDMAP(1,19)),
     3                (RDMAP4(1,1),RDMAP(1,28))
      DATA COMND    /
     1                4HSUBS    , 29    ,  3    , 13    , 16    ,  8  ,
     2                4HSUBS    ,  8    ,  1    ,  0    ,  3    ,  0  ,
     3                4HSUBS    ,  8    ,  1    ,  0    ,  3    ,  2  /
      DATA SLASH    / 1H/       /
      DATA ISAVE    /
     1     3,13,1, 19,8,2, 26,13,3, 26,15,2, 26,17,1, 27,5,1, 28,4,3  /
      DATA RDMAP 1  /
     1 4HALTE,4HR   ,4H  (B,4HEGIN,4H) $ ,13*4H    ,
     2 4HPARA,4HM   ,4H  //,4H*NOP,4H*/AL,4HLWAY,4HS=-1,4H $  ,4H    ,
     * 4H    ,8*4H    ,
     3 4HSGEN,4H    ,4H  CA,4HSECC,4H,,,/,4HCASE,4HSS,C,4HASEI,4H,,,,,
     * 4H,,,,,4H/S,N,4H,DRY,4H!*XX,4HXXXX,4HXX*/,4HS,N,,4HLUSE,4HT/  ,
     4 4H    ,4H    ,4H  S,,4HN,NO,4HGPDT,4H $  ,12*4H    ,
     5 4HEQUI,4HV   ,4H  CA,4HSEI,,4HCASE,4HCC/A,4HLLWA,4HYS $,4H    ,
     * 4H    ,8*4H    ,
     6 4HALTE,4HR   ,4H  (A,4HFTGP,4H4) $,13*4H    ,
     7 4HPARA,4HM   ,4H  //,4H*ADD,4H*/DR,4HY/-1,4H /0 ,4H$   ,4H    ,
     * 4H    ,8*4H    ,
     8 4HLABE,4HL   ,4H  LB,4HSBEG,4H $  ,13*4H    ,
     9 4HCOND,4H    ,4H  LB,4HLIS,,4HDRY ,4H$   ,12*4H    /
      DATA RDMAP 2  /
     O 4HSSG1,4H    ,4H  SL,4HT,BG,4HPDT,,4HCSTM,4H,SIL,4H,EST,4H,MPT,
     * 4H,GPT,4HT,ED,4HT,MG,4HG,CA,4HSECC,4H,DIT,4H,/PG,4H,,,,,4H/   ,
     1 4H    ,4H    ,4H  LU,4HSET/,4HNSKI,4HP $ ,12*4H    ,
     2 4HCHKP,4HNT  ,4H  PG,4H $  ,14*4H    ,
     3 4HALTE,4HR   ,4H  (S,4HOLVE,4H) $ ,13*4H    ,
     4 4HSSG2,4H    ,4H  US,4HET,G,4HM,,K,4HFS,G,4HO,,P,4HG/QR,4H,PO,,
     * 4HPS,P,4HL $ ,7*4H    ,
     5 4HCHKP,4HNT  ,4H  PO,4H,PS,,4HPL $,13*4H    ,
     6 4HLABE,4HL   ,4H  LB,4HLIS ,4H$   ,13*4H    ,
     7 4HALTE,4HR   ,4H  (S,4HDR) ,4H$   ,13*4H    ,
     8 4HSUBP,4HH1  ,4H  CA,4HSECC,4H,EQE,4HXIN,,4HUSET,4H,BGP,4HDT,C,
     * 4HSTM,,4HGPSE,4HTS,E,4HLSET,4HS//S,4H,N,D,4HRY/ ,4H    ,4H    /
      DATA RDMAP 3  /
     9 4H    ,4H    ,4H  *N,4HAME ,4H   *,4H/XPL,4HOTID,4H !*P,4HVEC*,
     * 4H $  ,8*4H    ,
     O 4HCOND,4H    ,4H  LB,4HSEND,4H,DRY,4H $  ,12*4H    ,
     1 4HEQUI,4HV   ,4H  PG,4H,PL/,4HNOSE,4HT $ ,12*4H    ,
     2 4HCOND,4H    ,4H  LB,4HL10,,4HNOSE,4HT $ ,12*4H    ,
     3 4HSSG2,4H    ,4H  US,4HET,G,4HM,YS,4H,KFS,4H,GO,,4H,PG/,4HQR,P,
     * 4HO,PS,4H,PL ,4H$   ,6*4H    ,
     4 4HCHKP,4HNT  ,4H  PO,4H,PS,,4HPL $,13*4H    ,
     5 4HLABE,4HL   ,4H  LB,4HL10 ,4H$   ,13*4H    ,
     6 4HSOFO,4H    ,4H  ,K,4HAA,M,4HAA,P,4HL,BA,4HA,K4,4HAA//,4HS,N,,
     * 4HDRY/,4H*XXX,4HXXXX,4HX*!*,4HKMTX,4H*!*M,4HMTX*,4H!*PV,4HEC*/,
     7 4H    ,4H    ,4H  *B,4HMTX*,4H!*K4,4HMX* ,4H$   ,4H    ,4H    ,
     * 4H    ,8*4H    /
      DATA RDMAP 4  /
     8 4HLODA,4HPP  ,4H  PL,4H,/!*,4HNAME,4H    ,4H*/S,,4HN,DR,4HY $ ,
     * 4H    ,8*4H    ,
     9 4HEQUI,4HV   ,4H  CA,4HSESS,4H,CAS,4HECC/,4HALWA,4HYS $,4H    ,
     * 4H    ,8*4H    /
      DATA XTRA     /
     1 4HSAVE,4HNAME,4HRUN      /
      DATA OCT 1    /
     1                     9    ,    524288    ,         0     ,
     2                    10    ,    983040    ,        12     ,
     3                    11    ,    983040    ,        12     ,
     4                    12    ,    983040    ,        12     ,
     5                    14    ,    983040    ,        12     ,
     6                    15    ,    983040    ,        12     ,
     7                    16    ,    524288    ,         0     ,
     8                    21    ,   1572864    ,        12     ,
     9                    22    ,   1572864    ,        12     ,
     O                    23    ,   1572864    ,        12     ,
     1                    24    ,   1572864    ,        12     ,
     2                    25    ,   1572864    ,        12     ,
     3                    28    ,    524288    ,         8     /
      DATA PTBS 1   /
     1           1  , 11  , 11  ,  7  ,     4  ,         0  ,  0  ,
     2           6  , 11  , 11  ,  8  ,     1  ,         0  ,  0  ,
     3           7  , 22  , 23  ,  3  ,4HRUN   ,         0  ,  0  ,
     4          13  , 11  , 11  ,  7  ,     2  ,         0  ,  0  ,
     5          17  , 11  , 11  ,  5  ,     3  ,         0  ,  0  ,
     6          19  , 11  , 12  ,  8  ,4HNAME  ,         0  ,  0  ,
     7          19  , 21  , 22  ,  8  ,4HSAVE  ,         0  ,  0  ,
     8          19  , 30  , 32  ,  4  ,4HPITM  ,    524300  ,  0  ,
     9          26  , 12  , 15  ,  0  ,4HNAME  ,         1  ,  0  ,
     O          26  , 16  , 19  ,  0  ,4HNAME  ,         2  ,  0  ,
     1          26  , 20  , 22  ,  0  ,4HNAME  ,        12  ,  0  ,
     2          26  , 23  , 26  ,  0  ,4HNAME  ,        16  ,  0  ,
     3          26  , 27  , 31  ,  0  ,4HNAME  ,        32  ,  0  ,
     4          26  , 40  , 42  ,  8  ,4HNAME  ,         0  ,  0  ,
     5          26  , 65  , 67  ,  4  ,4HPITM  ,    524288  ,  0  ,
     6          28  , 15  , 17  ,  8  ,4HNAME  ,         0  ,  0  /
      DATA SUBNAM   / 4HASCM,2H01  /
C
C     RESTORE ORIGINAL DATA BY REPLACING ! BY / IN RDMAP
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
      IF (NAME .NE. COMND(1,IPHASE)) GO TO 1000
      ICOMND = IPHASE
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
C      MOVE XTRA DATA
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
C     MOVE PHASE 1 DATA
C
      IF (IPHASE.NE.1 .OR. NPH.EQ.0) GO TO 80
      DO 70 I = 3,8
      K = K + 1
   70 IDAT(K) = IPAS11(I)
      DO 75 I = 1,2
      K = K + 1
      IDAT(K) = IPAS11(I)
   75 CONTINUE
      GO TO 200
   80 CONTINUE
C
C     MOVE PHASE 3 DATA
C
      IF (IPHASE.NE.3 .OR. NPH.EQ.0) GO TO 200
      DO 110 I = 1,NPH
      K = K + 1
  110 IDAT(K) = IPAS31(I)
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
