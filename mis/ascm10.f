      SUBROUTINE ASCM10 (NAME,IPHASE,ISOL,NOGO)
C
C     SUBSTRUCTURE UTILITY COMMANDS DMAP DATA
C
      INTEGER        COMND(6,6),XTRA(1),SUBNAM(2),RDMAP(18,2),PTBS(7,10)
      COMMON /ASDBD/ IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1               IPH,NPH,IDAT(109)
      DATA    COMND/
     1               4HDEST    ,  2    ,  0    ,  0    ,  3    ,  0  ,
     2               4HEDIT    ,  2    ,  0    ,  0    ,  3    ,  0  ,
     3               4HEQUI    ,  2    ,  1    ,  0    ,  5    ,  0  ,
     4               4HSOFP    ,  2    ,  0    ,  0    , 10    ,  0  ,
     5               4HDELE    ,  2    ,  0    ,  0    , 10    ,  0  ,
     6               4HRENA    ,  2    ,  0    ,  0    ,  5    ,  0  /
      DATA    SLASH/ 1H/       /
      DATA    RDMAP/
     1 4HSOFU,4HT   ,4H  //,4HDRY/,4H*NAM,4HE   ,4H *!*,4HOPER,4H*/OP,
     * 4HT!*N,4HAME0,4H002*,4H!*PR,4HEF*/,4H*ITM,4H1*!*,4HITM2,4H*/  ,
     2 4H    ,4H    ,4H  *I,4HTM3*,4H!*IT,4HM4*/,4H*ITM,4H5* $,4H    ,
     * 4H    ,8*4H          /
      DATA    XTRA / 4HPREF /
      DATA    PTBS /
     1          1  , 16  , 18  ,  8  ,4HNAME  ,         0  ,  0  ,
     2          1  , 27  , 29  ,  4  ,4HOPER  ,         0  ,  0  ,
     3          1  , 34  , 35  ,  3  ,4HOPTI  ,         0  ,  0  ,
     4          1  , 38  , 40  ,  8  ,4HNEW   ,         0  ,  0  ,
     5          1  , 49  , 51  ,  4  ,4HPREF  ,         0  ,  0  ,
     6          1  , 56  , 58  ,  4  ,4HITM1  ,         0  ,  0  ,
     7          1  , 63  , 65  ,  4  ,4HITM2  ,         0  ,  0  ,
     8          2  , 11  , 12  ,  4  ,4HITM3  ,         0  ,  0  ,
     9          2  , 17  , 19  ,  4  ,4HITM4  ,         0  ,  0  ,
     O          2  , 24  , 26  ,  4  ,4HITM5  ,         0  ,  0  /
      DATA   SUBNAM / 4HASCM,2H10    /
C
C     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
C     (SEE ASCM01 FOR EXPLANATION))
C
      RDMAP(7, 1) = KHRFN1(RDMAP(7, 1),3,SLASH,1)
      RDMAP(10,1) = KHRFN1(RDMAP(10,1),2,SLASH,1)
      RDMAP(13,1) = KHRFN1(RDMAP(13,1),1,SLASH,1)
      RDMAP(16,1) = KHRFN1(RDMAP(16,1),3,SLASH,1)
      RDMAP(5, 2) = KHRFN1(RDMAP(5, 2),1,SLASH,1)
C
C     VALIDATE COMMAND AND SET POINTERS
C
      DO 10 I = 1,6
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
