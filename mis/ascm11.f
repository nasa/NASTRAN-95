      SUBROUTINE ASCM11 (NAME,IPHASE,ISOL,NOGO)
C
C     EXIO COMMANDS DMAP DATA
C
      INTEGER        COMND(6,7),SUBNAM(2),RDMAP(18,2),XTRA(4),
     1               PTBS(7,12),ISAVE(21)
      COMMON /ASDBD/ IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1               IPH,NPH,IDAT(126)
      DATA    COMND/
     1               4HSOFI    ,  2    ,  4    ,  0    , 12    ,  0  ,
     2               4HSOFO    ,  2    ,  4    ,  0    , 12    ,  0  ,
     3               4HREST    ,  2    ,  4    ,  0    , 12    ,  0  ,
     4               4HDUMP    ,  2    ,  4    ,  0    , 12    ,  0  ,
     5               4HCHEC    ,  2    ,  4    ,  0    , 12    ,  0  ,
     6               4HCOMP    ,  2    ,  4    ,  0    , 12    ,  0  ,
     7               4HAPPE    ,  2    ,  4    ,  0    , 12    ,  0  /
      DATA    SLASH/ 1H/       /
      DATA    ISAVE/
     1   1, 7,1,  1,11,3,  1,13,2,  1,15,1,  2, 6,1,  2,11,3,  2,14,2/
      DATA    RDMAP/
     1 4HEXIO,4H    ,4H  //,4HS,N,,4HDRY/,4HMACH,4H!*DE,4HVI*/,4H*UNI,
     * 4HTNAM,4HE*!*,4HFORM,4H*!*M,4HODE*,4H!*PO,4HSI*/,4H*ITE,4HM*/ ,
     2 4H    ,4H    ,4H  *N,4HAME0,4H001*,4H!*NA,4HME00,4H02*/,4H*NAM,
     * 4HE000,4H3*!*,4HNAME,4H0004,4H*!*N,4HAME0,4H005*,4H $  ,4H    /
      DATA    XTRA / 4HMACH,4HPOSI,4HITEM,4HNAME /
      DATA    PTBS /
     1          1  , 21  , 21  ,  4  ,   101  ,         0  ,  0  ,
     2          1  , 27  , 27  ,  4  ,   102  ,         0  ,  0  ,
     3          1  , 34  , 34  ,  8  ,   103  ,         0  ,  0  ,
     4          1  , 45  , 45  ,  4  ,   104  ,         0  ,  0  ,
     5          1  , 52  , 52  ,  4  ,   105  ,         0  ,  0  ,
     6          1  , 59  , 59  ,  4  ,   106  ,         0  ,  0  ,
     7          1  , 66  , 66  ,  4  ,   107  ,         0  ,  0  ,
     8          2  , 12  , 12  ,  8  ,   108  ,         0  ,  0  ,
     9          2  , 23  , 23  ,  8  ,   109  ,         0  ,  0  ,
     O          2  , 34  , 34  ,  8  ,   110  ,         0  ,  0  ,
     1          2  , 45  , 45  ,  8  ,   111  ,         0  ,  0  ,
     2          2  , 56  , 56  ,  8  ,   112  ,         0  ,  0  /
      DATA SUBNAM  / 4HASCM,2H11  /
C
C     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
C     (SEE ASCM01 FOR EXPLANATION))
C
      DO 10 L = 1,21,3
      I = ISAVE(L+1)
      J = ISAVE(L  )
      K = ISAVE(L+2)
      RDMAP(I,J) = KHRFN1(RDMAP(I,J),K,SLASH,1)
   10 CONTINUE
C
C     VALIDATE COMMAND AND SET POINTERS
C
      DO 15 I = 1,7
      IF (NAME .EQ. COMND(1,I)) GO TO 20
   15 CONTINUE
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
