      SUBROUTINE FRMLT (IFILE,Z,Y,ZM)
C
C     FEER MATRIX TRANSPOSE MULTIPLY  (SINGLE PREC)
C              T
C     Y = IFILE * Z        WHERE Z IS A VECTOR ALREADY IN CORE
C                          IFILE IS A GINO MATIRX FILE
C
C     LAST REVISED  11/91, BY C.CHAN/UNISYS
C     ADDITION OF A NEW TRANSPOSE MULTIPLY METHOD WHICH IS MORE
C     EFFECIENT, AND IS ALREADY GOOD FOR VECTORIZATION
C
CDB   LOGICAL          DEBUG
      REAL             Z(1)     ,Y(1)     ,ZM(1)    ,DP     ,SUM
      DIMENSION        IFILE(7) ,NAM(2)
      COMMON  /UNPAKX/ ITYP     ,IP       ,NP       ,INCR
      COMMON  /SYSTEM/ IBUF     ,NOUT
      COMMON  /FEERXX/ DUM18(18),NZM
      COMMON  /ZZZZZZ/ IZ(1)
      EQUIVALENCE      (DP,IDP)
      DATA     NAM   / 4HFRML   ,4HT    /
CDB   DATA     DEBUG , ITER     ,MAX    / .FALSE.   ,0       ,3     /
C
CDB   IF (.NOT.DEBUG) GO TO 20
C     ITER = ITER + 1
C     IF (ITER .GT. MAX) DEBUG = .FALSE.
C     IF (DEBUG) WRITE (NOUT,10) NZM,IFILE(5)
C  10 FORMAT ('  .... IN FRMLT DEBUG.   NZM,IFILE(5) =',2I8)
C  20 CONTINUE
      N    = IFILE(2)
      IFL  = IFILE(1)
      IF (IFILE(7) .LT. 0) IFL = -IFILE(7)
      CALL REWIND (IFL)
      CALL SKPREC (IFL,1)
      IF (IFILE(7) .LT. 0) GO TO 50
      ITYP = IFILE(5)
C
C     NASTRAN ORIGIANL METHOD
C
      INCR = 1
      DO 40 I = 1,N
      Y(I) = 0.0
      IP   = 0
      CALL UNPACK (*40,IFL,ZM(1))
      SUM  = 0.0
      II   = 0
      DO 30 J = IP,NP
      II   = II + 1
   30 SUM  = SUM + ZM(II)*Z(J)
      Y(I) = SUM
   40 CONTINUE
      GO TO 200
C
C     NEW METHOD, READ ONLY AND NO UNPACK
C
C     UNLIKE FRMLTA, IFL WAS UNPACKED FORWARD BY UNPSCR
C
   50 NREC = 0
C     NWDS = IFILE(5)
CDB   N20  = N - 20
C     IF (DEBUG) WRITE (NOUT,60) IFILE(5),NZM
C  60 FORMAT ('   /@60   NWDS,NZM =',2I8)
      LL2  = 0
      NEXT = 1
      DO 140 I = 1,N
      IF (NEXT .LT. LL2) GO TO 100
      NREC = NREC + 1
CDB   IF (DEBUG) WRITE (NOUT,70) NREC,I
C  70 FORMAT ('  ...READING RECORD',I5,'.   I =',I7)
      CALL READ (*150,*80,IFL,ZM,NZM,1,LL)
      CALL MESAGE (-8,0,NAM)
C  50 LL2  = LL/NWDS
   80 LL2  = LL
CDB   IF (DEBUG) WRITE (NOUT,90) LL,NREC,LL2
C  90 FORMAT (1X,I10,'WORDS READ FROM RECORD',I5,'.   LL2 =',I10)
      NEXT = 1
  100 DP   = ZM(NEXT)
      II   = IDP
      DP   = ZM(NEXT+1)
      JJ   = IDP
CDB   IF (DEBUG .AND. (I.LT.20 .OR. I.GT.N20)) WRITE (NOUT,110) I,II,JJ,
C     1                                                         NEXT
C 110 FORMAT ('   @110  I,II,JJ,NEXT =',4I8)
      IF (JJ .EQ. II) GO TO 130
      SUM  = 0.0
      LL   = NEXT + 1
      DO 120 J = II,JJ
      LL   = LL + 1
  120 SUM  = SUM + ZM(LL)*Z(J)
      Y(I) = SUM
      GO TO 140
  130 Y(I) = ZM(NEXT+2)*Z(II)
  140 NEXT = NEXT + JJ - II + 3
      GO TO 200
C
  150 J = IFILE(4)/10
      WRITE  (NOUT,160) NREC,I,N,J
  160 FORMAT ('*** TRY TO READ RECORD',I5,'.  I,N,IFILE(4) =',2I7,I5)
      CALL MESAGE (-2,IFL,NAM)
C
  200 RETURN
      END
