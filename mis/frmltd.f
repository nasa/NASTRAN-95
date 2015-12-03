      SUBROUTINE FRMLTD (IFILE,DZ,DY,ZM)
C
C     FEER MATRIX TRANSPOSE MULTIPLY  (DOUBLE PREC)
C               T
C     DY = IFILE * DZ        WHERE DZ IS A VECTOR ALREADY IN CORE
C                            IFILE IS A GINO MATIRX FILE
C
C     LAST REVISED  11/91, BY C.CHAN/UNISYS
C     ADDITION OF A NEW TRANSPOSE MULTIPLY METHOD WHICH IS MORE
C     EFFECIENT, AND IS ALREADY GOOD FOR VECTORIZATION
C
CDB   LOGICAL          DEBUG
      DOUBLE PRECISION DZ(1)    ,DY(1)    ,ZM(1)    ,DP     ,DSUM
      DIMENSION        IFILE(7) ,IDP(2)   ,NAM(2)
      COMMON  /UNPAKX/ ITYP     ,IP       ,NP       ,INCR
      COMMON  /SYSTEM/ IBUF     ,NOUT
      COMMON  /FEERXX/ DUM18(18),NZM
      COMMON  /ZZZZZZ/ IZ(1)
      EQUIVALENCE      (DP,IDP(1))
      DATA     NAM   / 4HFRML   ,4HTD    /
CDB   DATA     DEBUG , ITER     ,MAX     / .FALSE.  ,0      ,4       /
C
CDB   IF (.NOT.DEBUG) GO TO 20
C     ITER = ITER + 1
C     IF (ITER .GT. MAX) DEBUG = .FALSE.
C     IF (DEBUG) WRITE (NOUT,10) NZM,IFILE(5)
C  10 FORMAT ('  .... IN FRMLTD DEBUG.   NZM,IFILE(5) =',2I8)
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
      INCR  = 1
      DO 40 I = 1,N
      DY(I) = 0.0D+0
      IP    = 0
      CALL UNPACK (*40,IFL,ZM(1))
      DSUM  = 0.0D+0
      II    = 0
      DO 30 J = IP,NP
      II    = II + 1
   30 DSUM  = DSUM + ZM(II)*DZ(J)
      DY(I) = DSUM
   40 CONTINUE
      GO TO 200
C
C     NEW METHOD, READ ONLY AND NO UNPACK
C
C     UNLIKE FRMLTX, IFL WAS UNPACKED FORWARD BY UNPSCR
C
   50 NREC = 0
      NWDS = IFILE(5)
CDB   N20  = N - 20
C     IF (DEBUG) WRITE (NOUT,60) NWDS,NZM
C  60 FORMAT ('  /@60   NWDS,NZM =',2I8)
      LL2  = 0
      NEXT = 1
      DO 140 I = 1,N
      IF (NEXT .LT. LL2) GO TO 100
      NREC = NREC + 1
CDB   IF (DEBUG) WRITE (NOUT,70) NREC,I
C  70 FORMAT ('  ...READING RECORD',I5,'.  I =',I7)
      CALL READ (*150,*80,IFL,ZM,NZM,1,LL)
      CALL MESAGE (-8,0,NAM)
   80 LL2  = LL/NWDS
CDB   IF (DEBUG) WRITE (NOUT,90) LL,NREC,LL2
C  90 FORMAT (1X,I10,' WORDS READ FROM RECORD NO.',I5,'   LL2 =',I10)
      NEXT = 1
  100 DP   = ZM(NEXT)
      II   = IDP(1)
      JJ   = IDP(2)
CDB   IF (DEBUG .AND. (I.LT.20 .OR. I.GT.N20)) WRITE (NOUT,110) I,II,JJ,
C     1                                                         NEXT
C 110 FORMAT ('   @110  I,II,JJ,NEXT =',4I8)
      IF (II .EQ. JJ) GO TO 130
      DSUM = 0.0D+0
      LL   = NEXT
      DO 120 J = II,JJ
      LL   = LL + 1
  120 DSUM = DSUM + ZM(LL)*DZ(J)
      DY(I)= DSUM
      GO TO 140
  130 DY(I)= ZM(NEXT+1)*DZ(II)
  140 NEXT = NEXT + JJ - II + 2
      GO TO 200
C
  150 J = IFILE(4)/10
      WRITE  (NOUT,160) NREC,I,N,J
  160 FORMAT ('0*** TRY TO READ RECORD',I5,'.   I,N,IFILE(4) =',2I7,I5)
      CALL MESAGE (-2,IFL,NAM)
C
  200 RETURN
      END
