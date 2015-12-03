      SUBROUTINE FRMLTA (IFILE,Z,Y,ZM)
C
C     LOWER TRIANGULAR TRANSPOSE WITH OFF-DIAGONAL SWITCH
C     SINGLE PRECISION VERSION
C
C     LAST REVISED  11/91, BY G.CHAN/UNISYS
C     ADDITIONAL OF A NEW METHODS WHICH IS MORE EFFICIENT, AND IS
C     ALREADY GOOD FOR VECTORIZATION
C
      REAL             Z(1)    ,Y(1)    ,ZM(1)
      DIMENSION        IFILE(7),NAM(2)
      COMMON  /UNPAKX/ ITYP    ,IP      ,NP      ,INCR
      COMMON  /FEERXX/ DM18(18),NZM
      COMMON  /ZZZZZZ/ IZ(1)
      COMMON  /SYSTEM/ IBUF    ,NOUT
      EQUIVALENCE      (DP,IDP)
      DATA     NAM   / 4HFRML  ,4HTA  /
C
      N    = IFILE(2)
      IFL  = IFILE(1)
      IF (IFILE(7) .LT. 0) IFL = -IFILE(7)
      CALL REWIND (IFL)
      IF (IFILE(7) .LT. 0) GO TO 30
      CALL SKPREC (IFL,1)
      ITYP = IFILE(5)
C
C     NASTRAN ORIGINAL METHOD
C
      INCR = 1
      DO 20 I = 1,N
      Y(I) = 0.0
      IP   = 0
      CALL UNPACK (*30,IFL,ZM(1))
      IF (IP .EQ. I) ZM(1) = -ZM(1)
      SUM  = 0.0
      II   = 0
      DO 10 J = IP,NP
      II   = II + 1
   10 SUM  = SUM - ZM(II)*Z(J)
      Y(I) = SUM
   20 CONTINUE
      GO TO 150
C
C     NEW METHOD
C
C     UNLIKE FRMLT, IFL WAS UNPACKED BACKWARD FIRST, THEN FORWARD BY
C     UNPSCR/FEER3. SO WE SKIP BACKWARD PASS BEFORE READING DATA
C
   30 NREC = IFILE(4)/10
      CALL SKPREC (IFL,NREC+1)
      NREC = 0
      LL2  = 0
      NTMS = 1
      DO 70 I = 1,N
      IF (NTMS .LT. LL2) GO TO 50
      NREC = NREC + 1
      CALL READ (*100,*40,IFL,ZM,NZM,1,LL)
      CALL MESAGE (-8,0,NAM)
   40 LL2  = LL
      NTMS = 1
   50 DP   = ZM(NTMS)
      II   = IDP
      IF (II .NE. I) GO TO 120
      DP   = ZM(NTMS+1)
      JJ   = IDP
      ZM(NTMS+2) = -ZM(NTMS+2)
      SUM  = 0.0
      LL   = NTMS + 1
      DO 60 J = II,JJ
      LL   = LL + 1
   60 SUM  = SUM - ZM(LL)*Z(J)
      Y(I) = SUM
   70 NTMS = NTMS + JJ - II + 3
      GO TO 150
C
  100 J = IFILE(4)/10
      WRITE  (NOUT,110) NREC,I,N,J
  110 FORMAT ('0*** TRY TO READ RECORD',I5,'.  I,N,IFILE(4) =',2I7,I5)
      CALL MESAGE (-2,ILF,NAM)
  120 WRITE  (NOUT,130) II,I
  130 FORMAT ('0*** II AND I MISMATCH =',2I8)
      CALL MESAGE (-37,0,NAM)
C
  150 RETURN
      END
