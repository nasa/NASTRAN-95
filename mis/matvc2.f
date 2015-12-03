      SUBROUTINE MATVC2 (Y,X,FILEA,BUF)
C
C     MATVC2 WILL FORM THE PRODUCT X = X + A*Y WHERE A IS A MATRIX
C     AND Y IS A VECTOR
C
C     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
C
      INTEGER            FILEA(7)  ,SUB(2)   ,DIAG     ,RDP     ,EOL
      DOUBLE PRECISION   Y(1)      ,X(1)     ,A        ,DA
      DIMENSION          BUF(1)
      COMMON   /ZNTPKX/  A(2)      ,II       ,EOL
C     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP       ,CSP      ,CDP      ,SQR      ,
     3                   RECT      ,DIAG     ,IWTRI    ,UPRTRI   ,
     4                   SYM       ,ROW      ,IDENTY
      COMMON   /TRDXX /  IDUM(27)  ,IOPEN
      EQUIVALENCE        (A(1),DA)
      DATA      SUB   /  4HMATV, 4HC2   /
C
      IF (FILEA(1) .EQ. 0) RETURN
      NCOL = FILEA(2)
      IF (FILEA(4) .EQ. IDENTY) GO TO 60
      IF (IOPEN .EQ. 1) GO TO 5
      CALL OPEN (*90,FILEA(1),BUF,RDREW)
    5 CALL FWDREC (*100,FILEA(1))
      IF (FILEA(4) .EQ. DIAG) GO TO 40
C
C     MATRIX IS FULL
C
      DO 30 I = 1,NCOL
      IF (Y(I) .EQ. 0.0D0) GO TO 20
      CALL INTPK (*30,FILEA(1),0,RDP,0)
   10 CALL ZNTPKI
      X(II) = DA*Y(I) + X(II)
      IF (EOL) 30,10,30
   20 CALL FWDREC (*100,FILEA(1))
   30 CONTINUE
      GO TO 80
C
C     MATRIX IS DIAGONAL
C
   40 CALL INTPK (*80,FILEA(1),0,RDP,0)
   50 CALL ZNTPKI
      X(II) = Y(II)*DA +X(II)
      IF (EOL) 80,50,80
C
C     MATRIX IS THE IDENTITY
C
   60 DO 70 I = 1,NCOL
   70 X(I) = Y(I) + X(I)
      RETURN
C
   80 CALL REWIND (FILEA(1))
      IF (IOPEN .EQ. 0) CALL CLOSE (FILEA(1),REW)
      RETURN
C
   90 NO = -1
      GO TO 110
  100 NO = -2
  110 CALL MESAGE (NO,FILEA(1),SUB(1))
      RETURN
      END
