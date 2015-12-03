      SUBROUTINE INTFBS (DX,DY,IOBUF)
C
C     GIVEN THE TRIANGULAR FACTORS FOR A GENERAL MATRIX, INTFBS WILL
C     PERFORM THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE
C     A SYSTEM OF EQUATIONS
C
C     DEFINITION OF INPUT PARAMETERS
C
C     FILEL    =  MATRIX CONTROL BLOCK FOR THE LOWER TRIANGLE L
C     FILEU    =  MATRIX CONTROL BLOCK FOR THE UPPER TRIANGLE U
C     DX       =  THE LOAD VECTOR B
C     DY       =  THE SOLUTION VECTOR X
C     IOBUF    =  THE INPUT BUFFER
C
C     NAMED COMMONS
C
      INTEGER            FILEL     ,FILEU    ,TYPEAR   ,RDP      ,
     1                   PARM(4)   ,RSP      ,EOL
      DIMENSION          IOBUF(1)  ,DX(1)    ,DY(1)
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP       ,CSP      ,CDP      ,SQR      ,
     3                   RECT      ,DIAG     ,LOWTRI   ,UPRTRI   ,
     4                   SYM       ,ROW      ,IDENTY
      COMMON   /ZNTPKX/  A(4)      ,II       ,EOL
      COMMON   /INFBSX/  FILEL(7)  ,FILEU(7)
      COMMON   /TRDXX /  IDUMMY(27),IOPEN
      EQUIVALENCE        (A(1),DA) ,
     1                   (FILEL(3) ,NROW)
      DATA      PARM(3), PARM(4)   /4HINTF   ,4HBS   /
C
C
C     TRANSFER THE LOAD VECTOR TO THE SOLUTION VECTOR
C
      DO 10 I = 1,NROW
   10 DY(I)  = DX(I)
      TYPEAR = RSP
C
C     OPEN FILE FOR THE LOWER TRIANGLE
C
      PARM(2) = FILEL(1)
      IF (IOPEN .EQ. -10) GO TO 15
      IF (IOPEN .EQ.   0) CALL OPEN (*200,FILEL(1),IOBUF,RDREW)
      CALL FWDREC (*210,FILEL(1))
C
C     BEGIN FORWARD PASS
C
   15 J = 1
   20 CALL INTPK (*90,FILEL(1),0,TYPEAR,0)
   30 IF (EOL) 220,40,220
   40 CALL ZNTPKI
      IF (J-II) 80,50,30
C
C     PERFORM THE REQUIRED ROW INTERCHANGE
C
   50 IN1   = J + IFIX(A(1))
      DTEMP = DY(J)
      DY(J) = DY(IN1)
      DY(IN1) = DTEMP
   60 IF (EOL) 90,70,90
   70 CALL ZNTPKI
   80 DY(II) = DY(II) - DY(J)*DA
      GO TO 60
   90 J = J + 1
      IF (J .LT. NROW) GO TO 20
      CALL REWIND (FILEL(1))
      IF (IOPEN .EQ.   0) CALL CLOSE (FILEL(1),REW)
      IF (IOPEN .EQ. -10) CALL SKPREC (FILEL,1)
C
C     BEGIN BACKWARD PASS
C
      IOFF = FILEU(7) - 1
      PARM(2) = FILEU(1)
      IF (IOPEN .EQ. -10) GO TO 95
      IF (IOPEN .EQ.   0) CALL OPEN (*200,FILEU(1),IOBUF,RDREW)
      CALL FWDREC (*210,FILEU(1))
   95 J = NROW
  100 CALL INTPK (*220,FILEU(1),0,TYPEAR,0)
      IF (EOL) 220,120,220
  120 CALL ZNTPKI
      I = NROW - II + 1
      IF (I .NE. J) GO TO 170
C
C     DIVIDE BY THE DIAGONAL
C
      DY(I) = DY(I)/DA
C
C     SUBTRACT OFF REMAINING TERMS
C
  140 IF (I .GT. J) GO TO 120
      IF (EOL) 190,160,190
  160 CALL ZNTPKI
      I = NROW - II + 1
  170 IN1 = I
      IN2 = J
      IF (I .LT. J) GO TO 180
      K   = IN1
      IN1 = IN2 - IOFF
      IN2 = K
  180 DY(IN1) = DY(IN1) - DY(IN2)*DA
      GO TO 140
  190 J = J - 1
      IF (J .GT. 0) GO TO 100
      CALL  REWIND (FILEU(1))
      IF (IOPEN .EQ.   0) CALL CLOSE (FILEU(1),REW)
      IF (IOPEN .EQ. -10) CALL SKPREC (FILEU,1)
      RETURN
C
  200 PARM(1) = -1
      GO TO 230
  210 PARM(1) = -2
      GO TO 230
  220 PARM(1) = -5
  230 CALL MESAGE (PARM(1),PARM(2),PARM(3))
      RETURN
      END
