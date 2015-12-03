      SUBROUTINE CFE2MY (TPOSE,Y,X,FILE,BUF)
C*******
C     CFE2MY FORMS THE COMPLEX DOUBLE PRECISION MATRIX
C     PRODUCT X = M*Y FOR THE COMPLEX FEER METHOD
C*******
C     DEFINITION OF INPUT AND OUTPUT PARAMETERS
C*******
C     TPOSE    = .FALSE. --- USE MATRIX M
C              = .TRUE.  --- USE MATRIX M-TRANSPOSE
C     Y        = INPUT  VECTOR
C     X        = OUTPUT VECTOR
C     FILE     = INPUT MATRIX CONTROL BLOCK FOR THE
C                REQUIRED MATRIX
C     BUF      = INPUT REQUIRED GINO BUFFER
C*******
      DOUBLE PRECISION  X(1)     ,Y(1)     ,DA
      INTEGER           FILE(7)  ,BUF(1)   ,EOL      ,DIAG
      LOGICAL           TPOSE(1)
      COMMON  /NAMES /  RD       ,RDREW    ,WRT      ,WRTREW
     2                 ,REW      ,NOREW    ,EOFNRW   ,RSP
     3                 ,RDP      ,CSP      ,CDP      ,SQR
     4                 ,RECT     ,DIAG     ,LOWTRI   ,UPRTRI
     5                 ,SYM      ,ROW      ,IDENTY
      COMMON  /ZNTPKX/  DA(2)    ,II       ,EOL
      NCOL2 = FILE(2)+FILE(2)
      IF (FILE(4).EQ.IDENTY) GO TO 50
      CALL GOPEN (FILE(1),BUF(1),RDREW)
      DO 10  I = 1,NCOL2
   10 X(I) = 0.D0
      IF (FILE(4).EQ.DIAG) GO TO 40
      IF (TPOSE(1)) GO TO 31
C*******
C     GENERAL MATRIX*VECTOR PRODUCT
C*******
      DO 30  I = 1,NCOL2,2
      J = I+1
      IF (Y(I).EQ.0.D0.AND.Y(J).EQ.0.D0) GO TO 25
      CALL INTPK(*30,FILE(1),0,CDP,0)
   22 CALL ZNTPKI
      JJ = II+II
      II = JJ-1
      X(II) = X(II) + DA(1)*Y(I) - DA(2)*Y(J)
      X(JJ) = X(JJ) + DA(1)*Y(J) + DA(2)*Y(I)
      IF (EOL) 30,22,30
   25 CALL SKPREC (FILE(1),1)
   30 CONTINUE
      GO TO 80
C*******
C     GENERAL MATRIX-TRANSPOSE*VECTOR PRODUCT
C*******
   31 DO 36  I = 1,NCOL2,2
      J = I+1
      CALL INTPK(*36,FILE(1),0,CDP,0)
   32 CALL ZNTPKI
      JJ = II+II
      II = JJ-1
      X(I) = X(I) + DA(1)*Y(II) - DA(2)*Y(JJ)
      X(J) = X(J) + DA(1)*Y(JJ) + DA(2)*Y(II)
      IF (EOL) 36,32,36
   36 CONTINUE
      GO TO 80
C*******
C     MATRIX IS DIAGONAL
C*******
  40  CALL INTPK(*80,FILE(1),0,CDP,0)
   45 CALL ZNTPKI
      JJ = II+II
      II = JJ-1
      X(II) = DA(1)*Y(II) - DA(2)*Y(JJ)
      X(JJ) = DA(1)*Y(JJ) + DA(2)*Y(II)
      IF (EOL) 80,45,80
C*******
C     MATRIX IS IDENTITY
C*******
   50 DO 55  I = 1,NCOL2
   55 X(I) = Y(I)
      GO TO 90
   80 CALL CLOSE (FILE(1),REW)
   90 RETURN
      END
