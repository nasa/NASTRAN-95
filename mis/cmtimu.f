      SUBROUTINE CM TIM U (Y,X,FILE,BUF)
C
C     CM TIM U FORMS THE MATRIX PRODUCT X = M*Y WHERE ALL MAY BE COMPLEX
C
      INTEGER            DIAG      ,EOL      ,EOR      ,FILEM(7) ,
     1                   FILEK     ,FILE(1)  ,FILEMM   ,BUF(1)   ,
     2                   NAME(2)
      DOUBLE PRECISION   X(1)      ,Y(1)     ,DA
      COMMON   /CINVPX/  FILEK(7)  ,FILEMM(7)
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP       ,CSP      ,CDP      ,SQR      ,
     3                   RECT      ,DIAG     ,LOWTRI   ,UPRTRI   ,
     4                   SYM       ,ROW      ,IDENTY
      COMMON   /CINVXX/  DUM(21)   ,NZERO
      COMMON   /ZNTPKX/  DA(2)     ,II       ,EOL      ,EOR
C     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
      EQUIVALENCE        (NCOL,FILEK(2))
      DATA      NAME  /  4HCMTI    ,4HMU     /
C
      IF (FILE(1) .EQ. 0) GO TO 5
C
C     USE MATRIX OTHER THAN THE MASS MATRIX
C
      DO 4 I = 1,7
    4 FILEM(I) = FILE(I)
      GO TO 8
C
C     USE MASS MATRIX
C
    5 DO 7 I = 1,7
    7 FILEM(I) = FILEMM(I)
    8 CONTINUE
      NCOL2 = NCOL + NCOL
      IF (FILEM(4) .EQ. IDENTY) GO TO 50
      NZERO = 0
      CALL GOPEN (FILEM(1),BUF,RDREW)
      DO 10 I = 1,NCOL2
   10 X(I) = 0.D0
      IF (FILEM(4) .EQ. DIAG) GO TO 40
C
C     MASS MATRIX IS NOT DIAGONAL
C
      DO 30 I = 1,NCOL2,2
      IF (Y(I).EQ.0.D0 .AND. Y(I+1).EQ.0.D0) GO TO 25
      CALL INTPK (*30,FILEM(1),0,CDP,0)
   22 CALL ZNTPKI
      IF (II .EQ. I) NZERO = NZERO + 1
      II = II+II-1
      X(II  ) = X(II  ) + DA(1)*Y(I  )-DA(2)*Y(I+1)
      X(II+1) = X(II+1) + DA(1)*Y(I+1)+DA(2)*Y(I  )
      IF (EOL .EQ. 0) IF (EOR) 30,22,30
      GO TO 30
   25 CALL FWDREC (*80,FILEM(1))
   30 CONTINUE
      GO TO 80
C
C     FILE ERROR
C
C  35 J = -1
C     GO TO 37
C  36 J = -2
C  37 CALL MESAGE (J,FILEM(1),NAME)
C
C     MASS MATRIX IS DIAGONAL
C
   40 CALL INTPK (*80,FILEM(1),0,CDP,0)
   45 CALL ZNTPKI
      II = II + II - 1
      X(II  ) = Y(II)*DA(1) - Y(II+1)*DA(2)
      X(II+1) = Y(II)*DA(2) + Y(II+1)*DA(1)
      NZERO = NZERO + 1
      IF (EOL .EQ. 0) IF (EOR) 80,45,80
      GO TO 80
C
C     MASS MATRIX IS THE IDENTY
C
   50 DO 55 I = 1,NCOL2
   55 X(I) = Y(I)
      NZERO = 0
      RETURN
C
   80 CALL CLOSE (FILEM(1),REW)
      NZERO = 0
      RETURN
      END
