      SUBROUTINE MTMSU1 (Y,X,BUF)
C
C     M TIMS U  FORMS THE PRODUCT  X = M*Y
C
      INTEGER            DIAG      ,EOL      ,FILEM    ,FILEK
      REAL               X(1)      ,Y(1)
      DIMENSION          BUF(1)
      COMMON   /INVPWX/  FILEK(7)  ,FILEM(7)
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP       ,CSP      ,CDP      ,SQR      ,
     3                   RECT      ,DIAG     ,LOWTRI   ,UPRTRI   ,
     4                   SYM       ,ROW      ,IDENTY
      COMMON   /INVPXX/  DUMM(13)  ,NZERO
      COMMON   /ZNTPKX/  A(4)      ,II       ,EOL
C     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
      EQUIVALENCE        (A(1),DA)
C
C
      NCOL = FILEK(2)
      DO 10 I = 1,NCOL
   10 X(I) = 0.0
C
C     MASS MATRIX IS NOT DIAGONAL
C
      NZERO = 0
      DO 40 I = 1,NCOL
      IF (Y(I) .EQ. 0.0) GO TO 30
      CALL INTPK (*40,FILEM(1),0,RSP,0)
      NZERO = NZERO + 1
   20 CALL ZNTPKI
      X(II) = DA*Y(I) + X(II)
      IF (EOL) 40,20,40
   30 CALL  SKPREC (FILEM,1)
      NZERO = NZERO + 1
   40 CONTINUE
      GO TO 90
   90 CALL REWIND (FILEM(1))
      CALL SKPREC (FILEM,1)
      NZERO = NCOL - NZERO
      RETURN
      END
