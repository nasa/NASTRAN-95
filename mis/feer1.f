      SUBROUTINE FEER1
C
C     FEER1 INITIALIZES AND CALLS SUBROUTINE ADD FOR FEER
C
      INTEGER           FILEA     ,FILEB    ,FILEC    ,FILEK    ,
     1                  FILEM     ,SCR1     ,TYPALP   ,TYPBTA   ,
     2                  SQR       ,RDP
      DOUBLE PRECISION  LAMBDA    ,DALPHA   ,DBETA
      COMMON   /FEERCX/ FILEK(7)  ,FILEM(7) ,SCR1
      COMMON   /FEERXX/ LAMBDA
      COMMON   /SADDX / NOMAT     ,NZ       ,FILEA(7) ,TYPALP   ,
     1                  DALPHA(2) ,FILEB(7) ,TYPBTA   ,DBETA(2) ,
     2                  DUM(36)   ,FILEC(7)
      COMMON   /ZZZZZZ/ Z(1)
      COMMON   /NAMES / IJ(8)     ,RDP      ,IK(2)    ,SQR
      COMMON   /SYSTEM/ KSYSTM(56)
      EQUIVALENCE       (KSYSTM(55),IPREC)
C
C     SET UP CALL TO ADD
C
      DO 10  I = 1,7
      FILEA(I) = FILEM(I)
   10 FILEB(I) = FILEK(I)
      DALPHA(1)= LAMBDA
      DBETA(1) = 1.0D+0
      TYPALP   = IPREC
      TYPBTA   = IPREC
      NZ       = KORSZ(Z)
      FILEC(1) = SCR1
      FILEC(2) = FILEK(2)
      FILEC(3) = FILEK(3)
      FILEC(4) = SQR
      FILEC(5) = IPREC
      NOMAT    = 2
      IF (FILEB(1) .EQ. 0) NOMAT = 1
      CALL SADD (Z,Z)
      CALL WRTTRL (FILEC)
      RETURN
      END
