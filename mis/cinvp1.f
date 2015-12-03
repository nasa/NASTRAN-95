      SUBROUTINE CINVP1
C*******
C     CINVP1 INITIALIZES AND CALLS SUBROUTINE ADD FOR CINVPR
C*******
      DOUBLE PRECISION   ALPHA(2) ,BETA(2)  ,LAMBDA
      INTEGER            SCR1     ,SCR2     ,SCR11     ,SQR
      INTEGER            FILEK    ,FILEM    ,FILEB     ,CDP
      INTEGER            IFILA(7) ,IFILB(7) ,IFILC(7)  ,CSP
      INTEGER            SYSBUF   ,SWITCH
C
      COMMON   /CINVPX/  FILEK(7)  ,FILEM(7) ,FILEB(7) ,DUM(15)  ,
     1                   SCR1      ,SCR2     ,SCR(8)   ,SCR11
      COMMON   /CINVXX/  LAMBDA(2) ,SWITCH
      COMMON   /SADDX /  NOMAT     ,NZ       ,MCBS(67)
      COMMON   /ZZZZZZ/  Z(1)
      COMMON   /NAMES /  DUMM(9)   ,CSP      ,CDP      ,SQR
      COMMON   /SYSTEM/  SYSBUF
C
      EQUIVALENCE        ( MCBS( 1), IFILA(1) )
     1                  ,( MCBS( 8), ITYPAL   )
     1                  ,( MCBS(61), IFILC(1) )
     1                  ,( MCBS(13), IFILB(1) )
     1                  ,( MCBS(20), ITYPBT   )
     1                  ,( MCBS(21), BETA(1)  )
     1                  ,( MCBS( 9), ALPHA(1) )
C*******
C     FORM -(B+LAMBDA*M) ON SCR2
C*******
      NOMAT = 2
      DO 10 I = 1,7
      IFILA(I) = FILEM(I)
   10 IFILB(I) = FILEB(I)
      ALPHA(1) = -LAMBDA(1)
      ALPHA(2) = -LAMBDA(2)
      BETA(1)  = -1.D0
      BETA(2)  = 0.D0
      ITYPAL   = CDP
      ITYPBT   = CDP
      NZ = KORSZ(Z)
      IF (SWITCH .EQ. -204) NZ = NZ - 2*SYSBUF
      IFILC(1) = SCR2
      IF (SWITCH .NE. 0) IFILC(1) = SCR11
      IFILC(2) = FILEK(2)
      IFILC(3) = FILEK(3)
      IFILC(4) = 1
      IFILC(5) = CDP
      CALL SADD (Z,Z)
C*******
C     FORM (LAMBDA**2*M+LAMBDA*B+K) ON SCR1
C*******
      DO 30 I = 1,7
   30 IFILA(I) = FILEK(I)
      IFILB(1) = IFILC(1)
      IFILB(2) = FILEK(2)
      IFILB(3) = FILEK(3)
      IFILB(4) = SQR
      ALPHA(2) = 0.D0
      BETA(1)  = -LAMBDA(1)
      BETA(2)  = -LAMBDA(2)
      IFILB(5) = CDP
      ALPHA(1) = 1.D0
      IFILC(1) = SCR1
      CALL SADD (Z,Z)
      RETURN
      END
