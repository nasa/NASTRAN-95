      SUBROUTINE GFSH(NUY,H)
C
C     ROUTINE TO CALCULTE THE H TRANSFORMATION MATRIX USED WHEN NO
C     SPC'S ARE ON THE FLUID
C
      REAL          RZ(2)
C
      INTEGER       Z        ,SYSBUF   ,MCB(7)   ,H        ,TI1
     1             ,TO1      ,NAME(2)
C
C     OPEN CORE
C
      COMMON / ZZZZZZ /        Z(1)
C
C     SYSTEM COMMON
C
      COMMON / SYSTEM /       SYSBUF
C
C     PACK - UNPACK COMMON BLOCKS
C
      COMMON / PACKX /        TI1      ,TO1      ,I1       ,N1
     1                       ,INCR1
C
      EQUIVALENCE   ( Z(1) , RZ(1) )
C
      DATA NAME / 4HGFSH , 4H     /
C
C     ALLOCATE CORE
C
      NZ = KORSZ(Z(1))
      IBUF = NZ - SYSBUF
      NZ = IBUF - 1
      IF(NZ .LT. NUY) GO TO 1008
      NUY1 = NUY - 1
      CALL MAKMCB(MCB,H,NUY1,2,2)
      TI1 = 1
      TO1 = 2
      I1 = 1
      N1 = NUY1
      INCR1 = 1
C
      DO 100 I=1,NUY
  100 RZ(I) = -1.0 / FLOAT(NUY)
      CALL GOPEN(H,Z(IBUF),1)
      DO 120 I=1,NUY
      RZ(I) = FLOAT(NUY1) / FLOAT(NUY)
      CALL PACK(RZ(2),H,MCB)
  120 RZ(I) = -1.0 / FLOAT(NUY)
      CALL CLOSE(H,1)
      CALL WRTTRL(MCB)
      RETURN
C
C     ERRORS
C
 1008 CALL MESAGE(-8,0,NAME)
      RETURN
      END
