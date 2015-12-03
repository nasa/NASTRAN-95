      SUBROUTINE KFLUD4
C
C     THIS ROUTINE IS USED FOR THE 4-SIDED FLUID ELEMENT. IT REARRANGES
C     THE DATA AND  CALLS THE KFLUD3 ROUTINE FOR EACH SUBELEMENT.
C
C     THE ECPT DATA FOR THE ELEMENT AND ITS SUBELEMENTS ARE
C
C        FIELD      SYMBOL(FLUID4)      SYMBOL(FLUID3)
C           1            ID                  ID
C           2            SIL1                SIL1
C           3            SIL2                SIL2
C           4            SIL3                SIL3
C           5            SIL4                RHO
C           6            RHO                 BULK
C           7            BULK                N
C           8            N                   CSF
C           9            CSF                 R1
C          10            R1                  Z1
C          11            Z1                  -
C          12            -                   CSF
C          13            CSF                 R2
C          14            R2                  Z2
C          15            Z2                  -
C          16            -                   CSF
C          17            CSF                 R3
C          18            R3                  Z3
C          19            Z3                  -
C          20            -
C          21            CSF
C          22            R4
C          23            Z4
C          24            -
C          25            -
C
      LOGICAL         NOGO
      INTEGER         OUT,NECPT(100)
      REAL            KI
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ SYSBUF,OUT,NOGO,SKIP(34),IAXIF
      COMMON /SMA1DP/ R(3),Z(3),NNEG,NJ,NPTJ,KI
      COMMON /SMA1CL/ IOPT1,K1GGSW,NPVT
      COMMON /SMA1ET/ ECPT(100)
      EQUIVALENCE     (ECPT(1),NECPT(1))
C
      IF (NECPT(6).LE. 0.0) RETURN
C
C     TEST FOR INTERIOR ANGLES GREATER THAN 180 DEGREES
C
      NNEG = 0
      IP   = 0
      DO 30 I = 1,4
      DO 20 J = 1,3
      NJ   = I + J - 1
      IF (NJ .GT. 4) NJ = NJ - 4
      NPTJ = 4*(NJ-1) + 10
      R(J) = ECPT(NPTJ  )
   20 Z(J) = ECPT(NPTJ+1)
      IF (NPVT .EQ. NECPT(I+1)) IP = IP + 1
      KI   = (R(2)-R(1))*(Z(3)-Z(1)) - (R(3)-R(1))*(Z(2)-Z(1))
      IF (KI) 25,2000,30
   25 NNEG = NNEG + 1
   30 CONTINUE
      IF (NNEG.EQ.1 .OR. NNEG.EQ.3) GO TO 2000
      IF (IP .NE. 1) GO TO 2000
      ECPT(6) = ECPT(6)*2.0
      DO 50 I = 1,24
   50 ECPT(I+50) = ECPT(I)
      DO 60 I = 5,24
   60 ECPT(I) = ECPT(I+1)
      IRET = 1
      GO TO 100
   70 ECPT( 4) = ECPT(55)
      ECPT(17) = ECPT(72)
      ECPT(18) = ECPT(73)
      IRET = 2
      GO TO 100
   80 ECPT(13) = ECPT(68)
      ECPT(14) = ECPT(69)
      ECPT( 3) = ECPT(54)
      IRET = 3
      GO TO 100
   90 ECPT( 9) = ECPT(64)
      ECPT(10) = ECPT(65)
      ECPT( 2) = ECPT(53)
      IRET = 4
C
  100 IF (NECPT(2).NE.NPVT .AND. NECPT(3).NE.NPVT .AND.
     1    NECPT(4).NE.NPVT)  GO TO 110
      CALL KFLUD3
  110 GO TO (70,80,90,120), IRET
  120 RETURN
C
 2000 CONTINUE
      NJ = NECPT(1)
      IF (IAXIF .EQ. 0) GO TO 2001
      NJ = NJ/1000
 2001 CONTINUE
      WRITE  (OUT,3000) UFM,NJ
 3000 FORMAT (A23,' 5002, INTERIOR ANGLE GREATER THAN OR EQUAL TO 180 ',
     1        'DEGREES FOR ELEMENT',I12)
      NOGO = .TRUE.
      RETURN
      END
