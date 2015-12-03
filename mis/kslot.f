      SUBROUTINE KSLOT (ITYPE)
C
C     THIS ROUTINE CALCULATES THE STIFFNESS MATRIX TERMS FOR THE
C     CSLOT3 AND CSLOT4 TWO DIMENSIONAL LAPLACE ELEMENTS
C
C     IOPT-  CSLOT3 = 0,  CSLOT4 = 1
C
C     THE ECPT DATA FOR THESE ELEMENTS ARE
C
C     FIELD   CSLOT3                CSLOT4
C       1       ID                  ID
C       2       SIL1                SIL1
C       3       SIL2                SIL2
C       4       SIL3                SIL3
C       5       RHO                 SIL4
C       6       BULK                RHO
C       7       M                   BULK
C       8       N                   M
C       9       CID1                N
C       10      R1                  CID1
C      11       Z1                  R1
C      12       W1                  Z1
C      13       CID2                W1
C      14       R2                  CID2
C      15       Z2                  R2
C      16       W2                  Z2
C      17       CID3                W2
C      18       R3                  CID3
C      19       Z3                  R3
C      20       W3                  Z3
C      21       TEMP                W3
C      22                           CID4
C      23                           R4
C      24                           Z4
C      25                           W4
C      26                           TEMP
C
      LOGICAL           NOGO
      INTEGER           NECPT(100) ,OUT
      DOUBLE PRECISION  COEF       ,FIR        ,FIZ        ,
     1                  R          ,Z          ,RKI        ,
     2                  A2         ,KIJ
      CHARACTER         UFM*23
      COMMON  /XMSSG /  UFM
      COMMON  /SYSTEM/  SYSBUF     ,OUT        ,NOGO
      COMMON  /SMA1CL/  IOPT4      ,K4GGSW     ,NPVT
      COMMON  /SMA1ET/  ECPT(100)
      COMMON  /SMA1IO/  DUM1(10)   ,IFILE
      COMMON  /SMA1DP/  COEF       ,FIR(3)     ,FIZ(3)      ,
     1                  R(3)       ,Z(3)       ,RKI         ,
     2                  A2         ,KIJ        ,NNEG        ,
     3                  IP         ,NPTJ       ,IRET        ,
     4                  LRI        ,LRJ        ,LRK         ,
     5                  IPVT
      EQUIVALENCE       (ECPT(1),NECPT(1))
C
      IF (ITYPE .GT. 0) GO TO 50
      IF (ECPT(5).EQ.0.0 .OR. NECPT(7).EQ.0) RETURN
      K = -1
   10 K = K + 1
      IF (2*NECPT(8)-K*NECPT(7)) 30,20,10
   20 NECPT(7) = NECPT(7)*2
   30 ECPT(7)  = FLOAT(NECPT(7))/2.0
      DO 40 I = 1,20
   40 ECPT(I+50) =  ECPT(I)
      IRET = 4
      GO TO 170
C
C     THE CSLOT4 ELEMENT IS CHECKED FOR VALIDITY AND THE DATA ARE
C     REARRANGED TO CONFORM TO THE CSLOT3 FORMAT
C
   50 IF (ECPT(6).EQ.0.0 .OR. NECPT(8).EQ.0) RETURN
      K = -1
   60 K = K + 1
      IF (2*NECPT(9)-K*NECPT(8)) 80,70,60
   70 NECPT(8) = NECPT(8)*2
   80 ECPT(8)  = FLOAT(NECPT(8))/2.0
C
      NNEG = 0
      IP   = 0
      DO 110 I = 1,4
      IF (NPVT .EQ. NECPT(I+1)) IP = IP + 1
      DO 90 J = 1,3
      NJ   = I + J - 1
      IF (NJ .GT.4) NJ = NJ - 4
      NPTJ = 4*(NJ-1) + 11
      R(J) = ECPT(NPTJ  )
   90 Z(J) = ECPT(NPTJ+1)
      COEF = (R(2)-R(1))*(Z(3)-Z(1)) - (R(3)-R(1))*(Z(2)-Z(1))
      IF (COEF) 100,220,110
  100 NNEG = NNEG + 1
  110 CONTINUE
      IF (NNEG.EQ.1 .OR. NNEG.EQ.3) GO TO 220
      IF (IP .NE. 1) GO TO 220
C
      DO 120 I = 1,4
  120 ECPT(I+50) = ECPT(I)
      DO 130 I = 7,21
  130 ECPT(I+49) = ECPT(I)
      ECPT(55) = ECPT(6)*2.0
      IRET = 1
      GO TO 170
  140 ECPT(54) = ECPT( 5)
      ECPT(68) = ECPT(23)
      ECPT(69) = ECPT(24)
      ECPT(70) = ECPT(25)
      IRET = 2
      GO TO 170
  150 ECPT(53) = ECPT( 4)
      ECPT(64) = ECPT(19)
      ECPT(65) = ECPT(20)
      ECPT(66) = ECPT(21)
      IRET = 3
      GO TO 170
  160 ECPT(52) = ECPT( 3)
      ECPT(60) = ECPT(15)
      ECPT(61) = ECPT(16)
      ECPT(62) = ECPT(17)
      IRET = 4
C
C     EACH CSLOT3 ELEMENT OR SUBELEMENT IS FORMULATED AS FOLLOWS
C
  170 IF (NECPT(52).NE.NPVT .AND. NECPT(53).NE.NPVT .AND.
     1    NECPT(54).NE.NPVT) GO TO 200
      COEF = 0.0
      A2   = 0.0
      DO 180 I = 1,3
      J    = I + 1
      IF (J .GT. 3) J = J - 3
      K    = J + 1
      IF (K .GT. 3) K = K - 3
      LRI  = 4*I + 56
      LRJ  = 4*J + 56
      LRK  = 4*K + 56
      COEF = COEF + ECPT(LRI+2)
      FIR(I) = ECPT(LRK  ) - ECPT(LRJ  )
      FIZ(I) = ECPT(LRJ+1) - ECPT(LRK+1)
      A2   = A2 + ECPT(LRI)*FIZ(I)
      IF (NECPT(I+51) .EQ. NPVT) IPVT = I
  180 CONTINUE
      IF (A2.EQ. 0.0D0) GO TO 220
      COEF = COEF*ECPT(57)/(6.0D0*ECPT(55)*DABS(A2))
      I    = NPVT
      DO 190 J = 1,3
      K    = NECPT(J+51)
      KIJ  = COEF*(FIR(IPVT)*FIR(J) + FIZ(IPVT)*FIZ(J))
      CALL SMA1B( KIJ,K,I,IFILE,0.0D0)
  190 CONTINUE
  200 GO TO (140,150,160,210), IRET
  210 RETURN
C
  220 WRITE  (OUT,230) UFM,NECPT(1)
  230 FORMAT (A23,' 2160, BAD GEOMETRY OR ZERO COEFFICIENT FOR SLOT ',
     1       'ELEMENT NUMBER',I18)
      NOGO =.TRUE.
      RETURN
      END
