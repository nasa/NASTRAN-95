      SUBROUTINE MSLOT(ITYPE)
C*****
C     THIS ROUTINE CALCULATES THE MASS MATRIX TERMS FOR THE
C         CSLOT3 AND CSLOT4 TWO DIMENSIONAL LAPLACE ELEMENTS
C                  IOPT-  CSLOT3 = 0,  CSLOT4 = 1
C*****
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
C***** 25                           W4
C***** 26                           TEMP
C
      INTEGER NECPT(100)
      DOUBLE PRECISION   COEF          ,A2            ,WB
     1                  ,R             ,Z             ,W
     2                  ,MIJ
C*****
      COMMON  /SMA2CL/  IOPT4,K4GGSW,NPVT
      COMMON  /SMA2ET/  ECPT(100)
      COMMON  /SMA2IO/  DUM1(10),IFILE
      COMMON/SMA2DP/     COEF          ,A2            ,WB
     1                  ,R(3)          ,Z(3)          ,W(3)
     2                  ,MIJ           ,IRET          ,IP
     3                  ,K
C*****
      EQUIVALENCE  (ECPT(1),NECPT(1))
C*****
      IF(ITYPE .GT. 0) GO TO 50
      IF(ECPT(6) .EQ.0.0.OR.NECPT(7) .EQ. 0 ) RETURN
      K=-1
   10 K=K+1
      IF(2*NECPT(8) - K*NECPT(7)  ) 30,20,10
   20 NECPT(7) = NECPT(7)*2
   30 ECPT(7) = FLOAT(NECPT(7))/2.0
      DO 40 I=1,20
   40 ECPT(I+50)= ECPT(I)
      IRET =4
      GO TO 140
C*****
C     THE CSLOT4 ELEMENT IS CHECKED FOR VALIDITY AND THE DATA ARE
C     REARRANGED TO CONFORM TO THE CSLOT3 FORMAT
C*****
   50 IF(ECPT(7).EQ.0.0 .OR.NECPT(8).EQ.0 ) RETURN
      K =-1
   60 K =K+1
      IF( 2*NECPT(9) - K*NECPT(8) ) 80,70,60
   70 NECPT(8) =NECPT(8)*2
   80 ECPT(8) = FLOAT(NECPT(8))/2.0
      DO 90 I=1,4
   90  ECPT(I+50) = ECPT(I)
      DO 100 I=6,21
  100  ECPT(I+49) = ECPT(I)
       ECPT(56) = ECPT(7)*2.0
      IRET =1
      GO TO 140
  110 ECPT(54)= ECPT(5)
      ECPT(68) = ECPT(23)
      ECPT(69) = ECPT(24)
      ECPT(70) = ECPT(25)
      IRET =2
      GO TO 140
  120 ECPT(53)= ECPT(4)
      ECPT(64)= ECPT(19)
      ECPT(65)= ECPT(20)
      ECPT(66)= ECPT(21)
      IRET =3
      GO TO 140
  130 ECPT(52)= ECPT(3)
      ECPT(60)= ECPT(15)
      ECPT(61)= ECPT(16)
      ECPT(62)= ECPT(17)
      IRET =4
C*****
C     EACH CSLOT3 ELEMENT OR SUBELEMENT IS FORMULATED AS FOLLOWS
C*****
  140 IF((NECPT(52).NE.NPVT).AND.(NECPT(53).NE.NPVT).AND.
     1   (NECPT(54).NE.NPVT)) GO TO 170
      DO 150 I=1,3
      IP = 4*(I-1)+60
      R(I) =ECPT(IP)
      Z(I) =ECPT(IP+1)
      W(I) =ECPT(IP+2)
      IF(NPVT .EQ. NECPT(I+51)) IPVT=I
  150 CONTINUE
      A2 = (R(2)-R(1))*(Z(3)-Z(1))  -(R(3)-R(1))*(Z(2)-Z(1))
      WB = W(1) +W(2) +W(3)+W(IPVT)
      COEF = DABS(A2)*ECPT(57) /(120.0D0 *ECPT(56))
      I=NPVT
      DO 160 J=1,3
      K = NECPT(J+51)
      MIJ = COEF *( WB + W(J) )
      IF (IPVT .EQ. J) MIJ =MIJ*2.0D0
      CALL SMA2B(MIJ,K,I,IFILE,0.0D0)
  160 CONTINUE
  170 GO TO (110,120,130,180),IRET
  180 RETURN
      END
