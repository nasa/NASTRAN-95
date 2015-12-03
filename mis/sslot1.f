      SUBROUTINE SSLOT1(IOPT)
C                  IOPT-  CSLOT3 = 0,  CSLOT4 = 1
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
      INTEGER NECPT(100),NOUT(100)
      DIMENSION SV(24)
      COMMON/SDR2X5/ ECPT(100),OUT(100)
      COMMON/SDR2X6/ R(4),Z(4),RHO,FACT,A,NC1,NC2,NC3,IRET,NR,
     1 COL,NR1,NR2,NR3,II,IJ
      EQUIVALENCE (ECPT(1),NECPT(1)),(OUT(1),NOUT(1)),(OUT(6),SV(1))
      IOT = 6
      DO 10 I=1,30
   10 NOUT(I) = 0
      NC1 =1
      NC2 =2
      NC3 =3
      IF(IOPT.NE.0) GO TO 30
C     SET UP FOR THE SLOT3 ELEMENT
C****
      RHO = ECPT(5)
      IRET=4
      DO 20 I=1,3
      NR= 4*(I-1)+10
      R(I)=ECPT(NR)
      Z(I)=ECPT(NR+1)
   20 CONTINUE
      GO TO 80
C****
C     THE CSLOT4 ELEMENT IS CALCULATED AS FOLLOWS
   30 CONTINUE
      RHO = ECPT(6)*4.0
      DO 40 I =1,4
      NR = 4*(I-1) +11
      R(I) = ECPT(NR)
      Z(I) = ECPT(NR+1)
   40 CONTINUE
      NCOL=6
      IRET =1
      GO TO 80
   50 NC3 =4
      IRET=2
      GO TO 80
   60 NC2 =3
      IRET=3
      GO TO 80
   70 NC1= 2
      IRET=4
   80 A =    (R(NC1)*(Z(NC2)-Z(NC3)) +R(NC2)*(Z(NC3)-Z(NC1))
     1        + R(NC3)*(Z(NC1)-Z(NC2)) )
      FACT = - RHO *A
      SV(NC1)=(Z(NC2) -Z(NC3))/FACT+SV(NC1)
      SV(NC2)=(Z(NC3) -Z(NC1))/FACT+SV(NC2)
      SV(NC3)=(Z(NC1) -Z(NC2))/FACT+SV(NC3)
C
      NR1= 3+IOPT +NC1
      NR2= 3+IOPT +NC2
      NR3= 3+IOPT +NC3
C
      SV(NR1)=(R(NC3)-R(NC2))/FACT +SV(NR1)
      SV(NR2)=(R(NC1)-R(NC3))/FACT +SV(NR2)
      SV(NR3)=(R(NC2)-R(NC1))/FACT +SV(NR3)
C
      GO TO (50,60,70,90),IRET
C
   90 CONTINUE
      NR = IOPT+3
      IF(IOPT .EQ. 1) RHO =RHO/4.0
      DO 100 I =1,NR
      J=I+1
      IF(J .GT. IOPT+3) J =J-IOPT-3
      FACT = 1.0/(SQRT((R(J)-R(I))**2+(Z(J)-Z(I))**2)*RHO)
      II=  IOPT*(I+1) +4*I+3
      FACT = 1.0/(SQRT((R(J)-R(I))**2+(Z(J)-Z(I))**2)*RHO)
      II=  IOPT*(I+1) +4*I+3
      IJ = II +J -I
      SV(II) = FACT
      SV(IJ) =-FACT
  100 CONTINUE
C
C*****
C     WRAP UP OUTPUT
C*****
      NOUT(1)= NECPT(1)
      NOUT(2)= NECPT(2)
      NOUT(3)= NECPT(3)
      NOUT(4)= NECPT(4)
      IF(IOPT.GT.0) NOUT(5)= NECPT(5)
      RETURN
      END
