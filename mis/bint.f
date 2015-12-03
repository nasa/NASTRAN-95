      FUNCTION BINT(I,J,A,B,IV,IW,R,Z)
      DIMENSION  R(1) , Z(1)
      BINT = 0.0
      IW1 = IW + 1
      C1P = B
      C2P = A
      C1 = C1P
       C2 = C2P
      AW = 0.0
      IF( R(I) .NE. 0.0E0 .AND. R(J) .NE. 0.0E0 ) AW = ALOG(R(J)/R(I))
      DO 100 IT = 1,IW1
      IC = IW - IT + 1
      IF (IC.EQ.0) C1 = 1.0
      IF (IT.EQ.1) C2 = 1.0
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C THE FOLLOWING CODE REPLACES REAL FUNCTION COEF
C
      IF(IT.EQ.1) GO TO 20
      IN = 1
      ID = 1
      DO 10 K=2,IT
      IN = IN*(IW-K+2)
      ID = ID*(K-1)
   10 CONTINUE
      COEF = IN/ID
      GO TO 30
   20 COEF = 1.0
   30 CONTINUE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C THE FOLLOWING CODE REPLACES REAL FUNCTION AJ
C
      IS1 = IC+IV+1
      IF(IS1.EQ.0) GO TO 60
      SP1 = IS1
      AJ = (R(J)**IS1-R(I)**IS1) / SP1
      GO TO 70
   60 AJ = AW
   70 CONTINUE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      BINT = BINT + C1 ** IC * AJ * C2 ** (IT - 1) * COEF
      C1 = C1P
      C2 = C2P
  100 CONTINUE
      AW = IW
      BINT = BINT / AW
      RETURN
      END
