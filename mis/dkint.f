      DOUBLE PRECISION FUNCTION DKINT(I,J,A,B,IV,IW,R,Z)
      DOUBLE PRECISION BINT, C1P, C2P, C1, C2, AW, A, B, R, Z,DKJ, DKEF
      DOUBLE PRECISION SP1
      DIMENSION  R(1) , Z(1)
      BINT = 0.0D0
      IW1 = IW + 1
      C1P = B
      C2P = A
      C1 = C1P
      C2 = C2P
      AW = 0.0D0
      IF( R(I) .NE. 0.0D0 .AND. R(J) .NE. 0.0D0 ) AW = DLOG(R(J)/R(I))
      DO 100 IT = 1,IW1
      IC = IW - IT + 1
      IF (IC.EQ.0) C1 = 1.0D0
      IF (IT.EQ.1) C2 = 1.0D0
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C THE FOLLOWING CODE REPLACES DOUBLE PRECISION FUNCTION DKEF
C
      IF(IT.EQ.1) GO TO 20
      IN=1
      ID=1
      DO 10 K=2,IT
      IN=IN*(IW-K+2)
      ID=ID*(K-1)
   10 CONTINUE
      DKEF=IN/ID
      GO TO 30
   20 DKEF=1.0D0
   30 CONTINUE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C THE FOLLOWING CODE REPLACES DOUBLE PRECISION FUNCTION DKJ
C
      IS1 = IC+IV+1
      IF(IS1.EQ.0) GO TO 60
      SP1=IS1
      DKJ=( R(J)**IS1 - R(I)**IS1 )  /  SP1
      GO TO 70
   60 DKJ = AW
   70 CONTINUE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      BINT = BINT + C1 ** IC *DKJ * C2 ** (IT - 1) * DKEF
      C1 = C1P
      C2 = C2P
  100 CONTINUE
      AW = IW
      BINT = BINT / AW
      DKINT = BINT
      RETURN
      END
