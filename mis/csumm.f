      SUBROUTINE  CSUMM(D1,D2,ID1,D3,D4,ID2,D5,D6,ID5)
C
C     ADDS  D1+D2 TO  D3+D4 SCALING OUTPUT
C
      DOUBLE PRECISION D1,D2,D3,D4,D5,D6,T1,T2,T3,T4
      MULT = IABS(ID1-ID2)
      IF(MULT .LE. 38) FACTOR = 10.0**MULT
      T1 =D1
      T2 =D2
      T3 =D3
      T4 =D4
      ID5 =ID1
      IF(ID1-ID2) 30,50,20
   30 IF(MULT .GT. 38) GO TO 40
      T3 =T3*FACTOR
      T4 =T4*FACTOR
      GO TO 50
   20 IF(MULT .GT. 38) GO TO 35
      T1 = T1*FACTOR
      T2 = T2*FACTOR
      ID5= ID2
      GO TO 50
   35 D5 = D3
      D6 =D4
      ID5 = ID2
      GO TO 70
   40 D5 = D1
      D6 = D2
      GO TO 70
   50 D5 = T1 +T3
      D6 = T2 + T4
   70 RETURN
      ENTRY CSQRTN(D1,D2,ID1,D3,D4,ID2)
C
C     COMPUTES COMPLEX SQRT = SCALED
C
      ID2 = ID1
      D3=D1
      D4= D2
      IF( MOD(ID1,2) .EQ. 0) GO TO 100
      ID2 = ID2 -1
      IF(ID2 .LT. 0) GO TO 105
  101 D3 = D3*10.0
      D4 =D4*10.0
  100 ID2 = ID2/2
      T1 =DSQRT(D3*D3 +D4*D4)
      T2 = DSQRT( DABS(D3+T1)/2.0)
      T3 = DSQRT(DABS(-D3+T1)/2.0)
      D3 =T2
      D4 = T3
      IF(D2 .EQ. 0.0D0) GO TO 70
      D4 =DSIGN(T3,D2)
      GO TO 70
C
C     NEGATIVE EXPONENT
C
  105 ID2 = ID2+1
      GO TO 101
C
C     SCALES DETERMINANT
C
      ENTRY CDETM3(D1,D2,ID1)
      T1 = DMAX1(DABS(D1),DABS(D2))
      IF(T1 .EQ. 0.0D0) GO TO 70
 4125 IF(T1 .GT. 10.0D0) GO TO 4153
 4126 IF(T1 .LT. 1.0D0) GO TO 4140
      GO TO 70
 4153 D1 = D1*0.1D0
      D2 = D2*0.1D0
      T1 = T1*0.1D0
      ID1 = ID1+1
      GO TO 4125
 4140 D1 = D1*10.0D0
      D2 = D2*10.0D0
      T1 = T1*10.0D0
      ID1 = ID1-1
      GO TO 4126
      END
