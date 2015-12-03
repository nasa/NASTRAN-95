      SUBROUTINE RAND3 (F,S,Q,N)
C
C     COMPUTES  MEAN RESPONSE  Q
C
      INTEGER         CHECK   ,NAME(2)
      REAL            Q(2)    ,F(1)     ,S(1)
      COMMON /CONDAS/ PHI     ,TWOPI    ,RADEG    ,DEGRA    ,
     1                S4PISQ
      DATA    NAME  / 4HRAND  ,4H3      /
C
C     F IS ARRAY OF FREQUENCIES
C     S IS ARRAY OF POWER SPECTRAL DENSITY FUNCTIONS
C     Q IS MEAN RESPONSE
C     N IS NUMBER OF FREQUENCIES
C
      SUM1 = 0.0
      NN   = N - 1
      SUM  = 0.0
      DO 10 I = 1,NN
      DF   = F(I+1) - F(I)
      SUM  = SUM + (S(I)+S(I+1))*DF
      FI   = F(I   )*F(I  )
      FI1  = F(I+1 )*F(I+1)
      FII1 = 2.*F(I)*F(I+1)
      ALP  = (3.*FI+FII1+FI1)/6.
      BTA  = (FI+FII1+3.*FI1)/6.
      SUM1 = SUM1 + (ALP*S(I)+BTA*S(I+1))*DF
   10 CONTINUE
      SUM  = SQRT(SUM*0.5)
      SUM1 = SQRT(SUM1*.5)
      Q(1) = SUM
      Q(2) = 0.0
      Q1   = Q(1)
      IF (Q1 .NE. 0.0) Q(2) = SUM1/Q1
      CHECK= 123456789
      RETURN
C
C     AUTOCORRALATION FUNCTION
C
      ENTRY RAND4 (F,S,TAU,R,N)
C     =========================
C
C     COMPUTES  AUTOCORRALATION FUNCTION R  AT TIME TAU
C     WHERE F,S AS ABOVE. IF TAU = 0.0  R = Q*Q
C
      IF (CHECK .NE. 123456789) CALL MESAGE (-37,0,NAME)
      IF (TAU .EQ. 0.0) GO TO 30
      NN  = N - 1
      A   = 2.0*PHI*TAU
      B   = 1.0/A
      SUM = 0.0
      DO 20 I = 1,NN
      SUM = SUM + B*(S(I+1)-S(I))/(F(I+1)-F(I))*(COS(A*F(I+1))
     1          - COS(A*F(I))) + S(I+1)*SIN(A*F(I+1))-S(I)*SIN(A*F(I))
   20 CONTINUE
      SUM = SUM*B
      R   = SUM
      GO TO 40
   30 R   = Q1*Q1
   40 RETURN
      END
