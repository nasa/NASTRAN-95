      FUNCTION GO ( R , ETAR , ETAL , EKM )
C
      DIMENSION  AS(2) , C(2) , S(2) , S0(2)
      DIMENSION BSL(23)
C
      DBSLJ = 1.0E-10
      S(1)  =  ETAR
      S(2)  =  ETAL
      DO   400   I = 1 , 2
      IF ( ABS ( S(I) ) .GE. R )   GO TO  200
      S(I)  =  S(I) / R
      C(I)  =  SQRT ( 1.0 - S(I) ** 2 )
      AS(I)  =  2.0 * ATAN ( S(I) / ( 1.0 + C(I) ) )
      S(I)  =  2.0 * S(I) * C(I)
      C(I)  =  2.0 * C(I) ** 2 - 1.0
      GO TO  300
C
 200  AS(I)  =  SIGN ( 1.570796   , S(I) )
      S(I)  =  0.0
C
 300  S0(I)  =  0.0
 400  CONTINUE
C
      GO  =  AS(1) - AS(2)
      IF ( ABS ( GO ) .LE. DBSLJ )   GO TO  700
C
      ARG  =  EKM * R
      IF ( ARG .EQ. 0.0 )   RETURN
      CALL MBBSLJ(ARG,N,BSL)
C
      GO  =  BSL(1) * GO
      F  =  1.0
      FI  =  1.0
      DO   600   J = 2 , N
      GO  =  BSL(J) * ( S(1) - S(2) ) / FI - GO
C
      DO   500   I = 1 , 2
      S4  =  2.0 * S(I) * C(I) - S0(I)
      S0(I)  =  S(I)
      S(I)  =  S4
 500  CONTINUE
C
      F  =  -F
      FI  =  FI + 1.0
 600  CONTINUE
C
      IF ( F .LT. 0.0 )   GO  =  -GO
      RETURN
C
 700  GO  =  0.0
      RETURN
      END
