      FUNCTION ZJ ( ARG )
C
C     ZERO ORDER BESSEL FUNCTION OF FIRST KIND
C
      DBSLJ = 1.0E-10
      A  =  - ( ARG / 2.0 ) ** 2
      ZJ  =  1.0
      PF  =  1.0
      AN  =  1.0
      DO   200   I = 1 , 20
      AN  =  AN * A / PF ** 2
      PF  =  PF + 1.0
      IF ( ABS ( AN ) .LE. DBSLJ )   RETURN
      ZJ  =  ZJ + AN
 200  CONTINUE
      RETURN
      END
