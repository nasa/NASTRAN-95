      SUBROUTINE SMCCRD ( DTEMP, ZIL, ILIM, ZOL )
      DOUBLE PRECISION DTEMP( ILIM ), ZIL( ILIM ), ZOL
      DO 10 I = 1, ILIM
      DTEMP( I ) = DTEMP( I ) + ZIL( I ) * ZOL
10    CONTINUE
      RETURN
      END
      
      
