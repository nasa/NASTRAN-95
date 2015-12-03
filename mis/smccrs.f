      SUBROUTINE SMCCRS ( TEMP, ZIL, ILIM, ZOL )
      REAL       TEMP( ILIM ), ZIL( ILIM ), ZOL
      DO 10 I = 1, ILIM
      TEMP( I ) = TEMP( I ) + ZIL( I ) * ZOL
10    CONTINUE
      RETURN
      END
      
      
