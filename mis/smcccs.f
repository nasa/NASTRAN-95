      SUBROUTINE SMCCCS ( CTEMP, ZIL, ILIM, ZOL )
      COMPLEX    CTEMP( ILIM ), ZIL( ILIM ), ZOL
      DO 10 I = 1, ILIM
      CTEMP( I ) = CTEMP( I ) + ZIL( I ) * ZOL
10    CONTINUE
      RETURN
      END
      
      
