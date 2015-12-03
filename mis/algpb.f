      SUBROUTINE ALGPB (IDAT,NTYPE)
C
      INTEGER NA(4)
      DATA    NA / 2 ,     2  ,   3 ,    1 /
C                ZERO, INTEGER, REAL, ALPHA
C
C     RETURN FROM NUMTYP IS            SET NTYPE TO
C       0 -  ZERO                        1 - ALPHA
C       1 -  INTEGER                     2 - INTEGER
C       2 -  REAL                        3 - REAL
C       3 -  BCD
C
C     BLANK IS ALPHA,  ZERO IS INTEGER UNLESS NUMTYP SET IT TO REAL
C
      ITYPE = NUMTYP(IDAT) + 1
      NTYPE = NA(ITYPE)
      RETURN
      END
