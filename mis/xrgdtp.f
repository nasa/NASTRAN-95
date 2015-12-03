      SUBROUTINE XRGDTP
C****
C    PURPOSE - XRGDTP DETERMINES A TYPE CODE FOR A CHARACTER
C
C    AUTHOR  - RPK CORPORATION; DECEMBER, 1983
C
C    INPUT
C      /XRGDXX/
C        ICHAR       AN ARRAY IN 80A1 FORMAT
C        ICOL        CURRENT ELEMENT IN THE ARRAY ICHAR
C
C    OUTPUT
C      /XRGDXX/
C        ITYPE       TYPE CODE ASSOCIATED WITH THE CHARACTER
C                    =1, IF CHARACTER IS A NUMBER
C                    =2, IF CHARACTER IS A ','
C                    =3, IF CHARACTER IS A '-'
C                    =4, IF CHARACTER IS A BLANK
C                    =5, OTHERWISE
C        NUMBER      INTEGER VALUE FOR CHARACTER OF ITYPE=1
C
C    LOCAL VARIABLES
C      DELIM         3 WORD ARRAY CONTAINING A COMMA, DASH, AND BLANK
C      NUMS          10 WORD ARRAY OF ALPHA NUMBERS 1,2..0
C      K             K DO LOOP INDEX TO SEARCH DELIM ARRAY
C
C   SUBROUTINES CALLED - NONE
C
C   CALLING SUBROUTINES - XRGDEV
C
C    FUNCTIONS - XRGDTP EXAMINES THE CHARACTER IN ICHAR(ICOL)
C                TO DETERMINE ITS TYPE CODE.
C
C    ERRORS - NONE
C
C****
      INTEGER           RECORD
      INTEGER NUMS( 10 ), DELIM( 3 )
      COMMON / XRGDXX / IRESTR, NSUBST, IPHASE, ICOL  , NUMBER, ITYPE
     *,                 ISTATE, IERROR, NUM(2), IND   , NUMENT
     *,                 RECORD(20)    , ICHAR(80)     , LIMIT(2)
     *,                 ICOUNT, IDMAP , ISCR  , NAME(2), MEMBER(2)
     *,                 IGNORE
      DATA NUMS / 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7, 1H8, 1H9, 1H0 /
      DATA DELIM/ 1H,, 1H-, 1H                                     /
C
      DO 10 K = 1,3
      IF ( ICHAR( ICOL ) .NE. DELIM( K ) ) GO TO 10
      ITYPE = K + 1
      GO TO 30
 10   CONTINUE
      DO 20 K = 1, 10
      IF ( ICHAR( ICOL ) .NE. NUMS( K ) ) GO TO 20
      ITYPE = 1
      NUMBER = MOD( K,10 )
      GO TO 30
 20   CONTINUE
      ITYPE = 5
 30   RETURN
      END
