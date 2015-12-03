      SUBROUTINE GI
C
      EXTERNAL        ANDF
      LOGICAL         MULTI,SINGLE,OMIT
      INTEGER         ANDF,TWO1,IA(7)
      INTEGER         UM,UO,UR,USG,USB,UL,UA,UF,US,UN,UG
      INTEGER         SPLINE,USETA,CSTM,BAGPDT,SILA,ECTA,GM,GO,SCR1,
     1                SCR2,SCR3,SCR4,SCR5,KSIZE,GSIZE,GTKA
      COMMON /GICOM / SPLINE,USETA,CSTM,BAGPDT,SILA,ECTA,GM,GO,GTKA,
     1                KSIZE,GSIZE,SCR1,SCR2,SCR3,SCR4,SCR5
      COMMON /BITPOS/ UM,UO,UR,USG,USB,UL,UA,UF,US,UN,UG
      COMMON /TWO   / TWO1(32)
      COMMON /BLANK / NK,NG
      DATA    SINGLE/ .TRUE./, MULTI /.TRUE./, OMIT /.TRUE./
      DATA       IA / 7*0  /
C
      SPLINE = 101
      USETA  = 102
      CSTM   = 103
      BAGPDT = 104
      SILA   = 105
      ECTA   = 106
      GM     = 107
      GO     = 108
      GTKA   = 201
      KSIZE  = NK
      GSIZE  = NG
      IF (GSIZE .GT. 0) GO TO 5
      IA(1)  = SILA
      CALL RDTRL (IA)
      GSIZE  = IA(3)
    5 CONTINUE
      SCR1   = 301
      SCR2   = 302
      SCR3   = 303
      SCR4   = 304
      SCR5   = 305
      CALL GIGGKS
      IA(1)  = USETA
      CALL RDTRL (IA)
      IF (ANDF(IA(5),TWO1(UM)) .EQ. 0) MULTI  = .FALSE.
      IF (ANDF(IA(5),TWO1(US)) .EQ. 0) SINGLE = .FALSE.
      IF (ANDF(IA(5),TWO1(UO)) .EQ. 0) OMIT   = .FALSE.
      IF (MULTI .OR. SINGLE .OR. OMIT) GO TO 10
      SCR2 = GTKA
   10 CALL GIGTKG
      CALL GIPSST
      IF (MULTI .OR. SINGLE .OR. OMIT) GO TO 20
      GO TO 30
   20 CALL GIGTKA (MULTI,SINGLE,OMIT)
   30 RETURN
      END
