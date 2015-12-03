      SUBROUTINE TRBSCS
C
C     THIS SUBROUTINE CALCULATES THE STIFFNESS AND MASS MATRICES FOR
C     THE BASIC BENDING TRIANGLE.  THE MASS MATRIX MAY BE CALCULATED
C     EITHER BY THE CONVENTIONAL OR THE CONSISTENT MASS METHODS (USING
C     EMASTQ OR INCLUDED CODE) ACCORDING TO THE PARAMETER ICMBAR.
C     THIS ELEMENT MAY NOT BE USED IN A HEAT PROBLEM.
C
C     SINGLE PRECISION VERSION
C
C     ECPT FOR THIS ELEMENT
C
C     INDEX  NAME      TYPE      DESCRIPTION
C     ----- ---------  ----   --------------------
C      1    IELID        I     ELEMENT ID
C      2    NGRID(1)     I     FIRST GRID POINT
C      3    NGRID(2)     I     SECOND GRID POINT
C      4    NGRID(3)     I     THIRD GRID POINT
C      5    ANGLE        R     ANGLE OF MATERIAL
C      6    MATID1       I     MATERIAL ID 1
C      7    EYE          R     MOMENT OF INERTIA
C      8    MATID2       I     MATERIAL ID 2
C      9    T2           R     T2
C     10    FMU          R     NON-STRUCTURAL MASS
C     11    Z11          R     Z1
C     12    Z22          R     Z2
C     13    NECPT(13)    I     COORD  SYSTEM ID 1
C     14    X1           R
C     15    Y1           R     COORDINATES
C     16    Z1           R
C     17    NECPT(17)    I     COORD SYSTEM ID 2
C     18    X2           R
C     19    Y2           R     COORDINATES
C     20    Z2           R
C     21    NECPT(21)    I     COORD SYSTEM ID 3
C     22    X3           R
C     23    Y3           R     COORDINATES
C     24    Z3           R
C     25    ELTEMP       R     ELEMENT TEMPERATURE
C
      LOGICAL         IHEAT,NOGO
      INTEGER         ELID,ESTID,DICT(9),IPART(3),NECPT(25)
      REAL            ECPT(25),KK,KOUT,M(324),MOUT(324)
      COMMON /EMGPRM/ IXTR,JCORE,NCORE,DM(12),ISMB(3),IPREC,NOGO,HEAT,
     1                ICMBAR
      COMMON /EMGDIC/ QQ,LDICT,NGRIDS,ELID,ESTID
      COMMON /EMGEST/ IELID,NGRID(3)
      COMMON /EMGTRX/ A(225),PROD(9),TEMP9(9),XSUBB,SXUBC,YSUBC,BFACT,
     1                E(18),KOUT(324),KK(324),KSAV(81)
      COMMON /SYSTEM/ KSYSTM(60)
      EQUIVALENCE     (KSYSTM(2),IOUTPT),(KSYSTM(56),IHEAT),
     1                (ECPT(1),NECPT(1),IELID),(DICT5,DICT(5)),
     2                (KK(1),MOUT(1)),(KOUT(1),M(1))
      DATA    IPART / 1,2,3/
C
      IP = IPREC
C
C     IF THIS IS A HEAT PROBLEM THIS SHOULD NOT CALL US, SO RETURN
C
      IF (IHEAT) RETURN
C
C     CREATE AN ARRAY POINTING TO THE GRID POINTS IN INCREASING  SIL
C     ORDER
C
  100 DO 120 I = 1,2
      IP1 = I + 1
      II  = IPART(I)
      DO 110 J = IP1,3
      JJ  = IPART(J)
      IF (NGRID(II) .LE. NGRID(JJ)) GO TO 110
      IPART(I) = JJ
      IPART(J) = II
      II = JJ
      GO TO 100
  110 CONTINUE
  120 CONTINUE
C
C     IF STIFFNESS MATRIX IS DESIRED CALL ETRBKS, OTHERWISE ONLY MASS
C     MATRIX IS DESIRED
C
      IF (ISMB(1) .EQ. 0) GO TO 200
C
      CALL ETRBKS (0)
      IF (NOGO) RETURN
      DICT5 = BFACT
C
C     RE ORDER THE MATRIX BY INCREASING SIL VALUE.    NOTE THAT
C
C     KK  = KK(1 TO  9)     KK   = KK(10 TO 18)     KK   = KK(19 TO  27)
C       AA                    AB                      AC
C
C     KK  = KK(28 TO  36)  KK   = KK(37 TO  45)   KK   =  KK(46 TO  54)
C       BA                   BB                     BC
C
C     KK  = KK(55 TO  63)  KK   = KK(64 TO  72)   KK  =  KK(73 TO  81)
C       CA                   CB                     CC
C
C     AND
C
C     KOUT  = KOUT(1 - 36) KOUT  = KOUT( 4 - 6)   KOUT  = KOUT( 7 -  9)
C         I I    (10 - 12)     I I     (13 - 15)      I I     (16 - 18)
C          1 1   (19 - 21)      1 2    (22- 24)        1 3    (25 - 27)
C
C     ETC
C
C
      DO 170 I = 1,3
      II = IPART(I)
      DO 160 J = 1,3
      JJ = IPART(J)
      DO 150 K = 1,3
      DO 140 L = 1,3
      IK   = (II-1)*27 + (JJ-1)*9 + (K-1)*3 + L
      IOUT = (I -1)*27 + (J -1)*3 + (K-1)*9 + L
  140 KOUT(IOUT) = KK(IK)
  150 CONTINUE
  160 CONTINUE
  170 CONTINUE
C
C     NOW OUTPUT THE MATRIX
C
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 9
      DICT(4) = 4 + 8 + 16
C
      CALL EMGOUT (KOUT,KOUT,81,1,DICT,1,IP)
C
C     NOW CALCULATE THE MASS MATRIX IF NEEDED
C
  200 IF (ISMB(2) .EQ. 0) RETURN
C
C     WHICH MASS METHOD TO BE USED (CONVENTIONAL OR CONSISTENT)
C
      IF (ICMBAR .GE. 0) GO TO 300
C
      CALL EMASTQ (3,M)
C
C     REORDER THE DIAGONAL MASS MATRIX
C
      DO 240 I = 1,3
      II = (I-1)*3 + 1
      IJ = IPART(I)
      JJ = (IJ-1)*3 + 1
      DO 220 J = 1,3
      IOUT = II + J - 1
      IK   = JJ + J - 1
  220 MOUT(IOUT) = M(IK)
  240 CONTINUE
C
C     NOW OUTPUT THE MATRIX
C
      DICT(1) = ESTID
      DICT(2) = 2
      DICT(3) = 9
      DICT(4) = 7
C
      CALL EMGOUT (MOUT,MOUT,9,1,DICT,2,IP)
C
      RETURN
C
C     THE COUPLED MASS MATRIX CALCULATIONS ARE MADE HERE VIA ETRBMS
C
300   CALL ETRBMS
      IF (NOGO) RETURN
C
C     INSERT THE MATRICES INTO THE OUTPUT MATRIX IN INCREASING SIL ORDER
C
      DO 550 I = 1,3
      II = IPART(I)
      DO 550 J = 1,3
      JJ = IPART(J)
      DO 550 K = 1,3
      DO 550 L = 1,3
      IA   = (II-1)*36 + (JJ-II)*9 + (K-1)*3 + L
      IOUT = (I -1)*27 + (J - 1)*3 + (K-1)*9 + L
550   MOUT(IOUT) = M(IA)
C
C     NOW OUTPUT THE MASS MATRIX
C
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 9
      DICT(4) = 4 + 8 + 16
C
      CALL EMGOUT (MOUT,MOUT,81,1,DICT,2,IP)
      RETURN
C
      END
