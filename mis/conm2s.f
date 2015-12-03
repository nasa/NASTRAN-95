      SUBROUTINE CONM2S
C
C THIS SUBROUTINE COMPUTES THE CONCENTRATED MASS ELEMENTS MASS MATRIX
C FOR THE M2 TYPE ELEMENT
C SINGLE PRECISION VERSION
C
C ECPTNO  NAME       TYPE  DESCRIPTION
C ******  ****       ****  ***********
C
C   1     IELID      I     ELEMENT ID
C   2     IGP        I     GRID POINT NUMBER
C   3     ICIDT2     I     COORDINATE SYSTEM ID FOR T2
C   4     MASS       R     LUMPED MASS
C   5     OFFSET(1)  R
C   6     OFFSET(2)  R     X,Y, AND Z COORDINATES OF THE
C   7     OFFSET(3)  R     OFFSET
C   8     MMI(1,1)   R
C   9     MMI(2,1)   R     MASS MOMENTS OF INERTIA
C  10     MMI(2,2)   R
C  11     MMI(3,1)   R
C  12     MMI(3,2)   R
C  13     MMI(3,3)   R
C  14     ICIDT1     I     COORDINATE SYSTEM ID FOR T1
C  15     X          R
C  16     Y          R
C  17     Z          R
C
      INTEGER DICT(11), ELID, ESTID, IECPT(14)
      REAL MM(36),TT(36),T(36)
      REAL MB, INER(6)
C
C
      COMMON /EMGEST/ ECPT(100)
C
      COMMON /EMGDIC/ DMM(2),NLOCS,ELID,ESTID
C
      COMMON /EMGPRM/ DUM(15),ISMB(3),IPREC,NOGO
C
      EQUIVALENCE (ECPT(1),IECPT(1),IELID)
      EQUIVALENCE (DICT(5),DICT5), (ECPT(4),MB)
      EQUIVALENCE (ECPT(5),XOF),(ECPT(6),YOF),(ECPT(7),ZOF)
      EQUIVALENCE (INER(1),ECPT(8))
C
C     INITIALIZE
C
      IF (ISMB(2) .EQ. 0) RETURN
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 6
      DICT(4) = 63
      DICT(5) = 0
      IP = IPREC
C
C COMPUTE NON-TRANSFORMED MASS MATRIX.  INITIALIZE TO ZERO
C THEN FILL IN NON-ZERO TERMS
C
      DO 100 I=1,36
  100 MM(I) = 0.
C
      ICIDT2 = IECPT(3)
      IF (ICIDT2 .GE. 0) GO TO 120
      ICIDT2 = 0
      DO 110 I = 1,3
  110 ECPT (I+4) = ECPT(I+4) - ECPT(I+14)
C
  120 MM(1) = MB
      MM(5) = MB*ZOF
      MM(6) = -MB*YOF
      MM(8) = MB
      MM(10) = -MM(5)
      MM(12) = MB*XOF
      MM(15) = MB
      MM(16) = -MM(6)
      MM(17) = -MM(12)
      MM(20) = MM(10)
      MM(21) = MM(16)
      X2 = XOF**2
      Y2 = YOF**2
      Z2 = ZOF**2
      MM(22) = INER(1) + (Y2 + Z2)*MB
      MM(23) = -INER(2) + MM(6)*XOF
      MM(24) =  -INER(4)+MM(10)*XOF
      MM(25) = MM(5)
      MM(27) = MM(17)
      MM(28) = MM(23)
      MM(29) = INER(3) + (X2 + Z2)*MB
      MM(30) = -INER(5) + MM(6)*ZOF
      MM(31) = MM(6)
      MM(32) = MM(12)
      MM(34) = MM(24)
      MM(35) = MM(30)
      MM(36) = INER(6) + (X2 + Y2)*MB
C
      ICIDT1 = IECPT(14)
C
C PERFORM TRANSFORMATIONS.  IF CSIDS 1 AND 2 ARE EQUAL,
C T1 = T2 SO MASS MATRIX IS COMPLETE
C
      IF (ICIDT2 .EQ. ICIDT1) GO TO 240
C                            T
C NOT EQUAL SO COMPUTE T = (T )(T )
C                            1   2
C GET T1 AND T2 IF NEEDED
      IT = 18
      IF (ICIDT1 .EQ. 0) GO TO 130
C
      CALL TRANSS (ECPT(14),T(1))
      GO TO 140
C ONLY T2 NEEDED SO T = T2
  130 IT = 9
  140 IF (ICIDT2 .EQ. 0) GO TO 150
      ITEMP = IECPT(14)
      IECPT(14) = ICIDT2
      CALL TRANSS (ECPT(14),T(10))
      IECPT(14) = ITEMP
C
      IF(ICIDT1 .EQ. 0) GO TO 210
      CALL GMMATS (T(1),3,3,2,T(10),3,3,0,T(19))
      GO TO 210
C
C HERE T2 IS IDENTITY AND T1 IS AT T(1) SO
C T = T1 (TRANSPOSE).  SO INSERT INTO T
  150 DO 170 I = 1,3
      DO 170 J = 1,3
      IJ = 3*(I-1) + J
      JI = I + 3*(J-1) + 18
170   T(JI) = T(IJ)
C
C T = (T ) (T ) IS COMPLETE. INSERT IT IN THE 6X6 TRANSFORMATION MATRIX.
C       1    2
C
  210 DO 220 I = 1,36
  220 TT(I) = 0.
C
      DO 230 I = 1,3
      IJ = I + IT
      TT(I) = T(IJ)
      TT(I + 6) = T(IJ + 3)
      TT(I + 12) = T(IJ + 6)
      TT(I + 21) = T(IJ)
      TT(I + 27) = T(IJ + 3)
  230 TT(I + 33) = T(IJ + 6)
C           T
C FORM T*M*T  AND STORE IN MM
C
      CALL GMMATS (TT(1),6,6,0,MM(1),6,6,0,T(1))
      CALL GMMATS (T(1),6,6,0,TT(1),6,6,1,MM(1))
C
  240 CALL EMGOUT (MM,MM,36,1,DICT,2,IP)
      RETURN
      END
