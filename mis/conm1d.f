      SUBROUTINE CONM1D
C
C THIS SUBROUTINE COMPUTES THE CONCENTRATED MASS ELEMENTS
C MASS MATRIX FOR THE M1 TYPE ELEMENT
C  DOUBLE PRECISION VERSION
C
C
C ECPT NO.  NAME              TYPE   DESCRIPTION
C 1         IELID             I      ELEMENT ID
C 2         IGP               I      GRID POINT NUMBER
C 3         ICIDT2            I      COORDINATE ID FOR T2
C 4         M(1,1)            R
C 5, 6      M(2,1) TO M(2,2)  R
C 7, 8, 9   M(3,1) TO M(3,3)  R      MASS MATRIX VALUES
C 10 TO 13  M(4,1) TO M(4,4)  R
C 14 TO 18  M(5,1) TO M(5,5)  R
C 19 TO 24  M(6,1) TO M(6,6)  R
C 25        ICIDT1            I      COORDINATE ID FOR T1
C 26        X                 R
C 27        Y                 R      TRANSFORMATION MATRIX
C 28        Z                 R
C
      INTEGER DICT(7),ELID,ESTID,IECPT(25)
      DOUBLE PRECISION MM(36),TT(36),T(36),M(21)
      LOGICAL NOGO
C
C
      COMMON /SYSTEM/ SS,IOUTPT,KSYSTM(56)
C
      COMMON /EMGPRM/ DUM(15),ISMB(3),IPREC,NOGO
      COMMON /EMGEST/ ECPT(100)
      COMMON /EMGDIC/ DMM(2),NLOCS,ELID,ESTID
C
      EQUIVALENCE (ECPT(1),IECPT(1),IELID)
      EQUIVALENCE (DICT(5),DICT5)
C
C     INITIALIZE
C
      IF (ISMB(2) .EQ. 0) RETURN
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 6
      DICT(4) = 63
      DICT5 = 0
      IP = IPREC
      XOF = ECPT(5)
      YOF = ECPT(6)
      ZOF = ECPT(7)
      DO 50 I=1,21
   50 M(I) = ECPT(I+3)
C
C COMPUTE NON-TRANSFORMED MASS MATRIX. INITIALIZE
C TO ZERO THEN FILL IN NON-ZERO TERMS
C
      DO  100 I = 1,36
  100 MM(I)=  0.D0
C
      K = 0
      DO 110 I = 1,6
      DO 110 J = 1,I
      K = K + 1
      JI = (J-1)*6 + I
      IJ = (I-1)*6 + J
      MM(IJ) = M(K)
  110 MM(JI) = M(K)
C
      ICIDT1 = IECPT(25)
      ICIDT2 = IECPT(3)
C
C PERFORM TRANSFORMATIONS.  IF CSIDS 1 AND 2 ARE EQUAL,
C T1 = T2 SO MASS MARRIX IS COMPLETE
C
      IF (ICIDT2 .EQ. ICIDT1) GO TO 240
C                             T
C NOT EQUAL. SO COMPUTE T = (T ) (T )
C                             1    2
C GET T1 AND T2 IF NEEDED
      IT = 18
      IF (ICIDT1 .EQ. 0) GO TO 130
C
      CALL TRANSD(ECPT(25),T(1))
      GO TO 140
C
C ONLY T2 NEEDED SO T = T2
C
  130 IT = 9
  140 IF(ICIDT2 .EQ. 0) GO TO 150
      CALL TRANSD(ECPT(25),T(10))
C
      IF(ICIDT1 .EQ. 0) GO TO 210
      CALL GMMATD (T(1),3,3,1,  T(10),3,3,0, T(19))
      GO TO 210
C
C HERE T2 IS IDENTITY AND T1 IS AT T(1) SO
C T = T1 (TRANSPOSE).  SO INSERT INTO T
  150 DO 170 I = 1,3
      DO 170 J = 1,3
      IJ = 3*(I - 1) + J
      JI = I + 3*(J-1) + 18
  170 T(JI)=T(IJ)
C
C T = (T ) (T ) IS COMPLETE. INSERT IT IN THE 6X6 TRANSFORMATION MATRIX.
C       1    2
C
  210 DO 220 I = 1,36
  220 TT(I)=0.D0
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
      CALL GMMATD (TT(1),6,6,0,  MM(1),6,6,0, T(1))
      CALL GMMATD(T(1),6,6,0, TT(1),6,6,1, MM(1))
C
  240 CALL EMGOUT (MM,MM,36,1,DICT,2,IP)
      RETURN
      END
