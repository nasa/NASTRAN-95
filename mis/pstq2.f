      SUBROUTINE PSTQ2 (NPTS)
C  THIS ROUTINE CALCULATES PHASE II OUTPUT FOR PLA3
C  FOR COMBINATION ELEMENTS
C
C     ****PHASE II OF STRESS DATA RECOVERY*********
C
C     NPTS = 3 IMPLIES STRIA1 OR STRIA2  (PHASE II)
C     NPTS = 4 IMPLIES SQUAD1 OR SQUAD2  (PHASE II)
C
      DIMENSION  NSIL(4), NPH1OU(2), SI(36)
C
      COMMON /PLA3UV/ IVEC,Z(24)
      COMMON /PLA3ES/ PH1OUT(200),FORVEC(6),DUMMY(94)
      COMMON /PLA32S/ STRESS(3),TEMP,DELTA,NPOINT,I,J,NPT1,VEC(5),TEM,
     1                Z1OVRI, Z2OVRI,DUM1(308)
      COMMON /SOUT/ STR(18)
      EQUIVALENCE
     1  (NSIL(1),PH1OUT(2))
     2 ,(NPH1OU(1),PH1OUT(1))
     3 ,(SI(1),PH1OUT(9))
C
C **********************************************************************
C **********************************************************************
C
C     PHASE I OUTPUT FROM THE PLATE IS THE FOLLWOING
C
C     PH1OUT(1)                        ELEMENT ID
C     PH1OUT(2 THRU 5)                 3 SILS AND DUMMY OR 4 SILS
C     PH1OUT(6)                        I
C     PH1OUT(7 THRU 8)                 Z1 AND Z2
C     PH1OUT(9 THRU 30*NPTS+8)         3 OR 4 S SUB I  5X6 ARRAYS
C
C **********************************************************************
C
C     PHASE I OUTPUT FROM THE MEMBRANE IS THE FOLLOWING
C     NOTE..BEGIN = 30*NPTS+8
C
C     PH1OUT(BEGIN + 1)                ELEMENT ID
C     PH1OUT(BEGIN + 2 THRU BEGIN + 5) 3 SILS AND DUMMY OR 4 SILS
C     PH1OUT(BEGIN + 6)                T SUB 0
C     PH1OUT(BEGIN + 7 THRU BEGIN + 9) S SUB T  3X1 ARRAY
C     PH1OUT(BEGIN + 10 THRU BEGIN + 9*NPTS+9) 3 OR 4 S SUB I 3X3 ARRAYS
C
C **********************************************************************
C **********************************************************************
C
C     THE ABOVE ELEMENTS ARE COMPOSED OF PLATES AND MEMBRANES...
C     SOME MAY ONLY CONTAIN PLATES WHILE OTHERS MAY ONLY CONTAIN
C     MEMBRANES.
C     A CHECK FOR A ZERO FIRST SIL IN THE PHASE I OUTPUT, WHICH
C     INDICATES WHETHER ONE OR THE OTHER HAS BEEN OMITTED, IS MADE BELOW
C
C
C
C     FIRST GET FORCE VECTOR FOR THE PLATE CONSIDERATION
C
C     M ,  M ,  M  ,  V ,  V
C      X    Y    XY    X    Y
C
C                                NPTS
C     THE  5X1 FORCE VECTOR = SUMMATION  (S )(U )
C                                I=1       I   I
C
C
C     ZERO OUT LOCAL STRESSES
C
      SIG X  1 = 0.0E0
      SIG Y  1 = 0.0E0
      SIG XY 1 = 0.0E0
      SIG X  2 = 0.0E0
      SIG Y  2 = 0.0E0
      SIG XY 2 = 0.0E0
C
      IF( NSIL(1) .EQ. 0 ) GO TO 30
C
C     FORM SUMMATION
C
      DO 20 I=1,NPTS
C
C     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
C
      NPOINT = IVEC + NSIL(I) - 1
C
      CALL GMMATS( SI(30*I-29),5,6,0,  Z(NPOINT),6,1,0,  VEC(1)  )
C
      DO 10 J=2,6
   10 FORVEC(J) = FORVEC(J) + VEC(J-1)
C
   20 CONTINUE
C
C     FORCE VECTOR IS NOW COMPLETE
C
      Z1 = PH1OUT(7)
      Z2 = PH1OUT(8)
C
      Z1 OVR I = - PH1OUT(7) / PH1OUT(6)
      Z2 OVR I = - PH1OUT(8) / PH1OUT(6)
C
      SIG X  1 = FORVEC(2) * Z1 OVR I
      SIG Y  1 = FORVEC(3) * Z1 OVR I
      SIG XY 1 = FORVEC(4) * Z1 OVR I
      SIG X  2 = FORVEC(2) * Z2 OVR I
      SIG Y  2 = FORVEC(3) * Z2 OVR I
      SIG XY 2 = FORVEC(4) * Z2 OVR I
C     *******************************
C
      GO TO 40
   30 Z1 = 0.0E0
      Z2 = 0.0E0
C
C     FIND SIG X, SIG Y, SIG XY, FOR MEMBRANE CONSIDERATION
   40 IF( NPH1OU(30*NPTS+10) .EQ. 0 ) GO TO 90
C
C
C                        I=NPTS
C     STRESS VECTOR = ( SUMMATION(S )(U ) )
C                        I=1       I   I
C
      DO 60 I=1,NPTS
C
C     POINTER TO I-TH SIL IN PH1OUT
      NPOINT = 30*NPTS + 9 + I
C     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
      NPOINT = IVEC + NPH1OU (NPOINT) - 1
C
C     POINTER TO S SUB I 3X3
      NPT1 = 30 * NPTS + 9 + 9 * I
C
      CALL GMMATS ( PH1OUT(NPT1),3,3,0,  Z(NPOINT),3,1,0,  VEC(1)  )
C
      DO 50 J=1,3
   50 STRESS(J) = STRESS(J) + VEC(J)
C
   60 CONTINUE
C
C
C     ADD MEMBRANE STRESSES TO PLATE STRESSES
C
      SIG X  1 = SIG X  1 + STRESS(1)
      SIG Y  1 = SIG Y  1 + STRESS(2)
      SIG XY 1 = SIG XY 1 + STRESS(3)
      SIG X  2 = SIG X  2 + STRESS(1)
      SIG Y  2 = SIG Y  2 + STRESS(2)
      SIG XY 2 = SIG XY 2 + STRESS(3)
C
C     STRESS OUTPUT VECTOR IS THE FOLLOWING
C
C      1) ELEMENT ID
C      2) Z1 = FIBER DISTANCE 1
C      3) SIG X  1
C      4) SIG Y  1
C      5) SIG XY 1
C      6) ANGLE OF ZERO SHEAR AT Z1
C      7) SIG P1 AT Z1
C      8) SIG P2 AT Z1
C      9) TAU MAX = MAXIMUM SHEAR STRESS AT Z1
C
C     10) ELEMENT ID
C     11) Z2 = FIBER DISTANCE 2
C     12) SIG X  2
C     13) SIG Y  2
C     14) SIG XY 2
C     15) ANGLE OF ZERO SHEAR AT Z2
C     16) SIG P1 AT Z2
C     17) SIG P2 AT Z2
C     S7) SIG P2 AT Z2
C     18) TAU MAX = MAXIMUM SHEAR STRESS AT Z2
C
C
   90 IF( NPH1OU(2) .EQ. 0 .AND. NPH1OU(30*NPTS+10) .EQ. 0 ) GO TO 120
C
C     COMPUTE PRINCIPAL STRESSES
C
      STR( 1) = PH1OUT(1)
      STR( 2) = Z1
      STR( 3) = SIG X  1
      STR( 4) = SIG Y  1
      STR( 5) = SIG XY 1
      STR(10) = PH1OUT(1)
      STR(11) = Z2
      STR(12) = SIG X  2
      STR(13) = SIG Y  2
      STR(14) = SIG XY 2
C
      DO 110 I=3,12,9
      TEMP = STR(I) - STR(I+1)
      STR(I+6) = SQRT( (TEMP/2.0E0)**2 + STR(I+2)**2 )
      DELTA = (  STR(I)  +  STR(I+1)  )  /  2.0E0
      STR(I+4) = DELTA + STR(I+6)
      STR(I+5) = DELTA - STR(I+6)
      DELTA = 2.0E0 * STR(I+2)
      IF( ABS(DELTA) .LT. 1.0E-15 .AND. ABS(TEMP) .LT. 1.0E-15)GO TO 100
      STR(I+3) = ATAN2( DELTA,TEMP ) * 28.6478898E0
      GO TO 110
  100 STR(I+3) = 0.0E0
  110 CONTINUE
C
      GO TO 140
  120 DO 130 I=2,18
  130 STR(I) = 0.0E0
  140 STR(1) = PH1OUT(1)
      STR(10) = PH1OUT(1)
C
C
C     ADDITION TO ELIMINATE 2ND ELEMENT ID IN OUTPUT
C
      DO 150 I=10,17
  150 STR(I) = STR(I+1)
C
      RETURN
      END
