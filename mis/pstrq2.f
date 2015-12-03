      SUBROUTINE PSTRQ2 (NTYPE)
C THIS ROUTINE CALCULATES PHASE II OUTPUT FOR PLA3
C
C     NTYPE = 1 TRI-MEMBRANE
C     NTYPE = 2 QUAD-MEMBRANE
C
C     PH1OUT CONTAINS THE FOLLOWING
C     *** NTYPE = 1 ***
C     ELEMENT ID
C     3 SILS
C     5 DUMMY-S
C     3 S ARRAYS EACH 3X3
C
C     *** NTYPE = 2 ***
C     ELEMENT ID
C     4 SILS
C     4 DUMMY-S
C     4 S ARRAYS EACH 3X3
C
      DIMENSION NSIL(4), SI(36)
C
      COMMON /PLA3UV/ IVEC, Z(24)
      COMMON /PLA3ES/ PH1OUT(300)
      COMMON /PLA32S/ STRESS(3),VEC(3),TEMP,DELTA,NSIZE,NPOINT,
     1   DUM(315)
      COMMON /SOUT/  STRES(9)
C
      EQUIVALENCE
     1            (NSIL(1),PH1OUT(2))
     2,           (SI(1),PH1OUT(10))
C
C
C                        I=NSIZE
C     STRESS VECTOR = (SUMMATION  (S ) (U ))
C                        I=1        I    I
C
      NSIZE = NTYPE + 2
      DO 20 I = 1,NSIZE
C     POINTER TO DISPLACEMENT VECTOR
      NPOINT = IVEC + NSIL(I) -1
C
      CALL GMMATS( SI(9*I-8),3,3,0,  Z(NPOINT),3,1,0, VEC(1))
C
      DO 30 J=1,3
   30 STRESS(J) = STRESS(J) + VEC(J)
   20 CONTINUE
C
      STRES(1) = PH1OUT(1)
      STRES(2) = STRESS(1)
      STRES(3) = STRESS(2)
      STRES(4) = STRESS(3)
C
C     ******************************************************************
C
C     PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
      TEMP = STRES(2) - STRES(3)
      STRES(8) = SQRT( (TEMP/2.0E0)**2 + STRES(4)**2 )
      DELTA = (STRES(2) + STRES(3))/2.0E0
      STRES(6) = DELTA + STRES(8)
      STRES(7) = DELTA - STRES(8)
      DELTA = 2.0E0 * STRES(4)
      IF( ABS(DELTA) .LT. 1.0E-15 .AND. ABS(TEMP) .LT. 1.0E-15)GO TO 101
      STRES(5) = ATAN2( DELTA,TEMP ) * 28.6478898 E00
      RETURN
  101 STRES(5) = 0.0E0
      RETURN
      END
