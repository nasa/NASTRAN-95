      SUBROUTINE STQME2( NTYPE )
C
C     PHASE TWO STRESS DATA RECOVERY TRIANGULAR MEMBRANE
C
C     NTYPE = 1 TRI-MEMBRANE
C     NTYPE = 2 QUAD-MEMBRANE
C
C     PH1OUT CONTAINS THE FOLLOWING
C     *** NTYPE = 1 ***
C     ELEMENT ID
C     3 SILS
C     1 DUMMY
C     T SUB 0
C     S SUB T 3X1
C     3 S ARRAYS EACH 3X3
C
C     *** NTYPE = 2 ***
C     ELEMENT ID
C     4 SILS
C     T SUB 0
C     S SUB T 3X1
C     4 S ARRAYS EACH 3X3
C
      REAL    FRLAST(2)
      INTEGER EJECT    ,ISHD(7)  ,ISTYP(2) ,TYP(3)
      DIMENSION NSIL(4), SI(36), PH1OUT(45), ST(3)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SDR2X4/ DUMMY(35),IVEC,IVECN,LDTEMP,DEFORM
      COMMON /SDR2X7/ EST(100),STRES(100),FORVEC(25)
      COMMON /SDR2X8/ STRESS(3),VEC(3),TEM,TEMP,NPOINT,DELTA,NSIZE,
     1             CVEC(3),CSTR(4)
      COMMON /SDR2X9/ NCHK,ISUB,ILD,FRTMEI(2),TWOTOP,FNCHK
      COMMON   /SYSTEM/  IBFSZ    ,NOUT     ,IDM(9)   ,LINE
C
      EQUIVALENCE
     1  (PH1OUT(1),EST(1))
     2 ,(NSIL(1),PH1OUT(2))
     3 ,(TSUB0,PH1OUT(6))
     4 ,(ST(1),PH1OUT(7))
     5 ,(SI(1),PH1OUT(10))
     6 ,(FTEMP,LDTEMP)
     7 , (ISHD(1),LSUB) , (ISHD(2),LLD) , (ISHD(6),FRLAST(1) )
C
      DATA LSUB,LLD,FRLAST / 2*-1 , -1.0E30, -1.0E30 /
      DATA TYP / 2HTR, 2HQD, 3HMEM /
C     ******************************************************************
C     ZERO OUT THE STRESS VECTOR
      DO 5 I = 1,3
      STRESS(I) = 0.0E0
    5 CSTR(I+1) = 0.0E0
C
C                          I=NSIZE
C        STRESS VECTOR =(SUMMATION (S )(U )) - (S )(LDTEMP - T SUB 0)
C                          I=1       I   I       T
      NSIZE = NTYPE + 2
      DO 20 I = 1,NSIZE
C
C     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
C
      NPOINT = IVEC + NSIL(I) - 1
C
      CALL SMMATS (SI(9*I-8),3,3,0, Z(NPOINT),3,1,0, VEC,CVEC)
      DO 30 J = 1,3
      CSTR(J+1) = CSTR(J+1) + CVEC(J)
   30 STRESS(J) = STRESS(J) + VEC(J)
C
   20 CONTINUE
C
      STRES(1) = PH1OUT(1)
      STRES(2) = STRESS(1)
      STRES(3) = STRESS(2)
      STRES(4) = STRESS(3)
C
C     ADD IN TEMPERATURE EFFECTS
C
      IF( LDTEMP .EQ. (-1) ) GO TO 200
      TEM = FTEMP - T SUB 0
      DO 90 I = 2,4
   90 STRES(I) = STRES(I) - ST(I-1) * TEM
C     STRESS VECTOR COMPLETE AND CONTAINS SIGMA X ,  SIGMA Y ,  SIGMA XY
C
C     ******************************************************************
C
C     PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
  200 TEMP = STRES(2) - STRES(3)
      STRES(8) = SQRT( (TEMP/2.0E0)**2 + STRES(4)**2 )
      DELTA = (STRES(2) + STRES(3))/2.0E0
      STRES(6) = DELTA + STRES(8)
      STRES(7) = DELTA - STRES(8)
      DELTA = 2.0E0 * STRES(4)
      IF( ABS(DELTA) .LT. 1.0E-15 .AND. ABS(TEMP) .LT. 1.0E-15)GO TO 101
      STRES(5) = ATAN2( DELTA,TEMP ) * 28.6478898 E00
      RETURN
  101 STRES(5) = 0.0E0
      IF (NCHK.LE.0) GO TO 250
C
C  . CHECK PRECISION...
C
      CSTR(1) = PH1OUT(1)
      K = 0
C
      CALL SDRCHK (STRES(2),CSTR(2),3,K)
      IF (K.EQ.0) GO TO 250
C
C  . LIMITS EXCEEDED...
      ISTYP(1) = TYP(NTYPE)
      ISTYP(2) = TYP(3)
      J = 0
      IF (LSUB.EQ.ISUB .AND. FRLAST(1).EQ.FRTMEI(1) .AND.
     1    LLD .EQ.ILD  .AND. FRLAST(2).EQ.FRTMEI(2) ) GO TO 220
      LSUB = ISUB
      LLD = ILD
      FRLAST(1) = FRTMEI(1)
      FRLAST(2) = FRTMEI(2)
      J = 2
      CALL PAGE1
  205 CALL SD2RHD (ISHD,J)
      LINE = LINE + 1
      WRITE(NOUT,210)
  210 FORMAT (7X,4HTYPE,5X,3HEID,5X,2HSX,5X,2HSY,4X,3HSXY)
      GO TO 230
  220 IF (EJECT(2).NE.0) GO TO 205
  230 WRITE(NOUT,240) ISTYP,CSTR
  240 FORMAT (1H0,6X,A2,A3,I7,3F7.1)
C
  250 CONTINUE
      RETURN
      END
