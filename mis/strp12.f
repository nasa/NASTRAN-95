      SUBROUTINE STRP12 (TI)
C
C     PHASE II OF STRESS DATA RECOVERY
C
      LOGICAL         FLAG
      INTEGER         TLOADS
      REAL            TI(6),SDELTA(3)
      DIMENSION       NSIL(6),STR(18),NPH1OU(990),SI(36),STOUT(68),
     1                REALI(4)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SDR2X4/ DUMMY(35),IVEC,IVECN,LDTEMP,DEFORM,DUM8(8),
     1                TLOADS,MAXSIZ
      COMMON /SDR2X7/ PH1OUT(990),FORVEC(24)
      COMMON /SDR2X8/ TEMP,DELTA,NPOINT,IJ1,IJ2,NPT1,VEC(5),TEM,
     1                Z1 OVR I,Z2 OVR I,STRESS(18)
      EQUIVALENCE     (NSIL(1),PH1OUT(2)),(NPH1OU(1),PH1OUT(1)),
     1                (SI(1),PH1OUT(19)),(LDTEMP,FTEMP),(F1,N1)
C
C     FIRST GET FORCE VECTOR FOR THE PLATE CONSIDERATION
C
C     M ,  M ,  M ,  V ,  V    FOR ALL SIX GRID POINTS
C      X   Y   XY   X   Y
C
C                                NPTS
C     THE 5X1 FORCE VECTOR = SUMMATION  (S )(U )   FOR EACH POINT
C                                I=1       I   I
C
      NPTS = 6
      DO 15 I = 1,24
   15 FORVEC( I) = 0.0
      FORVEC( 1) = PH1OUT(1)
      FORVEC( 7) = PH1OUT(1)
      FORVEC(13) = PH1OUT(1)
      FORVEC(19) = PH1OUT(1)
C
C     DO 155 II = 1,4
C
      II = 0
   17 II = II + 1
      IF (II .GT. 4) GO TO 155
C
C     ZERO OUT LOCAL STRESSES
C
      SIG X  1 = 0.0
      SIG Y  1 = 0.0
      SIG XY 1 = 0.0
      SIG X  2 = 0.0
      SIG Y  2 = 0.0
      SIG XY 2 = 0.0
C
      IF (NSIL(1) .EQ. 0) GO TO 30
C
C     FORM SUMMATION
C
      DO 20 I = 1,6
C
C     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
C
      NPOINT = IVEC + NSIL(I) - 1
C
      II1 = (II-1)*180 + 30*I - 29
      CALL GMMATS (SI(II1),5,6,0, Z(NPOINT),6,1,0, VEC(1))
C
      DO 10 J = 2,6
      IJ = (II-1)*6 + J
   10 FORVEC(IJ) = FORVEC(IJ) + VEC(J-1)
   20 CONTINUE
C
      IF (TLOADS .EQ. 0) GO TO 23
      JST  = (II-1)*3 + 738
      I1   = (II-1)*6
      FLAG = .FALSE.
      F1   = TI(6)
      IF (N1 .EQ. 1) GO TO 22
      FORVEC(I1+2) = FORVEC(I1+2) - TI(2)
      FORVEC(I1+3) = FORVEC(I1+3) - TI(3)
      FORVEC(I1+4) = FORVEC(I1+4) - TI(4)
      IF (TI(5).EQ.0.0 .AND. TI(6).EQ.0.0) FLAG = .TRUE.
      GO TO 23
   22 FORVEC(I1+2) = FORVEC(I1+2) + TI(2)*PH1OUT(JST+1)
      FORVEC(I1+3) = FORVEC(I1+3) + TI(2)*PH1OUT(JST+2)
      FORVEC(I1+4) = FORVEC(I1+4) + TI(2)*PH1OUT(JST+3)
      IF (TI(3).EQ.0.0 .AND. TI(4).EQ.0.0) FLAG = .TRUE.
   23 CONTINUE
C
C     FORCE VECTOR IS NOW COMPLETE
C
      IF (II .EQ. 4) GO TO 24
      I1 = II*2 + 9
      I2 = I1 + 1
      Z1 OVR I = -12.0*PH1OUT(I1)/PH1OUT(7+II)**3
      Z2 OVR I = -12.0*PH1OUT(I2)/PH1OUT(7+II)**3
      GO TO 25
   24 ZI OVR I = -1.5/PH1OUT(17)**2
      Z2 OVR I = -Z1 OVR I
   25 CONTINUE
      II1 = (II-1)*6
C
      K1 = 0
      ASSIGN 26 TO IRETRN
      GO TO 170
C
   26 SIG X  1 = FORVEC(II1+2)*Z1 OVR I - SDELTA(1)
      SIG Y  1 = FORVEC(II1+3)*Z1 OVR I - SDELTA(2)
      SIG XY 1 = FORVEC(II1+4)*Z1 OVR I - SDELTA(3)
C
      K1 = 1
      ASSIGN 27 TO IRETRN
      GO TO 170
C
   27 SIG X  2 = FORVEC(II1+2)*Z2 OVR I - SDELTA(1)
      SIG Y  2 = FORVEC(II1+3)*Z2 OVR I - SDELTA(2)
      SIG XY 2 = FORVEC(II1+4)*Z2 OVR I - SDELTA(3)
C
      GO TO 40
   30 Z1 = 0.0
      Z2 = 0.0
C
   40 CONTINUE
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
      IF (NPH1OU(2) .EQ. 0) GO TO 120
C
C     COMPUTE PRINCIPAL STRESSES
C
      STR( 1) = PH1OUT(1)
      STR( 2) = PH1OUT(II*2+9)
      STR( 3) = SIG X 1
      STR( 4) = SIG Y 1
      STR( 5) = SIG XY 1
      STR(10) = PH1OUT(1)
      STR(11) = PH1OUT(II*2+10)
      STR(12) = SIG X  2
      STR(13) = SIG Y  2
      STR(14) = SIG XY 2
C
      DO 110 I = 3,12,9
      TEMP     = STR(I)-STR(I+1)
      STR(I+6) = SQRT((TEMP/2.0)**2+STR(I+2)**2)
      DELTA    = (STR(I)+STR(I+1))/2.0
      STR(I+4) = DELTA+STR(I+6)
      STR(I+5) = DELTA-STR(I+6)
      DELTA    = 2.0*STR(I+2)
      IF (ABS(DELTA).LT.1.0E-15 .AND. ABS(TEMP).LT.1.0E-15) GO TO 100
      STR(I+3) = ATAN2(DELTA,TEMP)*28.6478898E0
      GO TO 110
  100 STR(I+3) = 0.0
  110 CONTINUE
      GO TO 140
  120 DO 130 I = 2,18
  130 STR( I) = 0.0
  140 STR( 1) = PH1OUT(1)
      STR(10) = PH1OUT(1)
C
C     ADDITION TO ELIMINATE 2ND ELEMENT ID IN OUTPUT
C
      IJK = (II-1)*17
      STOUT(IJK+1) = PH1OUT(1)
      DO 149 I = 2,9
  149 STOUT(IJK+I) = STR(I)
      DO 150 I = 10,17
  150 STOUT (IJK+I) = STR(I+1)
C
      GO TO 17
  155 CONTINUE
      DO 156 I = 1,17
  156 PH1OUT(100+I) = STOUT(I)
      DO 159 J = 1,3
      DO 159 I = 1,16
      J1 = 117 + (J-1)*16 + I
      J2 = (J-1)*17 + I + 18
      PH1OUT(J1) = STOUT(J2)
  159 CONTINUE
      DO 157 I = 1,6
  157 PH1OUT(200+I) = FORVEC(I)
      DO 158 I = 1,5
      PH1OUT(206+I) = FORVEC(I+ 7)
  158 PH1OUT(211+I) = FORVEC(I+13)
      RETURN
C
C     INTERNAL SUBROUTINE
C
  170 IF (TLOADS.EQ.0 .OR. FLAG) GO TO 200
      JST = 738 + (II-1)*3
      REALI(1) = PH1OUT(8)**3/12.0
      REALI(2) = PH1OUT(9)**3/12.0
      REALI(3) = PH1OUT(10)**3/12.0
      CENTHK   = PH1OUT(17)*2.0
      REALI(4) = CENTHK**3/12.0
      IF (N1 .EQ. 1) GO TO 190
      FF = TI(K1+5) - TI(1)
      IF (ABS(PH1OUT(K1+9+2*II)) .LE.1.0E-07) GO TO 200
      SDELTA(1) = (PH1OUT(JST+1)*FF + TI(2)*PH1OUT(K1+9+2*II))/REALI(II)
      SDELTA(2) = (PH1OUT(JST+2)*FF + TI(3)*PH1OUT(K1+9+2*II))/REALI(II)
      SDELTA(3) = (PH1OUT(JST+3)*FF + TI(4)*PH1OUT(K1+9+2*II))/REALI(II)
      GO TO 210
  190 CONTINUE
      IF (ABS(PH1OUT(K1+9+2*II)) .LE. 1.0E-07) GO TO 200
      FF1 = (TI(K1+3) - PH1OUT(K1+9+2*II)*TI(2)-TI(1))/REALI(II)
      FF2 = (TI(K1+3) - PH1OUT(K1+9+2*II)*TI(2)-TI(1))/REALI(II)
      FF3 = (TI(K1+3) - PH1OUT(K1+9+2*II)*TI(2)-TI(1))/REALI(II)
      SDELTA(1) = PH1OUT(JST+1)*FF1
      SDELTA(2) = PH1OUT(JST+2)*FF2
      SDELTA(3) = PH1OUT(JST+3)*FF3
      GO TO 210
  200 SDELTA(1) = 0.0
      SDELTA(2) = 0.0
      SDELTA(3) = 0.0
  210 GO TO IRETRN, (26,27)
      END
