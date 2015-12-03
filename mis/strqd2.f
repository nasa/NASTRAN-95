      SUBROUTINE STRQD2( NPTS, TI )
C
C     ****PHASE II OF STRESS DATA RECOVERY*********
C
C     NPTS = 3 IMPLIES STRIA1 OR STRIA2  (PHASE II)
C     NPTS = 4 IMPLIES SQUAD1 OR SQUAD2  (PHASE II)
C
      LOGICAL FLAG
      LOGICAL STRAIN
C
      REAL    TI(6)    ,SDELTA(3),SSTRSS(3),FRLAST(2)
C
      INTEGER IST(10)
      INTEGER TLOADS   ,EJECT    ,IFRVEC(12),ISHED(7),TR       ,QU
     1,       ONTW(2)  ,ISTYP(2)
C
      DIMENSION NSIL(4), STR(18), NPH1OU(2), SI(36)
C
      COMMON /BLANK / IDUMMY(10), STRAIN
      COMMON   /SYSTEM/  IBFSZ    ,NOUT     ,IDM(9)   ,LINE
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SDR2X4/ DUMMY(35), IVEC, IVECN, LDTEMP, DEFORM,DUM8(8),
     1  TLOADS
      COMMON /SDR2X7/ PH1OUT(200),FORVEC(6)
      COMMON /SDR2X8/ TEMP,DELTA,NPOINT,I,J,NPT1,VEC(5),TEM
     1,            Z1 OVR I, Z2 OVR I,STRESS(3),CHPOUT(30),CVEC(5)
     1,            CFRVEC(12)
      COMMON /SDR2X9/ NCHK,ISUB,ILD,FRTMEI(2),TWOTOP,FNCHK
      COMMON /SDR2DE/ SKP2DE(8),IELTYP
C
      EQUIVALENCE
     1  (NSIL(1),PH1OUT(2))
     2 ,(NPH1OU(1),PH1OUT(1))
     3 ,(SI(1),PH1OUT(9))
     6 ,(STR(1),PH1OUT(101))
     7 ,(LDTEMP,FTEMP)
     8 ,(F1,N1)
     9,  (CFRVEC(1),IFRVEC(1)) ,  (ISHED(6),FRLAST(1))
     *,  (ISHED(1),LSUB) , (ISHED(2),LLD)
      EQUIVALENCE (STR(1), IST(1))
C
      DATA TR,QU,ONTW / 4HTRIA , 4HQUAD , 1H1 , 1H2  / , LLD  / -100 /
      DATA LSUB,FRLAST / -100 , -1.0E30 , -1.0E30   /
      DATA IBLANK /4H    /
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
C     PH1OUT(30*NPTS+9 THRU 30*NPTS+11)  S SUB T MATRIX
C
C **********************************************************************
C
C     PHASE I OUTPUT FROM THE MEMBRANE IS THE FOLLOWING
C     NOTE..BEGIN = 30*NPTS+11
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
C
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
C *********************************************************************
C
C  . ZERO FORVEC AND PRECISION CHECK STORAGE...
C
      DO 5 I = 1,6
      FORVEC(I) = 0.0E0
      CFRVEC(I) = 0.0E0
    5 CFRVEC(I+6) = 0.0E0
      FORVEC(1) = PH1OUT(1)
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
      CALL SMMATS (SI(30*I-29),5,6,0, Z(NPOINT),6,1,0, VEC(1),CVEC(1) )
      DO 10 J=2,6
      CFRVEC(J) = CFRVEC(J) + CVEC(J-1)
   10 FORVEC(J) = FORVEC(J) + VEC(J-1)
C
   20 CONTINUE
      IF (STRAIN) GO TO 220
      IF( TLOADS .EQ. 0 ) GO TO 25
      JST = 30*NPTS+8
      FLAG = .FALSE.
      F1 = TI(6)
      IF( N1 .EQ. 1 ) GO TO 22
      FORVEC(2) = FORVEC(2) - TI(2)
      FORVEC(3) = FORVEC(3) - TI(3)
      FORVEC(4) = FORVEC(4) - TI(4)
      IF( TI(5).EQ.0.0 .AND. TI(6).EQ.0.0 ) FLAG = .TRUE.
      GO TO 25
   22 FORVEC(2) = FORVEC(2) + TI(2)*PH1OUT(JST+1)
      FORVEC(3) = FORVEC(3) + TI(2)*PH1OUT(JST+2)
      FORVEC(4) = FORVEC(4) + TI(2)*PH1OUT(JST+3)
      IF( TI(3).EQ.0.0 .AND. TI(4).EQ.0.0 ) FLAG = .TRUE.
   25 CONTINUE
C
C     FORCE VECTOR IS NOW COMPLETE
C
      Z1 = PH1OUT(7)
      Z2 = PH1OUT(8)
C
      Z1 OVR I = - PH1OUT(7) / PH1OUT(6)
      Z2 OVR I = - PH1OUT(8) / PH1OUT(6)
      Z1I = ABS (Z1OVRI)
      Z2I = ABS (Z2OVRI)
C
      K1 = 0
      ASSIGN 26 TO IRETRN
      GO TO 170
C
   26 SIG X  1 = FORVEC(2) * Z1 OVR I - SDELTA(1)
      SIG Y  1 = FORVEC(3) * Z1 OVR I - SDELTA(2)
      SIG XY 1 = FORVEC(4) * Z1 OVR I - SDELTA(3)
      CFRVEC(7) = CFRVEC(2) * Z1I
      CFRVEC(8) = CFRVEC(3) * Z1I
      CFRVEC(9) = CFRVEC(4) * Z1I
C
      K1 = 1
      ASSIGN 27 TO IRETRN
      GO TO 170
C
   27 SIG X  2 = FORVEC(2) * Z2 OVR I - SDELTA(1)
      SIG Y  2 = FORVEC(3) * Z2 OVR I - SDELTA(2)
      SIG XY 2 = FORVEC(4) * Z2 OVR I - SDELTA(3)
      CFRVEC(10) = CFRVEC(2) * Z2I
      CFRVEC(11) = CFRVEC(3) * Z2I
      CFRVEC(12) = CFRVEC(4) * Z2I
C     *******************************
C
      GO TO 40
   30 Z1 = 0.0E0
      Z2 = 0.0E0
C
C     FIND SIG X, SIG Y, SIG XY, FOR MEMBRANE CONSIDERATION
   40 IF( NPH1OU(30*NPTS+13) .EQ. 0 ) GO TO 90
C
C     ZERO STRESS VECTOR STORAGE
C
      STRESS(1) = 0.0E0
      STRESS(2) = 0.0E0
      STRESS(3) = 0.0E0
      SSTRSS(1) = 0.0E0
      SSTRSS(2) = 0.0E0
      SSTRSS(3) = 0.0E0
C
C                            I=NPTS
C        STRESS VECTOR = (  SUMMATION(S )(U )  ) - (S )(LDTEMP - T )
C                            I=1       I   I         T            0
C
      DO 60 I=1,NPTS
C
C     POINTER TO I-TH SIL IN PH1OUT
      NPOINT = 30*NPTS + 12 + I
C     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
      NPOINT = IVEC + NPH1OU (NPOINT) - 1
C
C     POINTER TO S SUB I 3X3
      NPT1 = 30*NPTS + 12 + 9*I
C
      CALL SMMATS (PH1OUT(NPT1),3,3,0, Z(NPOINT),3,1,0, VEC(1),CVEC(1))
      DO 50 J=1,3
      SSTRSS(J) = SSTRSS(J) + CVEC(J)
   50 STRESS(J) = STRESS(J) + VEC(J)
C
   60 CONTINUE
C
      IF (STRAIN) GO TO 230
      IF(LDTEMP .EQ. (-1) ) GO TO 80
C
C     POINTER TO T SUB 0 = 30*NPTS + 17
C
      TEM = FTEMP - PH1OUT(30*NPTS + 17)
      DO 70 I=1,3
      NPOINT = 30*NPTS + 17 + I
   70 STRESS(I) = STRESS(I) -PH1OUT(NPOINT) *TEM
C
C     ADD MEMBRANE STRESSES TO PLATE STRESSES
C
   80 SIG X  1 = SIG X  1 + STRESS(1)
      SIG Y  1 = SIG Y  1 + STRESS(2)
      SIG XY 1 = SIG XY 1 + STRESS(3)
      SIG X  2 = SIG X  2 + STRESS(1)
      SIG Y  2 = SIG Y  2 + STRESS(2)
      SIG XY 2 = SIG XY 2 + STRESS(3)
      CFRVEC( 7) = CFRVEC( 7) + SSTRSS(1)
      CFRVEC( 8) = CFRVEC( 8) + SSTRSS(2)
      CFRVEC( 9) = CFRVEC( 9) + SSTRSS(3)
      CFRVEC(10) = CFRVEC(10) + SSTRSS(1)
      CFRVEC(11) = CFRVEC(11) + SSTRSS(2)
      CFRVEC(12) = CFRVEC(12) + SSTRSS(3)
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
   90 IF( NPH1OU(2) .EQ. 0 .AND. NPH1OU(30*NPTS+13) .EQ. 0 ) GO TO 120
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
      IF (.NOT.STRAIN) GO TO 152
      IST( 2) = IBLANK
      STR( 5) = 2.0*STR(5)
      STR( 9) = 2.0*STR(9)
      IST(10) = IBLANK
      STR(13) = 2.0*STR(13)
      STR(17) = 2.0*STR(17)
  152 CONTINUE
C
C  . STRESS CHECK...
C
      IF (NCHK .LE. 0 ) GO TO 999
      CFRVEC(1) = PH1OUT(1)
      K = 0
C  . FORCES...
      CALL SDRCHK (FORVEC(2),CFRVEC(2),5,K)
C  . STRESSES...
      CALL SDRCHK (STR(3),CFRVEC( 7),3,K)
      CALL SDRCHK (STR(11),CFRVEC(10),3,K)
C
      IF (K.EQ.0) GO TO 999
C
C  . LIMITS EXCEEDED...
      J = 0
      ISTYP(1) = TR
      ISTYP(2) = ONTW(1)
      IF (IELTYP.GT.17) ISTYP(1) = QU
      IF (IABS (IELTYP-17).LT.2) ISTYP(2) = ONTW(2)
C
      IF (LSUB.EQ.ISUB .AND. FRLAST(1).EQ.FRTMEI(1) .AND.
     1    LLD .EQ.ILD  .AND. FRLAST(2).EQ.FRTMEI(2) ) GO TO 156
C
      LSUB = ISUB
      LLD = ILD
      FRLAST(1) = FRTMEI(1)
      FRLAST(2) = FRTMEI(2)
      J = 1
      CALL PAGE1
  154 CALL SD2RHD (ISHED,J)
      LINE = LINE + 1
      WRITE(NOUT,155)
  155 FORMAT (7X,51HTYPE     EID     MX     MY    MXY     VX     VY    ,
     *38HSX1    SY1   SXY1    SX2    SY2   SXY2)
      GO TO 157
C
  156 IF (EJECT(2).NE.0) GO TO 154
C
  157 WRITE(NOUT,158) ISTYP,IFRVEC(1),(CFRVEC(II),II=2,12)
  158 FORMAT (1H0,5X,A4,A2,I7,11F7.1)
C
      GO TO 999
C
C     INTERNAL SUBROUTINE
C
  170 IF( TLOADS.EQ.0 .OR. FLAG ) GO TO 200
      JST = 30*NPTS + 8
      IF( N1 .EQ. 1 ) GO TO 190
      FF = TI(K1+5) - TI(1)
      SDELTA(1) = (PH1OUT(JST+1)*FF + TI(2)*PH1OUT(K1+7)) / PH1OUT(6)
      SDELTA(2) = (PH1OUT(JST+2)*FF + TI(3)*PH1OUT(K1+7)) / PH1OUT(6)
      SDELTA(3) = (PH1OUT(JST+3)*FF + TI(4)*PH1OUT(K1+7)) / PH1OUT(6)
      GO TO 210
  190 FF = (TI(K1+3) - PH1OUT(K1+7)*TI(2) - TI(1))/PH1OUT(6)
      SDELTA(1) = PH1OUT(JST+1)*FF
      SDELTA(2) = PH1OUT(JST+2)*FF
      SDELTA(3) = PH1OUT(JST+3)*FF
      GO TO 210
  200 SDELTA(1) = 0.0
      SDELTA(2) = 0.0
      SDELTA(3) = 0.0
  210 GO TO IRETRN,(26,27)
C
C     SPECIAL CALCULATIONS FOR STRAINS.
C
  220 SIGX2 = FORVEC(2)
      SIGY2 = FORVEC(3)
      SIGXY2= FORVEC(4)
      GO TO 40
C
  230 SIGX1 = STRESS(1)
      SIGY1 = STRESS(2)
      SIGXY1= STRESS(3)
      GO TO 90
  999 RETURN
      END
