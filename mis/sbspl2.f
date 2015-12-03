      SUBROUTINE SBSPL2( NTYPE, TI )
C
C     PHASE TWO STRESS DATA RECOVERY BASIC BENDING TRIANGLE.
C
C     NTYPE = 0  IMPLIES BASIC BENDING TRIANGLE
C     NTYPE = 3 IMPLIES TRI-PLATE IS CALLING
C     NTYPE = 4 IMPLIES QUAD-PLATE IS CALLING
C
      DIMENSION NSIL(1), SI(1)
      REAL    TI(6)    ,SDELTA(3),FRLAST(2)
      INTEGER EJECT    ,ISHED(7) ,TLOADS   ,BSC   ,PLT   ,QD   ,TR
     1,       ISTYP(2)
      LOGICAL FLAG
C
      COMMON   /SYSTEM/  IBFSZ    ,NOUT     ,IDM(9)   ,LINE
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SDR2X4/ DUMMY(35),IVEC,DUM11(11),TLOADS
      COMMON /SDR2X7/ EST(100),STRES(100),FORVEC(25)
      COMMON /SDR2X8/ EYE,I,J,NPOINT,VEC(5),ZOVERI,TEMP,DELTA
     1,            CVEC(5),CFRVEC(12)
      COMMON /SDR2X9/ NCHK,ISUB,ILD,FRTMEI(2),TWOTOP,FNCHK
      COMMON /SDR2DE/ SKP(8),IELTYP
C
      EQUIVALENCE (SI(1),EST(9)),(NSIL(1),EST(2))
      EQUIVALENCE (NELID,EST(1))
      EQUIVALENCE (F1,N1)  ,  (ISHED(6),FRLAST(1) )
     1,           (ISHED(1),LSUB)  ,  (ISHED(2),LLD)
C
      DATA TR,QD,BSC,PLT / 4H  TR, 4H  QD, 4HBSC , 4HPLT  /
      DATA LLD,LSUB,FRLAST / 2*-100, -1.0E30, -1.0E30 /
C
C     ******************************************************************
C  . ZERO OUT FORCE AND PRECISION CHECK VECTOR...
      DO 5 I = 1,6
      FORVEC(I) = 0.0E0
      CFRVEC(I) = 0.0E0
    5 CFRVEC(I+6) = 0.0E0
      FORVEC(1) = EST(1)
C
      NPTS = 3
      IF( NTYPE .EQ. 4 ) NPTS = 4
C
C                          NPTS
C         FORCE VECTOR = SUMMATION (S )(U )
C                          I=1       I   I
C
      DO 20 I = 1,NPTS
C
C     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
C
      NPOINT = IVEC + NSIL(I) - 1
C
      CALL SMMATS (SI(30*I-29),5,6,0,  Z(NPOINT),6,1,0,  VEC,CVEC )
C
      DO 30 J = 1,5
      CFRVEC(J+1) = CFRVEC(J+1) + CVEC(J)
   30 FORVEC(J + 1) = FORVEC(J + 1) + VEC(J)
   20 CONTINUE
      IF( TLOADS .EQ. 0 ) GO TO 55
      FLAG = .FALSE.
      JST = 98
      IF( NTYPE .EQ. 4 ) JST = 128
      F1 = TI(6)
      IF( N1 .EQ. 1 ) GO TO 50
      FORVEC(2) = FORVEC(2) - TI(2)
      FORVEC(3) = FORVEC(3) - TI(3)
      FORVEC(4) = FORVEC(4) - TI(4)
      IF( TI(5).EQ.0.0 .AND. TI(6).EQ.0.0 ) FLAG = .TRUE.
      GO TO 55
   50 FORVEC(2) = FORVEC(2) + TI(2)*EST(JST+1)
      FORVEC(3) = FORVEC(3) + TI(2)*EST(JST+2)
      FORVEC(4) = FORVEC(4) + TI(2)*EST(JST+3)
      IF( TI(3).EQ.0.0 .AND. TI(4).EQ.0.0 ) FLAG = .TRUE.
   55 CONTINUE
C
C     FORCE VECTOR COMPLETE AND CONTAINS M , M , M  , V , V
C                                         X   Y   XY   X   Y
C
C     AND ALSO INCLUDES THE ELEMENT ID AS THE FIRST ENTRY
C     ******************************************************************
C
C     STRESSES AT FIBER DISTANCES Z1 AND Z2 = - M * Z / I
C
      STRES(2) = EST(7)
      STRES(11) = EST(8)
      EYE = EST(6)
      I = 2
      K = 7
      K1 = 0
  200 ZOVERI = - STRES(I) / EYE
      ZOVI = ABS (ZOVERI)
      IF( TLOADS.EQ.0  .OR.  FLAG ) GO TO 207
      J = 98
      IF( NTYPE .EQ. 4 ) J = 128
      IF( N1 .EQ. 1 ) GO TO 205
C
      FF = TI(K1+5) - TI(1)
      SDELTA(1) = (EST(JST+1)*FF + TI(2)*STRES(I))/EYE
      SDELTA(2) = (EST(JST+2)*FF + TI(3)*STRES(I))/EYE
      SDELTA(3) = (EST(JST+3)*FF + TI(4)*STRES(I))/EYE
      GO TO 210
C
  205 FF = (TI(K1+3) - STRES(I)*TI(2) - TI(1)) / EYE
      SDELTA(1) = EST(JST+1)*FF
      SDELTA(2) = EST(JST+2)*FF
      SDELTA(3) = EST(JST+3)*FF
      GO TO 210
C
  207 SDELTA(1) = 0.0
      SDELTA(2) = 0.0
      SDELTA(3) = 0.0
  210 CONTINUE
      STRES(I+1) = FORVEC(2) * ZOVERI - SDELTA(1)
      STRES(I+2) = FORVEC(3) * ZOVERI - SDELTA(2)
      STRES(I+3) = FORVEC(4) * ZOVERI - SDELTA(3)
      CFRVEC(K  ) = CFRVEC(2) * ZOVI
      CFRVEC(K+1) = CFRVEC(3) * ZOVI
      CFRVEC(K+2) = CFRVEC(4) * ZOVI
C
C     PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
      TEMP = STRES(I+1) - STRES(I+2)
      STRES(I+7) = SQRT(  (TEMP/2.0E0)**2  +  STRES(I+3)**2  )
      DELTA = (STRES(I + 1) + STRES(I + 2) ) / 2.0E0
      STRES(I+5) = DELTA + STRES(I+7)
      STRES(I+6) = DELTA - STRES(I+7)
      DELTA = 2.0E0 * STRES(I+3)
      IF( ABS(DELTA) .LT. 1.0E-15 .AND. ABS(TEMP) .LT. 1.0E-15)GO TO 101
      STRES(I+4) = ATAN2( DELTA,TEMP ) * 28.6478898E0
      GO TO 100
  101 STRES(I+4) = 0.0E0
  100 IF( I .EQ. 11 ) GO TO 111
      I = 11
      K1 = 1
      K = 10
      GO TO 200
  111 STRES( 1) = EST(1)
C
C     ABOVE COMPLETES 2 VECTORS EACH WITH...
C
C     ELEM ID, Z, SIGMA X, SIGMA Y, SIGMA XY, PHI, SIG 1, SIG 2, TAU-MAX
C
C     STRESSES AND FORCES COMPLETE
C
C
C     ADDITON TO ELIMINATE 2ND ELEMENT ID IN OUTPUT
C
      DO 5000 I = 10,17
 5000 STRES(I) = STRES(I+1)
C
C  . STRESS CHECK...
C
      IF (NCHK.LE.0) GO TO 350
      CFRVEC(1) = EST(1)
      K = 0
C
C  . FORCES...
      CALL SDRCHK (FORVEC(2),CFRVEC(2),5,K)
C
C  . STRESSES...
      CALL SDRCHK (STRES(3),CFRVEC(7),3,K)
      CALL SDRCHK (STRES(11),CFRVEC(10),3,K)
      IF (K.EQ.0) GO TO 350
C
C  . LIMITS EXCEEDED...
      J = 0
      ISTYP(1) = TR
      IF (IELTYP.EQ.15) ISTYP(1) = QD
      ISTYP(2) = PLT
      IF (IELTYP.EQ.7) ISTYP(2) = BSC
C
      IF (LSUB.EQ.ISUB .AND. FRLAST(1).EQ.FRTMEI(1) .AND.
     1    LLD .EQ.ILD  .AND. FRLAST(2).EQ.FRTMEI(2) ) GO TO 320
C
      LSUB = ISUB
      LLD = ILD
      FRLAST(1) = FRTMEI(1)
      FRLAST(2) = FRTMEI(2)
      J = 2
      CALL PAGE1
  300 CALL SD2RHD (ISHED,J)
      LINE = LINE + 1
      WRITE(NOUT,310)
  310 FORMAT (7X,4HTYPE,5X,3HEID,5X,2HMX,5X,2HMY,4X,3HMXY,5X,2HVX,5X,
     1  2HVY,4X,3HSX1,4X,3HSY1,3X,4HSXY1,4X,3HSX2,4X,3HSY2,3X,4HSXY2)
      GO TO 330
C
  320 IF (EJECT(2).NE.0) GO TO 300
  330 WRITE(NOUT,340) ISTYP,NELID,(CFRVEC(I),I=2,12)
  340 FORMAT (1H0,4X,A4,A3,I7,11F7.1)
C
  350 CONTINUE
      RETURN
      END
