      SUBROUTINE Q2TRMD(RA,RB,RC,ALPHA,ISINTH,ICOSTH,GSUBE,IT,
     1                  IERROR,IOPT,KMAT,PMAT,SMAT,ZMAT)
C*****
C  SUB-TRIANGLE COMPUTATION ROUTINE FOR THE QDMEM2 ELEMENT
C
C  ON INPUT
C  ========
C            RA,RB,RC = 3 (3X1) COORDINATE VECTORS FOR TRIANGLE
C            IOPT     = 1  CALL FROM STIFFNESS GENERATION MODULE
C                     = 2  CALL FROM STATIC LOAD MODULE
C                     = 3  CALL FROM STRESS RECOVERY MODULE
C            ALPHA    = 3X1 VECTOR APPROPRIATE FOR CALL
C            ISINTH   = SIN OF MATERIAL ANGLE(WHOLE - ELEMENT)
C            ICOSTH   = COS OF MATERIAL ANGLE(WHOLE - ELEMENT)
C            GSUBE    = MATERIAL MATRIX (3X3)
C            IT       = THICKNESS OF ELEMENT
C
C  ON OUTPUT
C  =========
C            IERROR = 0  IF NO ERROR
C                   = 1  IF BAD ELEMENT GEOMETRY
C
C            KMAT,PMAT,SMAT,ZMAT = FOLLOWING PER IOPT VALUE SENT
C
C
C            IOPT=1
C            ------
C            KMAT = 7 (3X3)-S = KCA,KCB,KCC,KAA,KAB,KBA,KBB
C            PMAT = UNCHANGED
C            SMAT = UNCHANGED
C            ZMAT = UNCHANGED
C
C            IOPT=2
C            ------
C            KMAT = 3 (3X3)-S = KCA,KCB,KCC
C            PMAT = 3 (3X1)-S = PA,PB,PC
C            SMAT = UNCHANGED
C            ZMAT = UNCHANGED
C
C            IOPT=3
C            ------
C            KMAT = 7 (3X3)-S = KCA,KCB,KCC,KAA,KAB,KBA,KBB
C            PMAT = 3 (3X1)-S = PTA,PTB,PTC
C            SMAT = 3 (3X3)-S = SA,SB,SC
C            ZMAT = 3 (3X1)-S = ZA,ZB,ZC
C
C*****
      DOUBLE PRECISION  ALPHA(3) ,E(9)     ,IAREAT   ,I33
      DOUBLE PRECISION  RA(3)    ,KMAT(1)  ,ZMAT(1)  ,MAG      ,IVEC(3)
      DOUBLE PRECISION  RB(3)    ,PMAT(1)  ,V12(3)   ,IAREA    ,JVEC(3)
      DOUBLE PRECISION  RC(3)    ,SMAT(1)  ,V13(3)   ,IXSUBB   ,KVEC(3)
      DOUBLE PRECISION  IC       ,ISINTH   ,CA(6)    ,IXSUBC   ,TM(9)
      DOUBLE PRECISION  IS       ,ICOSTH   ,CB(6)    ,IYSUBC   ,TM3(3)
      DOUBLE PRECISION  IT                 ,CC(6)    ,C(3,6)   ,ALP(3)
      DOUBLE PRECISION  TEMP9(9) ,HI(27)   ,HITGE(9) ,GSUBE(9)
C
      DOUBLE PRECISION  DADOTB
C
      INTEGER           IPART(3,3)
C
      EQUIVALENCE (C(1,1),CA(1)), (C(1,3),CB(1)), (C(1,5),CC(1))
      EQUIVALENCE (E(1),IVEC(1)), (E(4),JVEC(1)), (E(7),KVEC(1))
C
      DATA IPART/ 28,46,1,   37,55,10,   0,0,19 /
C
C     V    = R   - R   ,      V    = R   - R
C      12     B     A          13     C     B
C
      DO 10 I = 1,3
      V12(I) = RB(I) - RA(I)
      V13(I) = RC(I) - RA(I)
   10 CONTINUE
C
C     KVEC(UN-NORMALIZED)  =  V     X  V
C                              12       13
C
      CALL DAXB( V12, V13, KVEC )
      MAG = DSQRT( DADOTB(KVEC,KVEC) )
      IF( MAG ) 190,190,20
C
C     NORMALIZE  K-VECTOR, AND AREA
C
   20 KVEC(1) = KVEC(1) / MAG
      KVEC(2) = KVEC(2) / MAG
      KVEC(3) = KVEC(3) / MAG
      IAREA = 0.50D0 * MAG
C
C     I-VECTOR = V   (NORMALIZED) THUS
C                 12
C
      MAG = DSQRT( DADOTB( V12, V12 ) )
      IF( MAG ) 190,190,30
   30 IVEC(1) = V12(1) / MAG
      IVEC(2) = V12(2) / MAG
      IVEC(3) = V12(3) / MAG
      IXSUBB = MAG
C
C     J-VECTOR = K-VECTOR CROSS I-VECTOR THUS
C
      CALL DAXB( KVEC, IVEC, JVEC )
C
C     MATERIAL COEFFICIENTS C AND S    U,V,W = I-VECTOR
C
      MAG = DSQRT( IVEC(1)**2 + IVEC(2)**2 )
      IF( MAG .LE. 0.D0 ) GO TO 190
      IC =(IVEC(1)*ICOSTH + IVEC(2)*ISINTH)/MAG
      IS =(IVEC(1)*ISINTH - IVEC(2)*ICOSTH)/MAG
C
C     X = MAGNITUDE OF V  , X = I-VEC DOT V  , Y = J-VEC DOT V
C      B                12   C             13   C             13
C
      IXSUBC = DADOTB( IVEC, V13 )
      IYSUBC = DADOTB( JVEC, V13 )
      IF( IXSUBB ) 40,190,40
   40 IF( IYSUBC ) 50,190,50
C
   50 CA(1) = -1.0D0 / IXSUBB
      CA(2) = 0.0D0
      I33 = 1.0D0 / IYSUBC
      CA(3) = I33 * (IXSUBC/IXSUBB - 1.0D0)
      CA(4) = 0.0D0
      CA(5) = CA(3)
      CA(6) = CA(1)
C
      CB(1) = -CA(1)
      CB(2) = 0.0D0
      CB(3) = - I33   * (IXSUBC / IXSUBB )
      CB(4) = 0.0D0
      CB(5) = CB(3)
      CB(6) = CB(1)
C
      CC(1) = 0.0D0
      CC(2) = 0.0D0
      CC(3) = I33
      CC(4) = 0.0D0
      CC(5) = I33
      CC(6) = 0.0D0
C
C     FORM MATERIAL-ORIENTATION-TRANSFORMATION-MATRIX  (BY-ROWS)
C
      TM(1) = IC * IC
      TM(2) = IS * IS
      TM(3) = IC * IS
      TM(4) = TM(2)
      TM(5) = TM(1)
      TM(6) = -TM(3)
      TM(7) = 2.0D0 * TM(6)
      TM(8) = -TM(7)
      TM(9) = TM(1) - TM(2)
      IAREAT= IAREA * IT
C
C     IF SSG CALL MULTIPLY ALPHA(T-TO) VECTOR BY IAREAT
C
      IF( IOPT .NE. 2 ) GO TO 60
      ALP(1) = ALPHA(1) * IAREAT
      ALP(2) = ALPHA(2) * IAREAT
      ALP(3) = ALPHA(3) * IAREAT
C
C     IF SDR CALL COMPUTE AREA   = X  * T
C                                   B
   60 IF( IOPT .NE. 3 ) GO TO 70
      TM3(1) = TM(3) * IT
      TM3(2) = TM(6) * IT
      TM3(3) = TM(9) * IT
C
C     SET FIRST PARTITION ROW TO COMPUTE FOR STIFFNESS MATRICES.
C
   70 IROW1 = 1
      IF( IOPT .EQ. 2 ) IROW1 = 3
C*****
C           M
C     H  = T  C  E
C      I       I
C
C*****
      DO 80 I = 1,3
      CALL GMMATD( TM,3,3,0, C(1,2*I-1),2,3,1, TEMP9 )
      CALL GMMATD( TEMP9,3,2,0, E,2,3,0, HI(9*I-8) )
   80 CONTINUE
C*****
C     FORM OUTPUTS FOR POINTS I = A,B,C
C*****
      DO 180 I = 1,3
C
C              T
C     HITGE= H  G
C             I  E
C
      CALL GMMATD( HI(9*I-8),3,3,1, GSUBE,3,3,0, HITGE )
C
C     STIFFNESS MATRIX CALCULATIONS
C
C     ONLY KAA,KAB     ARE FORMED.  OUTPUT ORDER WITH EACH 3X3 STORED
C          KBA,KBB                  BY ROWS =
C          KCA,KCB,KCC              KCA,KCB,KCC,KAA,KAB,KBA,KBB
C
      IF( I .LT. IROW1 ) GO TO 150
      KK = 0
      DO 140 J = 1,3
      IPARTN = IPART(I,J)
      IF( IPARTN )140,140,90
   90 DO 100 K = 1,9
      KK = KK + 1
      TEMP9(K) = HI(KK)*IAREAT
  100 CONTINUE
      CALL GMMATD( HITGE,3,3,0, TEMP9,3,3,0, KMAT(IPARTN) )
  140 CONTINUE
  150 GO TO(180,160,170),IOPT
C****
C  SSG LOAD GENERATION CALL ADDITIONAL DATA TO OUTPUT.
C
C  ONLY PA,PB,PC ARE FORMED.
C*****
  160 CALL GMMATD( HITGE,3,3,0,   ALP,3,1,0, PMAT(3*I-2) )
      GO TO 180
C*****
C  SDR ADDITIONAL PHASE-1 STRESS OUTPUTS
C*****
  170 JPARTN = 9*I-8
      CALL GMMATD( GSUBE,3,3,0, HI(JPARTN),3,3,0, SMAT(JPARTN) )
      IPARTN = 3*I - 2
      CALL GMMATD( HITGE,3,3,0, ALPHA,3,1,0, PMAT(IPARTN)  )
      CALL GMMATD( TM3,3,1,1, SMAT(JPARTN),3,3,0, ZMAT(IPARTN) )
      DO 175 J=1,3
      K = IPARTN + J - 1
      PMAT(K) = PMAT(K)*IAREAT
  175 CONTINUE
C
  180 CONTINUE
      IERROR = 0
      RETURN
C*****
C  ERROR CONDITION, BAD GEOMETRY.
C*****
  190 IERROR = 1
      RETURN
      END
