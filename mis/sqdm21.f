      SUBROUTINE SQDM21
C
C     PHASE-I STRESS-DATA-RECOVERY ROUTINE FOR THE -QDMEM2- ELEMENT.
C
C     THIS ROUTINE WILL PREPARE FOR USE BY -SQDM22-, THE PHASE-II
C     ROUTINE, A TABLE CONTAINING THE FOLLOWING.
C
C     TABLE WORDS        DISCRIPTION
C     ------------------------------------------------------
C       1 THRU   1       ELEMENT-ID
C       2 THRU   5       4 SILS
C       6 THRU   6       ELEMENT-THICKNESS
C       7 THRU   7       REFERENCE TEMP -TSUB0-
C       8 THRU 151       16 (3X3) KIJ-G MATRICES
C     152 THRU 187       4 (3X3) STRESS MATRICES
C     188 THRU 199       4 (3X1) TEMP VECTORS
C     200 THRU 202       ST (3X1) STRESS-TEMPERATURE VECTOR
C     203 THRU 206       4 SIDE LENGTHS
C
C     ELEMENT EST ENTRY CONTENTS
C     + + + + + + + + + + + + + + + + + + + + + + + + + +
C     +   1 = ID                                        +
C     +   2 = SIL-PT-A            (ELEMENT CONNECTS     +
C     +   3 = SIL-PT-B             GRID POINTS A,B,     +
C     +   4 = SIL-PT-C             C,D IN THAT ORDER)   +
C     +   5 = SIL-PT-D                                  +
C     +   6 = MATERIAL-ANGLE                            +
C     +   7 = MATERIAL-ID                               +
C     +   8 = THICKNESS OF ELEMENT                      +
C     +   9 = NON-STRUCTURAL-MASS                       +
C     +  10 = COORD-SYS-ID PT-A OR 0                    +
C     +  11 = XA                                        +
C     +  12 = YA                                        +
C     +  13 = ZA                                        +
C     +  14 = COORD-SYS-ID PT-B OR 0                    +
C     +  15 = XB                                        +
C     +  16 = YB                                        +
C     +  17 = ZB                                        +
C     +  18 = COORD-SYS-ID PT-C OR 0                    +
C     +  19 = XC                                        +
C     +  20 = YC                                        +
C     +  21 = ZC                                        +
C     +  22 = COORD-SYS-ID PT-D OR 0                    +
C     +  23 = XD                                        +
C     +  24 = YD                                        +
C     +  25 = ZD                                        +
C     +  26 = AVERAGE OF CONNECTED GRID TEMPERATURES    +
C     + + + + + + + + + + + + + + + + + + + + + + + + + +
C
      LOGICAL         PLANAR
      INTEGER         NEST(7), MAP(4,3)
      REAL            K1SUM, K5SUM, ISINTH, ICOSTH, KMAT(63), SMAT(27),
     1                PMAT(9), JTEMP9, K5MOD, KTEMP9(9), ZMAT(9),
     2                ITEMP9(9), Q(3,3,4), IMAT12, RMAT(3,5), ETI(36),
     3                DVEC(3,4), KVEC(3)
      CHARACTER       UFM*23, UWM*25
      COMMON /XMSSG / UFM, UWM
C
C     FOLLOWING COMMON BLOCK MUST BE DIMENSIONED AT LEAST 350 IN SDR2B
C
      COMMON /SDR2X5/ EST(100), ID, ISILS(4), ELTHIK, REFTMP,
     1                K1SUM(9,16), SG(36),PT(3,4), ST(3),RG(4)
C
C     WORKING STORAGE BLOCK (KEEP .LE. 300 WORDS)
C
      COMMON /SDR2X6/ K5SUM(9,5), SISUM(9,5), PISUM(3,5), R(3,4,5),
     1                K5MOD(9,5), G(36),  T(9), E(9), IMAT12(12),
     2                JTEMP9(9), GSUBE(9)
      COMMON /MATIN / MATID, INFLAG, ELTEMP, STRESS, SINTH, COSTH
      COMMON /MATOUT/ G11, G12, G13, G22, G23, G33, RHO, ALPS(3), TSUB0
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /CONDAS/ PI, TWOPI, RADEG, DEGRA, S4PISQ
      EQUIVALENCE     (KSYSTM(2),IOUTPT), (NEST(1),EST(1))
      DATA    MAP   / 1, 2, 3, 4,
     1                2, 3, 4, 1,
     2                5, 5, 5, 5  /
C
C     COMPUTE BASIC SIN AND COSINE OF ELEMENT MATERIAL ANGLE.
C
      ANGL   = EST(6)*DEGRA
      ISINTH = SIN(ANGL)
      ICOSTH = COS(ANGL)
C
C     COMPUTE GSUBE MATRIX
C
      INFLAG = 2
      MATID  = NEST(7)
      ELTEMP = EST(26)
      SINTH  = 0.0
      COSTH  = 1.0
      CALL MAT (NEST(1))
      GSUBE(1) = G11
      GSUBE(2) = G12
      GSUBE(3) = G13
      GSUBE(4) = G12
      GSUBE(5) = G22
      GSUBE(6) = G23
      GSUBE(7) = G13
      GSUBE(8) = G23
      GSUBE(9) = G33
C
C     BASIC WHOLE-ELEMENT CALCULATIONS
C
      CALL Q2BCS (EST,PLANAR,RMAT,E,IERROR)
      IF (IERROR) 10,10,400
C
C     ZERO SUMMATION ARRAYS
C
   10 DO 40 I = 1,9
      DO 20 J = 1,16
      K1SUM(I,J) = 0.0
   20 CONTINUE
      DO 30 J = 1,5
      K5SUM(I,J) = 0.0
      SISUM(I,J) = 0.0
   30 CONTINUE
   40 CONTINUE
C
      DO 60 I = 1,5
      PISUM(1,I) = 0.0
      PISUM(2,I) = 0.0
      PISUM(3,I) = 0.0
   60 CONTINUE
C
C     SUB-TRIANGLES ARE COMPUTED AND RESULTS SUMMED.
C
      DO 100 I = 1,4
C
C     CALL TRIANGLE CALCULATION ROUTINE TO GET (3X3) SUB-PARTITIONS
C
      IA = MAP(I,1)
      IB = MAP(I,2)
      IC = MAP(I,3)
C
      CALL Q2TRMS (RMAT(1,IA),RMAT(1,IB),RMAT(1,IC),ALPS ,ISINTH,ICOSTH,
     1             GSUBE,EST(8),IERROR,3,KMAT,PMAT,SMAT,ZMAT)
      IF (IERROR) 70,70,400
C
C     SUM IN KCA,KCB,KCC 3-(3X3)-S STORED FIRST IN KMAT
C
C     ALSO SUM IN KAA,KAB,KBA,KBB = LAST 4-(3X3)-S STORED IN KMAT.
C     THESE GO INTO 4 OF THE 16 POSSIBLE (3X3) SUM MATRICES = ,
C
C     K11,K12,K13,K14,K21,K22,K23,K24,K31,K32,K33,K34,K41,K42,K43,K44
C
C     J1,J2,J3,J4 WILL EACH POINT TO 1 OF THE 16 (3X3)-S.
C
   70 J1 = 5*IA - 4
      J2 = 4*IA - 4 + IB
      J3 = 4*IB - 4 + IA
      J4 = 5*IB - 4
C
      DO 80 K = 1,9
      K5SUM(K,IA) = K5SUM(K,IA) + KMAT(K   )
      K5SUM(K,IB) = K5SUM(K,IB) + KMAT(K+ 9)
      K5SUM(K,IC) = K5SUM(K,IC) + KMAT(K+18)
      K1SUM(K,J1) = K1SUM(K,J1) + KMAT(K+27)
      K1SUM(K,J2) = K1SUM(K,J2) + KMAT(K+36)
      K1SUM(K,J3) = K1SUM(K,J3) + KMAT(K+45)
      K1SUM(K,J4) = K1SUM(K,J4) + KMAT(K+54)
      SISUM(K,IA) = SISUM(K,IA) + SMAT(K   )
      SISUM(K,IB) = SISUM(K,IB) + SMAT(K+ 9)
      SISUM(K,IC) = SISUM(K,IC) + SMAT(K+18)
   80 CONTINUE
C
      DO 90 K = 1,3
      PISUM(K,IA) = PISUM(K,IA) + PMAT(K  )
      PISUM(K,IB) = PISUM(K,IB) + PMAT(K+3)
      PISUM(K,IC) = PISUM(K,IC) + PMAT(K+6)
   90 CONTINUE
C
  100 CONTINUE
C
C     FORMATION OF THE FOUR (3X3) G MATRICES.
C                     -1
C     (G ) = -(K5SUM  ) (K  )   NOTE.  IF -PLANAR- THEN MODIFIED
C       I           55    5I           K5SUM MATRICES ARE USED.
C
      IF (PLANAR) GO TO 120
      DO 110 I = 1,5
      DO 110 J = 1,9
      K5MOD(J,I) = K5SUM(J,I)
  110 CONTINUE
      GO TO 140
C
  120 DO 130 I = 1,5
      K5MOD(1,I) = K5SUM(1,I)
      K5MOD(2,I) = K5SUM(2,I)
      K5MOD(3,I) = K5SUM(3,I)
      K5MOD(4,I) = K5SUM(4,I)
      K5MOD(5,I) = K5SUM(5,I)
      K5MOD(6,I) = K5SUM(6,I)
      K5MOD(7,I) = 0.0
      K5MOD(8,I) = 0.0
      K5MOD(9,I) =-0.25
  130 CONTINUE
      K5MOD(9,5) = 1.0
C
C     INVERT K5MOD   AND NEGATE RESULT.
C                 55
C
  140 CONTINUE
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (3,K5MOD(1,5),3,DUMMY,0,DETERM,ISING,ITEMP9)
      IF (ISING .EQ. 2) GO TO 400
C
      DO 150 I = 1,9
      K5MOD(I,5) = -K5MOD(I,5)
  150 CONTINUE
C
C     FORM G MATRICES
C
      DO 160 I = 1,4
      CALL GMMATS (K5MOD(1,5),3,3,0, K5MOD(1,I),3,3,0, G(9*I-8))
  160 CONTINUE
C
C     FORM STIFFNESS MATRIX BY ROW-PARTIONS.
C
      DO 210 I = 1,4
C                          T
C     IF -PLANAR- FORM (G ) (K  ) FOR USE IN COLUMN-PARTITIONS LOOP.
C                        I    55
C
      IF (.NOT.PLANAR) GO TO 170
      CALL GMMATS (G(9*I-8),3,3,1, K5SUM(1,5),3,3,0, ITEMP9)
C
C     COLUMN-PARTITIONS-LOOP
C
  170 DO 200 J = 1,4
C                                   T
C     FORM (K  ) = (K1SUM  ) + (K  ) (G )
C            IJ          IJ      5I    J
C
      CALL GMMATS (K5SUM(1,I),3,3,1, G(9*J-8),3,3,0, JTEMP9)
      LPART = 4*I - 4 + J
      DO 180 K = 1,9
      K1SUM(K,LPART) = K1SUM(K,LPART) + JTEMP9(K)
  180 CONTINUE
C
C     BALANCE OF TERMS IF -PLANAR-
C
C                T            T
C     ADD IN (G ) (K  ) + (G ) (K  )(G )
C              I    5J      I    55   J
C
      IF (.NOT.PLANAR) GO TO 200
      CALL GMMATS (ITEMP9,3,3,0, G(9*J-8),3,3,0, JTEMP9)
      CALL GMMATS (G(9*I-8),3,3,1, K5SUM(1,J),3,3,0, KTEMP9)
      DO 190 K = 1,9
      K1SUM(K,LPART) = K1SUM(K,LPART) + KTEMP9(K) + JTEMP9(K)
  190 CONTINUE
  200 CONTINUE
  210 CONTINUE
C
C     CALCULATION OF 4 (Q ) MATRICES, EACH 3X3.
C                        I
C
      DO 260 I = 1,4
      IA = MAP(I,1)
      IB = MAP(I,2)
      DO 230 J = 1,3
      DVEC(J,I) = RMAT(J,IB) - RMAT(J,IA)
  230 CONTINUE
      FMAG  = SQRT(SADOTB(DVEC(1,I),DVEC(1,I)))
      RG(I) = FMAG
      IF (FMAG) 400,400,240
  240 DO 250 J = 1,3
      DVEC(J,I) = DVEC(J,I)/FMAG
  250 CONTINUE
  260 CONTINUE
C
      DO 280 I = 1,4
      J = I - 1
      IF (J .EQ. 0) J = 4
      I1 = MAP(J,1)
      I2 = MAP(J,2)
      CALL SAXB (DVEC(1,I2),DVEC(1,I1),KVEC)
C
C     NORMALIZE, NEGATE, AND STORE AS DELTA-VEC IN (Q )
C                                                    I
      FMAG = SQRT(SADOTB(KVEC,KVEC))
      IF (FMAG) 400,400,270
  270 Q(1,3,I) = -KVEC(1)/FMAG
      Q(2,3,I) = -KVEC(2)/FMAG
      Q(3,3,I) = -KVEC(3)/FMAG
C
C     STORE D VECTORS AS ALPHA- VECTORS IN (Q )
C                                            I
      Q(1,1,I) = -DVEC(1,I)
      Q(2,1,I) = -DVEC(2,I)
      Q(3,1,I) = -DVEC(3,I)
C
      Q(1,2,I) = DVEC(1,J)
      Q(2,2,I) = DVEC(2,J)
      Q(3,2,I) = DVEC(3,J)
C
C     INVERT 3X3
C
C     AGAIN NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED .
C     SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (3,Q(1,1,I),3,DUMMY,0,DETERM,ISING,JTEMP9)
      IF (ISING .EQ. 2) GO TO 400
  280 CONTINUE
C
C     FORM FINAL OUTPUTS
C
      DO 370 I = 1,4
      II = 9*I - 8
C
C     TRANSFORMATION ETI = (E)(T )
C                               I
C
      KK = 4*I
      IF (NEST(KK+6)) 290,300,290
  290 CALL TRANSS (NEST(KK+6),T)
      CALL GMMATS (E,3,3,0, T,3,3,0, ETI(II))
      GO TO 320
C
  300 KK = II
      DO 310 J = 1,9
      ETI(KK) = E(J)
      KK = KK + 1
  310 CONTINUE
C
C       G            E      E
C     (S ) = 0.25( (S ) + (S )(G ) )(E)(T )
C       I            I      5   I        I
C
  320 CALL GMMATS (SISUM(1,5),3,3,0, G(II),3,3,0, JTEMP9)
      DO 330 J = 1,9
      JTEMP9(J) = 0.25*(JTEMP9(J)+SISUM(J,I))
  330 CONTINUE
      CALL GMMATS (JTEMP9,3,3,0, ETI(II),3,3,0, SG(II))
C
C       T     -         T -
C     (P ) = (P ) + (G ) (P )
C       I      I      I    5
C
      CALL GMMATS (G(II),3,3,1, PISUM(1,5),3,1,0, PT(1,I))
      DO 360 J = 1,3
      PISUM(J,I) = PT(J,I) + PISUM(J,I)
  360 CONTINUE
      CALL GMMATS (Q(1,1,I),3,3,1, PISUM(1,I),3,1,0, PT(1,I))
  370 CONTINUE
C
C     TRANSFORM STIFFNESS MATRIX TO GLOBAL
C
C        G           E
C     (K  ) = (Q )(K  )(E)(T )
C       IJ      I   IJ      J
C
      JPART = 0
      DO 390 I = 1,4
      DO 380 J = 1,4
      JPART = JPART + 1
      CALL GMMATS (Q(1,1,I),3,3,1, K1SUM(1,JPART),3,3,0, JTEMP9)
      CALL GMMATS (JTEMP9,3,3,0, ETI(9*J-8),3,3,0, K1SUM(1,JPART))
  380 CONTINUE
  390 CONTINUE
C
C     (S ) = (GSUBE)(ALPHAS)
C       T
C
      CALL GMMATS (GSUBE,3,3,0, ALPS,3,1,0, ST)
C
C     MISC. DATA FOR PHASE-II
C
      ID       = NEST(1)
      ISILS(1) = NEST(2)
      ISILS(2) = NEST(3)
      ISILS(3) = NEST(4)
      ISILS(4) = NEST(5)
      ELTHIK   = EST(8)
      REFTMP   = TSUB0
      RETURN
C
C     ERROR CONDITION
C
  400 WRITE  (IOUTPT,410) UWM,NEST(1)
  410 FORMAT (A25,' 3101, SINGULARITY OR BAD GEOMETRY FOR QDMEM2 ELEM.',
     1       ' ID =',I9, /5X,'STRESS OR FORCES WILL BE INCORRECT.')
      RETURN
      END
