      SUBROUTINE QDMM2D
C
C     THIS ROUTINE CALCULATES THE STIFFNESS, MASS AND DAMPING MATRICES
C     FOR THE QDMM2 ELEMENT.
C
C     DOUBLE PRECISION VERSION
C
C     THIS SUBROUTINE USES SUBROUTINE E MA D TQ TO CALCULATE THE LUMPED
C     MASS USING THE SAME METHOD AS WITH THE QDMEM ELEMENT.
C
C     THIS ROUTINE MAY NOT BE CALLED IN A HEAT PROBLEM.
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
      LOGICAL          PLANAR,NOGO,IHEAT
      INTEGER          DICT(11),ELID,ESTID,IPART(4),NEST(7),MAP(4,3)
      DOUBLE PRECISION KIJ(1),KOUT(144),RMAT(3,5),ET(9),K1SUM(9,16),
     1                 ISINTH,KMAT(63),K5SUM(9,5),ICOSTH,GSUBE(9),IT,
     2                 G(36),ITEMP9(9),K5MOD(9,5),TMAT(36),JTEMP9(9),
     3                 IDETRM,KTEMP9(9)
      CHARACTER        UFM*23,UWM*25
      COMMON /XMSSG /  UFM,UWM
      COMMON /EMGEST/  EST(26)
      COMMON /EMGPRM/  DUMM(15),ISMD(3),IPREC,NOGO,HEAT,ICMBAR
      COMMON /EMGDIC/  DUM(2),NGRIDS,ELID,ESTID
      COMMON /MATIN /  MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  G11,G12,G13,G22,G23,G33,RHO,ALPS(3),TSUB0,GE
      COMMON /SYSTEM/  KSYSTM(65)
      COMMON /CONDAS/  PI,TWOPI,RADEG,DEGRA,S4PISQ
      EQUIVALENCE      (KSYSTM(2),IOUTPT),(NEST(1),EST(1)),
     1                 (DICT(5),DICT5),(K1SUM(1,1),KIJ(1)),
     2                 (KSYSTM(56),IHEAT)
      DATA    MAP   /  1, 2, 3, 4,
     1                 2, 3, 4, 1,
     2                 5, 5, 5, 5  /
C
C     THIS ELEMENT NOT USED IN A HEAT PROBLEM
C
      IF (IHEAT) GO TO 320
C
C     CREATE AN ARRAY POINTING TO THE GRID POINTS ACCORDING TO
C     INCREASING SIL VALUE
C
      DO 2 I = 1,4
      IPART(I) = NEST(I+1)
    2 CONTINUE
      I = -4
    4 J = 0
      DO 6 K = 1,4
      IF (IPART(K) .LT. J) GO TO 6
      J = IPART(K)
      L = K
    6 CONTINUE
      IPART(L) = I
      I = I + 1
      IF (I .LT. 0) GO TO 4
      DO 8 I = 1,4
      IPART(I) = -IPART(I)
    8 CONTINUE
C
C     IF STIFFNESS MATRIX NEEDED
C     SET UP DICT ARRAY AND FOR STIFFNESS MATRIX
C     CALCULATIONS, OTHERWISE SKIP
C
      IF (ISMD(1) .EQ. 0) GO TO 400
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
      CALL Q2BCD (EST,PLANAR,RMAT,ET,IERROR)
      IF (IERROR) 10,10,270
C
C     ZERO SUMMATION ARRAYS
C
   10 DO 40 I = 1,9
      DO 20 J = 1,16
      K1SUM(I,J) = 0.0D0
   20 CONTINUE
      DO 30 J = 1,5
      K5SUM(I,J) = 0.0D0
   30 CONTINUE
   40 CONTINUE
C
C     SUB-TRIANGLES ARE COMPUTED AND RESULTS SUMMED.
C
      DO 70 I = 1,4
C
C     CALL TRIANGLE CALCULATION ROUTINE TO GET (3X3) SUB-PARTITIONS
C
      IA = MAP(I,1)
      IB = MAP(I,2)
      IC = MAP(I,3)
      IT = EST(8)
C
      CALL Q2TRMD (RMAT(1,IA),RMAT(1,IB),RMAT(1,IC),DUMMY,ISINTH,ICOSTH,
     1             GSUBE,IT,IERROR,1,KMAT,DUMMY,DUMMY,DUMMY)
      IF (IERROR) 50,50,270
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
   50 J1 = 5*IA - 4
      J2 = 4*IA - 4 + IB
      J3 = 4*IB - 4 + IA
      J4 = 5*IB - 4
C
      DO 60 K = 1,9
      K5SUM(K,IA) = K5SUM(K,IA) + KMAT(K   )
      K5SUM(K,IB) = K5SUM(K,IB) + KMAT(K+ 9)
      K5SUM(K,IC) = K5SUM(K,IC) + KMAT(K+18)
      K1SUM(K,J1) = K1SUM(K,J1) + KMAT(K+27)
      K1SUM(K,J2) = K1SUM(K,J2) + KMAT(K+36)
      K1SUM(K,J3) = K1SUM(K,J3) + KMAT(K+45)
      K1SUM(K,J4) = K1SUM(K,J4) + KMAT(K+54)
   60 CONTINUE
C
   70 CONTINUE
C
C     FORMATION OF THE FOUR (3X3) G MATRICES.
C                     -1
C     (G ) = -(K5SUM  ) (K  )   NOTE.  IF -PLANAR- THEN MODIFIED
C       I           55    5I           K5SUM MATRICES ARE USED.
C
      IF (PLANAR) GO TO 90
      DO 80 I = 1,5
      DO 80 J = 1,9
      K5MOD(J,I) = K5SUM(J,I)
   80 CONTINUE
      GO TO 110
C
   90 DO 100 I = 1,5
      K5MOD(1,I) = K5SUM(1,I)
      K5MOD(2,I) = K5SUM(2,I)
      K5MOD(3,I) = K5SUM(3,I)
      K5MOD(4,I) = K5SUM(4,I)
      K5MOD(5,I) = K5SUM(5,I)
      K5MOD(6,I) = K5SUM(6,I)
      K5MOD(7,I) = 0.0D0
      K5MOD(8,I) = 0.0D0
      K5MOD(9,I) =-0.25D0
  100 CONTINUE
      K5MOD(9,5) = 1.0D0
C
C     INVERT K5MOD   AND NEGATE RESULT.
C                 55
C
  110 CONTINUE
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERD (3,K5MOD(1,5),3,DUMMY,0,IDETRM,ISING,ITEMP9)
      IF (ISING .EQ. 2) GO TO 290
C
      DO 120 I = 1,9
      K5MOD(I,5) = -K5MOD(I,5)
  120 CONTINUE
C
C     FORM G MATRICES
C
      DO 130 I = 1,4
      CALL GMMATD (K5MOD(1,5),3,3,0, K5MOD(1,I),3,3,0, G(9*I-8))
  130 CONTINUE
C
C     FORMATION OF THE 4 TRANSFORMATION MATRICES EACH (3X3)
C
      DO 170 I = 1,4
      IEST = 4*I + 6
      IF (NEST(IEST)) 140,150,140
C
C     GET TRANSFORMATION MATRIX
C
  140 CALL TRANSD (NEST(IEST),ITEMP9)
      CALL GMMATD (ET,3,3,0, ITEMP9,3,3,0, TMAT(9*I-8))
      GO TO 170
C
  150 K = 9*I - 9
      DO 160 J = 1,9
      K = K + 1
      TMAT(K) = ET(J)
  160 CONTINUE
C
  170 CONTINUE
C
C     FORM STIFFNESS MATRIX BY ROW-PARTIONS.
C
      DO 260 I = 1,4
C                          T
C     IF -PLANAR- FORM (G ) (K  ) FOR USE IN COLUMN-PARTITIONS LOOP.
C                        I    55
C
      IF (.NOT.PLANAR) GO TO 190
      CALL GMMATD (G(9*I-8),3,3,1, K5SUM(1,5),3,3,0, ITEMP9)
C
C     COLUMN-PARTITIONS-LOOP
C
  190 DO 250 J = 1,4
C                                   T
C     FORM (K  ) = (K5SUM  ) + (K  ) (G )
C            IJ          IJ      5I    J
C
      CALL GMMATD (K5SUM(1,I),3,3,1, G(9*J-8),3,3,0, JTEMP9)
      LPART = 4*I - 4 + J
      DO 200 K = 1,9
      K1SUM(K,LPART) = K1SUM(K,LPART) + JTEMP9(K)
  200 CONTINUE
C
C     BALANCE OF TERMS IF -PLANAR-
C
C                T            T
C     ADD IN (G ) (K  ) + (G ) (K  )(G )
C              I    5J      I    55   J
C
      IF (.NOT.PLANAR) GO TO 220
      CALL GMMATD (ITEMP9,3,3,0, G(9*J-8),3,3,0, JTEMP9)
      CALL GMMATD (G(9*I-8),3,3,1, K5SUM(1,J),3,3,0, KTEMP9)
      DO 210 K = 1,9
      K1SUM(K,LPART) = K1SUM(K,LPART) + KTEMP9(K) + JTEMP9(K)
  210 CONTINUE
C
C     TRANSFORM THIS RESULTANT K   (3X3) STORED AT K1SUM(1,LPART)
C                               IJ
C     TO GLOBAL.
C
  220 CALL GMMATD (TMAT(9*I-8),3,3,1, K1SUM(1,LPART),3,3,0, JTEMP9)
      CALL GMMATD (JTEMP9,3,3,0, TMAT(9*J-8),3,3,0, K1SUM(1,LPART))
  250 CONTINUE
  260 CONTINUE
C
C     FOR THE MATRIX ASSEMBLER -EMG- THE 16 (3X3) PARTITIONS IN K1SUM
C     ARE REARRANGED TO STORE THEM BY ROWS TO A TOTAL OF
C     12X12 RATHER THAN 3X3.  BUT FIRST DICT MUST BE
C     SET UP.  THE SILS MUST BE SORTED SO THAT THE 12X12 WILL
C     BE BY INCREASING SIL VALUE
C
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 12
      DICT(4) = 7
      DICT5   = GE
      IP      = IPREC
C
C     REORDER K1SUM INTO KOUT AS DESCRIBED ABOVE
C
C         ****          ****
C         * K   K   K   K  *
C         *  AA  AB  AC  AD*
C     K = * K   K   K   K  *
C         *  BA  BB  BC  BD*
C         * K   K   K   K  *
C         *  CA  CB  CC  CD*
C         * K   K   K   K  *
C         *  DA  DB  DC  DD*
C         ****          ****
C
C     WHERE SUBSCRIPTS ARE ARRANGED BY INCREASING SIL VALUE
C
      DO 390 I = 1,4
      II = IPART(I)
      DO 380 J = 1,4
      JTT = IPART(J)
      JT = (I-1)*4  + J
      DO 370 K= 1,9
      MODK = MOD(K,3)
      IF(MODK .EQ. 0) MODK = 3
       L = (II-1)*36 + ((K-1)/3)*12 + (JTT-1)*3 + MODK
      KOUT(L) = K1SUM(K,JT)
  370 CONTINUE
  380 CONTINUE
  390 CONTINUE
C
      CALL EMGOUT (KOUT,KOUT,144,1,DICT,1,IP)
C
C     CALCULATE THE MASS MATRIX HERE.  SUBROUTINE
C     E MAS TQ IS USED TO GENERATE A LUMPED
C     MASS MATRIX EXACTLY LIKE A QDMEM ELEMENT
C
  400 IF (ISMD(2) .EQ. 0) RETURN
C
      CALL E MA D TQ (1,K1SUM)
C
      DICT(1) = ESTID
      DICT(2) = 2
      DICT(3) = 12
      DICT(4) = 7
      DICT(5) = 0
C
C     REARRANGE KIJ BY INCREASING SIL VALUE
C
      DO 440 I = 1,4
      II = 1 + (IPART(I)-1)*3
      IJ = (I-1)*3 + 1
      KOUT(IJ  ) = KIJ(II  )
      KOUT(IJ+1) = KIJ(II+1)
  440 KOUT(IJ+2) = KIJ(II+2)
C
      CALL EMGOUT (KOUT,KOUT,12,1,DICT,2,IP)
      RETURN
C
C     ELEMENT ERRORS DETECTED.
C
  270 WRITE  (IOUTPT,280) UFM,NEST(1)
  280 FORMAT (A23,' 3098,  QDMEM2 ELEMENT STIFFNESS ROUTINE DETECTS ',
     1       'ILLEGAL GEOMETRY FOR ELEMENT ID =',I10)
      GO TO 310
  290 WRITE  (IOUTPT,300) UFM,NEST(1)
  300 FORMAT (A23,' 3099.  ELEMENT STIFFNESS COMPUTATION FOR QDMEM2 ',
     1       'ELEMENT ID =',I10, /5X,'IS IMPOSSIBLE DUE TO SINGULARITY',
     2       ' IN CONSTRAINT EQUATION.')
  310 NOGO = .TRUE.
      RETURN
C
  320 WRITE (IOUTPT,330) UWM,NEST(1)
  330 FORMAT (A25,' 3115, QDMM2 FINDS ELEMENT NUMBER',I10,
     1       ' PRESENT IN A HEAT FORMULATION AND IS IGNORING SAME.')
C
      RETURN
      END
