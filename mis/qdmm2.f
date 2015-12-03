      SUBROUTINE QDMM2 (TEMPS,PG)
C
C     THERMAL LOAD GENERATION FOR THE QDMEM2 ELEMENT.
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
      INTEGER         NEST(7),MAP(4,3)
      REAL            RMAT(3,5),ET(9),K5SUM(9,5),ISINTH,KMAT(27),
     1                ITEMP9(9),ALPHA(3),PMAT(9),JTEMP9(9),ICOSTH,
     2                GSUBE(9),TEMPS(1),PG(1),PSUM(3,5),IT
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /MATIN / MATID, INFLAG, ELTEMP, STRESS, SINTH, COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33, RHO, ALPS(3), TSUB0
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /TRIMEX/ EST(26)
      COMMON /CONDAS/ PI,TWOPI,RADEG,DEGRA,S4PISQ
      EQUIVALENCE     (KSYSTM(2),IOUTPT),(NEST(1),EST(1))
      DATA    MAP   / 1, 2, 3, 4,
     1                2, 3, 4, 1,
     2                5, 5, 5, 5 /
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
C     FORM  ALPHA = ALPS *(T-T )  3X1 VECTOR USED IN SUB-TRIANGLE CALCS
C                       E     0
C
      TBAR     = TEMPS(1) - TSUB0
      ALPHA(1) = ALPS(1)*TBAR
      ALPHA(2) = ALPS(2)*TBAR
      ALPHA(3) = ALPS(3)*TBAR
C
C     NOTE THE ABOVE MAY BE MOVED TO BELOW AND COMPUTED USING THE
C     GRID TEMPS OF SUB-TRIANGLE.  (I.E. TOTAL AVERAGE FOR CENTER POINT
C     ONLY.)  AVERAGE OF WHOLE ELEMENT IS USED EXCLUSIVELY NOW.
C
C     BASIC WHOLE-ELEMENT CALCULATIONS
C
      CALL Q2BCS (EST,PLANAR,RMAT,ET,IERROR)
      IF (IERROR) 10,10,140
C
C     ZERO SUMMATION ARRAYS
C
   10 DO 30 I = 1,5
      DO 20 J = 1,9
      K5SUM(J,I) = 0.0
   20 CONTINUE
      PSUM(1,I) = 0.0
      PSUM(2,I) = 0.0
      PSUM(3,I) = 0.0
   30 CONTINUE
C
C     SUB-TRIANGLE COMPUTATIONS AND SUMMATIONS.
C
      DO 70 I = 1,4
      IA = MAP(I,1)
      IB = MAP(I,2)
      IC = MAP(I,3)
      IT = EST(8)
      CALL Q2TRMS (RMAT(1,IA),RMAT(1,IB),RMAT(1,IC),ALPHA(1),ISINTH,
     1             ICOSTH,GSUBE,IT,IERROR,2,KMAT,PMAT,DUMMY,DUMMY)
      IF (IERROR) 40,40,140
C
C     SUM IN KCA,KCB,KCC
C
   40 DO 50 K = 1,9
      K5SUM(K,IA) = K5SUM(K,IA) + KMAT(K   )
      K5SUM(K,IB) = K5SUM(K,IB) + KMAT(K+ 9)
      K5SUM(K,IC) = K5SUM(K,IC) + KMAT(K+18)
   50 CONTINUE
C
C     SUM IN PA,PB,PC
C
      DO 60 K = 1,3
      PSUM(K,IA) = PSUM(K,IA) + PMAT(K  )
      PSUM(K,IB) = PSUM(K,IB) + PMAT(K+3)
      PSUM(K,IC) = PSUM(K,IC) + PMAT(K+6)
   60 CONTINUE
C
   70 CONTINUE
C
C     IF -PLANAR- MODIFY THE K5SUM MATRICES.
C
      IF (.NOT.PLANAR) GO TO 90
      DO 80 I = 1,5
      K5SUM(7,I) = 0.0
      K5SUM(8,I) = 0.0
      K5SUM(9,I) =-0.25
   80 CONTINUE
      K5SUM(9,5) = 1.0
C
C     INVERT K   AND NEGATE THE RESULT.
C             55
C
   90 CONTINUE
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (3,K5SUM(1,5),3,DUMMY,0,DETERM,ISING,ITEMP9)
      IF (ISING .EQ. 2) GO TO 140
C
      DO 100 I = 1,9
      K5SUM(I,5) = -K5SUM(I,5)
  100 CONTINUE
C
C     4 (3X1) LOAD VECTORS ARE COMPUTED AND ADDED INTO THE P-VECTOR IN
C     CORE
C
C       G        T   T                  -1      T
C     (P ) = (T ) (E) ((PSUM ) + ((-K  ) (K  )) (PSUM ))
C       I      I            I        55    5I        5
C
      DO 130 I = 1,4
      CALL GMMATS (K5SUM(1,5),3,3,0,K5SUM(1,I),3,3,0,ITEMP9)
      CALL GMMATS (ITEMP9,3,3,1,PSUM(1,5),3,1,0,JTEMP9)
      DO 110 J = 1,3
      PSUM(J,I) = PSUM(J,I) + JTEMP9(J)
  110 CONTINUE
      CALL GMMATS (ET,3,3,1,PSUM(1,I),3,1,0,JTEMP9)
      JTEMP9(4) = 0.0
      JTEMP9(5) = 0.0
      JTEMP9(6) = 0.0
      K = 4*I + 6
      IF (NEST(K) .NE. 0) CALL BASGLB (JTEMP9,JTEMP9,NEST(K+1),NEST(K))
C
C     ADD LOAD TO CORE FOR THIS GRID
C                                   I
      L = NEST(I+1)
      DO 120 J = 1,3
      PG(L) = PG(L) + JTEMP9(J)
      L = L + 1
  120 CONTINUE
C
  130 CONTINUE
      RETURN
C
C     ERROR CONDITIONS
C
  140 WRITE  (IOUTPT,150) UWM,NEST(1)
  150 FORMAT (A25,' 3100, ELEMENT THERMAL LOAD COMPUTATION FOR QDMEM2 ',
     1       'ELEMENT ID =',I9, /5X,'FINDS ILLEGAL GEOMETRY THUS NO ',
     2       'LOADS OUTPUT FOR ELEMENT-ID NOTED.')
      RETURN
      END
