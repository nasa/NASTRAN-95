      SUBROUTINE PKTRPL
C  THIR ROUTINE CALCULATES THE STIFFNESS MATRIX FOR TRI-PLATES IN  PLA4
C
C     THIS ROUTINE GENERATES THE FOLLOWING
C
C                             3-6X6 STIFFNESS MATRICES WITH RESPECT
C                             TO ONE PIVOT POINT OF A TRIANGULAR PLATE
C                             ELEMENT.
C
C         REF. FMMS-55  NOVEMBER 1ST, 1967
C
C         CALLS FROM THIS ROUTINE ARE MADE TO
C                             PKTRBS - BASIC BENDING TRIANGLE
C                             TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
C                             INVERD - MATRIX INVERSION ROUTINE
C                             PLA4B  - INSERTION ROUTINE
C                             GMMATD - GENERAL MATRIX MULITPLY AND
C                                      TRANSPOSE ROUTINE
C                             MESAGE - ERROR MESSAGE WRITER
C
C
      INTEGER            SUBSCA        ,SUBSCB        ,SUBSCC
      DOUBLE PRECISION
     1                   R             ,D1            ,HABC
     2                  ,TEMP          ,D2            ,HINV
     3                  ,KSUM          ,IVECT         ,G
     4                  ,V             ,JVECT         ,E
     5                  ,VV            ,KVECT         ,TITE
     6                  ,XSUBB         ,TEMP9         ,TJTE
     7                  ,XSUBC         ,PROD9         ,ARR9
     8                  ,YSUBC         ,U1            ,ARRAY9
     9                  ,T             ,U2            ,TEMP18
     T                  ,A             ,TEMP1         ,PROD12
     1                  ,C1            ,TEMP2         ,HQ
     2                  ,C2            ,L1            ,Y1
     3                  ,X1            ,L2            ,Y2
     4                  ,X2            ,S1            ,DETERM
     5                  ,S2            ,KOUT          ,S ,REQUIV
C     ******************************************************************
C
C     ECPT LISTS AS OF AUGUST 4, 1967
C
C                 DEFINITION
C       ECPT      TRI.PLATE AND BASIC BENDING TRI.
C     ******************************************************************
C     ECPT( 1) = ELEMENT ID         INTEGER
C     ECPT( 2) = GRID PT. A         INTEGER
C     ECPT( 3) = GRID PT. B         INTEGER
C     ECPT( 4) = GRID PT. C         INTEGER
C     ECPT( 5) = THETA              REAL
C     ECPT( 6) = MAT ID 1           INTEGER
C     ECPT( 7) = I  MOM. OF INERT.  REAL
C     ECPT( 8) = MAT ID 2           INTEGER
C     ECPT( 9) = T2                 REAL
C     ECPT(10) = NON-STRUCT. MASS   REAL
C     ECPT(11) = Z1                 REAL
C     ECPT(12) = Z2                 REAL
C     ECPT(13) = COORD. SYS. ID 1   INTEGER
C     ECPT(14) = X1                 REAL
C     ECPT(15) = Y1                 REAL
C     ECPT(16) = Z1                 REAL
C     ECPT(17) = COORD. SYS. ID 2   INTEGER
C     ECPT(18) = X2                 REAL
C     ECPT(19) = Y2                 REAL
C     ECPT(20) = Z2                 REAL
C     ECPT(21) = COORD. SYS. ID 3   INTEGER
C     ECPT(22) = X3                 REAL
C     ECPT(23) = Y3                 REAL
C     ECPT(24) = Z3                 REAL
C     ECPT(25) = ELEMENT TEMP       REAL
C     ******************************************************************
      DIMENSION
     1                   NECPT(100)    ,M(9)          ,REQUIV(8)
     2                  ,HQ(12)        ,PROD12(12)    ,HABC(18)
     3                  ,G(36)         ,TITE(18)      ,TJTE(18)
     4                  ,KOUT(36)      ,TEMP18(18)    ,V1(3)
     5                  ,V2(3)         ,V3(3)         ,R(2,4)
     6                  ,D1(3)         ,D2(3)
C
      COMMON /CONDAS/ CONSTS(5)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALP12,
     1                T SUB 0, G SUB E, SIGTEN, SIGCOM, SIGSHE,
     2                G2X211, G2X212, G2X222
      COMMON /PLA42C/ NPVT, DUM1(3)
     1,                  DUMCL(145)         ,NOGO
      COMMON /PLA4ES/ ECPT(100)
      COMMON /PLA42D/
     1                   A(81)         ,S(18)         ,T(9)
     2                  ,TEMP9(9)      ,PROD9(9)      ,ARR9(9)
     3                  ,ARRAY9(9)     ,HINV(36)      ,KSUM(63)
     4                  ,XSUBB         ,XSUBC         ,YSUBC
     5                  ,E(18)         ,TEMP          ,L1
     6                  ,L2            ,S1            ,S2
     7                  ,C1            ,C2            ,X1
     8                  ,X2            ,Y1            ,Y2
     9                  ,TEMP1         ,TEMP2         ,DUMTWO(2) ,DETERM
     T                  ,NPOINT        ,KM            ,SUBSCA
     1                  ,SUBSCB        ,SUBSCC        ,NPIVOT
     2                  ,THETA         ,NSUBC         ,ISING
     3                  ,NPT1          ,V(2)          ,VV(2)
     4                  ,IVECT(3)      ,JVECT(3)      ,KVECT(3)
     5                  ,U1            ,U2            ,SINANG
     6                  ,COSANG
C
      EQUIVALENCE ( CONSTS(4) , DEGRA  )
      EQUIVALENCE
     1                                 (NECPT(1),ECPT(1))
     2                                 ,(PROD12(1),A(13))
     3                                 ,(HABC(1),A(25))
     4                                 ,(TITE(1),A(37))
     5                                 ,(TJTE(1),A(55))
     6                                 ,(KOUT(1),A(1))
     7                                 ,(TEMP18(1),HINV(1))
     8                                 ,(V1(1),ECPT(14))
     9                                 ,(V2(1),ECPT(18))
     T                                 ,(V3(1),ECPT(22))
     1                                 ,(REQUIV(1),R(1,1))
     2                                 ,(D1(1),A(1))
     3                                 ,(D2(1),A(4))
     4                                 ,(HQ(1),A(1))
C
      DATA M/ 1,2,4,   2,3,4,   3,1,4 /
C
C     DETERMINE PIVOT POINT NUMBER
C
      DO 10 I=1,3
      IF( NPVT .NE. NECPT(I+1) ) GO TO 10
      NPIVOT = I
      GO TO 20
   10 CONTINUE
C
C
C     FALL THRU ABOVE LOOP IMPLIES ERROR CONDITION
      CALL MESAGE(-30,34,ECPT(1))
C
   20 THETA = ECPT(5) * DEGRA
      SINANG = SIN( THETA )
      COSANG = COS( THETA )
C     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
C     SUB TRIANGLES. (2X4) FOR TRIANGULAR PLATE. (COLUMN 4 BLANK)
C     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
C
C     ZERO OUT R-MATRIX
      DO 30 I=1,8
   30 REQUIV(I)=0.0D0
C
      DO 40 I=1,3
      D2(I) = DBLE( V2(I) ) - DBLE( V1(I) )
   40 D1(I) = DBLE( V3(I) ) - DBLE( V1(I) )
C
C     X2  GOES IN R(1,2)
      R(1,2) = DSQRT ( D2(1)**2  +  D2(2)**2  +  D2(3)**2 )
      IF (R(1,2).EQ.0.0D0) GO TO 370
      DO 50 I=1,3
   50 IVECT(I) = D2(I) / R(1,2)
C
C     NON-NORMALIZED K-VECTOR
      KVECT(1) = IVECT(2) * D1(3)    -    D1(2) * IVECT(3)
      KVECT(2) = IVECT(3) * D1(1)    -    D1(3) * IVECT(1)
      KVECT(3) = IVECT(1) * D1(2)    -    D1(1) * IVECT(2)
C
C     Y3 GOES INTO R(2,3)
      R(2,3) = DSQRT ( KVECT(1)**2 + KVECT(2)**2 + KVECT(3)**2 )
      IF (R(2,3).EQ.0.0D0) GO TO 370
      DO 60 I=1,3
   60 KVECT(I) = KVECT(I) / R(2,3)
C
C     J-VECTOR = K X I  VECTORS
      JVECT(1) = KVECT(2) * IVECT(3) - IVECT(2) * KVECT(3)
      JVECT(2) = KVECT(3) * IVECT(1) - IVECT(3) * KVECT(1)
      JVECT(3) = KVECT(1) * IVECT(2) - IVECT(1) * KVECT(2)
C     NORMALIZE J VECTOR TO MAKE SURE
      TEMP = DSQRT ( JVECT(1)**2 + JVECT(2)**2 + JVECT(3)**2 )
      IF (TEMP.EQ.0.0D0) GO TO 370
      DO 70 I=1,3
   70 JVECT(I) = JVECT(I) / TEMP
C     X3 GOES INTO R(1,3) = D1 DOT IVECT
      R(1,3) = D1(1) * IVECT(1)  +  D1(2) * IVECT(2) + D1(3) * IVECT(3)
C
C     CENTROID POINT GOES INTO R(1,4) AND R(2,4)
      R(1,4) = (  R(1,2) + R(1,3)  ) / 3.0D0
      R(2,4) = R(2,3) / 3.0D0
C     ******************************************************************
C            THE COORDINATES AND CENTROID OF THE PLATE IN THE ELEMENT
C     SYSTEM ARE STORED IN THE R-MATRIX WHERE THE COLUMN DENOTES THE
C     POINT AND THE ROW DENOTES THE X OR Y COORDINATE FOR ROW 1 OR
C     ROW 2 RESPECTIVELY.
C     ******************************************************************
C
C     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT.
C
C     ******************************************************************
C     ZERO OUT THE KSUM MATRIX FOR 63 AND THE GSUM MATRIX FOR 36...
C
      DO 80 I=1,63
   80 KSUM(I) = 0.0D0
      DO 90 I=1,36
   90 G(I) = 0.0D0
C
C
      DO 210 J=1,3
      KM = 3*J - 3
C
      SUBSCA = M(KM+1)
      SUBSCB = M(KM+2)
      SUBSCC = M(KM+3)
C
      DO 100 I=1,2
      V(I) = R(I,SUBSCB) - R(I,SUBSCA)
  100 VV(I)= R(I,SUBSCC) - R(I,SUBSCA)
      XSUBB = DSQRT ( V(1)**2 + V(2)**2 )
      U1 = V(1) / XSUBB
      U2 = V(2) / XSUBB
      XSUBC = U1 * VV(1) + U2 * VV(2)
      YSUBC = U1 * VV(2) - U2 * VV(1)
C
      SINTH = SINANG * U1  -  COSANG * U2
      COSTH = COSANG * U1  +  SINANG * U2
      IF(ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0E0
C
C     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR
C     TRIANGLE -J-
C
      CALL PKTRBS(2)
C                         U
C     NOW HAVE AT HAND  K    I,J, =1,2,3.   9-3X3 MATRICES STORED AT
C                        IJ                 A(1) THROUGH A(81).
C
C           -1
C     ALSO H   (6X6) AT A(145) TO A(181) AND S (6X3) AT A(82) TO A(99)
C
C     NOW ADD CERTAIN OF THESE INTO THE SUMMED MATRICES
C
C
C     SET UP OF T-MATRIX
C
      T(1) = 1.0D0
      T(2) = 0.0D0
      T(3) = 0.0D0
      T(4) = 0.0D0
      T(5) = U1
      T(6) = U2
      T(7) = 0.0D0
      T(8) =-U2
      T(9) = U1
C
      DO 120 I=1,3
      CALL GMMATD( T(1),3,3,1,  A(27*I-8),3,3,0,  TEMP9(1) )
      CALL GMMATD( TEMP9(1),3,3,0,  T(1),3,3,0,  PROD9(1) )
C
C     ADD THIS PRODUCT IN NOW.
C     COMPUTE POINTER TO KSUM MATRIX DESIRED.  (ZERO POINTER)
      NPOINT = KM + I
      NPOINT = 9*M(NPOINT) + 18
C
      DO 110 K=1,9
      NSUBC  = NPOINT + K
  110 KSUM(NSUBC) = KSUM(NSUBC) + PROD9(K)
  120 CONTINUE
      DO 150 K=1,2
      NPOINT = KM + K
      IF( M(NPOINT) .NE. NPIVOT ) GO TO 150
      CALL GMMATD( T(1),3,3,1,  A(36*K-35),3,3,0,  TEMP9(1) )
      CALL GMMATD( TEMP9(1),3,3,0,  T(1),3,3,0,  PROD9(1) )
C
C     COMPUTE POINTER TO KSUM MATRIX (ZERO POINTER)
C
      NPOINT = 9 * NPIVOT - 9
      DO 130 I=1,9
      NSUBC = NPOINT + I
  130 KSUM(NSUBC) = KSUM(NSUBC) + PROD9(I)
C
      CALL GMMATD(T(1),3,3,1,  A(18*K-8),3,3,0,  TEMP9(1) )
      CALL GMMATD( TEMP9(1),3,3,0,  T(1),3,3,0,  PROD9(1) )
C
C     COMPUTE ZERO POINTER TO KSUM MATRIX DESIRED
C
      NPOINT = KM + 3 - K
      NPOINT = 9 * M(NPOINT) - 9
      DO 140 I=1,9
      NSUBC = NPOINT + I
  140 KSUM(NSUBC) = KSUM(NSUBC) +  PROD9(I)
  150 CONTINUE
C
C     FORM HQ (2X6)
C
      TEMP1 = XSUBB - XSUBC
      TEMP2 = YSUBC ** 2
      L1 = DSQRT( XSUBC**2 + TEMP2 )
      L2 = DSQRT( TEMP1**2 + TEMP2 )
      S1 = XSUBC / L1
      S2 = TEMP1 / L2
      C1 = YSUBC / L1
      C2 = YSUBC / L2
      X1 = XSUBC / 2.0D0
      Y1 = YSUBC / 2.0D0
      X2 = (XSUBB + XSUBC) / 2.0D0
      Y2 = Y1
      HQ( 1) = -XSUBC * C1
      HQ( 2) = X1 * S1 - Y1 * C1
      HQ( 3) = 2.0D0 * Y1 * S1
      HQ( 4) = -3.0D0 * X1 * X1 * C1
      HQ( 5) = Y1 * (2.0D0 * X1 * S1 - Y1 * C1 )
      HQ( 6) = 3.0D0 * Y1 * Y1 * S1
      HQ( 7) = 2.0D0 * X2 * C2
      HQ( 8) = X2 * S2  + Y2 * C2
      HQ( 9) = 2.0D0 * Y2 * S2
      HQ(10) = 3.0D0 * X2 * X2 * C2
      HQ(11) = Y2 * ( 2.0D0 * X2 * S2 + Y2 * C2 )
      HQ(12) = 3.0D0 * Y2 * Y2 * S2
C
C                      I                    -1
C     COMPUTE (H       I  H     )  = (HQ)(H)    STORE IN PROD12
C               PSI,B  I   PSI,C
C                      I
C
C
      CALL GMMATD( HQ(1),2,6,0, HINV(1),6,6,0,  PROD12(1) )
C
C
C     COMPUTE (H     ) = -(PROD12)(S)
C               PSI,A
C
      CALL GMMATD( PROD12(1),2,6,0, S(1),6,3,0,  HABC(1) )
C
      HABC(1) = -HABC(1)
      HABC(2) = -HABC(2) + S1
      HABC(3) = -HABC(3) + C1
      HABC(4) = -HABC(4)
      HABC(5) = -HABC(5) + S2
      HABC(6) = -HABC(6) - C2
C
C     SPLIT (H     ) AND (H     )    PARTITION
C             PSI,B        PSI,C
C
      HABC( 7) = PROD12( 1)
      HABC( 8) = PROD12( 2)
      HABC( 9) = PROD12( 3)
      HABC(10) = PROD12( 7)
      HABC(11) = PROD12( 8)
      HABC(12) = PROD12( 9)
      HABC(13) = PROD12( 4)
      HABC(14) = PROD12( 5)
      HABC(15) = PROD12( 6)
      HABC(16) = PROD12(10)
      HABC(17) = PROD12(11)
      HABC(18) = PROD12(12)
C
C     MAP  H , H , AND H  INTO THE G-MATRICES.
C           A   B       C
C
C     TRIANGLE NUMBER = J, THE THREE POINTS ARE SUBSCA, SUBSCB, SUBSCC.
C
      DO 200 I=1,3
C
C     POINTER TO H  = 6*I-6
C                 I
C
C
C     TRANSFORM H SUB I
C
      CALL GMMATD( HABC(6*I-5),2,3,0,  T(1),3,3,0,  TEMP9(1) )
C
C
      NPOINT = KM + I
      NPOINT = 9*M(NPOINT) - 9
C
C     J = 1    ROW 1 OF H INTO ROW 1 OF G.
C              ROW 2 OF H INTO ROW 2 OF G.
C     J = 2    ROW 1 OF H INTO ROW 2 OF G.
C              ROW 2 OF H INTO ROW 3 OF G.
C     J = 3    ROW 1 OF H INTO ROW 3 OF G.
C              ROW 2 OF H INTO ROW 1 OF G.
C
      IF( J-2 ) 170,160,190
C
  160 NPOINT = NPOINT + 3
  170 DO 180 K=1,6
      NPOINT = NPOINT + 1
  180 G(NPOINT) = G(NPOINT) + TEMP9(K)
      GO TO 200
  190 G(NPOINT + 7) = G(NPOINT + 7) + TEMP9(1)
      G(NPOINT + 8) = G(NPOINT + 8) + TEMP9(2)
      G(NPOINT + 9) = G(NPOINT + 9) + TEMP9(3)
      G(NPOINT + 1) = G(NPOINT + 1) + TEMP9(4)
      G(NPOINT + 2) = G(NPOINT + 2) + TEMP9(5)
      G(NPOINT + 3) = G(NPOINT + 3) + TEMP9(6)
C
  200 CONTINUE
C
C
C     END OF LOOP FOR BASIC TRIANGLES
C
  210 CONTINUE
C     ******************************************************************
C
C     FILL E-MATRIX
C
      DO 220 I=1,18
  220 E(I) = 0.0D0
      E( 1) = KVECT(1)
      E( 4) = KVECT(2)
      E( 7) = KVECT(3)
      E(11) = IVECT(1)
      E(14) = IVECT(2)
      E(17) = IVECT(3)
      E(12) = JVECT(1)
      E(15) = JVECT(2)
      E(18) = JVECT(3)
C
C              T
C     FORM   T   E      STORE IN TITE-MATRIX (6X3)
C             I
C
      IF( NECPT(4*NPIVOT+9) .EQ. 0 ) GO TO 230
      CALL TRANSD( NECPT(4*NPIVOT+9), T(1) )
      CALL GMMATD( T(1),3,3,1,  E( 1),3,3,0,  TITE( 1) )
      CALL GMMATD( T(1),3,3,1,  E(10),3,3,0,  TITE(10) )
      GO TO 250
  230 DO 240 K=1,18
  240 TITE(K) = E(K)
C
C     SOLVE NOW FOR ....
C
C    E                   T     T                       T
C (K  ) = (K  ) - (TERM ) (K  ) - (K  )(TERM ) + (TERM )(K  )(TERM )
C   IJ      IJ         I    J4      I4      J         I   44      J
C
C                        -1                               I=NPIVOT
C WHERE... (TERM ) = (G )  (G ) ,I=NPIVOT                 J=1,2,3
C               I      4     I
C
C                        -1
C          (TERM ) = (G )  (G ) ,J=1,2,3 AS ABOVE
C               J      4     J
C
C     AND WITH TRANSFORMATIONS....
C
C    G        T      E   T
C (K  ) = (C ) (E)(K  )(E )(C )
C   IJ      I       IJ       J
C
C
C     COMPUTE  (TERM        )  STORE IN PROD9
C                   I=NPIVOT
C
C                   -1
C     FIRST GET (G )
C                 4
C
  250 CONTINUE
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ISING = -1
      CALL INVERD( 3,G(28),3,PROD9,0,DETERM,ISING,TEMP9 )
C
C     CHECK FOR SINGULARITY. ISING=2 IMPLIES SINGULARITY.
      GO TO(270,260),ISING
  260 CALL MESAGE(30,36,ECPT(1))
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
      NOGO=1
      RETURN
C
  270 CALL GMMATD ( G(28),3,3,0,  G(9*NPIVOT-8),3,3,0,  PROD9(1) )
C
C                       T
C     GET  (TERM        )(K  )  STORE IN TEMP9
C               I=NPIVOT   44
C
      CALL GMMATD( PROD9(1),3,3,1,  KSUM(55),3,3,0,  TEMP9(1) )
C
C
C
C     THE TWO COMMON PRODUCTS ARE NOW AT HAND IN PROD9 AND TEMP9.
C
      DO 360 J=1,3
C
C                   T     T
C     (TERM        ) (K  )    STORE IN ARR9
C          I=NPIVOT    J4
C
      CALL GMMATD( PROD9(1),3,3,1,  KSUM(9*J+19),3,3,1,  ARR9(1) )
C
C     SUBTRACT FROM (K  )
C                     IJ
C
      NBEGIN = 9*J-9
      DO 280 I=1,9
      NPOINT = NBEGIN + I
  280 KSUM(NPOINT) = KSUM(NPOINT) - ARR9(I)
C
C
C      COMPUTE  (TERM )  STORE IN ARR9
C                   J
C
      CALL GMMATD( G(28),3,3,0,  G(9*J-8),3,3,0,  ARR9(1) )
C
C
C     GET  (K  )(TERM )  STORE IN ARRAY9
C            I4      J
C
      CALL GMMATD( KSUM(9*NPIVOT+19),3,3,0,  ARR9(1),3,3,0,  ARRAY9(1))
C
C     SUBTRACT FROM KIJ
C
      DO 290 I=1,9
      NPOINT = NBEGIN + I
  290 KSUM(NPOINT) = KSUM(NPOINT) - ARRAY9(I)
C
C                           T
C     COMPUTE  (TERM        )(K  )(TERM ) = (TEMP9)(ARR9)
C                   I=NPOINT   44      J
C
      CALL GMMATD( TEMP9(1),3,3,0,  ARR9(1),3,3,0,  ARRAY9(1) )
C
C     ADD TO K
C             IJ
C
      DO 300 I=1,9
      NPOINT = NBEGIN + I
  300 KSUM(NPOINT) = KSUM(NPOINT) + ARRAY9(I)
C
C       E
C     K    COMPLETE
C      IJ
C
C     TRANSFORM NOW, AND INSERT.
C
C
C     TRANSFORMATIONS AND INSERTION
C
      IF( NECPT(4*J+9) .EQ. 0) GO TO 310
      CALL TRANSD( NECPT(4*J+9), T(1) )
      CALL GMMATD( T(1),3,3,1,  E( 1),3,3,0,  TJTE( 1) )
      CALL GMMATD( T(1),3,3,1,  E(10),3,3,0,  TJTE(10) )
      GO TO 330
  310 DO 320 K=1,18
  320 TJTE(K) = E(K)
  330 CALL GMMATD( KSUM(NBEGIN+1),3,3,0,  TJTE(1),6,3,1,  TEMP18(1) )
      CALL GMMATD ( TITE(1),6,3,0,  TEMP18(1),3,6,0,  KOUT(1))
      CALL PLA4B(KOUT(1),NECPT(J+1))
C
  360 CONTINUE
      RETURN
  370 CALL MESAGE(30,26,ECPT(1))
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
      NOGO=1
      RETURN
      END
