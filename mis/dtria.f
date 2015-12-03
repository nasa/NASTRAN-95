      SUBROUTINE DTRIA (IOPT)
C
C     THIS ROUTINE GENERATES THE FOLLOWING
C
C     THREE 6X6 DIFFERENTIAL STIFFNESS MATRIX PARTITION FOR ONE PIVOT
C     POINT FOR A TRIA1, TRIA2 OR TORA3 ELEMENT.
C
C
C     CALLS FROM THIS ROUTINE ARE MADE TO
C           DTRBSC - BASIC BENDING TRI. ROUTINE.
C           DTRMEM - TRIANGLULAR MEMBRANE ROUTINE
C           TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
C           INVERD - MATRIX INVERSION ROUTINE
C           GMMATD - GENERAL MATRIX MULITPLY AND TRANSPOSE ROUTINE
C           DS1B   - INSERTION ROUTINE
C
C
C          IOPT  = 1               2           3
C     ECPT INDEX   TRIA1           TRIA2       TRIA3       TRMEM
C     **********   *********       ********    ********    ********
C           1      EL ID           EL ID       EL ID        EL ID
C           2      SIL1            SIL1        SIL1         SIL1
C           3      SIL2            SIL2        SIL2         SIL2
C           4      SIL3            SIL3        SIL3         SIL3
C           5      THETA           THETA       MEM T1       THETA
C           6      MAT ID 1        MAT ID      MEM T2       MAT ID
C           7      T1              T           MEM T3       T
C           8      MAT ID 2        NSM         THETA        NSM
C           9      INERTIA I       CID1        FLAG FOR 8   CID1
C          10      MAT ID 3        X1          GRD OFFSET   X1
C          11      T2              Y1          MAT ID1      Y1
C          12      NSM             Z1          THICKNESS    Z1
C          13      Z1              CID2        MAT ID2      CID2
C          14      Z2              X2          INERTIA I    X2
C          15      CID1            Y2          MAT ID 3     Y2
C          16      X1              Z2          TS/T         Z2
C          17      Y1              CID3        NSM          CID3
C          18      Z1              X3          Z1           X3
C          19      CID2            Y3          Z2           Y3
C          20      X2              Z3          MAT ID 4     Z3
C          21      Y2              EL TEMP     THETA        EL TEMP
C          22      Z2                          FLAG FOR 21  EL DEFORM
C          23      CID3                        INTEGRATION  LOAD TEMP
C          24      X3              U1          STRESS ANGLE U1
C          25      Y3              V1          FLAG FOR 24  V2
C          26      Z3              W1          ZOFF1        W3
C          27      EL TEMP         U2          CID1         U2
C          28      EL DEFORM       V2          X1           V2
C          29      EL LOAD TEMP    W2          Y1           W2
C          30      U1 -DISP FOR U1 U3          Z1           U3
C          31      V1 -DISP FOR V1 V3          CID2         V3
C          32      W1 -DISP FOR Z1 W3          X2           W3
C          33      U2 -DISP FOR X2             Y2
C          34      V2 -DISP FOR Y2             Z2
C          35      W2 -DISP FOR Z2             CID3
C          36      U3 -DISP FOR X3             X3
C          37      V3 -DISP FOR Y3             Y3
C          38      W3 -DISP FOR Z3             Z3
C          39                                  EL TEMP
C          40
C          41
C          42                                  U1
C          43                                  V1
C          44                                  W1
C          45                                  U2
C          46                                  V2
C          47                                  W2
C          48                                  U3
C          49                                  V3
C          50                                  W3
C
      INTEGER          SUBSCA        ,SUBSCB        ,SUBSCC        ,
     1                 CID1
      DOUBLE PRECISION
     1                 R             ,D1            ,HABC          ,
     2                 TEMP          ,D2            ,HINV          ,
     3                 KSUM          ,IVECT         ,G             ,
     4                 V             ,JVECT         ,E             ,
     5                 VV            ,KVECT         ,TITE          ,
     6                 XSUBB         ,TEMP9         ,TJTE          ,
     7                 XSUBC         ,PROD9         ,ARR9          ,
     8                 YSUBC         ,U1            ,ARRAY9        ,
     9                 T             ,U2            ,TEMP18        ,
     T                 A             ,TEMP1         ,PROD12        ,
     1                 C1            ,TEMP2         ,HQ            ,
     2                 C2            ,L1            ,Y1            ,
     3                 X1            ,L2            ,Y2            ,
     4                 X2            ,S1            ,DETERM        ,
     5                 S2            ,KOUT          ,S             ,
     6                 REQUIV
      DOUBLE PRECISION SIGX          ,SIGY          ,SIGXY         ,
     1                 STRES         ,DUMTWO
      DIMENSION
     1                 NECPT(100)    ,M(9)          ,REQUIV(8)     ,
     2                 HQ(12)        ,PROD12(12)    ,HABC(18)      ,
     3                 G(36)         ,TITE(18)      ,TJTE(18)      ,
     4                 KOUT(36)      ,TEMP18(18)    ,V1(3)         ,
     5                 V2(3)         ,V3(3)         ,D1(3)         ,
     6                 D2(3)
      CHARACTER        UFM*23        ,UWM*25        ,UIM*29        ,
     1                 SFM*25
      COMMON /XMSSG /  UFM           ,UWM           ,UIM           ,
     1                 SFM
      COMMON /MATIN /  MATID,INFLAG  ,ELTEMP,STRESS ,SINTH,COSTH
      COMMON /MATOUT/  G11,G12,G13   ,G22,G23,G33   ,RHO,ALPHA1    ,
     1                 ALPHA2,ALP12  ,T SUB 0       ,G SUB E       ,
     2                 SIGTEN,SIGCOM ,SIGSHE,G2X211 ,G2X212 ,G2X222
      COMMON /DS1AAA/  NPVT          ,ICSTM         ,NCSTM
      COMMON /DS1AET/  ECPT(100)
      COMMON /DS1ADP/  A(54)         ,S(18)         ,HINV(36)      ,
     1                 T(9)          ,TEMP9(9)      ,PROD9(9)      ,
     2                 ARR9(9)       ,ARRAY9(9)     ,
     3                 E(18)         ,TEMP          ,TEMP1         ,
     4                 TEMP2         ,L1            ,L2            ,
     5                 S1            ,S2            ,C1            ,
     6                 C2            ,X1            ,X2            ,
     7                 Y1            ,Y2            ,DUMTWO(2)     ,
     8                 DETERM        ,SIGX          ,SIGY          ,
     9                 SIGXY         ,XSUBB         ,XSUBC         ,
     T                 YSUBC         ,STRES(3)      ,KSUM(63)      ,
     1                 IVECT(3)      ,JVECT(3)      ,KVECT(3)      ,
     2                 R(2,4)        ,
     3                 V(2)          ,VV(2)         ,U1            ,
     4                 U2            ,NPOINT        ,KM            ,
     5                 SUBSCA        ,SUBSCB        ,SUBSCC        ,
     6                 NPIVOT        ,IPVT          ,THETA         ,
     7                 NSUBB         ,NSUBC         ,ISING         ,
     8                 NPT1          ,SINANG        ,COSANG
      COMMON /CONDAS/  PI            ,TWOPI         ,RADEG         ,
     1                 DEGRA         ,S4PISQ
      COMMON /SYSTEM/  IBUFF         ,NOUT          ,NOGO
      EQUIVALENCE
     1                 (NECPT(1),ECPT(1)) , (PROD12(1),A(13))      ,
     2                 (HABC(1),A(25))    , (TITE(1),A(37))        ,
     3                 (TJTE(1),S( 1))    , (KOUT(1),A(1))         ,
     4                 (TEMP18(1),HINV(1)), (V1(1),ECPT(66))       ,
     5                 (V2(1),ECPT(70))   , (V3(1),ECPT(74))       ,
     6                 (REQUIV(1),R(1,1)) , (D1(1),A(1))           ,
     7                 (D2(1),A(4))       , (HQ(1),A(1))
C
C
      DATA     M    /  1,2,4,  2,3,4,  3,1,4 /,  CID1  / 65        /
C
C
C     THE ECPT DATA IS COPIED TO ECPT(PLUS 50)
C     THE DATA IN ECPT(BELOW 50) IS THEN PUT INTO TRMEM FORMAT TO BE
C     USED BY DTRMEM
C     THE DATA IN ECPT(ABOVE 50, SPECIALLY 51 THRU 62, 65 THRU 88) IS
C     PUT INTO TRIA1 FORMAT, WHICH WILL BE USED BY DTRBSC AND LOCALLY
C
      ICID = CID1 - 4
      DO 10 I = 1,50
   10 ECPT(I+50) = ECPT(I)
      GO TO (15,25,35), IOPT
C
C     TRIA1
C
   15 J = 15
      DO 20 I = 9,32
      ECPT(I) = ECPT(J)
   20 J = J + 1
      GO TO 60
C
C     TRIA2
C
   25 ECPT(58) = ECPT(6)
      ECPT(59) =(ECPT(7)**3)/12.0
      ECPT(60) = ECPT(6)
      ECPT(61) = ECPT(7)
C
      J = 9
      DO 30 I = 65,88
      ECPT(I) = ECPT(J)
   30 J = J + 1
      GO TO 60
C
C     TRIA3
C
C     IF NECPT(9)=0, ECPT(8) IS MATERIAL PROPERTY ORIENTAION ANGLE THETA
C     IF NECPT(9).NE.0, NECPT(8) IS MATERIAL COORDINATE SYSTEM ID. IN
C     THIS CASE, WE CAN NOT CONTINUE (NEED MORE STUFFS TO COMPUTE THETA,
C     SEE SHCSGD)
C
   35 IF (NECPT(9) .NE. 0) GO TO 410
      ECPT(5) = ECPT( 8)
      ECPT(6) = ECPT(11)
      ECPT(7) = ECPT(12)
      J = 27
      DO 40 I = 9,32
      ECPT(I) = ECPT(J)
   40 J = J + 1
C
      ECPT(55) = ECPT(58)
      J = 61
      DO 45 I = 56,60
      ECPT(I) = ECPT(J)
   45 J = J + 1
      ECPT(61) = ECPT(62)
      J = 77
      DO 50 I = 65,88
      ECPT(I) = ECPT(J)
   50 J = J + 1
C
   60 THETA  = ECPT(5)*DEGRA
      SINANG = SIN(THETA)
      COSANG = COS(THETA)
      SINTH  = SINANG
      COSTH  = COSANG
C
      CALL DTRMEM (2)
C
C     SIGX, SIGY , SIGXY ARE NOW AVAILABLE. SAVE THEM.
C
      STRES(1) = SIGX
      STRES(2) = SIGY
      STRES(3) = SIGXY
C
      ELTEMP = ECPT(21)
C
C     DETERMINE PIVOT POINT NUMBER
C
      DO 70 I = 1,3
      IF (NPVT .NE. NECPT(I+1)) GO TO 70
      NPIVOT = I
      GO TO 80
   70 CONTINUE
      RETURN
C
C     FALL THRU ABOVE LOOP IMPLIES ERROR CONDITION
C
   80 CONTINUE
C
C     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
C     SUB TRIANGLES. (2X4) FOR TRIANGULAR PLATE. (COLUMN 4 BLANK)
C     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
C
C     ZERO OUT R-MATRIX
C
      DO 90 I = 1,8
   90 REQUIV(I) = 0.0D0
C
      DO 100 I = 1,3
      D2(I) = DBLE(V2(I)) - DBLE(V1(I))
  100 D1(I) = DBLE(V3(I)) - DBLE(V1(I))
C
C     X2  GOES IN R(1,2)
C
      R(1,2) = DSQRT(D2(1)**2 + D2(2)**2 + D2(3)**2)
      DO 110 I = 1,3
  110 IVECT(I) = D2(I)/R(1,2)
C
C     NON-NORMALIZED K-VECTOR
C
      KVECT(1) = IVECT(2)*D1(3) - D1(2)*IVECT(3)
      KVECT(2) = IVECT(3)*D1(1) - D1(3)*IVECT(1)
      KVECT(3) = IVECT(1)*D1(2) - D1(1)*IVECT(2)
C
C     Y3 GOES INTO R(2,3)
C
      R(2,3) = DSQRT(KVECT(1)**2 + KVECT(2)**2 + KVECT(3)**2)
      DO 120 I = 1,3
  120 KVECT(I) = KVECT(I)/R(2,3)
C
C     J-VECTOR = K X I  VECTORS
C
      JVECT(1) = KVECT(2)*IVECT(3) - IVECT(2)*KVECT(3)
      JVECT(2) = KVECT(3)*IVECT(1) - IVECT(3)*KVECT(1)
      JVECT(3) = KVECT(1)*IVECT(2) - IVECT(1)*KVECT(2)
C
C     NORMALIZE J VECTOR TO MAKE SURE
C
      TEMP = DSQRT(JVECT(1)**2 + JVECT(2)**2 + JVECT(3)**2)
      DO 130 I = 1,3
  130 JVECT(I) = JVECT(I)/TEMP
C
C     X3 GOES INTO R(1,3) = D1 DOT IVECT
C
      R(1,3) = D1(1)*IVECT(1) + D1(2)*IVECT(2) + D1(3)*IVECT(3)
C
C     CENTROID POINT GOES INTO R(1,4) AND R(2,4)
C
      R(1,4) = (R(1,2) + R(1,3))/3.0D0
      R(2,4) = R(2,3)/3.0D0
C
C
C     THE COORDINATES AND CENTROID OF THE PLATE IN THE ELEMENT
C     SYSTEM ARE STORED IN THE R-MATRIX WHERE THE COLUMN DENOTES THE
C     POINT AND THE ROW DENOTES THE X OR Y COORDINATE FOR ROW 1 OR
C     ROW 2 RESPECTIVELY.
C
C
C     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT.
C
C     ZERO OUT THE KSUM MATRIX FOR 63 AND THE GSUM MATRIX FOR 36
C
      DO 140 I = 1,63
  140 KSUM(I) = 0.0D0
      DO 150 I = 1,36
  150 G(I) = 0.0D0
C
      DO 280 J = 1,3
      KM = 3*J - 3
      SUBSCA = M(KM+1)
      SUBSCB = M(KM+2)
      SUBSCC = M(KM+3)
C
      DO 160 I = 1,2
      V(I)  = R(I,SUBSCB) - R(I,SUBSCA)
  160 VV(I) = R(I,SUBSCC) - R(I,SUBSCA)
      XSUBB = DSQRT(V(1)**2 + V(2)**2)
      U1    = V(1)/XSUBB
      U2    = V(2)/XSUBB
      XSUBC = U1*VV(1) + U2*VV(2)
      YSUBC = U1*VV(2) - U2*VV(1)
C
      SINTH = SINANG*U1 - COSANG*U2
      COSTH = COSANG*U1 + SINANG*U2
      IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0
C
C     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR TRIANGLE -J-
C
      C2   = U1**2
      S2   = U2**2
      L1   = U1*U2
      SIGX = C2*STRES(1) + S2*STRES(2) + 2.0D0*L1*STRES(3)
      SIGY = S2*STRES(1) + C2*STRES(2) - 2.0D0*L1*STRES(3)
      SIGXY=-L1*STRES(1)+ L1*STRES(2) + (C2-S2)*STRES(3)
      IPVT = 0
      DO 170 I = 1,2
      NPOINT = KM + I
      IF (M(NPOINT) .EQ. NPIVOT) IPVT = I
  170 CONTINUE
      CALL DTRBSC (1,IPVT)
C
C     NOW WE HAVE 6 MATRICES STORED AT A(1) TO A(54)- HIA,HIB,HIC
C                                                     HAC,HBC,HCC
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
      DO 190 I = 1,3
      CALL GMMATD (T(1),3,3,1, A(9*I+19),3,3,0, TEMP9(1))
      CALL GMMATD (TEMP9(1),3,3,0, T(1),3,3,0,  PROD9(1))
C
C     ADD THIS PRODUCT IN NOW.
C     COMPUTE POINTER TO KSUM MATRIX DESIRED.  (ZERO POINTER)
C
      NPOINT = KM + I
      NPOINT = 9*M(NPOINT) + 18
C
      DO 180 K = 1,9
      NSUBC  = NPOINT + K
  180 KSUM(NSUBC) = KSUM(NSUBC) + PROD9(K)
  190 CONTINUE
      IF (IPVT .EQ. 0) GO TO 220
      DO 210 I = 1,2
      NPOINT = KM +I
      NPOINT = 9*M(NPOINT) -9
C
C     TRANSFORM
C
      CALL GMMATD (T(1),3,3,1, A(9*I-8),3,3,0, TEMP9(1))
      CALL GMMATD (TEMP9(1),3,3,0, T(1),3,3,0, PROD9(1))
C
C     INSERT
C
      DO 200 K = 1,9
      NSUBC = K + NPOINT
  200 KSUM(NSUBC) = KSUM(NSUBC) + PROD9(K)
  210 CONTINUE
  220 CONTINUE
C
C     FORM HQ (2X6)
C
      TEMP1 = XSUBB - XSUBC
      TEMP2 = YSUBC**2
      L1 = DSQRT(XSUBC**2 + TEMP2)
      L2 = DSQRT(TEMP1**2 + TEMP2)
      S1 = XSUBC/L1
      S2 = TEMP1/L2
      C1 = YSUBC/L1
      C2 = YSUBC/L2
      X1 = XSUBC/2.0D0
      Y1 = YSUBC/2.0D0
      X2 = (XSUBB+XSUBC)/2.0D0
      Y2 = Y1
      HQ( 1) =-XSUBC*C1
      HQ( 2) = X1*S1 - Y1*C1
      HQ( 3) = 2.0D0*Y1*S1
      HQ( 4) =-3.0D0*X1*X1*C1
      HQ( 5) = Y1*(2.0D0*X1*S1 - Y1*C1)
      HQ( 6) = 3.0D0*Y1*Y1*S1
      HQ( 7) = 2.0D0*X2*C2
      HQ( 8) = X2*S2 + Y2*C2
      HQ( 9) = 2.0D0*Y2*S2
      HQ(10) = 3.0D0*X2*X2*C2
      HQ(11) = Y2*(2.0D0*X2*S2 + Y2*C2)
      HQ(12) = 3.0D0*Y2*Y2*S2
C
C                      I                    -1
C     COMPUTE (H       I  H     )  = (HQ)(H)    STORE IN PROD12
C               PSI,B  I   PSI,C
C                      I
C
C
      CALL GMMATD (HQ(1),2,6,0, HINV(1),6,6,0, PROD12(1))
C
C
C     COMPUTE (H     ) = -(PROD12)(S)
C               PSI,A
C
      CALL GMMATD (PROD12(1),2,6,0, S(1),6,3,0, HABC(1))
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
      DO 270 I = 1,3
C
C     POINTER TO H  = 6*I-6
C                 I
C
C
C     TRANSFORM H SUB I
C
      CALL GMMATD (HABC(6*I-5),2,3,0, T(1),3,3,0, TEMP9(1))
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
      IF (J-2) 240,230,260
C
  230 NPOINT = NPOINT + 3
  240 DO 250 K = 1,6
      NPOINT = NPOINT + 1
  250 G(NPOINT) = G(NPOINT) + TEMP9(K)
      GO TO 270
  260 G(NPOINT+7) = G(NPOINT+7) + TEMP9(1)
      G(NPOINT+8) = G(NPOINT+8) + TEMP9(2)
      G(NPOINT+9) = G(NPOINT+9) + TEMP9(3)
      G(NPOINT+1) = G(NPOINT+1) + TEMP9(4)
      G(NPOINT+2) = G(NPOINT+2) + TEMP9(5)
      G(NPOINT+3) = G(NPOINT+3) + TEMP9(6)
C
  270 CONTINUE
C
C
C     END OF LOOP FOR BASIC TRIANGLES
C
  280 CONTINUE
C
C
C     FILL E-MATRIX
C
      DO 290 I = 1,18
  290 E(I) = 0.0D0
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
      IF (NECPT(4*NPIVOT+ICID) .EQ. 0) GO TO 300
      CALL TRANSD (NECPT(4*NPIVOT+ICID),T(1))
      CALL GMMATD (T(1),3,3,1, E( 1),3,3,0, TITE( 1))
      CALL GMMATD (T(1),3,3,1, E(10),3,3,0, TITE(10))
      GO TO 320
C
  300 DO 310 K = 1,18
  310 TITE(K) = E(K)
C
C     SOLVE NOW FOR
C
C       E                   T     T                       T
C    (K  ) = (K  ) - (TERM ) (K  ) - (K  )(TERM ) + (TERM )(K  )(TERM )
C      IJ      IJ         I    J4      I4      J         I   44      J
C
C                           -1                               I=NPIVOT
C      WHERE  (TERM ) = (G )  (G ) ,I=NPIVOT                 J=1,2,3
C                  I      4     I
C
C                           -1
C             (TERM ) = (G )  (G ) ,J=1,2,3 AS ABOVE
C                  J      4     J
C
C     AND WITH TRANSFORMATIONS
C
C       G        T      E   T
C    (K  ) = (C ) (E)(K  )(E )(C )
C      IJ      I       IJ       J
C
C
C     COMPUTE  (TERM        )  STORE IN PROD9
C                   I=NPIVOT
C
C                  -1
C     FIRST GET (G )
C                 4
C
  320 CONTINUE
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERD (3,G(28),3,PROD9,0,DETERM,ISING,TEMP9)
C
      CALL GMMATD (G(28),3,3,0, G(9*NPIVOT-8),3,3,0, PROD9(1))
C
C                       T
C     GET  (TERM        )(K  ) -(K  )  STORE IN TEMP9
C               I=NPIVOT   44     I4
C
      CALL GMMATD (PROD9(1),3,3,1, KSUM(55),3,3,0, TEMP9(1))
      DO 340 K = 1,9
      NPOINT = 9*NPIVOT + 18 + K
  340 TEMP9(K) = TEMP9(K) - KSUM(NPOINT)
C
C
C     THE TWO COMMON PRODUCTS ARE NOW AT HAND IN PROD9 AND TEMP9.
C
      DO 400 J = 1,3
C
C                   T     T
C     (TERM        ) (K  )    STORE IN ARR9
C          I=NPIVOT    J4
C
      CALL GMMATD (PROD9(1),3,3,1, KSUM(9*J+19),3,3,1, ARR9(1))
C
C     SUBTRACT FROM (K  )
C                     IJ
C
      NBEGIN = 9*J - 9
      DO 350 I = 1,9
      NPOINT = NBEGIN + I
  350 KSUM(NPOINT) = KSUM(NPOINT) - ARR9(I)
C
C
C      COMPUTE  (TERM )  STORE IN ARR9
C                   J
C
      CALL GMMATD (G(28),3,3,0, G(9*J-8),3,3,0, ARR9(1))
C
C                            T
C     COMPUTE ((TERM        )(K  ) -(K  )) (TERM ) = (TEMP9)(ARR9)
C                   I=NPOINT   44     I4        J
C
      CALL GMMATD (TEMP9(1),3,3,0, ARR9(1),3,3,0, ARRAY9(1))
C
C     ADD TO K
C             IJ
C
      DO 360 I = 1,9
      NPOINT = NBEGIN + I
  360 KSUM(NPOINT) = KSUM(NPOINT) + ARRAY9(I)
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
      IF (NECPT(4*J+ICID) .EQ. 0) GO TO 370
      CALL TRANSD (NECPT(4*J+ICID),T(1))
      CALL GMMATD (T(1),3,3,1, E( 1),3,3,0, TJTE( 1))
      CALL GMMATD (T(1),3,3,1, E(10),3,3,0, TJTE(10))
      GO TO 390
C
  370 DO 380 K = 1,18
  380 TJTE(K) = E(K)
  390 CALL GMMATD (KSUM(NBEGIN+1),3,3,0, TJTE(1),6,3,1, TEMP18(1))
      CALL GMMATD (TITE(1),6,3,0, TEMP18(1),3,6,0, KOUT(1))
      CALL DS1B (KOUT(1),NECPT(J+1))
  400 CONTINUE
      RETURN
C
C     COULD NOT DO IT
C
  410 WRITE  (NOUT,420) SFM
  420 FORMAT (A25,', DEFFICIENT SOURCE CODE IN DTRIA TO HANDLE CTRIA3 ',
     1       'ELEMENT WITH MATERIAL', /5X,
     2       'PROPERTY COORD. SYSTEM. ANGLE MUST BE SPECIFIED')
      NOGO = 1
      RETURN
      END
