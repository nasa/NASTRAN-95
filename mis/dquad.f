      SUBROUTINE DQUAD (ITYPE)
C
C     THIS ROUTINE GENERATES THE FOLLOWING
C
C     FOUR 6X6 DIFFERENTIAL STIFFNESS MATRICES FOR ONE PIVOT POINT OF
C     A QUADRILATERAL
C
C
C     CALLS FROM THIS ROUTINE ARE MADE TO
C           DTRBSC - BASIC BENDING TRI. ROUTINE.
C           DTRMEM - TRIANGULAR MEMBRANE ROUTINE
C           TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
C           GMMATD - GENERAL MATRIX MULITPLY AND TRANSPOSE ROUTINE
C           DS1B   - INSERTION ROUTINE
C
C
C        ITYPE    = 1             2                       4
C     ECPT INDEX    QUAD1         QUAD2        TRMEM      QUAD4
C     **********    *******       *******      *******    ********
C          1        EL. ID.       EL. ID.      EL. ID.    EL. ID
C          2        SIL1          SIL1         SIL1       SIL1
C          3        SIL2          SIL2         SIL2       SIL2
C          4        SIL3          SIL3         SIL3       SIL3
C          5        SIL4          SIL4         THETA      SIL4
C          6        THETA         THETA        MAT. ID.   MEM.T1
C          7        MAT. ID. 1    MAT. ID.     T          MEM.T2
C          8        T1            T            NSM        MEM.T3
C          9        MAT. ID. 2    NSM          CID1       MEM.T4
C         10        INERTIA I     CID1         X1         THETA
C         11        MAT ID  3     X1           Y1         FLAG FOR 10
C         12        T2            Y1           Z1         GRD OFFSET
C         13        NSM           Z1           CID2       MAT. ID 1
C         14        Z1            CID2         X2         THICKNESS
C         15        Z2            X2           Y2         MAT. ID 2
C         16        CID1          Y2           Z2         INERTIA I
C         17        X1            Z2           CID3       MAT. ID 3
C         18        Y1            CID3         X3         TS/T
C         19        Z1            X3           Y3         NSM
C         20        CID2          Y3           Z3         Z1
C         21        X2            Z3           EL TEMP    Z2
C         22        Y2            CID4         EL DEFORM  MAT. ID 4
C         23        Z2            X4           LOAD TEMP  THETA
C         24        CID3          Y4           U1         FLAG FOR 23
C         25        X3            Z4           V1         INTEGRATION
C         26        Y3            EL TEMP      W1         STRESS ANGLE
C         27        Z3            EL DEFORM    U2         FLAG FOR 26
C         28        CID4          LOAD TEMP    V2         ZOFF1
C         29        X4            U1           W2         CID1
C         30        Y4            V1           U3         X1
C         31        Z4            W1           V3         Y1
C         32        EL TEMP       U2           W3         Z1
C         33        EL DEFORM     V2                      CID2
C         34        LOAD TEMP     W2                      X2
C         35        U1            U3                      Y2
C         36        V1            V3                      Z2
C         37        W1            W3                      CID3
C         38        U2            U4                      X3
C         39        V2            V4                      Y3
C         40        W2            W4                      Z3
C         41        U3                                    CID4
C         42        V3                                    X4
C         43        W3                                    Y4
C         44        U4                                    Z4
C         45        V4                                    EL TEMP
C         46        W4
C         47
C         48                                              U1
C         49                                              V1
C         50                                              W1
C         51                                              U2
C         52                                              V2
C         53                                              W2
C         54                                              U3
C         55                                              V3
C         56                                              W3
C         57                                              U4
C         58                                              V4
C         59                                              W4
C
      INTEGER         SUBSCA        ,SUBSCB        ,SUBSCC
      DOUBLE PRECISION
     1                KOUT          ,TITE          ,DPDUM        ,
     2                TJTE          ,DPDUM2        ,IVECT        ,
     3                D1            ,JVECT         ,D2           ,
     4                KVECT         ,A1            ,KSUM         ,
     5                T             ,XSUBB         ,V            ,
     6                XSUBC         ,VV            ,YSUBC        ,
     7                PROD9         ,TEMP          ,TEMP9        ,
     8                U1            ,H             ,U2           ,
     9                E             ,A             ,TEMP18       ,
     O                REQUIV        ,R             ,SIGXY        ,
     1                SIGX          ,SIGY
      DIMENSION       M(12)         ,NECPT(100)    ,REQUIV(8)    ,
     1                VQ1(3),VQ2(3) ,VQ3(3),VQ4(3) ,A(1)
      CHARACTER       UFM*23        ,UWM*25        ,UIM*29       ,
     1                SFM*25
      COMMON /XMSSG / UFM           ,UWM           ,UIM          ,
     1                SFM
      COMMON /CONDAS/ CONSTS(5)
      COMMON /SYSTEM/ IBUFF         ,NOUT          ,NOGO
      COMMON /MATIN / MATID         ,INFLAG        ,ELTEMP       ,
     1                STRESS        ,SINTH         ,COSTH
      COMMON /MATOUT/ G11           ,G12           ,G13          ,
     1                G22           ,G23           ,G33          ,
     2                RHO           ,ALPHA1        ,ALPHA2       ,
     3                ALP12         ,T SUB 0       ,G SUB E      ,
     4                SIGTEN        ,SIGCOM        ,SIGSHE       ,
     5                G2X211        ,G2X212        ,G2X222
      COMMON /DS1AAA/ NPVT          ,ICSTM         ,NCSTM
      COMMON /DS1AET/ ECPT(100)
      COMMON /DS1ADP/ KOUT(36)      ,TITE(18)      ,TJTE(18)     ,
     1                TEMP18(18)    ,D1(3)         ,D2(3)        ,
     2                A1(3)         ,V(2)          ,VV(2)        ,
     3                PROD9(9)      ,TEMP9(9)      ,H            ,
     4                U1            ,U2            ,DPDUM(1)     ,
     5                TEMP          ,DPDUM2(43)    ,E(18)        ,
     6                SIGX          ,SIGY          ,SIGXY        ,
     7                XSUBB         ,XSUBC         ,YSUBC        ,
     8                KSUM(36)      ,T(9)          ,IVECT(3)     ,
     9                JVECT(3)      ,KVECT(3)      ,R(2,4)       ,
     O                SP1(2)        ,THETA         ,SINANG       ,
     1                COSANG        ,KM            ,NBEGIN       ,
     2                JNOT          ,NPIVOT        ,NSUBC        ,
     3                ISING         ,SUBSCA        ,SUBSCB       ,
     4                SUBSCC        ,NPOINT        ,IPVT
      EQUIVALENCE     (CONSTS(4),DEGRA) , (NECPT(1),ECPT(1))     ,
     2                (REQUIV(1),R(1,1)), (VQ1(1),ECPT(17))      ,
     4                (VQ2(1),ECPT(21)) , (VQ3(1),ECPT(25))      ,
     6                (VQ4(1),ECPT(29)) , (A(1),KOUT(1))
      DATA     M   /  2, 4, 1,   3, 1, 2,   4, 2, 3,   1, 3, 4   /
C
C
C     IF ITYPE = 2, QUAD2 EST DATA IS MOVED AND STORED IN QUAD1 FORMAT
C     IF ITYPE = 4, QUAD4 EST DATA IS MOVED AND STORED IN QUAD1 FORMAT
C
      IF (ITYPE .EQ. 4) GO TO 15
      IF (ITYPE .NE. 2) GO TO 20
C
      DO 10 I = 10,40
      NPOINT = 50 - I
   10 ECPT(NPOINT+6) = ECPT(NPOINT)
C
      ECPT( 9) = ECPT(7)
      ECPT(10) =(ECPT(8)**3.0)/12.0
      ECPT(11) = ECPT(7)
      ECPT(12) = ECPT(8)
      GO TO 20
C
C     QUAD4
C
C     IF NECPT(11)=0, ECPT(10) IS THE MATERIAL PROPERTY ORIENTAION
C     ANGLE THETA. IF IT IS NOT, NECPT(10) IS MATERIAL COORDINATE
C     SYSTEM ID. IN THIS CASE, WE CAN NOT CONTINUE
C
   15 IF (NECPT(11) .NE. 0) GO TO 350
      ECPT(6) = ECPT(10)
      ECPT(7) = ECPT(13)
      ECPT(8) = ECPT(14)
      ECPT(9) = ECPT(15)
      ECPT(10)= ECPT(16)
      ECPT(11)= ECPT(17)
      ECPT(12)= ECPT(14)
      DO 17 I = 16,46
   17 ECPT(I) = ECPT(I+13)
   20 IF (ECPT(8) .EQ. 0.0) RETURN
C
C     CALL BUG (4HQDET,5,ECPT,52-6*ITYPE)
C
C     DETERMINE PIVOT POINT NUMBER
C
      DO 30 I = 1,4
      IF (NPVT .NE. NECPT(I+1)) GO TO 30
      NPIVOT = I
      GO TO 40
   30 CONTINUE
      RETURN
C
   40 THETA  = ECPT(6)*DEGRA
      SINANG = SIN(THETA)
      COSANG = COS(THETA)
C
      IF (NPIVOT-2) 50,50,60
   50 JNOT = NPIVOT + 2
      GO TO 70
   60 JNOT = NPIVOT - 2
C
C     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
C     SUB TRIANGLES.  (2X4) FOR QUADRILATERAL PLATE...
C     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
C
C     ZERO OUT R-MATRIX
C
   70 DO 80 I = 1,8
   80 REQUIV(I) = 0.0D0
C
      DO 90 I = 1,3
      D1(I) = DBLE(VQ3(I)) - DBLE(VQ1(I))
      D2(I) = DBLE(VQ4(I)) - DBLE(VQ2(I))
   90 A1(I) = DBLE(VQ2(I)) - DBLE(VQ1(I))
C
C     NON-NORMALIZED K-VECTOR = D1 CROSS D2
C
      KVECT(1) = D1(2)*D2(3) - D2(2)*D1(3)
      KVECT(2) = D1(3)*D2(1) - D2(3)*D1(1)
      KVECT(3) = D1(1)*D2(2) - D2(1)*D1(2)
C
C     NORMALIZE K-VECTOR
C
      TEMP = DSQRT(KVECT(1)**2 + KVECT(2)**2 + KVECT(3)**2)
      IF (TEMP .EQ. 0.0D0) CALL MESAGE (-30,26,ECPT(1))
      DO 100 I = 1,3
  100 KVECT(I) = KVECT(I)/TEMP
C
C     COMPUTE H = (A1 DOT KVECT) / 2
C
      TEMP = (A1(1)*KVECT(1) + A1(2)*KVECT(2) + A1(3)*KVECT(3))/2.0D0
C
C     I-VECTOR =(A1) - H*(KVECT)    NON-NORMALIZED
C
      DO 110 I = 1,3
  110 IVECT(I) = A1(I) - TEMP*KVECT(I)
C
C     NORMALIZE I-VECTOR
C
      TEMP = DSQRT(IVECT(1)**2 + IVECT(2)**2 + IVECT(3)**2)
      IF (TEMP .EQ. 0.0D0) CALL MESAGE (-30,26,ECPT(1))
      DO 120 I = 1,3
  120 IVECT(I) = IVECT(I)/TEMP
C
C     J-VECTOR = K CROSS I, AND X3 CALCULATION
C
      JVECT(1) = KVECT(2)*IVECT(3) - IVECT(2)*KVECT(3)
      JVECT(2) = KVECT(3)*IVECT(1) - IVECT(3)*KVECT(1)
      JVECT(3) = KVECT(1)*IVECT(2) - IVECT(1)*KVECT(2)
C
C     NORMALIZE J VECTOR TO MAKE SURE
C
      TEMP =  DSQRT(JVECT(1)**2 + JVECT(2)**2 + JVECT(3)**2)
      IF (TEMP .EQ. 0.0D0) CALL MESAGE (-30,26,ECPT(1))
      DO 130 I = 1,3
  130 JVECT(I) = JVECT(I)/TEMP
C
C     X3 GOES INTO R(1,3) = D1 DOT IVECT
C
      R(1,3) = D1(1)*IVECT(1) + D1(2)*IVECT(2) + D1(3)*IVECT(3)
C
C     X2 GOES INTO R(1,2) AND Y3 GOES INTO R(2,3)
C
      R(1,2) = A1(1)*IVECT(1) + A1(2)*IVECT(2) + A1(3)*IVECT(3)
      R(2,3) = D1(1)*JVECT(1) + D1(2)*JVECT(2) + D1(3)*JVECT(3)
C
C     X4 GOES INTO R(1,4) AND Y4 GOES INTO R(2,4)
C
      R(1,4) = D2(1)*IVECT(1) + D2(2)*IVECT(2) + D2(3)*IVECT(3) + R(1,2)
      R(2,4) = D2(1)*JVECT(1) + D2(2)*JVECT(2) + D2(3)*JVECT(3)
C
C     AT THIS POINT, THE COORDINATES OF THE PLATE IN THE ELEMENT
C     SYSTEM ARE STORED IN THE R-MATRIX WHERE THE COLUMN DENOTES THE
C     POINT AND THE ROW DENOTES THE X OR Y COORDINATE FOR ROW 1 OR
C     ROW 2 RESPECTIVELY.
C
C     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT.
C
C     COMPUTE SUB-TRIANGLE COORDINATES
C
C     ZERO OUT KSUM MATRICES
C
      DO 150 I = 1,36
  150 KSUM(I) = 0.0D0
C
      ELTEMP = ECPT(32)
C
C     MOVE ECPT INTO POSITIONS 51-93
C
      DO 160 I = 1,46
  160 ECPT(I+50) = ECPT(I)
C
C     MOVE MISCELLANEOUS VARIABLES INTO TRMEM FORMAT
C
      ECPT( 6) = ECPT( 7)
      ECPT( 7) = ECPT( 8)
      ECPT(21) = ECPT(32)
      ECPT(22) = ECPT(33)
      ECPT(23) = ECPT(34)
C
      DO 240 J = 1,4
      IF (J .EQ. JNOT) GO TO 240
      KM   = 3*J - 3
      IPVT = 0
      DO 190 I = 1,3
      NPOINT = KM+I
      NSUBC  = M(NPOINT)
      IF (NSUBC .EQ. NPIVOT) IPVT = I
      NECPT(I+1) = NECPT(NSUBC+51)
      DO 170 K = 1,4
      NPOINT = 4*(NSUBC-1) + K + 65
      SUBSCA = 4*(I-1) + K + 8
      ECPT(SUBSCA) = ECPT(NPOINT)
  170 CONTINUE
      DO 180 K = 1,3
      NPOINT = 3*(NSUBC-1) + K + 84
      SUBSCA = 3*(I-1) + K + 23
      ECPT(SUBSCA) = ECPT(NPOINT)
  180 CONTINUE
  190 CONTINUE
      IF (IPVT .EQ. 0) GO TO 240
C
      SUBSCA = M(KM+1)
      SUBSCB = M(KM+2)
      SUBSCC = M(KM+3)
C
      DO 200 I = 1,2
      V(I)  = R(I,SUBSCB) - R(I,SUBSCA)
  200 VV(I) = R(I,SUBSCC) - R(I,SUBSCA)
      XSUBB = DSQRT(V(1)**2 + V(2)**2)
      U1    = V(1)/XSUBB
      U2    = V(2)/XSUBB
      XSUBC = U1*VV(1) + U2*VV(2)
      YSUBC = U1*VV(2) - U2*VV(1)
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
      SINTH = SINANG*U1 - COSANG*U2
      COSTH = COSANG*U1 + SINANG*U2
      IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0
C
C     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR TRIANGLE -J-
C
      CALL DTRMEM (3)
      CALL DTRBSC (2,IPVT)
C
C     NOW WE HAVE AT HAND  K   I=NPIVOT,J=1,2,3   THREE 6X6 MATRICES
C                           IJ
C                                STORED AT  A(1) THROUGH A(27)
C
C     MAP THE THE 3X3 S FOR THE PIVOT ROW INTO THE SUMMATION ARRAYS
C
      DO 230 I = 1,3
      NPOINT = 9*I - 8
C
      CALL GMMATD (T,3,3,1, A(NPOINT),3,3,0, TEMP9)
      CALL GMMATD (TEMP9,3,3,0, T,3,3,0, PROD9)
C
C     ADD THIS PRODUCT IN NOW.
C
      NPOINT = KM + I
      NPOINT = 9*M(NPOINT) - 9
      DO 220 K = 1,9
      NPOINT = NPOINT + 1
  220 KSUM(NPOINT) = KSUM(NPOINT) + PROD9(K)/2.0D0
  230 CONTINUE
C
  240 CONTINUE
C
C     CALL BUG (4HQDKD,220,KSUM,72)
C
C     FILL E-MATRIX
C
      DO 250 I = 1,18
  250 E(I)  = 0.0D0
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
      IF (NECPT(4*NPIVOT + 62) .EQ. 0) GO TO 260
      CALL TRANSD (NECPT(4*NPIVOT+62),T)
      CALL GMMATD (T,3,3,1, E( 1),3,3,0, TITE( 1))
      CALL GMMATD (T,3,3,1, E(10),3,3,0, TITE(10))
      GO TO 290
C
  260 DO 270 K = 1,18
  270 TITE(K) = E(K)
C
C     RESTORE ECPT FOR CKECKOUT
C
      DO 280 K = 1,46
  280 ECPT(K) = ECPT(K+50)
C
  290 DO 330 J = 1,4
C
C     TRANSFORMATIONS AND INSERTION
C
      IF (NECPT(4*J+62) .EQ. 0) GO TO 300
      CALL TRANSD (NECPT(4*J+62),T)
      CALL GMMATD (T,3,3,1, E(1),3,3,0,  TJTE( 1))
      CALL GMMATD (T,3,3,1, E(10),3,3,0, TJTE(10))
      GO TO 320
C
  300 DO 310 K = 1,18
  310 TJTE(K) = E(K)
  320 CALL GMMATD (KSUM(9*J-8),3,3,0, TJTE,6,3,1, TEMP18(1))
      CALL GMMATD (TITE(1),6,3,0, TEMP18(1),3,6,0, KOUT(1))
      CALL DS1B (KOUT,NECPT(J+51))
  330 CONTINUE
      RETURN
C
C     COULD NOT CONTINUE
C
  350 WRITE  (NOUT,360) SFM
  360 FORMAT (A25,', DEFFICIENT SOURCE CODE IN DQUAD TO HANDLE CQUAD4 ',
     1       'ELEMENT WITH MATERIAL', /5X,
     2       'PROPERTY COORD. SYSTEM. ANGLE MUST BE SPECIFIED')
      NOGO = 1
      RETURN
      END
