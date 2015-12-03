      SUBROUTINE DROD
C*****
C THIS ROUTINE COMPUTES THE TWO 6 X 6 MATRICES  K(NPVT,NPVT) AND
C K(NPVT,J) FOR A ROD HAVING END POINTS NUMBERED NPVT AND J.
C*****
C
C
C
C                        E C P T  F O R  T H E  R O D
C
C
C
C                                                                CARD
C                                                 TYPE   TABLE   TYPE
C ECPT( 1)ELEMENT ID.                               I     ECT    CROD
C ECPT( 2)SCALAR INDEX NUMBER FOR GRID POINT A      I     ECT    CROD
C ECPT( 3)SCALAR INDEX NUMBER FOR GRID POINT B      I     ECT    CROD
C ECPT( 4)MATERIAL ID.                              I     EPT    PROD
C ECPT( 5)AREA  (A)                                 R     EPT    PROD
C ECPT( 6)POLAR MOMENT OF INERTIA (J)               R     EPT    PROD
C ECPT( 7) TORSIONAL STRESS COEFF (C)                R    EPT    PROD
C ECPT( 8) NON-STRUCTRAL MASS (MU)                   R    EPT    PROD
C ECPT( 9) COOR. SYS. ID. NO. FOR GRID POINT A       I   BGPDT   GRID
C ECPT(10) X-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
C ECPT(11) Y-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
C ECPT(12) Z-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
C ECPT(13) COOR. SYS. ID. NO. FOR GRID POINT B       I   BGPDT
C ECPT(14) X-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
C ECPT(15) Y-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
C ECPT(16) Z-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
C ECPT(17) ELEMENT TEMPERATURE
C ECPT(18) ELEMENT DEFORMATION
C ECPT(19) AVERAGE ELEMENT LOADING TEMPERATURE
C ECPT(20)                ...
C ECPT(21) DISPLACEMENT COOR. FOR GRID PT. A
C ECPT(22)                ...
C ECPT(23)                ...
C ECPT(24) DISPLACEMENT COOR. FOR GRID PT. B
C ECPT(25)                ...
C
C
C
      DOUBLE PRECISION
     1                   DZ(1)              ,X
     2,                  Y                  ,Z
     3,                  XL                 ,XN(3)
     4,                  KE(36)             ,TA(9)
     5,                  TB(9)              ,A
     6,                  E                  ,ALPHA
     7,                  TSUB0              ,UA(6)
     8,                  UB(6)              ,DIFF(3)
     9,                  DPTERM             ,DELTA
     T,                  AVGLTP             ,FX
     1,                  XM(3)              ,YYT(18)
     2,                  ZZT(9)             ,YVEC(3)
     3,                  ZVEC(3)            ,D(6)
     4,                  YL                 ,ZL
     5,                  GX
C
C
C
      DIMENSION
     1                   IZ(1)              ,IECPT(19)
C
C DS1A VARIABLE CORE
C
      COMMON   /ZZZZZZ/  RZ(1)
C
C DS1A COMMON BLOCK
C
      COMMON   /DS1AAA/  NPVT
     1,                  ICSTM              ,NCSTM
     2,                  IGPCT              ,NGPCT
     3,                  IPOINT             ,NPOINT
     4,                  I6X6K              ,N6X6K
     5,                  CSTM               ,MPT
     6,                  DIT                ,ECPTDS
     7,                  GPCT               ,KGGD
     8,                  INRW               ,OUTRW
     9,                  EOR                ,NEOR
     T,                  CLSRW
     1,                  JMAX               ,FROWIC
     2,                  LROWIC             ,NROWSC
     3,                  NLINKS             ,LINK(10)
     4,                  NOGO
C
C ECPT COMMON BLOCK
C
      COMMON   /DS1AET/  ECPT(100)
C
C DS1A LOCAL VARIABLE (SCRATCH) BLOCK
C
      COMMON   /DS1ADP/
     1                   X                  ,Y
     2,                  Z                  ,XL
     3,                  XN                 ,KE
     4,                  TA                 ,TB
     5,                  A                  ,E
     6,                  ALPHA              ,T SUB 0
     7,                  UA                 ,UB
     8,                  DIFF               ,DPTERM
     9,                  DELTA              ,AVGLTP
     T,                  FX                 ,XM
     1,                  YYT
     2,                  YVEC               ,ZVEC
     3,                  YL                 ,ZL
C
C INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON   /MATIN/
     1                   MATIDC             ,MATFLG
     2,                  ELTEMP             ,STRESS
     3,                  SINTH              ,COSTH
C
C
C
      COMMON   /MATOUT/
     1                   ES                 ,G
     2,                  NU                 ,RHO
     3,                  ALPHA S            ,T SUB 0 S
     4,                  GSUBE              ,SIGT
     5,                  SIGC               ,SIGS
C
C
C
      EQUIVALENCE
     1                   (RZ(1),IZ(1),DZ(1)),(ECPT(1),IECPT(1))
     2,                  (ZZT(1),YYT(10))
C
C BEGIN EXECUTION
C
      IF (IECPT(2) .EQ. NPVT) GO TO 10
      IF (IECPT(3) .NE. NPVT) CALL MESAGE (-30,34,IECPT(1))
      ITEMP = IECPT(2)
      IECPT(2) = IECPT(3)
      IECPT(3) = ITEMP
      KA  = 13
      KB  =  9
      IDISPA = 22
      IDISPB = 19
      GO TO 20
   10 KA  =  9
      KB  =  13
      IDISPA = 19
      IDISPB = 22
C
C AT THIS POINT KA POINTS TO THE COOR. SYS. ID. OF THE PIVOT GRID POINT.
C SIMILARLY FOR KB AND THE NON-PIVOT GRID POINT.
C NOW COMPUTE THE LENGTH OF THE ROD.
C
C
C WE STORE THE COORDINATES IN THE D ARRAY SO THAT ALL ARITHMETIC WILL BE
C DOUBLE PRECISION
C
   20 D(1) = ECPT(KA+1)
      D(2) = ECPT(KA+2)
      D(3) = ECPT(KA+3)
      D(4) = ECPT(KB+1)
      D(5) = ECPT(KB+2)
      D(6) = ECPT(KB+3)
      X    = D(1) - D(4)
      Y    = D(2) - D(5)
      Z    = D(3) - D(6)
      XL = DSQRT (X**2 + Y**2 + Z**2)
      IF (XL.EQ.0.0D0) GO TO 120
C
C CALCULATE A NORMALIZED DIRECTION VECTOR IN BASIC COORDINATES.
C
      XN(1) = X / XL
      XN(2) = Y / XL
      XN(3) = Z / XL
C
C CALL SUBROUTINE MAT TO FETCH MATERIAL PROPERTIES.
C
      MATIDC = IECPT(4)
      MATFLG = 1
      ELTEMP = ECPT(17)
      CALL MAT (IECPT(1))
C
C STORE DISPLACEMENT VECTORS IN DOUBLE PRECISION LOCATIONS
C
      UA(1) = ECPT(IDISPA+1)
      UA(2) = ECPT(IDISPA+2)
      UA(3) = ECPT(IDISPA+3)
      UB(1) = ECPT(IDISPB+1)
      UB(2) = ECPT(IDISPB+2)
      UB(3) = ECPT(IDISPB+3)
C
C
C COMPUTE THE DIFFERENCE VECTOR DIFF =  T  * U   -  T  * U
C                                        A    A      B    B
C
      IBASEA = 0
      IF (IECPT(KA) .EQ. 0) GO TO 30
      CALL TRANSD (ECPT(KA),TA)
      IBASEA = 3
      CALL GMMATD (TA,3,3,0, UA(1),3,1,0, UA(4))
   30 IBASEB = 0
      IF (IECPT(KB) .EQ. 0) GO TO 40
      CALL TRANSD (ECPT(KB),TB)
      IBASEB = 3
      CALL GMMATD (TB,3,3,0, UB(1),3,1,0, UB(4))
   40 DIFF(1) = UA(IBASEA+1) - UB(IBASEB+1)
      DIFF(2) = UA(IBASEA+2) - UB(IBASEB+2)
      DIFF(3) = UA(IBASEA+3) - UB(IBASEB+3)
C
C COMPUTE DOT PRODUCT XN . DIFF
C
      CALL GMMATD (XN,3,1,1, DIFF,3,1,0, DPTERM)
C
C COMPUTE AXIAL FORCE FX, AND TORSIONAL FORCE GX
C
      DELTA = ECPT(18)
      FX = DPTERM - DELTA
      IF (IECPT(19) .EQ. (-1)) GO TO 50
      T SUB 0 = T SUB 0 S
      ALPHA = ALPHA S
      AVGLTP = ECPT(19)
      FX = FX - ALPHA*XL*(AVGLTP - T SUB 0)
   50 A  = ECPT(5)
      E  = E S
      FX = A * E * FX / XL**2
      GX = ECPT(6) * FX / A
C
C COMPUTE THE XM VECTOR
C
      XM(1) = 0.0D0
      XM(2) = 0.0D0
      XM(3) = 0.0D0
      I = 1
      IF (DABS(XN(2)) .LT. DABS(XN(1))) I = 2
      IF (DABS(XN(3)) .LT. DABS(XN(I))) I = 3
      XM(I) = 1.0D0
C
C COMPUTE YVEC, THE CROSS PRODUCT XM X XN
C
      YVEC(1) = XM(2) * XN(3)  -  XM(3) * XN(2)
      YVEC(2) = XM(3) * XN(1)  -  XM(1) * XN(3)
      YVEC(3) = XM(1) * XN(2)  -  XM(2) * XN(1)
      YL = DSQRT (YVEC(1)**2  +  YVEC(2)**2  +  YVEC(3)**2)
      YVEC(1) = YVEC(1) / YL
      YVEC(2) = YVEC(2) / YL
      YVEC(3) = YVEC(3) / YL
C
C COMPUTE ZVEC, THE CROSS PRODUCT XN X YVEC
C
      ZVEC(1) = XN(2) * YVEC(3)  -  XN(3) * YVEC(2)
      ZVEC(2) = XN(3) * YVEC(1)  -  XN(1) * YVEC(3)
      ZVEC(3) = XN(1) * YVEC(2)  -  XN(2) * YVEC(1)
      ZL = DSQRT (ZVEC(1)**2  +  ZVEC(2)**2  +  ZVEC(3)**2)
      ZVEC(1) = ZVEC(1) / ZL
      ZVEC(2) = ZVEC(2) / ZL
      ZVEC(3) = ZVEC(3) / ZL
C
C                    T                 T
C COMPUTE YVEC * YVEC  AND  ZVEC * ZVEC
C
      CALL GMMATD (YVEC,3,1,0, YVEC,3,1,1, YYT)
      CALL GMMATD (ZVEC,3,1,0, ZVEC,3,1,1, ZZT)
C
C ADD THESE TWO MATRICES AND STORE IN YYT
C
      DO 60 I = 1,9
   60 YYT(I) = YYT(I) + ZZT(I)
C
C          T
C COMPUTE T  (YYT) IF POINT A IS NOT IN BASIC COORDINATES
C          A
C
      IAYPNT = 1
      IF (IECPT(KA) .EQ. 0) GO TO 70
      IAYPNT = 10
      CALL GMMATD (TA,3,3,1, YYT,3,3,0, YYT(10))
C
C          T
C COMPUTE T  (YYT) T  AND STORE IN YYT(1)
C          A        A
C
      CALL GMMATD (YYT(10),3,3,0, TA,3,3,0, YYT(1))
C
C ZERO OUT KE MATRIX
C
   70 DO 80 I = 1,36
   80 KE(I) = 0.0D0
      K = 1
      J = 2
C
C FILL UP THE 6 X 6 KE
C
   90 KE( 1) = FX * YYT(K  )
      KE( 2) = FX * YYT(K+1)
      KE( 3) = FX * YYT(K+2)
      KE( 7) = FX * YYT(K+3)
      KE( 8) = FX * YYT(K+4)
      KE( 9) = FX * YYT(K+5)
      KE(13) = FX * YYT(K+6)
      KE(14) = FX * YYT(K+7)
      KE(15) = FX * YYT(K+8)
      KE(22) = GX * YYT(K  )
      KE(23) = GX * YYT(K+1)
      KE(24) = GX * YYT(K+2)
      KE(28) = GX * YYT(K+3)
      KE(29) = GX * YYT(K+4)
      KE(30) = GX * YYT(K+5)
      KE(34) = GX * YYT(K+6)
      KE(35) = GX * YYT(K+7)
      KE(36) = GX * YYT(K+8)
      CALL DS1B (KE,IECPT(J))
      IF (J .EQ. 3) RETURN
      IF (IECPT(KB) .EQ. 0) GO TO 100
      IBYPNT = 1
      IF (IAYPNT .EQ. 1) IBYPNT = 10
      CALL GMMATD (YYT(IAYPNT),3,3,0, TB,3,3,0, YYT(IBYPNT))
      K = IBYPNT
      GO TO 110
  100 K = IAYPNT
  110 J = 3
      FX = -FX
      GX = -GX
      GO TO 90
  120 CALL MESAGE(30,26,IECPT(1))
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
      NOGO=1
      RETURN
      END
