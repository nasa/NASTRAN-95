      SUBROUTINE EM2D (ITYPE,ISTART,JTYPE,NCOUNT,IDO,IWORDS,NBDYS,ALL,
     1                 NELOUT)
C
C     COMPUTES ADDITIONAL E AND M LOADS FOR TWO DIMENSIONAL ELEMENTS
C
C     THIS ROUTINE HANDLES THE FOLLOWING 2-D ELEMENTS
C
C     TRIA1 -6-   TRMEM -9-   QDMEM-16-  TRIA2-17-  QUAD2-18-  QUAD1-19-
C     TRIARG-36-  TRAPRG-37   IS2D8-80-
C
      LOGICAL         ONLYC
      INTEGER         OTPE,ALL,POINTR(9,9),TYPOLD,SCR6
      REAL            L(3,4),W(4)
      DIMENSION       BUF(50),JBUF(50),XLACC(3),IZ(1),NAM(2),NECPT(10),
     1                R(3,8),IP(3),HC(3),XLOAD(3),D12(3),D13(3),XN(18),
     2                G(9),DXX(3),ZI(3),ZJ(3),ZK(3),ET(9),XNG(9),HCX(3),
     3                HCY(3),HCZ(3),ISC(5),SC(5),PT(3),H(3),Z14(3),
     4                XZ(16),VEC(3),VVEC(3),HCI(24),F(8),GH(3),DN(8),
     5                DNXI(1),DNETA(1),DNC(16),DNL(16),DNX(1),DNY(1),
     6                XI(8),ETA(8),XJB(4),XXJB(2,2),IWS(2,3),HCXYZ(3),
     7                DDNL(24),DDNLB(24)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ IBUF,OTPE,IDUM(78)
      COMMON /BLANK / NROWSP
      COMMON /EMECPT/ ECPT(200)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /HMTOUT/ XMAT(6)
      EQUIVALENCE     (BUF(1),JBUF(1)),(SC(1),ISC(1)),(Z(1),IZ(1)),
     1                (ECPT(1),NECPT(1)),(I1,IP(1)),(I2,IP(2)),
     2                (I3,IP(3)),(DNC(1),DNXI(1)),(DNC(9),DNETA(1)),
     3                (DNL(1),DNX(1)),(DNL(9),DNY(1))
      DATA    XI    / -1., 1., 1.,-1., 0., 1., 0.,-1./
      DATA    ETA   / -1.,-1., 1., 1.,-1., 0., 1., 0./
      DATA    TWOPI3/ 2.094395103  /
      DATA    NAM   / 4HEM2D,4H    /
      DATA    TYPOLD/ 0 /,   SCR6  / 306/
C
C     EST STARTING POINTERS
C
C     ISIL   = 1ST SIL NUMBER
C     ITH    = MATERIAL ANGLE
C     MID    = MATERIAL ID
C     IA     = AREA FACTOR (TO COMPUTE VOLUME)
C     ISYS   = 1ST OUTPUT CORRDINATE SYSYTEM NUMBER
C     NGRIDS = NUMBER OF GRID POINTS
C     ITEMP  = ELEMENT TEMPERATURE
C     NEL    = NUMBER OF TRIANGLES USED TO FORM ELEMENT
C
C              ITYPE ISIL ITH MID IA ISYS NGRIDS ITEMP NEL
C
      DATA    POINTR/  6,    2,  5,  6,  7, 15,    3,    27,  1,
     1                 9,    2,  5,  6,  7,  9,    3,    21,  1,
     2                16,    2,  6,  7,  8, 10,    4,    26,  4,
     3                17,    2,  5,  6,  7,  9,    3,    21,  1,
     4                18,    2,  6,  7,  8, 10,    4,    26,  4,
     5                19,    2,  6,  7,  8, 16,    4,    32,  4,
     6                36,    2,  5,  6,  0,  7,    3,    19,  1,
     7                37,    2,  6,  7,  0,  8,    4,    24,  4,
     8                80,    2, 11, 12, 13, 14,    8,    46,  1/
C
      ONLYC  = .FALSE.
      IF (ITYPE .EQ. 80) GO TO 10
      L(1,1) = 1./3.
      L(2,1) = L(1,1)
      L(3,1) = L(1,1)
      L(1,2) = .6
      L(2,2) = .2
      L(3,2) = .2
      L(1,3) = .2
      L(2,3) = .6
      L(3,3) = .2
      L(1,4) = .2
      L(2,4) = .2
      L(3,4) = .6
      W(1)   =-27./48.
      W(2)   = 25./48.
      W(3)   = W(2)
      W(4)   = W(2)
      NOPTS  = 4
   10 CONTINUE
      ISC(1) = NECPT(1)
      ISC(2) = 1
      IF (ITYPE .EQ. 80) ISC(2) = 9
C
C     FIND ELEMENT TYPE TO PICK UP POINTERS
C
      IF (ITYPE .EQ. TYPOLD) GO TO 40
      TYPOLD = ITYPE
      DO 20 I = 1,9
      JEL = I
      IF (ITYPE-POINTR(1,I)) 1600,30,20
   20 CONTINUE
      GO TO 1600
C
   30 ISIL  = POINTR(2,JEL)
      ITH   = POINTR(3,JEL)
      MID   = POINTR(4,JEL)
      IA    = POINTR(5,JEL)
      ISYS  = POINTR(6,JEL)
      NGRIDS= POINTR(7,JEL)
      ITEMP = POINTR(8,JEL)
      NEL   = POINTR(9,JEL)
C
C     CHECK TO SEE IF THIS ELEMENT CONTAINS A GRID POINT ON A PERMBDY
C     CARD. IF SO, OR IF NO PERMBDY CARD EXISTS, COMPUTE LOADS FOR THE
C     ELEMENT. IF NOT, COMPUTE HC CENTROIDAL VALUE ONLY. (ONLYC=.TRUE.)
C     THE PERMBDY SILS START AT Z(ISTART-NBDYS-1)
C
   40 IF (NBDYS .EQ. 0) GO TO 60
C
      DO 50 I = 1,NGRIDS
      NG = NECPT(ISIL+I-1)
      DO 50 J = 1,NBDYS
      IF (NG .EQ. IZ(ISTART-NBDYS-NELOUT+J-1)) GO TO 60
   50 CONTINUE
C
C     ELEMENT HAS NO GRIDS ON PERMBDY
C
      ONLYC = .TRUE.
      NOPTS = 0
   60 IF (ONLYC .AND. JTYPE.EQ.24) RETURN
C
C     IF ONLYC=TRUE, CHECK TO SEE IF THE ELEMENT HAD AN ELFORCE REQUEST.
C     IF SO, CONTINUE. IF NOT, JUST WRITE ZEROS TO HCCEN,SCR6) AND
C     RETURN.
C
      IF(.NOT.ONLYC) GO TO 80
      IF(ALL .EQ. 1) GO TO 80
      IF(NELOUT .EQ. 0) GO TO 110
C
      DO 70 I = 1,NELOUT
      IF (NECPT(1) .EQ. IZ(ISTART-NELOUT+I-1)) GO TO 80
   70 CONTINUE
      GO TO 110
C
C     CHECK FOR ZERO LOAD
C
   80 IF (JTYPE.NE.20 .AND. JTYPE.NE.24) GO TO 210
      H1 = 0.
      H2 = 0.
      H3 = 0.
      G1 = 0.
      G2 = 0.
      G3 = 0.
      DO 90 I = 1,NGRIDS
      ISUB = ISTART + 3*NECPT(ISIL+I-1) - 3
      IF (JTYPE .EQ. 24) ISUB = ISTART + 3*NCOUNT - 3
      H1 = H1 + ABS(Z(ISUB  ))
      H2 = H2 + ABS(Z(ISUB+1))
      H3 = H3 + ABS(Z(ISUB+2))
      G1 = G1 + Z(ISUB  )
      G2 = G2 + Z(ISUB+1)
      G3 = G3 + Z(ISUB+2)
      IF (JTYPE .EQ. 24) GO TO 100
   90 CONTINUE
  100 HL = H1 + H2 + H3
      IF (HL .NE. 0.) GO TO 200
      IF (JTYPE .EQ. 24) RETURN
C
C     ALL ZEROS - WRITE TO SCR6
C
  110 SC(3) = 0.
      SC(4) = 0.
      SC(5) = 0.
      CALL WRITE (SCR6,SC,2,0)
      ISC2  = ISC(2)
      DO 120 I = 1,ISC2
      CALL WRITE (SCR6,SC(3),3,0)
  120 CONTINUE
      RETURN
C
  200 IF (JTYPE .EQ. 24) GO TO 210
C
C     AVERAGE SPCFLD
C
      AHCX = G1/FLOAT(NGRIDS)
      AHCY = G2/FLOAT(NGRIDS)
      AHCZ = G3/FLOAT(NGRIDS)
C
  210 IF (ONLYC) GO TO 310
C
C     PICK UP MATERIAL INFO
C     INFLAG = 3 MEANS A 3 X 3 MATERIAL MATRIX WILL BE RETURNED. THE
C     REASON FOR DOING THIS FOR A 2-D ELEMENT IS THAT HC CAN HAVE A
C     COMPONENT NORMAL TO THE PLANE OF THE ELEMENT. PARTIAL DERIVATIVE
C     W.R.T Z IS 0.  BUT IF THE MATERIAL IS ANISOTROPIC, THEN A
C     CONTRIBUTION TO THE SCALAR LOAD IS POSSIBLE IF MATERIAL CONTAINS
C     A NON-ZERO X-Z TERM. FOR ISOTROPIC MATERIALS, THE NORMAL COMPONENT
C     OF HC WILL BE IGNORED W.R.T ITS CONTRIBUTION TO THE LOAD. IF ALL
C     TERMS OF MATERIAL MATRIX W.R.T.Z ARE 0, AND IF ANISOTROPIC ANGLE
C     IS NOT 0, THEN WE MUST TRANSFORM MATERIALS TO ELEMENT SYSTEM HERE.
C
      INFLAG = 3
      IF (JTYPE .EQ. 24) GO TO 260
      MATID  = NECPT(MID)
      ELTEMP = ECPT(ITEMP)
      ANGLE  = ECPT(ITH)*0.017453293
      SINTH  = SIN(ANGLE)
      COSTH  = COS(ANGLE)
      CALL HMAT (NECPT(1))
C
C     CHECK FOR 3-D ANISOTROPY
C
      IF (XMAT(3).EQ.0. .AND. XMAT(5).EQ.0.) GO TO 230
C
  220 G(1) = XMAT(1)
      G(2) = XMAT(2)
      G(3) = XMAT(3)
      G(5) = XMAT(4)
      G(6) = XMAT(5)
      G(9) = XMAT(6)
      GO TO 240
C
C     CHECK FOR 2-D ANISOTROPY
C
  230 IF (ABS(ANGLE) .LE. .0001) GO TO 220
C
C     2-D ANISOTROPY
C
      CSQ  = COSTH*COSTH
      SSQ  = SINTH*SINTH
      CS   = COSTH*SINTH
      G(1) = CSQ*XMAT(1) - 2.*CS*XMAT(2) + SSQ*XMAT(4)
      G(2) = CS*(XMAT(1) - XMAT(4)) + (CSQ-SSQ)*XMAT(2)
      G(3) = 0.
      G(5) = SSQ*XMAT(1) + 2.*CS*XMAT(2) + CSQ*XMAT(4)
      G(6) = 0.
      G(9) = XMAT(6)
C
  240 IF (ITYPE.NE.36 .AND. ITYPE.NE.37) GO TO 250
C
C     SWITCH Y-Z MATERIALS FOR TRAPRG AND TRIARG
C
      TEMP = G(5)
      G(5) = G(9)
      G(9) = TEMP
      TEMP = G(2)
      G(2) = G(3)
      G(3) = TEMP
C
C     FILL IN SYMMETRIC PART
C
  250 G(4) = G(2)
      G(7) = G(3)
      G(8) = G(6)
C
C     SINCE QUADRILATERALS ARE COVERED BY 4 OVERLAPPING TRIANGLES,
C     MUST DIVIDE QUAD RESULTS BY 2
C
  260 XMUL = 1.
      IF (NGRIDS .EQ. 4) XMUL = .5
C
C     PICK UP COORDINATES OF GRID POINTS
C
      DO 300 I = 1,NGRIDS
      ISUBI = ISYS + 4*I - 4
      DO 300 J = 1,3
      ISUB  = ISUBI + J
      R(J,I)= ECPT(ISUB)
  300 CONTINUE
  310 IF (ITYPE .EQ. 80) GO TO 900
C
C     COMPUTE COORDINATES OF CENTROID (OR, AT LEAST, AVERAGE ELEMENT
C     COORDS)
C
      XXC = 0.
      YYC = 0.
      ZZC = 0.
      DO 320 I = 1,NGRIDS
      XXC = XXC + R(1,I)
      YYC = YYC + R(2,I)
      ZZC = ZZC + R(3,I)
  320 CONTINUE
      XXC = XXC/FLOAT(NGRIDS)
      YYC = YYC/FLOAT(NGRIDS)
      ZZC = ZZC/FLOAT(NGRIDS)
C
C     NOW COMPUTE PROPER LOADS FOR EACH TRIANGLE
C
      DO 800 IEL = 1,NEL
      IF (ONLYC) GO TO 500
C
C     1ST SET UP AN ARRAY TO PICK UP GRID POINTS IN A PARTICULAR ORDER.
C     FOR TRIANGLES, IT IS 1,2,3. FOR QUADRILATERALS, FORM 4 TRIANGLES
C     BY TAKING GRIDS 1,2,3, 2,3,4, 3,4,1, AND 4,1,2
C
      DO 330 I = 1,3
      IP(I) = I + IEL - 1
      IF (IP(I) .GT. 4) IP(I) = IP(I) - 4
  330 CONTINUE
C
C     COMPUTE VECTORS FROM 1ST GRID TO 2ND AND FROM 1ST TO 3RD
C
      DO 340 I = 1,3
      D12(I) = R(I,I2) - R(I,I1)
  340 D13(I) = R(I,I3) - R(I,I1)
C
C     SET UP GRADIENTS FOR AXISYMMETRIC ELEMENTS SEPARATELY
C
      IF (ITYPE.NE.36 .AND. ITYPE.NE.37) GO TO 360
C
C     THE LENGTH OF THE CROSS PRODUCT VECTOR IS TWICE THE AREA OF THE
C     TRIANG
C
      CALL SAXB (D12(1),D13(1),D12(1))
      AREA = .5*SQRT(D12(1)**2 + D12(2)**2 + D12(3)**2)
      VOL  = AREA*TWOPI3*(R(1,I1) + R(1,I2) + R(1,I3))
C
C     NOW SET UP GRADIENT OF THE SHAPE FUNCTION AT EACH GRID POINT.
C     SET UP A 3 X3 MATRIX ROW-STORED FOR GMMATS
C
      D     = (R(1,I2)-R(1,I1))*R(3,I3) + (R(1,I1)-R(1,I3))*R(3,I2) +
     1        (R(1,I3)-R(1,I2))*R(3,I1)
      XN(1) = R(3,I2) - R(3,I3)
      XN(2) = 0.
      XN(3) = R(1,I3) - R(1,I2)
      XN(4) = R(3,I3) - R(3,I1)
      XN(5) = 0.
      XN(6) = R(1,I1) - R(1,I3)
      XN(7) = R(3,I1) - R(3,I2)
      XN(8) = 0.
      XN(9) = R(1,I2) - R(1,I1)
C
      DO 350 I = 1,9
      XN(I) = XN(I)/D
  350 CONTINUE
C
C     FOR ALL EXCEPT REMFLUX, MULT. GRADIENTS INTO MATERIALS
C
      IF (JTYPE .NE. 24) CALL GMMATS (XN(1),3,3,0,G,3,3,0,XN(10))
      GO TO 420
C
C     FIRST, CONVERT COORDINATES TO ELEMNT COORDINATE SYSTEM
C
  360 ZLEN = SQRT(D12(1)**2 + D12(2)**2 + D12(3)**2)
      DO 370 I = 1,3
  370 ZI(I) = D12(I)/ZLEN
C
      CALL SAXB (ZI(1),D13(1),DXX(1))
C
      X2 = ZLEN
      X3 = D13(1)*ZI(1) + D13(2)*ZI(2) + D13(3)*ZI(3)
      Y3 = SQRT(DXX(1)**2 + DXX(2)**2 + DXX(3)**2)
C
      AREA = .5*X2*Y3
      VOL  = AREA*ECPT(IA)
C
C     GET J AND K VECTORS FOR LATER USE
C
      DO 380 I = 1,3
  380 ZK(I) = DXX(I)/Y3
C
      CALL SAXB (ZK(1),ZI(1),ZJ(1))
      ZLEN = SQRT(ZJ(1)**2 + ZJ(2)**2 + ZJ(3)**2)
      DO 390 I = 1,3
  390 ZJ(I) = ZJ(I)/ZLEN
      DO 400 I = 1,3
      ET(I  ) = ZI(I)
      ET(I+3) = ZJ(I)
  400 ET(I+6) = ZK(I)
C
C     SHAPE FUNCTION GRADIENTS
C
      XN(1) = -1./X2
      XN(2) = (X3-X2)/(X2*Y3)
      XN(3) = 0.
      XN(4) = -XN(1)
      XN(5) = -X3/(X2*Y3)
      XN(6) = 0.
      XN(7) = 0.
      XN(8) = 1./Y3
      XN(9) = 0.
C
C     TRANSFORM SHAPE FN GRADIENTS FROM LOCAL TO BASIC
C
      CALL GMMATS (ET,3,3,1,XN(1),3,3,1,XNG(1))
C
C     FOR ALL EXCEPT REMFLUX, MULT. GRADIENTS OF SHAPE FNS INTO
C     MATERIALS
C
      IF (JTYPE .EQ. 24) GO TO 410
      CALL GMMATS (XNG(1),3,3,1,G,3,3,0,XN(10))
      GO TO 420
  410 XN(1) = XNG(1)
      XN(2) = XNG(4)
      XN(3) = XNG(7)
      XN(4) = XNG(2)
      XN(5) = XNG(5)
      XN(6) = XNG(8)
      XN(7) = XNG(3)
      XN(8) = XNG(6)
      XN(9) = XNG(9)
  420 IF (JTYPE .EQ. 24) GO TO 740
C
C     START INTEGRATION PROCEDURE- 4 POINTS FOR CUBIC PLUS ONE AT
C     CENTROID
C
  500 KTYPE    = JTYPE - 19
      XLACC(1) = 0.
      XLACC(2) = 0.
      XLACC(3) = 0.
      NOPTSP   = NOPTS + 1
      DO 720 NPTS = 1,NOPTSP
C
C     DO CENTROID FOR ONLY 1ST TRIANGLE
C
      IF (NPTS.EQ.NOPTSP .AND. IEL.GT.1) GO TO 720
C
C     COMPUTE BASIC COORDS OF INTEGRATION POINT
C
      IF (NPTS .NE. NOPTSP) GO TO 510
C
C     CENTROID
C
      XX = XXC
      YY = YYC
      ZZ = ZZC
      IF (JTYPE .NE. 20) GO TO 520
C
C     AVERAGE SPCFLD
C
      HC(1) = AHCX
      HC(2) = AHCY
      HC(3) = AHCZ
      GO TO 610
  510 XX    = L(1,NPTS)*R(1,I1) + L(2,NPTS)*R(1,I2) + L(3,NPTS)*R(1,I3)
      YY    = L(1,NPTS)*R(2,I1) + L(2,NPTS)*R(2,I2) + L(3,NPTS)*R(2,I3)
      ZZ    = L(1,NPTS)*R(3,I1) + L(2,NPTS)*R(3,I2) + L(3,NPTS)*R(3,I3)
  520 HC(1) = 0.
      HC(2) = 0.
      HC(3) = 0.
C
C     COMPUTE HC AT THIS POINT FOR ALL LOADS OF THIS TYPE
C
      DO 600 IJK = 1,IDO
      IF (JTYPE .EQ. 20) GO TO 540
      ISUB = ISTART + (IJK-1)*IWORDS - 1
      DO 530 I = 1,IWORDS
  530 BUF(I) = Z(ISUB+I)
      GO TO (540,560,570,580), KTYPE
  540 DO 550 I = 1,3
      IPI  = IP(I)
      NSIL = NECPT(ISIL+IPI-1)
      IPT  = ISTART + 3*NSIL - 3
      HCX(I) = Z(IPT  )
      HCY(I) = Z(IPT+1)
      HCZ(I) = Z(IPT+2)
  550 CONTINUE
      HC1 = L(1,NPTS)*HCX(1) + L(2,NPTS)*HCX(2) + L(3,NPTS)*HCX(3)
      HC2 = L(1,NPTS)*HCY(1) + L(2,NPTS)*HCY(2) + L(3,NPTS)*HCY(3)
      HC3 = L(1,NPTS)*HCZ(1) + L(2,NPTS)*HCZ(2) + L(3,NPTS)*HCZ(3)
      GO TO 590
C
C     CEMLOOP, GEMLOOP, MDIPOLE
C
  560 CALL AXLOOP (BUF,JBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 590
  570 CALL GELOOP (BUF,JBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 590
  580 CALL DIPOLE (BUF,JBUF,XX,YY,ZZ,HC1,HC2,HC3)
  590 HC(1) = HC(1) + HC1
      HC(2) = HC(2) + HC2
      HC(3) = HC(3) + HC3
  600 CONTINUE
C
  610 IF (NPTS .NE. NOPTSP) GO TO 700
      SC(3) = HC(1)
      SC(4) = HC(2)
      SC(5) = HC(3)
      CALL WRITE (SCR6,SC,5,0)
      GO TO 720
C
C     WE HAVE HC AT THIS INTEG. PT. MULT. BY WEIGHT AND ACCUMULATE
C
  700 DO 710 I = 1,3
  710 XLACC(I) = XLACC(I) + HC(I)*W(NPTS)
C
C     GET ANOTHER INTEGRATION POINT
C
  720 CONTINUE
C
      IF (ONLYC) RETURN
      DO 730 I = 1,3
  730 HC(I) = XLACC(I)
      GO TO 750
C
C     REMFLUX
C
  740 IPT   = ISTART + 3*NCOUNT - 3
      HC(1) = Z(IPT  )
      HC(2) = Z(IPT+1)
      HC(3) = Z(IPT+2)
C
C    TAKE XMUL MULTIPLIER INTO ACCOUNT
C
  750 DO 760 I = 1,3
  760 HC(I) = HC(I)*XMUL
C
C     MAKE FINAL COMPUTATION. MULTIPLY PRODUCT OF SHAPE FUNCTION
C     GRADIENTS AND MATERIAL MATRIX INTO HC AND MULTIPLY BY VOLUME
C
      ISUB = 10
      IF (JTYPE .EQ. 24) ISUB = 1
      CALL GMMATS (XN(ISUB),3,3,0,HC,3,1,0,XLOAD(1))
C
C     ADD THIS ELEMENT LOAD VECTOR IN OVERALL VECTOR. USE NSIL AND IP TO
C     POI
C
      DO 790 J = 1,3
      IPI  = IP(J)
      NSIL = NECPT(ISIL+IPI-1)
C
C     IF PERMBDY EXISTS AND IF GRID IS NOT ON IT, IGNORE ITS LOAD
C
      IF (NBDYS .EQ. 0) GO TO 780
      DO 770 I = 1,NBDYS
      IF (NSIL .NE. IZ(ISTART-NBDYS-NELOUT+I-1)) GO TO 770
      GO TO 780
  770 CONTINUE
      GO TO 790
  780 Z(NSIL) = Z(NSIL) - XLOAD(J)*VOL
  790 CONTINUE
C
C     DONE FOR THIS TRIANGLE. GO BACK FOR ANOTHER
C
  800 CONTINUE
      RETURN
C
C     IS2D8
C
C     SET UP QUADRATURE POINTS AND WEIGHTS
C
  900 IF (ONLYC) GO TO 1000
      PT(1) = -0.57735027
      PT(2) = -PT(1)
      H(1)  = 1.
      H(2)  = 1.
      IF (NECPT(10) .EQ. 2) GO TO 910
      PT(1) = -0.77459667
      PT(2) = 0.
      PT(3) = -PT(1)
      H(1)  = 5./9.
      H(2)  = 8./9.
      H(3)  = H(1)
C
C     COMPUTE I,J,K VECTORS- I IS 1 TO 2
C
  910 DO 920 I = 1,3
      ZI(I) = R(I,2) - R(I,1)
  920 Z14(I)= R(I,4) - R(I,1)
      ZLEN  = SQRT(ZI(1)**2 + ZI(2)**2 + ZI(3)**2)
      DO 930 I = 1,3
  930 ZI(I) = ZI(I)/ZLEN
C
C     GET K BY CROSSING I INTO VECTOR FROM 1 TO 4
C
      ZK(1) = ZI(2)*Z14(3) - ZI(3)*Z14(2)
      ZK(2) = ZI(3)*Z14(1) - ZI(1)*Z14(3)
      ZK(3) = ZI(1)*Z14(2) - ZI(2)*Z14(1)
      ZKLEN = SQRT(ZK(1)**2 + ZK(2)**2 + ZK(3)**2)
      DO 940 I = 1,3
  940 ZK(I) = ZK(I)/ZKLEN
C
C     GET J BY CROSSING K INTO I AND STORE INTO TRANSFORMATION MATRIX
C
      ZJ(1) = ZK(2)*ZI(3) - ZK(3)*ZI(2)
      ZJ(2) = ZK(3)*ZI(1) - ZK(1)*ZI(3)
      ZJ(3) = ZK(1)*ZI(2) - ZK(2)*ZI(1)
      ZJLEN = SQRT(ZJ(1)**2 + ZJ(2)**2 + ZJ(3)**2)
      DO 950 I = 1,3
  950 ZJ(I) = ZJ(I)/ZJLEN
C
      DO 960 I = 1,3
      ET(I  ) = ZI(I)
      ET(I+3) = ZJ(I)
  960 ET(I+6) = ZK(I)
C
C     COMPUTE ELMENT COORDS FOR 1 AND 2
C
      XZ(1) = 0.
      XZ(2) = 0.
      XZ(3) = ZLEN
      XZ(4) = 0.
C
C     FOR 3-8, X IS DOT PRODUCT OF VECTOR FROM 1 TO GRID WITH I.
C     Y IS THE LENFTH OF THE VECTOR RESULTING FROM CROSSING I INTO
C     VECTOR FROM 1 TO GRID
C
      DO 980 I = 3,8
      IXX = 2*I - 1
      DO 970 J = 1,3
  970 VEC(J)  = R(J,I) - R(J,1)
      XZ(IXX) = VEC(1)*ZI(1) + VEC(2)*ZI(2) + VEC(3)*ZI(3)
      VVEC(1) = ZI(2)*VEC(3) - ZI(3)*VEC(2)
      VVEC(2) = ZI(3)*VEC(1) - ZI(1)*VEC(3)
      VVEC(3) = ZI(1)*VEC(2) - ZI(2)*VEC(1)
      XZ(IXX+1) = SQRT(VVEC(1)**2 + VVEC(2)**2 + VVEC(3)**2)
  980 CONTINUE
C
      DO 990 I = 1,8
  990 F(I) = 0.
C
C     GET HC AT EACH GRID
C
      IF (JTYPE .NE. 24) GO TO 1000
C
C     REMFLUX
C
      ISUB  = ISTART + 3*NCOUNT - 3
      GH(1) = Z(ISUB  )
      GH(2) = Z(ISUB+1)
      GH(3) = Z(ISUB+2)
      GO TO 1020
C
C     IF SPCFLD, PICK UP GRID VALUES HERE. IF NOT, PICK UP INTEGRATION
C     POINT VALUES LATER
C
 1000 IF (JTYPE .NE. 20) GO TO 1020
      DO 1010 I = 1,NGRIDS
      ISIL = 3*NECPT(I+1)
      HCI(3*I-2) = Z(ISTART+ISIL-3)
      HCI(3*I-1) = Z(ISTART+ISIL-2)
      HCI(3*I  ) = Z(ISTART+ISIL-1)
 1010 CONTINUE
 1020 INIP  = NECPT(10)
      KTYPE = JTYPE - 20
      IF (ONLYC) GO TO 1340
C
C     START INTEGRATION
C
      DO 1300 III = 1,INIP
      DO 1300 JJJ = 1,INIP
C
C     COMPUTE DERIVATIVES WITH RESPECT TO XI AND ETA
C     EACH GRID POINT
C
      DO 1030 N = 1,4
      DN(N)   = .25*(1.+PT(III)*XI(N))*(1.+PT(JJJ)*ETA(N))*
     1          (PT(III)*XI(N)+PT(JJJ)*ETA(N)-1.)
      DNXI(N) = .25*XI(N)*(1.+PT(JJJ)*ETA(N))*
     1          (2.*PT(III)*XI(N)+PT(JJJ)*ETA(N))
      DNETA(N)= .25*ETA(N)*(1.+PT(III)*XI(N))*
     1          (PT(III)*XI(N)+2.*PT(JJJ)*ETA(N))
 1030 CONTINUE
      DO 1040 N = 5,7,2
C
      DN(N)   = .5*(1.-PT(III)*PT(III))*(1.+PT(JJJ)*ETA(N))
      DNXI(N) = -PT(III)*(1.+PT(JJJ)*ETA(N))
      DNETA(N)= .5*(1.-PT(III)*PT(III))*ETA(N)
 1040 CONTINUE
C
      DO 1050 N = 6,8,2
      DN(N)   = .5*(1.+PT(III)*XI(N))*(1.-PT(JJJ)*PT(JJJ))
      DNXI(N) = .5*XI(N)*(1.-PT(JJJ)*PT(JJJ))
      DNETA(N)= -PT(JJJ)*(1.+PT(III)*XI(N))
 1050 CONTINUE
C
C     COMPUTE JACOBEAN
C
C           N1XI   N2XI   N3XI   N4XI   N5XI   N6XI   N7XI   N8XI
C     DNC = N1ETA  N2ETA  N3ETA  N4ETA  N5ETA  N6ETA  N7ETA  N8ETA
C
C          X1  Y1
C          X2  Y2
C          X3  Y3
C     XX = X4  Y4
C          X5  Y5
C          X6  Y6
C          X7  Y7
C          X8  Y8
C
      CALL GMMATS (DNC,2,8,0,XZ,8,2,0,XJB)
C
C     XJB IS ROW-STORED-IT MUST BE COLUMN-STORED AND DOUBLY DIMENSIONED
C     FOR INVERSION
C
      K = 0
      DO 1060 I = 1,2
      DO 1060 J = 1,2
      K = K + 1
 1060 XXJB(I,J) = XJB(K)
C
C     COMPUTE INVERSE AND DETERMINANT OF JACOBEAN
C
      CALL INVERS (2,XXJB,2,DUMARG,0,DETERM,ISING,IWS)
C
C     COMPUTE DERIVATIVES WITH RESPECT TO X AND Y
C
      K = 0
      DO 1070 I = 1,2
      DO 1070 J = 1,2
      K = K + 1
 1070 XJB(K) = XXJB(I,J)
      CALL GMMATS (XJB,2,2,0,DNC,2,8,0,DNL)
C
C           N1X N2X N3X N4X N5X N6X N7X N8X
C     DNL = N1Y N2Y N3Y N4Y N5Y N6Y N7Y N8Y
C
      IF (JTYPE .EQ. 24) GO TO 1190
C
C     INITIALIZE HC AT PRESENT UNTEGRATION POINT
C
      DO 1080 I = 1,3
 1080 HCXYZ(I) = 0.
      IF (JTYPE .EQ. 20) GO TO 1160
C
C     FOR LOOPS AND DIPOLES, COMPITE BASIC COORDS. FOR THIS INTEGRATION
C     PT
C
      XX = 0.
      YY = 0.
      ZZ = 0.
      DO 1090 M = 1,NGRIDS
      XX = XX + DN(M)*R(1,M)
      YY = YY + DN(M)*R(2,M)
 1090 ZZ = ZZ + DN(M)*R(3,M)
C
      DO 1150 IJK = 1,IDO
      ISUB = ISTART + (IJK-1)*IWORDS - 1
      DO 1100 M = 1,IWORDS
 1100 BUF(M) = Z(ISUB+M)
      GO TO (1110,1120,1130), KTYPE
 1110 CALL AXLOOP (BUF,JBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 1140
 1120 CALL GELOOP (BUF,JBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 1140
 1130 CALL DIPOLE (BUF,JBUF,XX,YY,ZZ,HC1,HC2,HC3)
 1140 HCXYZ(1) = HCXYZ(1) + HC1
      HCXYZ(2) = HCXYZ(2) + HC2
      HCXYZ(3) = HCXYZ(3) + HC3
 1150 CONTINUE
      GO TO 1180
C
C     SPCFLD
C
 1160 DO 1170 M = 1,NGRIDS
      HCXYZ(1) = HCXYZ(1) + DN(M)*HCI(3*M-2)
      HCXYZ(2) = HCXYZ(2) + DN(M)*HCI(3*M-1)
 1170 HCXYZ(3) = HCXYZ(3) + DN(M)*HCI(3*M)
C
C     MULTIPLY MATERIAL INTO HC AT THIS INTEGRATION POINT
C
 1180 CALL GMMATS (G,3,3,0,HCXYZ,3,1,0,GH)
 1190 SFACT = H(III)*H(JJJ)*DETERM
C
C     TRANSFORM DNL FROM LOCAL TO BASIC
C     1 ST EXPAND TO ADD IN ZEROS CORRESPONDING TO Z DIRECTION
C
      DO 1200 I = 1,16
 1200 DDNL(I) = DNL(I)
      DO 1210 I = 17,24
 1210 DDNL(I) = 0.
C
      CALL GMMATS (ET,3,3,1,DDNL,3,8,0,DDNLB)
C
      DO 1220 M = 1,NGRIDS
      F(M) = F(M) + (DDNLB(M)*GH(1) + DDNLB(M+8)*GH(2) +
     1       DDNLB(M+16)*GH(3))*SFACT
 1220 CONTINUE
C
C     GET ANOTHER INTEGRATION POINT
C
 1300 CONTINUE
C
C     ADD LOAD INTO LOAD ARRAY
C
      DO 1330 M = 1,NGRIDS
      ISIL = NECPT(M+1)
C
C     IF PERMBDY EXISTS AND IF GRID IS NOT ON IT, IGNORE ITS LOAD
C
      IF (NBDYS .EQ. 0) GO TO 1320
      DO 1310 I = 1,NBDYS
      IF (ISIL .NE. IZ(ISTART-NBDYS-NELOUT+I-1)) GO TO 1310
      GO TO 1320
 1310 CONTINUE
      GO TO 1330
 1320 Z(ISIL) = Z(ISIL)-F(M)*ECPT(IA)
 1330 CONTINUE
C
C     BEFORE LEAVING COMPUTE HC AT GRIDS AND CENTROID AND WRITE TO SCR6
C
 1340 IF (JTYPE .EQ. 24) RETURN
      CALL WRITE (SCR6,ISC,2,0)
C
C     SET UP SHAPE FUNCTIONS AT CENTROID
C
      DO 1350 I = 1,4
 1350 DN(I) = -.25
      DO 1360 I = 5,8
 1360 DN(I) = .5
C
      IF (JTYPE .NE. 20) GO TO 1400
C
C     FOR SPCFLD HC VALUES AT GRIDS ARE IN CORE
C
      CALL WRITE (SCR6,HCI,24,0)
C
      DO 1370 I = 1,3
 1370 HCXYZ(I) = 0.
      DO 1380 M = 1,NGRIDS
      HCXYZ(1) = HCXYZ(1) + DN(M)*HCI(3*M-2)
      HCXYZ(2) = HCXYZ(2) + DN(M)*HCI(3*M-1)
      HCXYZ(3) = HCXYZ(3) + DN(M)*HCI(3*M  )
 1380 CONTINUE
C
      CALL WRITE (SCR6,HCXYZ,3,0)
      RETURN
C
C     NOT SPCFLD
C
 1400 DO 1500 J = 1,9
      IF (J .NE. 9) GO TO 1420
C
C     CENTROID
C
      XX = 0.
      YY = 0.
      ZZ = 0.
      DO 1410 M = 1,8
      XX = XX + DN(M)*R(1,M)
      YY = YY + DN(M)*R(2,M)
 1410 ZZ = ZZ + DN(M)*R(3,M)
      GO TO 1430
 1420 XX = R(1,J)
      YY = R(2,J)
      ZZ = R(3,J)
 1430 HC(1) = 0.
      HC(2) = 0.
      HC(3) = 0.
      DO 1490 IJK = 1,IDO
      ISUB  = ISTART + (IJK-1)*IWORDS - 1
      DO 1440 I = 1,IWORDS
 1440 BUF(I) = Z(ISUB+I)
      GO TO (1450,1460,1470), KTYPE
 1450 CALL AXLOOP (BUF,JBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 1480
 1460 CALL GELOOP (BUF,JBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 1480
 1470 CALL DIPOLE (BUF,JBUF,XX,YY,ZZ,HC1,HC2,HC3)
 1480 HC(1) = HC(1) + HC1
      HC(2) = HC(2) + HC2
      HC(3) = HC(3) + HC3
 1490 CONTINUE
C
      CALL WRITE (SCR6,HC,3,0)
 1500 CONTINUE
C
      RETURN
C
 1600 WRITE  (OTPE,1610) UFM,NAM,ITYPE
 1610 FORMAT (A23,', IN SUBROUTINE',2A4,' ELEMENT TYPE',I8,' IS NOT ',
     1        'LEGAL')
      CALL MESAGE (-61,0,0)
      RETURN
      END
