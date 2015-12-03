      SUBROUTINE EM3D (ELTYPE,ISTART,ITYPE,NCOUNT,IDO,IWORDS,NBDYS,ALL,
     1                 NELOUT)
C
C     E  AND  M LOADS FOR 3-D ELEMENTS
C     TETRA  39   WEDGE  40   HEXA1 41  HEXA2  42
C     IHEX1  65   IHEX2  66   IHEX3 67
C
      LOGICAL         ONLYC
      INTEGER         SCR6,ALL,TYPOLD,ELID,
     1                ELTYPE,OUTPT,SYSBUF,POINTR(7,7),FRSTGD,TMAP(88)
      REAL            LL(4,5),W(5)
      DIMENSION       ISC(5),SC(5),XLACC(3),BUF(50),IBUF(50),HCX3(60),
     1                HCX(4),HCY(4),HCZ(4),
     2                G(9),NECPT(1),DR(24),IZ(1),IP(4),R(3,8),XLOAD(8),
     3                GPT(32),BXYZ(3,32),S(4),H(4),GAUSS(8),F(32),
     4                SHP(32),DSHP(3,32),XJACOB(3,3),DSHPB(3,32),HC(96),
     5                HCXYZ(3),GH(3)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ KSYSTM(2)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /EMECPT/ ECPT(200)
      COMMON /MATIN / MATID,INFLAG,ELTEMP
      COMMON /HMTOUT/ XMAT(6)
      EQUIVALENCE     (KSYSTM(1),SYSBUF), (KSYSTM(2),OUTPT),
     1                (ECPT(1),NECPT(1)), (Z(1),IZ(1)), (I1,IP(1)),
     2                (I2,IP(2)),(I3,IP(3)), (I4,IP(4))
      EQUIVALENCE     (BUF(1),IBUF(1))  , (ISC(1),SC(1))
C
C     GRID POINT NO FOR EACH ELEMENT
C
      DATA    TMAP  / 1, 2, 3, 4,    1, 2, 3, 5,    1, 2, 3, 6,
     1                1, 4, 5, 6,    2, 4, 5, 6,    3, 4, 5, 6,
     2                1, 2, 4, 6,    2, 3, 4, 6,    1, 3, 4, 5,
     3                2, 3, 4, 5,    1, 3, 5, 6,    1, 2, 5, 6,
     4                1, 2, 3, 6,    1, 3, 4, 8,    1, 3, 8, 6,
     5                1, 5, 6, 8,    3, 6, 7, 8,    2, 3, 4, 7,
     6                1, 2, 4, 5,    2, 4, 5, 7,    2, 5, 6, 7,
     7                4, 5, 7, 8/
C     DATA    NAM   / 4HEM3D,4H       /
      DATA    TYPOLD/ 0               /
      DATA    SCR6  / 306             /
C
C     SET UP GAUSSIAN INTEGRATION POINTS
C
      DATA    GAUSS / 0.57735027,     0.55555556,
     1                0.77459667,     0.88888889,
     2                0.34785484,     0.86113631,
     3                0.65214515,     0.33998104/
C
C     SET UP POINTR ARRAY ONTO EST
C
C                    TYPE  MID   FRSTGD  ISYS1   NIP     ITEMP     NELS
      DATA    POINTR/ 39,   2,    3    ,  7,      0,      23,       1,
     1                40,   2,    3,      9,      0,      33,      12,
     2                41,   2,    3,      11,     0,      43,       5,
     3                42,   2,    3,      11,     0,      43,       10,
     4                65,   10,   2,      16,     12,     48,       1,
     5                66,   22,   2,      28,     24,     108,      1,
     6                67,   34,   2,      40,     36,     168,      1 /
C
C
      ONLYC  =.FALSE.
      NOPTS  = 6
      IF (ELTYPE .EQ. TYPOLD) GO TO 30
      TYPOLD = ELTYPE
      DO 10 L = 1,7
      ILIS   = L
      IF (ELTYPE-POINTR(1,L)) 1200,20,10
   10 CONTINUE
      GO TO 1200
C
C     SET UPPOINTERS INTO EST(ECPT) DATA
C
   20 MID = POINTR(2,ILIS)
C
C     MATERIAL ID
C
      FRSTGD = POINTR(3,ILIS)
C
C     FIRST SIL
C
      ISYS1 = POINTR(4,ILIS)
C
C     FIRST CSIL
C
      NIP = POINTR(5,ILIS)
C
C     NO OF INTEGRATION POINTS (ISOPARAMETRICS ONLY)
C
      ITEMP = POINTR(6,ILIS)
C
C     TEMPERATURE DATA
C
      NELS = POINTR(7,ILIS)
C
C     NO. OF ELEMENTS
C
C     GO TO SECTION 190 FOR ISOPARAMETRICS
C
C     CHECK FOR ZERO LOAD
C
   30 NGRID = ISYS1 - FRSTGD
      IF (ELTYPE .GE. 65) NGRID = NGRID - 6
C                     65 TO 67 ??
      ISC(1) = NECPT(1)
      ISC(2) = 1
      IF (ELTYPE .EQ. 65) ISC(2) = 9
      IF (ELTYPE.EQ.66 .OR. ELTYPE.EQ.67) ISC(2) = 21
C
C     CHECK TO SEE IF THIS ELEMENT CONTAINS A GRID POINT ON A PERMBDY
C     CARD. IF SO, OR IF NO PERMBDY CARD EXISTS, COMPUTE LOADS FOR THE
C     ELEMENT. IF NOT, COMPUTE HC CENTROIDAL VALUE ONLY. (ONLYC=.TRUE.)
C     THE PERMBDY SILS START AT Z(ISTART-NBDYS-1)
C
      IF (NBDYS .EQ. 0) GO TO 50
C
      DO 40 I = 1,NGRID
      NG = NECPT(FRSTGD+I-1)
      DO 40 J = 1,NBDYS
      IF (NG .EQ. IZ(ISTART-NBDYS-NELOUT+J-1)) GO TO 50
   40 CONTINUE
C
C     ELEMENT HAS NO GRIDS ON PERMBDY
C
      ONLYC =.TRUE.
      NOPTS = 1
   50 IF (ONLYC .AND. ITYPE.EQ.24) RETURN
C
C     IF ONLYC=TRUE, CHECK TO SEE IF THE ELEMENT HAD AN ELFORCE REQUEST.
C     IF SO, CONTINUE. IF NOT, JUST WRITE ZEROS TO HCCEN,SCR6) AND
C     RETURN.
C
      IF (.NOT.ONLYC) GO TO 70
      IF (ALL .EQ. 1) GO TO 70
      IF (NELOUT .EQ. 0) GO TO 100
C
      DO 60 I = 1,NELOUT
      IF (NECPT(1) .EQ. IZ(ISTART-NELOUT+I-1)) GO TO 70
   60 CONTINUE
      GO TO 100
   70 IF (ITYPE.NE.20 .AND. ITYPE.NE.24) GO TO 130
      G1 = 0.
      G2 = 0.
      G3 = 0.
      H1 = 0.
      H2 = 0.
      H3 = 0.
      DO 80 I = 1,NGRID
      ISUB = ISTART + 3*NECPT(FRSTGD+I-1) - 3
      IF (ITYPE .EQ. 24) ISUB = ISTART + 3*NCOUNT - 3
      H1 = H1 + ABS(Z(ISUB  ))
      H2 = H2 + ABS(Z(ISUB+1))
      H3 = H3 + ABS(Z(ISUB+2))
      G1 = G1 + Z(ISUB  )
      G2 = G2 + Z(ISUB+1)
      G3 = G3 + Z(ISUB+2)
      IF (ITYPE .EQ. 24) GO TO 90
   80 CONTINUE
   90 HL = H1 + H2 + H3
      IF (HL .NE. 0.) GO TO 120
      IF (ITYPE .EQ. 24) RETURN
C
  100 SC(3) = 0.
      SC(4) = 0.
      SC(5) = 0.
      CALL WRITE (SCR6,SC,2,0)
      ISC2  = ISC(2)
      DO 110 I = 1,ISC2
      CALL WRITE (SCR6,SC(3),3,0)
  110 CONTINUE
      RETURN
C
  120 IF (ITYPE .EQ. 24) GO TO 130
C
C     AVERGAGE SPCFLD
C
      AHCX = G1/FLOAT(NGRID)
      AHCY = G2/FLOAT(NGRID)
      AHCZ = G3/FLOAT(NGRID)
C
  130 IF (ELTYPE .GE. 65) GO TO 500
      IF (ONLYC) GO TO 140
C
C     GET MATERIAL INFO
C     INFLAG = 3  RETURNS A 3X3 MATRIX
C
      LL(1,1) = .25
      LL(2,1) = .25
      LL(3,1) = .25
      LL(4,1) = .25
      LL(1,2) = .5
      LL(2,2) = 1./6.
      LL(3,2) = LL(2,2)
      LL(4,2) = LL(2,2)
      LL(1,3) = 1./6.
      LL(2,3) = .5
      LL(3,3) = LL(1,3)
      LL(4,3) = LL(1,3)
      LL(1,4) = 1./6.
      LL(2,4) = LL(1,4)
      LL(3,4) = .5
      LL(4,4) = LL(1,4)
      LL(1,5) = 1./6.
      LL(2,5) = LL(1,5)
      LL(3,5) = LL(1,5)
      LL(4,5) = .5
      W(1)    =-.8
      W(2)    = 9./20.
      W(3)    = W(2)
      W(4)    = W(2)
      W(5)    = W(2)
      INFLAG  = 3
      MATID   = NECPT(MID)
      ELTEMP  = ECPT(ITEMP)
      CALL HMAT (NECPT(1))
C
C     G STORED BY ROW
C
      G(1) = XMAT(1)
      G(2) = XMAT(2)
      G(3) = XMAT(3)
      G(4) = XMAT(2)
      G(5) = XMAT(4)
      G(6) = XMAT(5)
      G(7) = XMAT(3)
      G(8) = XMAT(5)
      G(9) = XMAT(6)
C
C     PUT COORDINATES OF GRID POINTS INTO ARRAY
C     FOR HEXA2  DIVIDE VOLUME BY 2.
C
      XM = 1.
      IF (ELTYPE .EQ. 42) XM = 2.
C
C     TETRA   4 GRID PTS    1 ELEMENT
C     WEDGE   6 GRID PTS   18 ELEMENTS(6 ARE DUPLICATES-4 POINTS AT A
C     HEXA1   8 GRID PTS    5 ELEMENT (4 PTS AT A TIME)
C     HEXA2   8 GRID PTS    10ELEMENT (4 PTS AT A TIME)
C     SET UP PROPER POINTERS VIA TMAP
C     R ARRAY CONTAINS COORDINATE INFO
C
  140 DO 150 I = 1,NGRID
      ITT    = ISYS1 + 4*I - 4
      R(1,I) = ECPT(ITT+1)
      R(2,I) = ECPT(ITT+2)
      R(3,I) = ECPT(ITT+3)
  150 CONTINUE
C
C     SET UP POINTER TO GRID PT NO
C
      IROW = 0
      IF (ELTYPE.EQ.41 .OR. ELTYPE.EQ.42) IROW = 12
      DO 160 I = 1,8
  160 XLOAD(I) = 0.0
C
C     SET UP POINTS FOR AVERAGE COORDINATES
C
      XXC = 0.
      YYC = 0.
      ZZC = 0.
      DO 170 I = 1,NGRID
      XXC = XXC + R(1,I)
      YYC = YYC + R(2,I)
      ZZC = ZZC + R(3,I)
  170 CONTINUE
      XXC = XXC/FLOAT(NGRID)
      YYC = YYC/FLOAT(NGRID)
      ZZC = ZZC/FLOAT(NGRID)
C
C     PRINCIPAL LOOP OVER ELEMENT OF THE GIVEN TYPE
C
      DO 400 IEL = 1,NELS
      IF (ONLYC) GO TO 200
C
C     RESET XM FOR WEDGES. 1ST 12 CONFIGURATIONS ARE MULTIPLIED BY 2.
C     ALL 18 ARE DIVIDED BY 6.(SINCE XM IS A DIVISOR, USE RECIPROCALS)
C
      IF (ELTYPE.EQ.40 .AND. IEL.LE.6) XM = 6./2.
      IF (ELTYPE.EQ.40 .AND. IEL.GT.6) XM = 6.
      ISUB = (IROW+IEL-1)*4
      DO 180 I = 1,4
      F(I)  = 0.
      IP(I) = I
      IF (ELTYPE .GE. 40) IP(I) = TMAP(ISUB+I)
  180 CONTINUE
C
C     NEED DET TO COMPUTE VOL
C
      TERM1 =  R(3,I4)*((R(1,I2)-R(1,I1))*R(2,I3) +
     1        (R(1,I1)-R(1,I3))*R(2,I2) + (R(1,I3)-R(1,I2))*R(2,I1))
      TERM2 =  R(3,I3)*((R(1,I1)-R(1,I2))*R(2,I4) +
     1        (R(1,I4)-R(1,I1))*R(2,I2) + (R(1,I2)-R(1,I4))*R(2,I1))
      TERM3 =  R(3,I2)*((R(1,I3)-R(1,I1))*R(2,I4) + (R(1,I1)-R(1,I4))*
     1         R(2,I3) + (R(1,I4)-R(1,I3))*R(2,I1))
      TERM4 =  R(3,I1)*((R(1,I2)-R(1,I3))*R(2,I4) + (R(1,I4)-R(1,I2))*
     1         R(2,I3) + (R(1,I3)-R(1,I4))*R(2,I2))
      DET   =  TERM1 + TERM2 + TERM3 + TERM4
      VOL   =  ABS(DET)/6.
C
C     GRADIENTS OF SHAPE FUNCTIONS
C
      DR( 1) = R(3,I3)*R(2,I4) - R(3,I4)*R(2,I3) + R(2,I2)*(R(3,I4)-
     1         R(3,I3)) - R(3,I2)*(R(2,I4)-R(2,I3))
      DR( 2) = R(1,I3)*R(3,I4) - R(1,I4)*R(3,I3) - R(1,I2)*(R(3,I4)-
     1         R(3,I3)) + R(3,I2)*(R(1,I4)-R(1,I3))
      DR( 3) = R(2,I3)*R(1,I4) - R(1,I3)*R(2,I4) + R(1,I2)*(R(2,I4)-
     1         R(2,I3)) - R(2,I2)*(R(1,I4)-R(1,I3))
      DR( 4) = R(2,I3)*R(3,I4) - R(2,I4)*R(3,I3) - R(2,I1)*(R(3,I4)-
     1         R(3,I3)) + R(3,I1)*(R(2,I4)-R(2,I3))
      DR( 5) = R(1,I4)*R(3,I3) - R(1,I3)*R(3,I4) + R(1,I1)*(R(3,I4)-
     1         R(3,I3)) - R(3,I1)*(R(1,I4)-R(1,I3))
      DR( 6) = R(1,I3)*R(2,I4) - R(2,I3)*R(1,I4) - R(1,I1)*(R(2,I4)-
     1         R(2,I3)) + R(2,I1)*(R(1,I4)-R(1,I3))
      DR( 7) = R(3,I2)*R(2,I4) - R(2,I2)*R(3,I4) + R(2,I1)*(R(3,I4)-
     1         R(3,I2)) - R(3,I1)*(R(2,I4)-R(2,I2))
      DR( 8) = R(1,I2)*R(3,I4) - R(1,I4)*R(3,I2) - R(1,I1)*(R(3,I4)-
     1         R(3,I2)) + R(3,I1)*(R(1,I4)-R(1,I2))
      DR( 9) = R(2,I2)*R(1,I4) - R(1,I2)*R(2,I4) + R(1,I1)*(R(2,I4)-
     1         R(2,I2)) - R(2,I1)*(R(1,I4)-R(1,I2))
      DR(10) = R(2,I2)*R(3,I3) - R(3,I2)*R(2,I3) - R(2,I1)*(R(3,I3)-
     1         R(3,I2)) + R(3,I1)*(R(2,I3)-R(2,I2))
      DR(11) = R(3,I2)*R(1,I3) - R(1,I2)*R(3,I3) + R(1,I1)*(R(3,I3)-
     1         R(3,I2)) - R(3,I1)*(R(1,I3)-R(1,I2))
      DR(12) = R(1,I2)*R(2,I3) - R(2,I2)*R(1,I3) - R(1,I1)*(R(2,I3)-
     1         R(2,I2)) + R(2,I1)*(R(1,I3)-R(1,I2))
C
      DO 190  K = 1,12
  190 DR(K) = DR(K)/DET
C
C     MULTIPLY SHAPE FUNCTION  BY G
C
      IF (ITYPE .NE. 24) CALL GMMATS (DR(1),4,3,0,G,3,3,0,DR(13))
C
C     COMPUTE HC
C
      IF (ITYPE .NE. 24) GO TO 200
C
C     REMFLUX
C
      NSUBX = ISTART + 3*NCOUNT - 3
      HC(1) = Z(NSUBX)
      HC(2) = Z(NSUBX+1)
      HC(3) = Z(NSUBX+2)
      GO TO 360
C
C     INTEGRATE TO GET HC
C
  200 KTYPE = ITYPE - 19
      XLACC(1) = 0.
      XLACC(2) = 0.
      XLACC(3) = 0.
C
C     START INTEGRATION PROCEDURE-NEED 5 POINTS FOR CUBIC + CENTROID
C
      DO 340 NPTS = 1,NOPTS
C
C     DO CENTROID FOR ONLY 1ST TETRA
C
      IF (NPTS.EQ.NOPTS .AND. IEL.GT.1) GO TO 340
C
C     COMPUTE BASIC COORDS OF INTEGRATION POINT
C
      IF (NPTS .NE. NOPTS) GO TO 210
C
C     CENTROID
C
      XX = XXC
      YY = YYC
      ZZ = ZZC
      IF (ITYPE .NE. 20) GO TO 220
C
C     AVERAGE SPCFLD
C
      HC(1) = AHCX
      HC(2) = AHCY
      HC(3) = AHCZ
      GO TO 310
  210 XX = LL(1,NPTS)*R(1,I1) + LL(2,NPTS)*R(1,I2) + LL(3,NPTS)*R(1,I3)
     1   + LL(4,NPTS)*R(1,I4)
      YY = LL(1,NPTS)*R(2,I1) + LL(2,NPTS)*R(2,I2) + LL(3,NPTS)*R(2,I3)
     1   + LL(4,NPTS)*R(2,I4)
      ZZ = LL(1,NPTS)*R(3,I1) + LL(2,NPTS)*R(3,I2) + LL(3,NPTS)*R(3,I3)
     1   + LL(4,NPTS)*R(3,I4)
  220 HC(1) = 0.
      HC(2) = 0.
      HC(3) = 0.
C
C     COMPUTE HC AT THIS PPOINT FOR ALL LOADS OF THIS TYPE IN THIS
C     SUBCASE
C
      DO 300 IJK = 1,IDO
      IF (ITYPE .EQ. 20) GO TO 240
      ISUB = ISTART + (IJK-1)*IWORDS - 1
      DO 230 I = 1,IWORDS
  230 BUF(I) = Z(ISUB+I)
C
      GO TO (240,260,270,280), KTYPE
C
C     SPCFLD
C
  240 DO 250 I = 1,4
      ISIL = FRSTGD - 1 + IP(I)
      IST  = ISTART + 3*NECPT(ISIL) - 3
      HCX(I) = Z(IST  )
      HCY(I) = Z(IST+1)
      HCZ(I) = Z(IST+2)
  250 CONTINUE
      HC1 = LL(1,NPTS)*HCX(1) + LL(2,NPTS)*HCX(2) + LL(3,NPTS)*HCX(3) +
     1      LL(4,NPTS)*HCX(4)
      HC2 = LL(1,NPTS)*HCY(1) + LL(2,NPTS)*HCY(2) + LL(3,NPTS)*HCY(3) +
     1      LL(4,NPTS)*HCY(4)
      HC3 = LL(1,NPTS)*HCZ(1) + LL(2,NPTS)*HCZ(2) + LL(3,NPTS)*HCZ(3) +
     1      LL(4,NPTS)*HCZ(4)
      GO TO 290
C
C     CEMLOOP,GEMLOOP,MDIPOLE
C
  260 CALL AXLOOP (BUF,IBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 290
  270 CALL GELOOP (BUF,IBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 290
  280 CALL DIPOLE (BUF,IBUF,XX,YY,ZZ,HC1,HC2,HC3)
  290 HC(1) = HC(1) + HC1
      HC(2) = HC(2) + HC2
      HC(3) = HC(3) + HC3
  300 CONTINUE
  310 IF (NPTS .NE. NOPTS) GO TO 320
      SC(3) = HC(1)
      SC(4) = HC(2)
      SC(5) = HC(3)
      CALL WRITE (SCR6,SC,5,0)
      GO TO 340
C
C     WE HAVE HC AT THIS POINT. MULT. BY  WEIGHT AND ACCUMULATE
C
  320 DO 330 I = 1,3
  330 XLACC(I) = XLACC(I) + HC(I)*W(NPTS)
C
C     GET ANOTHER INTEGRATTION POINT
C
  340 CONTINUE
C
      IF (ONLYC) RETURN
      DO 350 I = 1,3
  350 HC(I) = XLACC(I)
  360 CONTINUE
C
C     MULTIPLY HC BY GRADIENTS AND MATERIALS
C
      ISUBX = 13
      IF (ITYPE .EQ. 24) ISUBX = 1
      CALL GMMATS (DR(ISUBX),4,3,0,HC,3,1,0,F(1))
C
      DO 370 K = 1,4
      KK = IP(K)
  370 XLOAD(KK) = XLOAD(KK) + F(K)*VOL/XM
C
C     XLOAD   IS SUM OF ALL LOADS FOR ALL THE ELEMENTS
C     F COMPUTED FOR A GIVEN TETRA OF THE TOTAL SHAPE
C     SO MULTIPLY BY VOL
C
  400  CONTINUE
C
      DO 430  I = 1,NGRID
      IS   = FRSTGD - 1 + I
      ISIL = NECPT(IS)
C
C     IF PERMBDY EXISTS AND IF GRID IS NOT ON IT, IGNORE ITS LOAD
C
      IF (NBDYS .EQ. 0) GO TO 420
      DO 410 J = 1,NBDYS
      IF (ISIL .NE. IZ(ISTART-NBDYS-NELOUT+J-1)) GO TO 410
      GO TO 420
  410 CONTINUE
      GO TO 430
  420 Z(ISIL) = Z(ISIL) - XLOAD(I)
  430 CONTINUE
      RETURN
C
C     ISOPARAMETRIC SOLIDS
C
  500 JTYPE = ITYPE
      ITYPE = ELTYPE - 64
      INIP  = NECPT(NIP)
      IF (INIP .EQ. 0) INIP = ITYPE/2 + 2
      NP    = 12*ITYPE - 4
      ELID  = NECPT(1)
C
C     SET UP FOR FETCHING SHAPE FUNCTIONS
C
      DO 510 I = 1,NP
      GPT(I) = ECPT(ITEMP-1+I)
      DO 510 J = 1,3
      BXYZ(J,I) = ECPT(NP+4+4*I+J)
  510 CONTINUE
      IF (ONLYC) GO TO 570
      I = INIP - 1
      GO TO (520,530,540), I
  520 H(1)  = 1.
      S(1)  = GAUSS(1)
      H(2 ) = H(1)
      S(2)  = -S(1)
      GO TO 550
  530 H(1)  = GAUSS(2)
      S(1)  = GAUSS(3)
      H(2 ) = GAUSS(4)
      S(2)  = 0.
      H(3 ) = H(1)
      S(3 ) = -S(1)
      GO TO 550
  540 H(1 ) = GAUSS(5)
      S(1)  = GAUSS(6)
      H(2)  = GAUSS(7)
      S(2)  = GAUSS(8)
      H(3)  = H(2)
      S(3)  = -S(2)
      H(4) = H(1)
      S(4) = -S(1)
  550 DO 560 I = 1,32
  560 F(I) = 0.0
C
C     SET UP HC ARRAY GIVING HC AT EACH GRID
C
      IF (JTYPE .NE. 24) GO TO 570
C
C     REMFLUX
C
      ISUB  = ISTART + 3*NCOUNT - 3
      GH(1) = Z(ISUB)
      GH(2) = Z(ISUB+1)
      GH(3) = Z(ISUB+2)
      GO TO 610
C
C     IF SPCFLD,PICK UP GRID VALUES HERE. IF NOT, PICK UP INTEGRATION
C     POINT VALUES LATER.(THERE IS ONLY ONE SPCFLD CARD AT THIS POINT)
C
  570 IF (JTYPE .NE. 20) GO TO 590
      DO 580 I = 1,NP
      ISIL = 3*NECPT(I+1)
      HC(3*I-2) = Z(ISTART+ISIL-3)
      HC(3*I-1) = Z(ISTART+ISIL-2)
      HC(3*I  ) = Z(ISTART+ISIL-1)
  580 CONTINUE
  590 INFLAG = 3
      MATID  = NECPT(MID)
  610 KTYPE  = JTYPE - 20
      IF (ONLYC) GO TO 850
C
C     START INTEGRATION
C
      DO 800 I = 1,INIP
      DO 800 J = 1,INIP
      DO 800 K = 1,INIP
C
C     FETCH SHAPE FUNCTIONS FOR THIS INTEGRATION POINT
C
      CALL IHEXSS(ITYPE,SHP,DSHP,XJACOB,DETJ,ELID,S(I),S(J),S(K),BXYZ)
C
C     COMPUTE NI W.R.T. X,Y,Z(REVERVSE CALLING SEQUENCE,SINCE COL STOR)
C
      CALL GMMATS(DSHP,NP,3,0,XJACOB,3,3,0,DSHPB)
C
C     COMPUTE TEMPERATURES AND HC  AT THIS INTEGRSTION POINT
C
      ELTEMP = 0
      DO 620 L = 1,NP
      ELTEMP = ELTEMP + SHP(L)*GPT(L)
  620 CONTINUE
      IF (JTYPE .EQ. 24) GO TO 730
      HCXYZ(1) = 0.
      HCXYZ(2) = 0.
      HCXYZ(3) = 0.
      IF (JTYPE .EQ. 20) GO TO 700
C
C     FOR LOOPS AND DIPOLES, COMPUTE BASIC COORDS FOR THIS INTEGRATION
C     POINT
C
      XX = 0.
      YY = 0.
      ZZ = 0.
      DO 630 L = 1,NP
      XX = XX + SHP(L)*BXYZ(1,L)
      YY = YY + SHP(L)*BXYZ(2,L)
      ZZ = ZZ + SHP(L)*BXYZ(3,L)
  630 CONTINUE
      DO 690 IJK = 1,IDO
      ISUB = ISTART + (IJK-1)*IWORDS - 1
      DO 640 L = 1,IWORDS
  640 BUF(L) = Z(ISUB+L)
C
C     COMPUTE HC AT THIS POINT DUE TO ALL LOADS OF PRESENT TYPE
C
      GO TO (650,660,670), KTYPE
  650 CALL AXLOOP (BUF,IBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 680
  660 CALL GELOOP (BUF,IBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 680
  670 CALL DIPOLE (BUF,IBUF,XX,YY,ZZ,HC1,HC2,HC3)
  680 HCXYZ(1) = HCXYZ(1) + HC1
      HCXYZ(2) = HCXYZ(2) + HC2
      HCXYZ(3) = HCXYZ(3) + HC3
  690 CONTINUE
      GO TO 720
C
C     SPCFLD
C
  700 DO 710  L = 1,NP
      HCXYZ(1) = HCXYZ(1) + SHP(L)*HC(3*L-2)
      HCXYZ(2) = HCXYZ(2) + SHP(L)*HC(3*L-1)
      HCXYZ(3) = HCXYZ(3) + SHP(L)*HC(3*L  )
  710 CONTINUE
C
      CALL HMAT(ELID)
C
  720 G(1) = XMAT(1)
      G(2) = XMAT(2)
      G(3) = XMAT(3)
      G(4) = XMAT(2)
      G(5) = XMAT(4)
      G(6) = XMAT(5)
      G(7) = XMAT(3)
      G(8) = XMAT(5)
      G(9) = XMAT(6)
C
      CALL GMMATS (G,3,3,0,HCXYZ,3,1,0,GH)
C
  730 SFACT = H(I)*H(J)*H(K)*DETJ
      DO 740 L = 1,NP
      F(L) = F(L) + (DSHPB(1,L)*GH(1) + DSHPB(2,L)*GH(2) +
     1       DSHPB(3,L)*GH(3))*SFACT
  740 CONTINUE
C
C     GET ANOTHER INTEGRATIONPOINT
C
  800 CONTINUE
C
C     ADD LOADS INTO LOAD ARRAY
C
      DO 840 L = 1,NP
      ISIL = NECPT(FRSTGD+L-1)
C
C     IF PERMBDY EXISTS AND IF GRID IS NOT ON IT, IGNORE ITS LOAD
C
      IF (NBDYS .EQ. 0) GO TO 830
      DO 820 I = 1,NBDYS
      IF (ISIL .NE. IZ(ISTART-NBDYS-NELOUT+I-1)) GO TO 820
      GO TO 830
  820 CONTINUE
      GO TO 840
  830 Z(ISIL) = Z(ISIL) - F(L)
  840 CONTINUE
  850 ITYPE = JTYPE
C
C     BEFORE LEAVING, WE MUST COMPUTE HC VALUES AT GRIDS OF ISOPARA-
C     METRICS AND WRITE TO SCR6
C
      IF (JTYPE .EQ. 24) GO TO 1150
      CALL WRITE (SCR6,ISC,2,0)
      IF (JTYPE .NE. 20) GO TO 1010
C
C     FOR SPCFLD THE VALUES ARE IN CORE(EXCEPT FOR MIFPOINTS OF IHEX3)
C
      IF (ELTYPE .EQ. 67) GO TO 880
      CALL WRITE (SCR6,HC,3*NP,0)
C
C     CENTROID/ XI = ETA = ZETA = 0
C
  860 CALL IHEXSS (ELTYPE-64,SHP,DSHP,XJACOB,DETJ,ELID,0.,0.,0.,BXYZ)
      HCX3(1) = 0.
      HCX3(2) = 0.
      HCX3(3) = 0.
      DO 870 L = 1,NP
      HCX3(1) = HCX3(1) + SHP(L)*HC(3*L-2)
      HCX3(2) = HCX3(2) + SHP(L)*HC(3*L-1)
      HCX3(3) = HCX3(3) + SHP(L)*HC(3*L  )
  870 CONTINUE
      CALL WRITE (SCR6,HCX3,3,0)
      GO TO 1150
  880 ISUB1 = 1
      ISUB2 = 10
      J = -5
  890 DO 900 I = ISUB1,ISUB2,3
      J = J + 6
      K = 3*I - 2
      HCX3(J  ) = HC(K  )
      HCX3(J+1) = HC(K+1)
      HCX3(J+2) = HC(K+2)
      HCX3(J+3) = .5*(HC(K+3) + HC(K+6))
      HCX3(J+4) = .5*(HC(K+4) + HC(K+7))
      HCX3(J+5) = .5*(HC(K+5) + HC(K+8))
  900 CONTINUE
      IF (ISUB1 .EQ. 21) GO TO 1000
      J = 22
      DO 910 I = 13,16
      J = J + 3
      K = 3*I - 2
      HCX3(J  ) = .5*(HC(K  ) + HC(K+12))
      HCX3(J+1) = .5*(HC(K+1) + HC(K+13))
      HCX3(J+2) = .5*(HC(K+2) + HC(K+14))
  910 CONTINUE
      ISUB1 = 21
      ISUB2 = 30
      J = 31
      GO TO 890
C
C     DONE - WRITE RESULTS
C
 1000 CALL WRITE (SCR6,HCX3,60,0)
      GO TO 860
C
C     CEMLOOP, GEMLOOP, MDIPOLE
C
 1010 NX = NP + 1
      IF (ELTYPE .EQ. 67) NX = 21
      DO 1140 J = 1,NX
      IF (J .NE. NX) GO TO 1030
C
C     CENTROID
C
      CALL IHEXSS (ELTYPE-64,SHP,DSHP,XJACOB,DETJ,ELID,0.,0.,0.,BXYZ)
      XX = 0.
      YY = 0.
      ZZ = 0.
      DO 1020 L = 1,NP
      XX = XX + SHP(L)*BXYZ(1,L)
      YY = YY + SHP(L)*BXYZ(2,L)
      ZZ = ZZ + SHP(L)*BXYZ(3,L)
 1020 CONTINUE
      GO TO 1070
C
 1030 IF (ELTYPE .NE. 67) GO TO 1060
C
C     IHEX3
C
      IF (J.EQ. 1) K1 =-1
      IF (J.EQ.13) K1 = 7
      IF (J.EQ. 1) K2 =-1
      IF (J.EQ.13) K2 = 7
      IF (J.LT.9 .OR. J.GT.12) GO TO 1040
      XX = .5*(BXYZ(1,J+4) + BXYZ(1,J+8))
      YY = .5*(BXYZ(2,J+4) + BXYZ(2,J+8))
      ZZ = .5*(BXYZ(3,J+4) + BXYZ(3,J+8))
      GO TO 1070
 1040 IF ((J/2)*2 .NE. J)GO TO 1050
      K1 = K1 + 1
      XX = .5*(BXYZ(1,J+K1) + BXYZ(1,J+K1+1))
      YY = .5*(BXYZ(2,J+K1) + BXYZ(2,J+K1+1))
      ZZ = .5*(BXYZ(3,J+K1) + BXYZ(3,J+K1+1))
      GO TO 1070
 1050 K2 = K2 + 1
      XX = BXYZ(1,J+K2)
      YY = BXYZ(2,J+K2)
      ZZ = BXYZ(3,J+K2)
      GO TO 1070
C
 1060 XX = BXYZ(1,J)
      YY = BXYZ(2,J)
      ZZ = BXYZ(3,J)
 1070 HC(1) = 0.
      HC(2) = 0.
      HC(3) = 0.
      DO 1130 IJK = 1,IDO
      ISUB = ISTART + (IJK-1)*IWORDS - 1
      DO 1080 I = 1,IWORDS
 1080 BUF(I) = Z(ISUB+I)
C
C     COMPUTE HC AT THIS POINT
C
      GO TO (1090,1100,1110), KTYPE
 1090 CALL AXLOOP (BUF,IBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 1120
 1100 CALL GELOOP (BUF,IBUF,XX,YY,ZZ,HC1,HC2,HC3)
      GO TO 1120
 1110 CALL DIPOLE (BUF,IBUF,XX,YY,ZZ,HC1,HC2,HC3)
 1120 HC(1) = HC(1) + HC1
      HC(2) = HC(2) + HC2
      HC(3) = HC(3) + HC3
 1130 CONTINUE
C
      CALL WRITE (SCR6,HC,3,0)
 1140 CONTINUE
C
 1150 RETURN
C
 1200 WRITE  (OUTPT,1210) UFM
 1210 FORMAT (A23,' - WRONG ELEMENT TYPE IN EM3D PROBLEM.')
      CALL MESAGE (-61,0,0)
      RETURN
      END
