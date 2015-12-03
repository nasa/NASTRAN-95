      SUBROUTINE KROD
C*****
C THIS ROUTINE COMPUTES THE TWO 6 X 6 MATRICES  K(NPVT,NPVT) AND
C K(NPVT,J) FOR A ROD HAVING END POINTS NUMBERED NPVT AND J.
C*****
C
C                        E C P T  F O R  T H E  R O D
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
C
      LOGICAL  HEAT
C
      DOUBLE PRECISION
     1                   X                  ,Y
     2,                  Z                  ,XL
     3,                  XN                 ,DSCL
     4,                  DSCR               ,DAMPC
     5,                  D                  ,KE
     6,                  TI                 ,DUMDP
C
      DIMENSION
     1                   IECPT(4)
      COMMON /BLANK/ICOM
      COMMON   /SYSTEM/
     1                   ISYS
C
C SMA1 I/O PARAMETERS
C
      COMMON   /SMA1IO/
     1                   IFCSTM             ,IFMPT
     2,                  IFDIT              ,IDUM1
     3,                  IFECPT             ,IGECPT
     4,                  IFGPCT             ,IGGPCT
     5,                  IFGEI              ,IGGEI
     6,                  IFKGG              ,IGKGG
     7,                  IF4GG              ,IG4GG
     8,                  IFGPST             ,IGGPST
     9,                  INRW               ,OUTRW
     T,                  CLSNRW             ,CLSRW
     1,                  NEOR               ,EOR
     2,                  MCBKGG(7)          ,MCB4GG(7)
C
C SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
C
      COMMON   /SMA1BK/
     1                   ICSTM              ,NCSTM
     2,                  IGPCT              ,NGPCT
     3,                  IPOINT             ,NPOINT
     4,                  I6X6K              ,N6X6K
     5,                  I6X64              ,N6X64
C
C SMA1 PROGRAM CONTROL PARAMETERS
C
      COMMON   /SMA1CL/
     1                   IOPT4              ,K4GGSW
     2,                  NPVT               ,LEFT
     3,                  FROWIC             ,LROWIC
     4,                  NROWSC             ,TNROWS
     5,                  JMAX               ,NLINKS
     6,                  LINK(10)           ,IDETCK
     7,                  DODET              ,NOGO
      COMMON/SMA1HT/     HEAT
C
C ECPT COMMON BLOCK
C
      COMMON   /SMA1ET/
     1                   ECPT(17)           ,DUMET(83)
C
C INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON   /MATIN/
     1                   MATIDC             ,MATFLG
     2,                  ELTEMP             ,STRESS
     3,                  SINTH              ,COSTH
      COMMON   /MATOUT/
     1                   E                  ,G
     2,                  NU                 ,RHO
     3,                  ALPHA              ,TSUBO
     4,                  GSUBE              ,SIGT
     5,                  SIGC               ,SIGS
      COMMON/HMTOUT/     FK
C
C LOCAL DOUBLE PRECISION VARIABLES
C
      COMMON   /SMA1DP/
     1                   X                  ,Y
     2,                  Z                  ,XL
     3,                  XN(3)              ,DSCL
     4,                  DSCR               ,DAMPC
     5,                  D(18)              ,KE(36)
     6,                  TI(9)              ,DUMDP(227)
C
C
C NOTE THAT EQUIVALENCE IS NECESSARY SINCE ECPT IS A MIXED --- INTEGERS
C AND REAL --- ARRAY
C
      EQUIVALENCE
     1                   (IECPT(1),ECPT(1))
C*****
C  BRANCH ON HEAT FORMULATION.
C*****
      IF( HEAT ) GO TO 200
      IF (IECPT(2) .EQ. NPVT) GO TO 10
      IF (IECPT(3) .NE. NPVT) CALL MESAGE (-30,34,IECPT(1))
      ITEMP = IECPT(2)
      IECPT(2) = IECPT(3)
      IECPT(3) = ITEMP
      KA  = 13
      KB  =  9
      GO TO 20
   10 KA  =  9
      KB  =  13
C
C AT THIS POINT KA POINTS TO THE COOR. SYS. ID. OF THE PIVOT GRID POINT.
C SIMILARLY FOR KB AND THE NON-PIVOT GRID POINT.
C NOW COMPUTE THE LENGTH OF THE ROD.
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
      IF (XL.NE.0.0D0) GO TO 30
      CALL MESAGE(30,26,IECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULA
C
      NOGO=1
      RETURN
   30 CONTINUE
C
C CALCULATE A NORMALIZED DIRECTION VECTOR IN BASIC COORDINATES.
C
      XN(1) = X / XL
      XN(2) = Y / XL
      XN(3) = Z / XL
C
C LOCATE E = YOUNG-S MODULUS, G = SHEAR MODULUS AND DAMPC = DAMPING
C CONSTANT IN THE MAT1 TABLE AND COMPUTE DSCL = A * E / XL AND
C DSCR = J * G / XL.  A IS ECPT(5) AND J IS ECPT(6)
C
      MATIDC = IECPT(4)
      MATFLG = 1
      ELTEMP = ECPT(17)
      CALL MAT (IECPT(1))
C
C WE STORE ECPT(5), ECPT(6), E AND G IN DOUBLE PRECISION LOCATIONS SO
C THAT ALL ARITHMETIC WILL BE DOUBLE PRECISION
C
      D(1) = ECPT(5)
      D(2) = E
      D(3) = ECPT(6)
      D(4) = G
      DSCL = D(1) * D(2) / XL
      DSCR = D(3) * D(4) / XL
      DAMPC  = G SUB E
C
C SET UP THE -N- MATRIX AND STORE AT D(1)
C
      D(1) = XN(1) * XN(1)
      D(2) = XN(1) * XN(2)
      D(3) = XN(1) * XN(3)
      D(4) = D(2)
      D(5) = XN(2) * XN(2)
      D(6) = XN(2) * XN(3)
      D(7) = D(3)
      D(8) = D(6)
      D(9) = XN(3) * XN(3)
C
C ZERO OUT THE 6X6 WHICH WILL BE USED FOR STORAGE OF KGG(NPVT,NONPVT),
C NONPVT = NPVT,J
C KGG(NPVT,NONPVT), NONPVT = NPVT,J
C
      DO 50 I = 1,36
   50 KE(I) = 0.0D0
      NONPVT = 2
      K2 = 1
C
C IF PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 70
C
      IF (IECPT(KA) .EQ. 0) GO TO 70
      CALL TRANSD (ECPT(KA),TI(1))
      CALL GMMATD (TI(1),3,3,1, D(1),3,3,0, D(10))
      CALL GMMATD (D(10),3,3,0, TI(1),3,3,0, D(1))
C
C AT THIS POINT D(1) CONTAINS THE MATRIX PRODUCT TAT * N * TA
C AND D(10) CONTAINS THE MATRIX PRODUCT TAT * N.
C
      ASSIGN 100 TO IRETRN
      GO TO 80
   70 ASSIGN 90 TO IRETRN
C
C FILL THE KE MATRIX
C
   80 KE( 1) = DSCL * D(K2  )
      KE( 2) = DSCL * D(K2+1)
      KE( 3) = DSCL * D(K2+2)
      KE( 7) = DSCL * D(K2+3)
      KE( 8) = DSCL * D(K2+4)
      KE( 9) = DSCL * D(K2+5)
      KE(13) = DSCL * D(K2+6)
      KE(14) = DSCL * D(K2+7)
      KE(15) = DSCL * D(K2+8)
      KE(22) = DSCR * D(K2  )
      KE(23) = DSCR * D(K2+1)
      KE(24) = DSCR * D(K2+2)
      KE(28) = DSCR * D(K2+3)
      KE(29) = DSCR * D(K2+4)
      KE(30) = DSCR * D(K2+5)
      KE(34) = DSCR * D(K2+6)
      KE(35) = DSCR * D(K2+7)
      KE(36) = DSCR * D(K2+8)
      CALL SMA1B (KE,ECPT(NONPVT),-1,IFKGG,0.0D0)
      IF (IOPT4 .EQ. 0  .OR.  G SUB E .EQ. 0.0) GO TO 85
      K4GGSW = 1
      CALL SMA1B (KE,ECPT(NONPVT),-1,IF4GG,DAMPC)
C
C  RETURN  FROM  FILL  CODE W/ IRETRN = 90 IMPLIES G.P. A WAS IN BASIC
C    .      .     .      .      .     =100 IMPLIES G.P. A WAS NOT BASIC
C    .      .     .      .      .     =140 IMPLIES THE K(NPVT,NONPVT)
C                                      HAS BEEN COMPUTED AND INSERTED
C                                      AND HENCE WE ARE FINISHED.
C
   85 GO TO IRETRN , (90,100,140)
   90 K1 = 1
      K2 = 10
      GO TO 110
  100 K1 = 10
      K2 = 1
  110 NONPVT = 3
C
C IF NON-PIVOT GRID POINT IS IN BASIC COORDINATES, GO TO 120
C
      IF (IECPT(KB) .EQ. 0) GO TO 120
      CALL TRANSD (ECPT(KB),TI(1))
C
C RECALL THAT D(K1) CONTAINS TAT * N.
C
      CALL GMMATD (D(K1),3,3,0, TI(1),3,3,0, D(K2))
C
C AT THIS POINT D(K2) CONTAINS TAT * N * TB.
C
      GO TO 130
  120 K2 = K1
  130 ASSIGN 140 TO IRETRN
C
C SET CONSTANTS NEGATIVE TO PROPERLY COMPUTE K(NPVT,NONPVT)
C
      DSCR = -DSCR
      DSCL = -DSCL
      GO TO 80
C
C A TRANSFER TO STATEMENT NO. 140 IMPLIES KGG AND/OR K4GG CALCULATIONS
C HAVE BEEN COMPLETED.
C
  140 RETURN
C*****
C  HEAT FORMULATION.  FIRST COMPUTE LENGTH OF ELEMENT.
C*****
  200 X = ECPT(14) - ECPT(10)
      Y = ECPT(15) - ECPT(11)
      Z = ECPT(16) - ECPT(12)
      XL= DSQRT(X**2 + Y**2 + Z**2)
      IF( XL ) 300,300,400
  300 CALL MESAGE( -30, 26, IECPT(1) )
C
C     GET MATERIAL PROPERTY -K- FROM HMAT ROUTINE
C
  400 MATFLG = 1
      MATIDC = IECPT(4)
      ELTEMP = ECPT(17)
      CALL HMAT( IECPT )
C
      XL = DBLE(FK) * DBLE(ECPT(5)) / XL
C
      IF( NPVT .EQ. IECPT(3) ) XL = -XL
      DO 700 I = 1,2
      CALL SMA1B( XL, IECPT(I+1), NPVT, IFKGG, 0.0D0 )
      XL = -XL
  700 CONTINUE
      RETURN
      END
