      SUBROUTINE PKTRBS (IOPT)
C
C     BASIC BENDING TRIANGLE ELEMENT ROUTINE
C
C     THIS ROUTINE DOES SUB-CALCULATIONS FOR TRI OR QUAD PLATES IN PLA4
C
C     CALLS FROM THIS ROUTINE ARE MADE TO
C          MAT    - MATERIAL DATA ROUTINE
C
C     PLAMAT - ROTATES AND RETURNS GP MATRIX
C          TRANSD - DOUBLE PRECISION TRANSFORMATION SUPPLIER
C          INVERD - DOUBLE PRECISION INVERSE ROUTINE
C          GMMATD - DOUBLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
C          MESAGE - ERROR MESSAGE WRITER
C
C     IOPT = 1   IMPLIES COMPUTE ONLY THE NINE (3X3)MATRICES
C                WHICH FORM THE 9X9 K SUPER U - MATRIX.
C     IOPT = 2   SAME AS IOPT = 1,BUT SAVE H-INVERSE AND S
C
C     ECPT LIST FOR BASIC BENDING TRIANGLE          NAME IN
C                                                   THIS
C     ECPT                                          ROUTINE   TYPE
C     ------------------------------------        -------------------
C     ECPT( 1) = ELEMENT ID                         NECPT(1)  INTEGER
C     ECPT( 2) = GRID POINT A                       NGRID(1)  INTEGER
C     ECPT( 3) = GRID POINT B                       NGRID(2)  INTEGER
C     ECPT( 4) = GRID POINT C                       NGRID(3)  INTEGER
C     ECPT( 5) = THETA = ANGLE OF MATERIAL          ANGLE     REAL
C     ECPT( 6) = MATERIAL ID 1                      MATID1    INTEGER
C     ECPT( 7) = I = MOMENT OF INERTIA              EYE       REAL
C     ECPT( 8) = MATERIAL ID 2                      MATID2    INTEGER
C     ECPT( 9) = T2                                 T2        REAL
C     ECPT(10) = NON-STRUCTURAL-MASS                FMU       REAL
C     ECPT(11) = Z1                                 Z11       REAL
C     ECPT(12) = Z2                                 Z22       REAL
C     ECPT(13) = COORD. SYSTEM ID 1                 NECPT(13) INTEGER
C     ECPT(14) = X1                                 X1        REAL
C     ECPT(15) = Y1                                 Y1        REAL
C     ECPT(16) = Z1                                 Z1        REAL
C     ECPT(17) = COORD. SYSTEM ID 2                 NECPT(17) INTEGER
C     ECPT(18) = X2                                 X2        REAL
C     ECPT(19) = Y2                                 Y2        REAL
C     ECPT(20) = Z2                                 Z2        REAL
C     ECPT(21) = COORD. SYSTEM ID 3                 NECPT(21) INTEGER
C     ECPT(22) = X3                                 X3        REAL
C     ECPT(23) = Y3                                 Y3        REAL
C     ECPT(24) = Z3                                 Z3        REAL
C     ECPT(25) = ELEMENT TEMPERATURE                ELTEMP    REAL
C
      INTEGER          SUBSCA,SUBSCB
      DOUBLE PRECISION A,E,XSUBB,TEMP,XSUBC,D,YSUBC,XCYC,XCSQ,DETERM,
     1                 YCSQ,XBSQ,G2X2,TITE,TJTE,S,TI,J2X2,AREA,G,XBAR,
     2                 YBAR,PX2,PY2,PXY2,XBAR3,YBAR2,YBAR3,PROD9,TEMP9
      DIMENSION        D(9),G2X2(4),J2X2(4),S(18),ECPT(1),G(9),TJTE(18),
     1                 TITE(18),TI(9)
      COMMON /PLA42C/  NPVT,DUM1(148),NOGO
      COMMON /MATIN /  MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALP12,
     1                 T SUB 0, G SUB E, SIGTEN, SIGCOM, SIGSHE,
     2                 G2X211, G2X212, G2X222
C
C     ECPT BLOCK
      COMMON /PLA4ES/  NECPT(1),NGRID(3),ANGLE,MATID1,EYE,MATID2,T2,FMU,
     1                 Z11,Z22,DUMMY1,X1,Y1,Z1,DUMMY2,X2,Y2,Z2,DUMMY3,
     2                 X3,Y3,Z3,DUMB(76)
      COMMON /PLA42D/  A(225),PROD9(9),TEMP9(9),XSUBB,XSUBC,YSUBC,E(18),
     1                 TEMP,XBAR,AREA,XCSQ,YBAR2,YCSQ,YBAR,XBSQ,PX2,
     2                 XCYC,PY2,PXY2,XBAR3,YBAR3,DETERM,NSIZED,
     3                 DUMDUM(4),NPIVOT,THETA,NSUBC,ISING,SUBSCA,SUBSCB,
     4                 NBEGIN,DUMMY(30)
      EQUIVALENCE      (D(1),G(1),A(79)),(ECPT(1),NECPT(1)),
     1                 (G2X2(1),A(88)),(TJTE(1),A(100)),
     2                 (TITE(1),S(1),A(82)),(J2X2(1),A(92)),
     3                 (TI(1),A(118))
C
C
      MATID  = MATID1
      INFLAG = -1
C
      CALL PLAMAT
C
C     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
C
      G(1) = G11
      G(2) = G12
      G(3) = G13
      G(4) = G12
      G(5) = G22
      G(6) = G23
      G(7) = G13
      G(8) = G23
      G(9) = G33
C
C     COMPUTATION OF D = I.G-MATRIX (EYE IS INPUT FROM THE ECPT)
C
      DO 90 I = 1,9
   90 D(I) = G(I)*DBLE(EYE)
C
      AREA = XSUBB*YSUBC/2.0D0
      XBAR =(XSUBB + XSUBC)/3.0D0
      YBAR = YSUBC/3.0D0
C
      XCSQ  = XSUBC**2
      YCSQ  = YSUBC**2
      XBSQ  = XSUBB**2
      XCYC  = XSUBC*YSUBC
      PX2   = (XBSQ + XSUBB*XSUBC + XCSQ)/6.0D0
      PY2   = YCSQ/6.0D0
      PXY2  = YSUBC*(XSUBB + 2.0D0*XSUBC)/12.0D0
      XBAR3 = 3.0D0*XBAR
      YBAR3 = 3.0D0*YBAR
      YBAR2 = 2.0D0*YBAR
C
C                 X
C     FILL THE  (K ) MATRIX STORING IN  A(1) THRU A(36)
C
      A( 1) = D( 1)
      A( 2) = D( 3)
      A( 3) = D( 2)
      A( 4) = D( 1)*XBAR3
      A( 5) = D( 2)*XBAR + YBAR2*D(3)
      A( 6) = D( 2)*YBAR3
      A( 7) = A( 2)
      A( 8) = D( 9)
      A( 9) = D( 6)
      A(10) = D( 3)*XBAR3
      A(11) = D( 6)*XBAR + YBAR2*D(9)
      A(12) = D( 6)*YBAR3
      A(13) = A( 3)
      A(14) = A( 9)
      A(15) = D( 5)
      A(16) = D( 2)*XBAR3
      A(17) = D( 5)*XBAR + YBAR2*D(6)
      A(18) = D( 5)*YBAR3
      A(19) = A( 4)
      A(20) = A(10)
      A(21) = A(16)
      A(22) = D( 1)*9.0D0*PX2
      A(23) = D( 2)*3.0D0*PX2 + 6.0D0*PXY2*D(3)
      A(24) = D( 2)*9.0D0*PXY2
      A(25) = A( 5)
      A(26) = A(11)
      A(27) = A(17)
      A(28) = A(23)
      A(29) = D( 5)*PX2 + 4.0D0*PXY2*D(6) + 4.0D0*PY2*D(9)
      A(30) = D( 5)*3.0D0*PXY2 + 6.0D0*PY2*D(6)
      A(31) = A( 6)
      A(32) = A(12)
      A(33) = A(18)
      A(34) = A(24)
      A(35) = A(30)
      A(36) = D( 5)*9.0D0*PY2
      TEMP  = 4.0D0*AREA
      DO 70 I = 1,36
   70 A(I)  = A(I)*TEMP
C
C     F1LL  (HBAR) MATRIX STORING AT A(37) THRU A(72)
C
      DO 130 I = 37,72
  130 A(I) = 0.0D0
C
      A(37) = XBSQ
      A(40) = XBSQ*XSUBB
      A(44) = XSUBB
      A(49) =-2.0D0*XSUBB
      A(52) =-3.0D0*XBSQ
      A(55) = XCSQ
      A(56) = XCYC
      A(57) = YCSQ
      A(58) = XCSQ*XSUBC
      A(59) = YCSQ*XSUBC
      A(60) = YCSQ*YSUBC
      A(62) = XSUBC
      A(63) = YSUBC*2.0D0
      A(65) = XCYC *2.0D0
      A(66) = YCSQ *3.0D0
      A(67) =-2.0D0*XSUBC
      A(68) =-YSUBC
      A(70) =-3.0D0*XCSQ
      A(71) =-YCSQ
C
      IF (T2 .EQ. 0.0E0) GO TO 500
C
C     ALL OF THE FOLLOWING OPERATIONS THROUGH STATEMENT LABEL 500
C     ARE NECESSARY IF T2 IS NON-ZERO.
C
C     GET THE G2X2 MATRIX
C
      MATID  = MATID2
      INFLAG = 3
      CALL MAT (ECPT(1))
      IF (G2X211.EQ.0.0 .AND. G2X212.EQ.0.0 .AND. G2X222.EQ.0.0)
     1    GO TO 500
      G2X2(1) = G2X211*T2
      G2X2(2) = G2X212*T2
      G2X2(3) = G2X212*T2
      G2X2(4) = G2X222*T2
C
      DETERM  = G2X2(1)*G2X2(4) - G2X2(3)*G2X2(2)
      J2X2(1) = G2X2(4)/DETERM
      J2X2(2) =-G2X2(2)/DETERM
      J2X2(3) =-G2X2(3)/DETERM
      J2X2(4) = G2X2(1)/DETERM
C
C     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
C       YQ  RIGHT PORTION IS COMPUTED AND USED AS A  (2X3). THE LEFT
C           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
C           A(73)...A(78) UNTIL NOT NEEDED ANY FURTHER.
C
      TEMP  =  2.0D0*D(2) + 4.0D0*D(9)
      A(73) = -6.0D0*(J2X2(1)*D(1) + J2X2(2)*D(3))
      A(74) = -J2X2(1)*TEMP + 6.0D0*J2X2(2)*D(6)
      A(75) = -6.0D0*(J2X2(1)*D(6) + J2X2(2)*D(5))
      A(76) = -6.0D0*(J2X2(2)*D(1) + J2X2(4)*D(3))
      A(77) = -J2X2(2)*TEMP + 6.0D0*J2X2(4)*D(6)
      A(78) = -6.0D0*(J2X2(2)*D(6) + J2X2(4)*D(5))
C
C     THE ABOVE 6 ELEMENTS NOW REPRESENT THE (H  ) MATRIX (2X3)
C                                              YQ
C
C     NOW FORMING  PRODUCT (G2X2)(H  ) AND STORING AS AN INTERMEDIATE
C     STEP.                        YQ
C
      CALL GMMATD (G2X2(1),2,2,0, A(73),2,3,0, A(79))
C
C                                                               Y
C     WITH LAST PRODUCT  FORM  LOWER RIGHT 3 X 3 PARTITION OF (K )
C
C              Y                   T
C     THUS   (K ) PARTITION = (H  ) (LAST PRODUCT)   STORE AT A(85)
C                               YQ
C
      CALL GMMATD (A(73),2,3,1, A(79),2,3,0, A(85))
C
C                                                     X
C     NOW ADD THE 9 ELEMENTS OF THIS 3X3 PORTION TO (K )
C     PER STEP 5 PAGE -16- MS-17                            Y
C     MULTIPLY IN AREA AT SAME TIME WHICH WAS LEFT OUT OF (K ) ABOVE.
C
      DO 60 I = 1,3
      A(I+21) = A(I+21) + A(I+84)*AREA
      A(I+27) = A(I+27) + A(I+87)*AREA
   60 A(I+33) = A(I+33) + A(I+90)*AREA
C
C     ADD TO 6 OF THE (HBAR) ELEMENTS THE RESULT OF (H  )(H  )
C                                                    UY   YQ
C     THE PRODUCT IS FORMED DIRECTLY IN THE ADDITION PROCESS BELOW.
C     NO (H  ) MATRIX IS ACTUALLY COMPUTED DIRECTLY.
C          UY
C
C     THE FOLLOWING IS THEN PER STEPS 6 AND 7 PAGE -16- MS-17.
C
      DO 75 I = 1,3
      A(I+39) = A(I+39) + XSUBB*A(I+72)
   75 A(I+57) = A(I+57) + XSUBC*A(I+72) + YSUBC*A(I+75)
C
C     THIS ENDS ADDED COMPUTATION FOR CASE OF T2 NOT ZERO
C
  500 CONTINUE
C
C     AT THIS POINT INVERT  (H) WHICH IS STORED AT A(37) THRU A(72)
C     STORE INVERSE BACK IN A(37) THRU A(72)
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERD (6,A(37),6,A(73),0,DETERM,ISING,A(79))
C
C     CHECK TO SEE IF H WAS SINGULAR
C
      IF (ISING .NE. 2) GO TO 440
C
C     ISING = 2 IMPLIES SINGULAR MATRIX THUS ERROR CONDITION.
C
      CALL MESAGE (30,33,ECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
C              Q   -1
C 440 FORM   (K )(H  )  AND STORE AT  A(73) THRU A(108)
C
C                 X                     Q
C     NOTE THAT (K ) AT THIS POINT IS (K )
C
  440 CALL GMMATD (A(1),6,6,0, A(37),6,6,0, A(73))
C
C                    -1 T
C     FORM(K  ) = (H  ) (LAST PRODUCT) STORE AT A(109) THRU A(144)
C            II
C
      CALL GMMATD (A(37),6,6,1, A(73),6,6,0, A(109))
C
C     FILL S-MATRIX EQUIVALENCED TO A(82)  (S IS 6X3)
C
      IF (IOPT .NE. 2) GO TO 700
C
C     SAVE H-INVERSE TO BE USED BY TRIANGULAR PLATE ROUTINE.
C
      DO 710 I = 37,72
  710 A(I+108) = A(I)
C
  700 S( 1) = 1.0D0
      S( 2) = 0.0D0
      S( 3) =-XSUBB
      S( 4) = 0.0D0
      S( 5) = 1.0D0
      S( 6) = 0.0D0
      S( 7) = 0.0D0
      S( 8) = 0.0D0
      S( 9) = 1.0D0
      S(10) = 1.0D0
      S(11) = YSUBC
      S(12) =-XSUBC
      S(13) = 0.0D0
      S(14) = 1.0D0
      S(15) = 0.0D0
      S(16) = 0.0D0
      S(17) = 0.0D0
      S(18) = 1.0D0
C
C                     T
C     FORM   K   = K   = -K   S  STORING AT A(46)   (K   IS 6X3)
C             IA    AI     II                         IA
C
       CALL GMMATD (A(109),6,6,0, S(1),6,3,0, A(46))
C
C     THIS PRODUCT IS MULTIPLIED BY SCALER -1 BELOW.
C
C                    T
C          (K  ) = (S )(-K  )
C            AA           IA
C
C     NOTE K    HAS NOT BEEN MULTIPLIED ABOVE BY -1, THUS IGNORE MINUS
C           IA                                                   HERE.
C
      CALL GMMATD (S(1),6,3,1, A(46),6,3,0, A(1))
C
C     NOW MULTIPLY  K   BY SCALER (-1)
C                    IA
C
      DO 190 I = 46,63
  190 A(I) = -A(I)
C
C     AT THIS POINT, STORED BY ROWS ARE
C
C        K     (6X6) AT A(109) THRU A(144)
C         II
C
C        K     (6,3) AT  A(46) THRU A(63)
C         IA
C
C        K     (3X3) AT A(1) THRU A(9)
C         AA
C
C     ARRANGE NINE 3X3 MATRICES OF K SUPER U
C
      A( I) = A(I+18)
      A(10) = A( 46)
      A(11) = A( 49)
      A(12) = A( 52)
      A(13) = A( 47)
      A(14) = A( 50)
      A(15) = A( 53)
      A(16) = A( 48)
      A(17) = A( 51)
      A(18) = A( 54)
      A(19) = A( 55)
      A(20) = A( 58)
      A(21) = A( 61)
      A(22) = A( 56)
      A(23) = A( 59)
      A(24) = A( 62)
      A(25) = A( 57)
      A(26) = A( 60)
      A(27) = A( 63)
      A(37) = A(109)
      A(38) = A(110)
      A(39) = A(111)
      A(40) = A(115)
      A(41) = A(116)
      A(42) = A(117)
      A(43) = A(121)
      A(44) = A(122)
      A(45) = A(123)
      A(46) = A(112)
      A(47) = A(113)
      A(48) = A(114)
      A(49) = A(118)
      A(50) = A(119)
      A(51) = A(120)
      A(52) = A(124)
      A(53) = A(125)
      A(54) = A(126)
      A(64) = A(127)
      A(65) = A(128)
      A(66) = A(129)
      A(67) = A(133)
      A(68) = A(134)
      A(69) = A(135)
      A(70) = A(139)
      A(71) = A(140)
      A(72) = A(141)
      A(73) = A(130)
      A(74) = A(131)
      A(75) = A(132)
      A(76) = A(136)
      A(77) = A(137)
      A(78) = A(138)
      A(79) = A(142)
      A(80) = A(143)
      A(81) = A(144)
      RETURN
      END
