      SUBROUTINE PSTRI1
C  THIS ROUTINE CALCULATES GP,SET-S UP THE ECPT AND UPDATES THE ECPT
C  FOR THE TRIA1 ELEMENTS
C
C     ECPT FOR TRIA1
C     EL.ID                                       ECPT( 1)
C     GRID A                                      ECPT( 2)
C     GRID B                                      ECPT( 3)
C     GRID C                                      ECPT( 4)
C     THETA                                       ECPT( 5)
C     MATID1                                      ECPT( 6)
C     T1                                          ECPT( 7)
C     MATID2                                      ECPT( 8)
C     I                                           ECPT( 9)
C     MATID3                                      ECPT(10)
C     T2                                          ECPT(11)
C     NS MASS                                     ECPT(12)
C     Z1                                          ECPT(13)
C     Z2                                          ECPT(14)
C     CSID 1                                      ECPT(15)
C     X1                                          ECPT(16)
C     Y1                                          ECPT(17)
C     Z1                                          ECPT(18)
C     CSID 2                                      ECPT(19)
C     X2                                          ECPT(20)
C     Y2                                          ECPT(21)
C     Z2                                          ECPT(22)
C     CSID3                                       ECPT(23)
C     X3                                          ECPT(24)
C     Y3                                          ECPT(25)
C     Z3                                          ECPT(26)
C     TEMP                                        ECPT(27)
C     EPS SUB 0      (PREVIOUS  STRAIN)           ECPT(28)
C     EPS SUB STAR   (LAST STRAIN)                ECPT(29)
C     MODULUS OF ELASTICITY                       ECPT(30)
C     SIGMA X        STRESS                       ECPT(31)
C     SIGMA Y        STRESS                       ECPT(32)
C     SIGMA XY       STRESS                       ECPT(33)
C     M X STAR       FORCE                        ECPT(34)
C     M Y STAR       FORCE                        ECPT(35)
C     M XX STAR      FORCE                        ECPT(36)
C     V X  STAR      FORCE                        ECPT(37)
C     V Y  STAR      FORCE                        ECPT(38)
C     U A       (6X1 DISPLACEMENT VECTOR)         ECPT(39)
C     U B       (6X1 DISPLACEMENT VECTOR)         ECPT(45)
C     U C       (6X1 DISPLACEMENT VECTOR)         ECPT(51)
C
C     ******************************************************************
C
      REAL NU
C
      DIMENSION NECPT(27),NECPTS(27)
      COMMON /PLA32E/ ECPT(27), EPS0,EPSS,ESTAR,SIGXS,SIGYS,SIGXYS,
     1                FORVEC(5),UI(18),DUMMY(44)
      COMMON /PLA3ES/ ECPTSA(100), PH1OUT(200)
      COMMON /PLA3UV/ IVEC, Z(24)
C
C SCRATCH BLOCK  325 CELLS
C
      COMMON /PLA32S/S(3),DUM(297),TAU0 ,TAU1 ,TAU2 ,F,SX,SY,DEPS,DEPSS,
     1                EPS1,EPS2, DUM1,IDUM2,IDUM3(3,3)
     2,              EXTRA(4)
      COMMON /MATIN/ MATID,INFLAG,ELTEMP,PLAARG,SINTH,COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33
      COMMON /PLA32C/ GAMMA, GAMMAS, IPASS
      COMMON /PLAGP/  GP(9),  MIDGP  , ELID
C
      EQUIVALENCE (NECPT(6),MATID1) , (ECPT(1),NECPT(1)) , (G11,PLAANS)
     1,           (G13,NU)   , (G11,ESUB0)
     2,  (NECPTS(1),ECPTSA(1))
     3,   (G12,NIROF)
C
C SETUP GP MATRIX FOR PLAMAT
C
      ELID = ECPT(1)
      MIDGP = MATID1
      DO 10 I=1,9
   10 GP(I)=0.0
      TAU0  = SQRT(SIGXS**2 - SIGXS*SIGYS + SIGYS**2 + 3.0*SIGXYS**2)
      IF(ESTAR .EQ. 0.0) GO TO 50
      IF(IPASS .NE. 1  ) GO TO 20
      MATID = MATID1
      COSTH = 1.0
      SINTH = 0.0E0
      INFLAG= 2
C
      CALL MAT(ECPT(1))
C
      GP(1) = G11
      GP(2) = G12
      GP(3) = G13
      GP(4) = G12
      GP(5) = G22
      GP(6) = G23
      GP(7) = G13
      GP(8) = G23
      GP(9) = G33
      GO TO 50
   20 IF(TAU0  .EQ. 0.0) GO TO 50
      MATID = MATID1
      INFLAG = 1
C
      CALL MAT(ECPT(1))
C
      F =   9.0*(ESUB0 - ESTAR) / (4.0 * TAU0**2 * ESTAR)
      SX = (2.0*SIGXS - SIGYS)/ 3.0
      SY = (2.0*SIGYS - SIGXS)/ 3.0
      GP(1) = (1.0+SX**2*F) / ESUB0
      GP(2) = (-NU+SX*SY*F) / ESUB0
      GP(3) = (2.0*SIGXYS*SX*F) / ESUB0
      GP(4) = GP(2)
      GP(5) = (1.0+SY**2*F) / ESUB0
      GP(6) = (2.0*SIGXYS*SY*F) / ESUB0
      GP(7) = GP(3)
      GP(8) = GP(6)
      GP(9) = (2.0*(1.0+NU) + 4.0*F*SIGXYS**2) / ESUB0
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      IDUM2 = -1
      CALL INVERS(3,GP,3,0,0,DUM1,IDUM2,IDUM3)
C
C CHECK SINGULARITY
C
      IF( IDUM2 .EQ. 2) CALL MESAGE(-30,38,ECPT(1))
C
C CALCULATE PHASE I STRESSES
C
   50 DO 30 I = 1,32
   30 ECPTSA(I) = ECPT(I)
      NECPTS(2) = 1
      NECPTS(3) = 7
      NECPTS(4) = 13
C
      CALL PSTQ1 (1)
C
C
C CALCULATE PHASE II STRESSES
C
      IVEC = 1
      DO 60 I = 1,24
   60 Z(I) = UI(I)
      DO 70 I = 1,200
   70 ECPTSA(I) = PH1OUT(I)
      S(1) = SIGXS
      S(2) = SIGYS
      S(3) = SIGXYS
      I201 = 201
      ECPTSA(I201) = ECPT(1)
      DO 75 I=1,5
   75 ECPTSA(I+201) = FORVEC(I)
C
      CALL PSTQ2 (3)
C
C
C  UPDATE ECPT FOR STRESSES
C
      SIGXS = S(1)
      SIGYS = S(2)
      SIGXYS = S(3)
C
C     NEW FORCES ARE IN  /PLA3ES/ AT LOCATIONS 202-206
C
      DO 76 I=1,5
   76 FORVEC(I)   = ECPTSA(I+201)
      TAU1  = SQRT(SIGXS**2 - SIGXS*SIGYS + SIGYS**2 + 3.0*SIGXYS**2)
      MATID= MATID1
      INFLAG = 8
      PLAARG = TAU1
C
      CALL MAT(ECPT(1))
C
C TEST FOR TAU 1 OUTSIDE THE RANGE OF FUNCTION
C
      IF ( NIROF . EQ. 1 ) GO TO 80
C
C RETURNS EPS SUB 1 GIVEN TAU1
C
      EPS1 = PLAANS
      DEPS = EPS1 - EPSS
      DEPSS= EPSS - EPS0
      EPS2 = EPS1 + GAMMA * DEPS
      INFLAG=6
      PLAARG = EPS2
C
      CALL MAT(ECPT(1))
C
C RETURNS  TAU2 GIVEN EPS2
C
      TAU2  = PLAANS
      ESTAR = 0.0
      IF( (EPS2 - EPS1) .NE. 0.0) ESTAR = (TAU2 - TAU1) / (EPS2-EPS1)
      EPS0  = EPSS
      EPSS  = EPS1
      RETURN
C
   80 ESTAR = 0.0
      RETURN
      END
