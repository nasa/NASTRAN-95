      SUBROUTINE TVOR (SL1,CL1,TL1,SL2,CL2,TL2,SGS,CGS,SGR,CGR,X01,X02,
     1                 Y0,Z0,E,BETA,CBAR,FMACH,KR,BRE,BIM)
C
C     NORMALWASH AT A POINT (X,Y,Z) - OF A SURFACE DIHEDRAL -
C     DUE TO A TRAPEZOIDAL UNSTEADY VORTEX RING OF UNIT STRENGTH.
C
C     THIS SUBROUTINE CALLS - SNPDF, IDF1, IDF2, FLLD
C
C     SL1, CL1, TL1  SIN(LAMBDA-1), COS(LAMBDA-1), TAN(LAMBDA-1)
C     SL2, CL2, TL2  SIN(LAMBDA-2), .....
C     SGS, CGS       SIN(GAMMA-S),  ....
C     SGR, CGR       SIN(GAMMA-R),  ....
C     X01            X-XI1
C     X02            X-XI2
C     Y0             Y - ETA
C     Z0             Z - ZETA
C     E
C     BETA           SQRT(1-FMACH**2)
C     CV
C     BR
C     FMACH          MACH NO.
C     BRE            REAL PART OF B      (RETURNED)
C     BIM            IMAGINARY PART OF B (RETURNED)
C
      REAL           KR, KD1, KD2
C
C     VARIABLES DIMENSIONED (2), FIRST WORD IS THE REAL PART OF THE
C     VALUE AND THE SECOND IS THE IMAGINARY PART
C
      DIMENSION      DKI(2), DKC(2), DKO(2), KD1(2), KD2(2)
C
      DATA  PI48   / 150.79644720 /
C
C     CALCULATE  BS
C
      L  = 1
      CV = X01 - X02
      SL = SL1
      CL = CL1
      TL = TL1
      X0 = X01
      EE = E**2
      TE = 2.0*E
      ASSIGN 50 TO ISNP
C
C     CALL SNPDF
C
      GO TO 1000
   50 BS = DIJ
      SL = SL2
      CL = CL2
      TL = TL2
      X0 = X02
      ASSIGN 100 TO ISNP
C
C     CALL SNPDF
C
      GO TO 1000
  100 BS = BS - DIJ
C
C     CALCULATE   DELTA-B
C     LIMITS FOR SMALL VALUES OF RADII
C
      EPS = 0.25*EE
      IB  = 0
      FB  = 1.0
      FC  = 4.0
C
C     FIRST CALC.
C     DELTA-KD- 1I, 1C, AND 1O
C
      ETL1 = E*TL1
      ETL2 = E*TL2
      ESGS = E*SGS
      ECGS = E*CGS
C
      DX01 = X01 + ETL1
      DX02 = X02 + ETL2
      DY0  = Y0  + ECGS
      DZ0  = Z0  + ESGS
      ASSIGN 200 TO IFLLD
C
C     CALCULATE  R-I  SQUARED AND CALL FLLD IF LARGE ENOUGH
C
      R2  = DY0**2 + DZ0**2
      IF (R2 .GE. EPS) GO TO 2000
      IB  = 1
      FC  = 6.0
      FB  = 0.0
      GO TO 230
  200 DKI(1) = KD1(1)/R2 + KD2(1)/R4
      DKI(2) = KD1(2)/R2 + KD2(2)/R4
C
C     KD1C AND KD2C
C
  230 DX01 = X01
      DX02 = X02
      DY0  = Y0
      DZ0  = Z0
      ASSIGN 300 TO IFLLD
C
C     CALCULATE  R-C  SQUARED AND CALL FLLD IF LARGE ENOUGH
C
      R2 = DY0**2 + DZ0**2
      IF (R2 .GE. EPS) GO TO 2000
      FC = 0.0
      FB = 3.0
      GO TO 330
  300 DKC(1) = KD1(1)/R2 + KD2(1)/R4
      DKC(2) = KD1(2)/R2 + KD2(2)/R4
C
C     KD1O AND KD2O
C     SKIP IF  R-I IS TOO SMALL
C
  330 IF (IB .NE. 0) GO TO 430
      DX01 = X01 - ETL1
      DX02 = X02 - ETL2
      DY0  = Y0  - ECGS
      DZ0  = Z0  - ESGS
      ASSIGN 400 TO IFLLD
C
C     CALCULATE  R-O  SQUARED AND CALL FLLD IF LARGE ENOUGH
C
      R2 = DY0**2 + DZ0**2
      IF (R2 .GE. EPS) GO TO 2000
      FB = 0.0
      FC = 6.0
      IB = 1
      GO TO 430
  400 DKO(1) = KD1(1)/R2 + KD2(1)/R4
      DKO(2) = KD1(2)/R2 + KD2(2)/R4
C
  430 COEF = 1.0/PI48
      BRE  = BS/(TE*CV) - COEF*(FB*(DKI(1) + DKO(1)) + FC*DKC(1))
      BIM  =            - COEF*(FB*(DKI(2) + DKO(2)) + FC*DKC(2))
      RETURN
C
 1000 CALL SNPDF (SL,CL,TL,SGS,CGS,SGR,CGR,X0,Y0,Z0,E,DIJ,BETA,CV)
      GO TO ISNP, (50,100)
C
 2000 CALL FLLD (DX01,DX02,DY0,DZ0,SGR,CGR,SGS,CGS,KR,CBAR,FMACH,E,L,
     1           KD1(1),KD1(2),KD2(1),KD2(2))
      R4 = R2*R2
      GO TO IFLLD, (200,300,400)
C
      END
