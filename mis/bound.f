      SUBROUTINE BOUND (FBREC,AFE,NAFE,KGE,NKGE)
C
C     COMPUTES AREA FACTOR AND GRAVITIONAL STIFFNESS MATRICES FOR A FACE
C     OF A INDIVIDUAL FLUID ELEMENT
C
      LOGICAL         ERROR    ,GRAV
      INTEGER         GF1      ,GF2      ,GF3      ,GF4      ,FBREC(12),
     1                GS1      ,GS2      ,GS3      ,GS4      ,GRID(3,4),
     2                GSI      ,GSJ      ,IZ(1)    ,LOCSOF(4),LOCTOF(3),
     3                LOCFOS(4),FLEDGE(2,4)        ,FEDGE(2,4)         ,
     4                STEDGE(2,3)
      REAL            Z
      DOUBLE PRECISION          AFE(48)  ,KGE(144) ,IN(3)    ,JN(3)    ,
     1                KN(3)    ,R12(3)   ,R13(3)   ,R14(3)   ,R24(3)   ,
     2                H        ,NN       ,KS(3)    ,MAG      ,X3       ,
     3                Y3       ,Y4       ,S(48)    ,X4       ,AKJ(3,4) ,
     4                AA       ,BB       ,CC       ,A        ,ZZ       ,
     5                DVMAG    ,RHOXG    ,Y(3)     ,E(3,2)   ,KII(144) ,
     6                KTWO(2,2),KIK(9)   ,T(3,3)   ,KTEMP(2,3)         ,
     7                TFST(3,3),Z1       ,X1       ,DHALF    ,C1       ,
     8                ST(3,4)  ,Z2       ,Y1       ,EPS(2)   ,C2       ,
     9                FL(3,4)  ,Z3       ,X2       ,DLB      ,C3       ,
     O                TR(3,3)  ,Z4       ,Y2       ,DUB      ,D1       ,
     1                P(2,7)   ,SS(9)    ,AA2      ,NN1      ,D2       ,
     2                C(4,7)   ,EPSLON   ,FDET     ,ZZ1      ,DZ       ,
     3                F(3,7)   ,NX       ,AKJCON   ,DD       ,AEPS     ,
     4                PT(3,4)  ,TRIA     ,EPSO10   ,AFLEL    ,LEPS     ,
     5                VTEMP(3) ,KSB(3)   ,NZ       ,KIDENT(3),FACTII   ,
     6                CONII    ,FII      ,DPOLY    ,ASTRIA   ,ASTREL   ,
     7                AFLSTR   ,DADOTB   ,DAPOLY
      CHARACTER       UFM*23   ,UWM*25
      COMMON /XMSSG / UFM      ,UWM
C
C     OPEN CORE
C
      COMMON /ZZZZZZ/ Z(1)
C
C     CORE POINTERS
C
      COMMON /FLBPTR/ ERROR    ,ICORE    ,LCORE    ,IBGPDT   ,NBGPDT   ,
     1                ISIL     ,NSIL     ,IGRAV    ,NGRAV
C
C     MATERIAL PROPERTIES
C
      COMMON /MATIN / MATID    ,INFLAG
      COMMON /MATOUT/ DUM(3)   ,RHO
C
C     MODULE PARAMETERS
C
      COMMON /BLANK / NOGRAV
C
C     NASTRAN PARAMETERS
C
      COMMON /SYSTEM/ SYSBUF   ,NOUT
      EQUIVALENCE     (TFST(1,1),IN(1))  ,(X1,FL(1,1))  ,(X2,FL(1,2))  ,
     1                (TFST(1,2),JN(1))  ,(Y1,FL(2,1))  ,(Y2,FL(2,2))  ,
     2                (TFST(1,3),KN(1))  ,(Z1,FL(3,1))  ,(Z2,FL(3,2))  ,
     3                (SS(1),BB)         ,(X3,FL(1,3))  ,(X4,FL(1,4))  ,
     4                (SS(2),CC)         ,(Y3,FL(2,3))  ,(Y4,FL(2,4))  ,
     5                (SS(3),ZZ)         ,(Z3,FL(3,3))  ,(Z4,FL(3,4))  ,
     6                (SS(4),NN)         ,(SS(5),NN1)   ,(SS(6),ZZ1 )  ,
     7                (EPS(1),AEPS)      ,(EPS(2),LEPS) ,(FII,BB    )  ,
     8                (FACTII,CC)        ,(CONII,AKJCON),(Z(1),IZ(1))
C
C     GRID POINTS TO BE USED IN SUBDIVIDING QUADS INTO TRIANGLES
C
      DATA    GRID  /  1     ,2     ,3   ,
     1                 2     ,3     ,4   ,
     2                 3     ,4     ,1   ,
     3                 4     ,1     ,2   /
C
      DATA    DZ, D1,  D2, DHALF / 0.D0,  1.D0, 2.D0, .5D0 /
      DATA    EPSLON,  EPSO10    / 1.D-3, 1.D-4            /
      DATA    DLB   ,  DUB       /-1.D-3, 1.001D0          /
      DATA    X1, X2,  Y1, Y2, Z1, Z2, Z3, Z4 / 8*0.D0     /
C
      DATA    FEDGE /  1,2, 2,3, 3,4, 4,1 /
      DATA    STEDGE/  1,2, 2,3, 3,1      /
      DATA    KIDENT/  0.D0, 0.D0, 1.D0   /
C
C
C     DETERMINE SIZES OF MATRIX PARTITIONS
C
      NGRIDS = 4
      IF (FBREC( 6) .LT. 0) NGRIDS = 3
      NGRIDF = 4
      IF (FBREC(12) .LT. 0) NGRIDF = 3
C
      NROW = 3*NGRIDS
      NAFE = NROW*NGRIDF*2
      NKGE = 0
C
C     OBTAIN MATERIAL PROPERTY AND GRAVITY DATA IF GRAV ID IS
C     PRESENT
C
      GRAV   = .FALSE.
      IF (FBREC(7) .EQ. 0) GO TO 600
      INFLAG = 11
      MATID  = FBREC(8)
      CALL MAT (FBREC(1))
C
      IF (NGRAV .EQ. 0) GO TO 8013
      LGRAV = IGRAV + NGRAV - 1
      DO 200 I = IGRAV,LGRAV,6
      IF (IZ(I) .EQ. FBREC(7)) GO TO 400
  200 CONTINUE
C
      GO TO 8013
C
  400 G     = SQRT(Z(I+3)**2 + Z(I+4)**2 + Z(I+5)**2)
      G     = G*Z(I+2)
      RHOXG = DBLE(RHO)*DBLE(G)
      NKGE  = NROW*NROW*2
      NOGRAV= 1
      GRAV  = .TRUE.
C
C     NORMILIZE THE GRAVITY VECTOR
C
      E(1,2) = DBLE(Z(I+3))
      E(2,2) = DBLE(Z(I+4))
      E(3,2) = DBLE(Z(I+5))
      CALL DNORM (E(1,2),MAG)
      IF (IZ(I+1) .EQ. 0) GO TO 600
C
C     TRANSFORM GRAVITY VECTOR TO BASIC
C
      J = IZ(IBGPDT)
      IZ(IBGPDT) = IZ(I+1)
      CALL TRANSD (IZ(IBGPDT),TR)
      IZ(IBGPDT) = J
      CALL GMMATD (TR,3,3,0,E(1,2),3,1,0,VTEMP)
      DO 500 J = 1,3
  500 E(J,2) = VTEMP(J)
C
C
C     COMPUTE NEW COORDINATES FOR FLUID FACE BASED ON FLUID COORDINATE
C     SYSTEM - PERFORM THIS ONLY IF THE FLUID FACE HAS CHANGED
C     THESE COMPUTATIONS INCLUDE --
C
C        IN,JN,KN  - NORMAL VECTORS TO DEFINE FLUID COORDINATE SYSTEM
C        X2,X3,X4  - X COORDINATES OF GRID POINTS IN NEW SYSTEM
C                    ( X1 = 0 )
C        Y3,Y4       Y COORDINATES OF GRID POINTS IN NEW SYSTEM
C                    ( Y1,Y2 = 0 )
C
C     NORMAL (UNIT) VECTORS STORED *COLUMN-WISE* IN U --
C           I IN U(L,1), J IN U(L,2), K IN U(L,3), L= 1,3
C        TRANSFORMED FLUID COORDINATES STORED IN FL
C
C
C     LOCATE GRID POINTS COORDINATES FOR THE FLUID GRID POINTS IN THE
C     BGPDT TABLE
C
  600 GF1 = IBGPDT + (FBREC( 9)-1)*4
      GF2 = IBGPDT + (FBREC(10)-1)*4
      GF3 = IBGPDT + (FBREC(11)-1)*4
      GF4 = -1
      IF (NGRIDF .EQ. 4) GF4 = IBGPDT + (FBREC(12)-1)*4
C
      IF (NGRIDF .EQ. 4) GO TO 700
C
C     TRIANGULAR FLUID FACE
C
      DO 660 I = 1,3
      R12(I) = Z(GF2+I) - Z(GF1+I)
      IN(I)  = R12(I)
  660 R13(I) = Z(GF3+I) - Z(GF1+I)
C
      CALL DNORM (IN,MAG)
      X2 = MAG
C
      CALL DAXB  (R12,R13,KN)
      CALL DNORM (KN,MAG)
C
      CALL DAXB (KN,IN,JN)
C
      X3 = R13(1)*IN(1) + R13(2)*IN(2) + R13(3)*IN(3)
      Y3 = R13(1)*JN(1) + R13(2)*JN(2) + R13(3)*JN(3)
      GO TO 1000
C
C     QUADRATIC FLUID FACE
C
  700 DO 800 I = 1,3
      R12(I) = Z(GF2+I) - Z(GF1+I)
      R13(I) = Z(GF3+I) - Z(GF1+I)
      R14(I) = Z(GF4+I) - Z(GF1+I)
  800 R24(I) = Z(GF4+I) - Z(GF2+I)
C
      CALL DAXB  (R13,R24,KN)
      CALL DNORM (KN,MAG)
C
      H = R12(1)*KN(1) + R12(2)*KN(2) + R12(3)*KN(3)
C
      DO 900 I = 1,3
  900 IN(I) = R12(I) - H*KN(I)
      CALL DNORM (IN,MAG)
C
      X2 = MAG
C
      CALL DAXB (KN,IN,JN)
C
      X3 = R13(1)*IN(1) + R13(2)*IN(2) + R13(3)*IN(3)
      X4 = R14(1)*IN(1) + R14(2)*IN(2) + R14(3)*IN(3)
      Y3 = R13(1)*JN(1) + R13(2)*JN(2) + R13(3)*JN(3)
      Y4 = R14(1)*JN(1) + R14(2)*JN(2) + R14(3)*JN(3)
C
C     VARIOUS CALCULATIONS DEPENDENT ON FLUID FACE
C
C     INDICES FOR CORNERS OF FLUID ELEMENT
C
 1000 DO 1010 N = 1,2
      DO 1010 J = 1,NGRIDF
 1010 FLEDGE(N,J) = FEDGE(N,J)
      FLEDGE(2,NGRIDF) = 1
C
C     SET UP FOR FLUID TRIANGLE
C
      C1 = (D1 - FL(1,3)/FL(1,2))/FL(2,3)
      C2 = FL(1,3)/(FL(1,2)*FL(2,3))
      DO 1020 N = 1,3
      R12(N) = FL(N,2) - FL(N,1)
 1020 R13(N) = FL(N,3) - FL(N,1)
      CALL DAXB (R12,R13,VTEMP)
C
      IF (NGRIDF .EQ. 3) GO TO 1040
C
C     SET UP FOR FLUID QUADRANGLE
C
      C1  = FL(2,3) - FL(2,4)
      C2  = FL(1,2)*FL(2,4)
      C3  = FL(1,2) - FL(1,3) + FL(1,4)
      AA  =-FL(1,2)*C1
      AA2 = D2*AA
C
      DO 1030 N = 1,3
      R13(N) = FL(N,3) - FL(N,1)
 1030 R24(N) = FL(N,4) - FL(N,2)
      CALL DAXB (R13, R24,VTEMP)
 1040 AFLEL = DVMAG(VTEMP,DZ)
C
C     ZERO OUT AREA FACTOR MATRIX
C     AND AREA COMMON TO FLUID AND STRUCTURE ELEMENTS (AFLSTR)
C
      DO 1042 I = 1,48
      AFE(I) = DZ
 1042 S(I) = 0.0D0
      DO 1044 I = 1,144
 1044 KGE(I) = 0.0D0
      AFLSTR = 0.0
C
C     DETERMINE NUMBER OF STRUCTURAL TRIANGLES TO BE USED, ITRIA
C     AND CUMULATIVE AREA CONSTANT, TRIA
C        ITRIA= 4, TRIA= .5 WHEN STRUCTURE ELEMENT IS QUADRANGLE
C        ITRIA= 1, TRIA= 1. WHEN STRUCTURE ELEMENT IS TRIANGLE
C
      ITRIA = 1
      TRIA  = D1
      IF (NGRIDS .EQ. 3) GO TO 1050
      ITRIA = 4
      TRIA  = DHALF
C
C     TRANSFORM STRUCTURE COORDINATES TO FLUID COORDINATE SYSTEM
C
 1050 GS1 = IBGPDT + (FBREC(3)-1)*4
      GS2 = IBGPDT + (FBREC(4)-1)*4
      GS3 = IBGPDT + (FBREC(5)-1)*4
      GS4 = -1
      IF (NGRIDS .EQ. 4) GS4 = IBGPDT + (FBREC(6)-1)*4
C
      DO 1060 N = 1,3
      PT(N,1) = Z(GS1+N) - Z(GF1+N)
      PT(N,2) = Z(GS2+N) - Z(GF1+N)
      PT(N,3) = Z(GS3+N) - Z(GF1+N)
      PT(N,4) = DZ
      IF (NGRIDS .EQ. 4) PT(N,4) = Z(GS4+N) - Z(GF1+N)
      DO 1060 K = 1,4
      ST(N,K) = DZ
 1060 CONTINUE
C
      DO 1070 K = 1,NGRIDS
      DO 1070 N = 1,3
      DO 1070 M = 1,3
 1070 ST(N,K) =  ST(N,K) + PT(M,K)*TFST(M,N)
      DO 1075 N = 1,2
      R12(N) = ST(N,2) - ST(N,1)
      R13(N) = ST(N,3) - ST(N,1)
      IF (NGRIDS .EQ. 4) R24(N) = ST(N,4) - ST(N,2)
 1075 CONTINUE
      CALL DAXB (R12,R13,VTEMP)
      IF (NGRIDS .EQ. 4) CALL DAXB (R12,R24,VTEMP)
      ASTREL = DVMAG(VTEMP,DZ)
      AEPS   = DHALF*DMIN1(AFLEL,ASTREL)
      LEPS   = DZ
      IF (AEPS .GT. DZ) LEPS = EPSLON*DSQRT(AEPS)
      AEPS   = EPSLON*AEPS
C
C     LOCATE STRUCTURE ELEMENT GRIDS RELATIVE TO FLUID SURFACE
C     LOCSOF FLAGS STRUCTURE ON FLUID:
C            1= INSIDE, -1= OUTSIDE, 0= ON FLUID EDGE
C
      CALL LOCPT (NGRIDS,ST,NGRIDF,FL,FLEDGE,KIDENT,EPS,LOCSOF)
C
C
C     LOOP THRU (INCREMENTAL) STRUCTURAL TRIANGLES (ITRIA IS 1 OR 4)
C
      DO 2500 IT = 1,ITRIA
C
C     LOCATE COORDINATES OF CURRENT TRIANGLE
C
      GS1 = GRID(1,IT)
      GS2 = GRID(2,IT)
      GS3 = GRID(3,IT)
C
      LOCTOF(1) = LOCSOF(GS1)
      LOCTOF(2) = LOCSOF(GS2)
      LOCTOF(3) = LOCSOF(GS3)
C
C     TRANSFER COORDINATES OF CURRENT STRUCTURE TRIANGLE TO CONTIGUOUS
C     ARRAY, AND DO VARIOUS CALCULATIONS DEPENDENT ON THEM
C
      DO 1100 N = 1,3
      TR(N,1) = ST(N,GS1)
      TR(N,2) = ST(N,GS2)
      TR(N,3) = ST(N,GS3)
      R12(N)  = TR(N,2) - TR(N,1)
 1100 R13(N)  = TR(N,3) - TR(N,1)
C
C     OBTAIN KS, UNIT VECTOR NORMAL TO (XY) PLANE OF CURRENT STRUCTURAL
C     TRIANGLE (IN SYSTEM LOCAL TO FLUID ELEMENT)
C
      CALL DAXB (R12,R13,KS)
      ASTRIA = DVMAG(KS,DZ)
      CALL DNORM (KS,MAG)
C
C     OBTAIN KSB, UNIT VECTOR NORMAL TO (XY) PLANE OF CURRENT STRUCTURE
C     TRIANGLE (IN NASTRAN BASIC COORD SYSTEM)
C
      DO 1150 N = 1,3
      R12(N) = PT(N,GS2) - PT(N,GS1)
      R13(N) = PT(N,GS3) - PT(N,GS1)
 1150 CONTINUE
C
      CALL DAXB  (R12,R13,KSB)
      CALL DNORM (KSB,MAG)
C
C     CALCULATE EPSLON FUNCTIONS FOR SIGNIFICANCE TESTING
C
      LEPS = DZ
      AEPS = DHALF*DMIN1(AFLEL,ASTRIA)*EPSLON
      IF (AEPS .GT. DZ)  LEPS = DSQRT(AEPS)
C
C     DETERMINE POINTS DESCRIBING AREA POLYGON COMMON TO BOTH FLUID
C     ELEMENT AND (INCREMENTAL) STRUCTURAL TRIANGLE
C
C        POLYGON POINTS IN   P(2,I)    I .LE. 7
C        FLUID POINTS IN     FL(3,J)   J .LE. 4
C        TRIANGLE POINTS IN  TR(3,K)   K=1,3
C
C     DETERMINE POINTS DESCRIBING POLYGON OF COMMON AREA
C
C
C     LOCATE FLUID ELEMENT POINTS RELATIVE TO BOUNDRY OF THIS STRUCTURAL
C     TRIANGLE
C
      CALL LOCPT (NGRIDF,FL,3,TR,STEDGE,KS,EPS,LOCFOS)
      DO 1240 J = 1,NGRIDF
      IF (LOCFOS(J) .LT. 0) GO TO 1300
 1240 CONTINUE
C
C     FLUID ELEMENT IS COMMON AREA POLYGON WHEN NO FLUID POINTS ARE
C     OUTSIDE BOUNDRY OF THIS STRUCTURAL TRIANGLE
C
      NPOLY = NGRIDF
      DO 1250 N = 1,2
      DO 1250 J = 1,NGRIDF
 1250 P(N,J) = FL(N,J)
      GO TO 2000
C
C     CALL POLYPT TO DETERMINE POINTS DESCRIBING THE COMMON AREA POLYGON
C
 1300 CALL POLYPT (LOCTOF,STEDGE,TR,NGRIDF,FLEDGE,FL,LOCFOS,EPS,NPOLY,P)
C
C     SKIP TO NEXT (INCREMENTAL) STRUCTURAL TRIANGLE WHEN THIS TRIANGLE
C     IS DISJOINT FROM FLUID ELEMENT
C
      IF (NPOLY .LT. 3) GO TO 2500
C
C     AREA OF COMMON POLYGON AND HALVED WHEN OVERLAPPING (INCREMENTAL)
C     STRUCTURE TRIANGLES USED CUMULATIVE AREA OF FLUID/STRUCTURAL
C     ELEMENT OVERLAP
C
 2000 A = TRIA*DAPOLY(NPOLY,P)
      AFLSTR = AFLSTR + A
C
C     TERMS FOR LOAD FACTORS
C
      SS(1) =  TR(1,1)*TR(2,2)
      SS(2) = -TR(1,1)*TR(2,3)
      SS(3) =  TR(1,2)*TR(2,3)
      SS(4) = -TR(1,2)*TR(2,1)
      SS(5) =  TR(1,3)*TR(2,1)
      SS(6) = -TR(1,3)*TR(2,2)
      FDET  =  DZ
      DO 2005 M = 1,6
 2005 FDET  =  FDET  + SS(M)
      SS(1) =  SS(1) + SS(4)
      SS(2) =  SS(2) + SS(5)
      SS(3) =  SS(3) + SS(6)
      SS(4) =  TR(2,2) - TR(2,3)
      SS(5) =  TR(2,3) - TR(2,1)
      SS(6) =  TR(2,1) - TR(2,2)
      SS(7) =  TR(1,3) - TR(1,2)
      SS(8) =  TR(1,1) - TR(1,3)
      SS(9) =  TR(1,2) - TR(1,1)
C
C     GET LOAD DISTRIBUTION FACTORS, F(K,I)
C     - FROM -
C         I -- AREA POLYGON POINT -- P(N,I)
C         K -- STRUCTURE TRIANGLE POINT -- TR(N,K)
C
      DO 2010 I = 1,NPOLY
      F(1,I) = P(1,I)*SS(4) + P(2,I)*SS(7) + SS(3)
      F(2,I) = P(1,I)*SS(5) + P(2,I)*SS(8) + SS(2)
 2010 F(3,I) = P(1,I)*SS(6) + P(2,I)*SS(9) + SS(1)
C
C     GET PRESSURE DISTRIBUTION FACTORS, C(J,I)
C     - FROM -
C         I -- AREA POLYGON POINT  -- P(N,I)
C         J -- FLUID ELEMENT POINT -- FL(N,J)
C
      IF (NGRIDF .EQ. 4) GO TO 2030
C
C     FLUID ELEMENT IS TRIANGLE
C
      DO 2020 I = 1,NPOLY
      BB     = P(1,I)/FL(1,2)
      C(1,I) = D1 - BB - P(2,I)*C1
      C(2,I) = BB - P(2,I)*C2
 2020 C(3,I) = P(2,I)/FL(2,3)
      GO TO 2100
C
C     FLUID ELEMENT IS QUADRANGLE
C
 2030 DO 2050 I = 1,NPOLY
      BB = P(1,I)*C1 - C2 + P(2,I)*C3
      CC = P(1,I)*FL(2,4) - P(2,I)*FL(1,4)
      IF (BB.EQ.DZ .OR. DABS(AA).GT.DABS(BB*EPSLON)) GO TO 2040
      ZZ = -CC/BB
      GO TO 2045
C
 2040 DD = DSQRT(BB*BB - D2*AA2*CC)
      ZZ = (DD-BB)/AA2
      IF (ZZ.GT.DLB .AND. ZZ.LT.DUB) GO TO 2045
      ZZ = (-DD-BB)/AA2
C
 2045 NN = P(2,I)/(FL(2,4) + ZZ*C1)
      IF (NN.LE.DLB .OR. NN.GE.DUB) GO TO 8005
C
      ZZ1 = D1 - ZZ
      NN1 = D1 - NN
      C(1,I) = ZZ1*NN1
      C(2,I) = ZZ *NN1
      C(3,I) = ZZ *NN
 2050 C(4,I) = ZZ1*NN
C
C     CALCULATE AREA TERMS FOR THIS STRUCTURAL TRIANGLE AND INSERT IN
C     MATRIX
C
 2100 DPOLY  = NPOLY
      AKJCON = A/(FDET*DPOLY)
      DPOLY  = NPOLY - 1
      FACTII = D1/DPOLY
C
      DO 2120 J = 1,NGRIDF
      JLOC = 3*NGRIDS*(J-1)
C
      DO 2120 K = 1,3
      LOC = JLOC + 3*(GRID(K,IT)-1)
C
      AKJ(K,J) = DZ
      DO 2110 I = 1,NPOLY
 2110 AKJ(K,J) = AKJ(K,J) + F(K,I)*C(J,I)
      AKJ(K,J) = AKJCON*AKJ(K,J)
C
      DO 2119 N = 1,3
 2119 S(LOC+N) = S(LOC+N) + AKJ(K,J)*KSB(N)
 2120 CONTINUE
C
      IF (.NOT. GRAV) GO TO 2500
C
C     CALCULATE GRAVITATIONAL STIFFNESS TERMS FOR THIS TRIANGLE
C     AND INSERT INTO MATRIX
C
      DO 2210 N = 1,3
 2210 E(N,1) = DZ
      CALL DAXB (E(1,2),KSB,Y)
      MAG = DADOTB(Y,Y)
      IF (MAG .GT. DZ) MAG = DSQRT(MAG)
      IF (MAG .LT. EPSO10) GO TO 2220
C
      CALL DAXB  (E(1,2),Y,E)
      CALL DNORM (E,MAG)
C
 2220 NX = 0.D0
      NZ = 0.D0
      DO 2230 N = 1,3
      NX = NX + E(N,1)*KSB(N)
 2230 NZ = NZ + E(N,2)*KSB(N)
      CONII = RHOXG*AKJCON/(D2*FDET)
      KTWO(1,1) = DZ
C
      KTWO(2,1) = NX
      KTWO(1,2) = KTWO(2,1)
      KTWO(2,2) = NZ
      CALL GMMATD (E,2,3,1, KTWO,2,2,0, KTEMP)
      CALL GMMATD (KTEMP,3,2,0, E,2,3,0, KIK )
C
      DO 2250 KK1 = 1,3
      K1LOC = 9*NGRIDS*(GRID(KK1,IT)-1)
C
      DO 2250 KK2 = 1,3
      LOC = K1LOC + 9*(GRID(KK2,IT)-1)
C
      H = 0.D0
      DO 2240 I1 = 1,NPOLY
      DO 2240 I2 = 1,NPOLY
      FII = F(KK1,I1)*F(KK2,I2)
      IF (I1 .NE. I2) FII= FACTII*FII
 2240 H = H + FII
C
      DO 2249 N = 1,9
      KGE(LOC+N) = KGE(LOC+N) - KIK(N)*H*CONII
 2249 CONTINUE
C
 2250 CONTINUE
C
C     END OF (INCREMENTAL) STRUCTURAL TRIANGLE LOOP
C
 2500 CONTINUE
C
C     WARNING MESSAGE WHEN FLUID AND STRUCTURE ELEMENTS ARE DISJOINT
C
      IF (AFLSTR .LE. DZ) GO TO 8014
C
C     TRANSFORM THE AREA AND STIFFNESS MATRICES TO GLOBAL COORDINATES IF
C     REQUIRED
C
      DO 2610 IROW = 1,NGRIDS
      GSI = IBGPDT + (FBREC(IROW+2)-1)*4
      CALL TRANSD (Z(GSI),T)
C
C     AREA FACTOR MATRIX
C
      JLOC = 3*(IROW-1)
C
      DO 2530 ICOL = 1,NGRIDF
      ILOC = 3*NGRIDS*(ICOL-1) + JLOC
C
      IF (IZ(GSI) .EQ. 0) GO TO 2510
      CALL GMMATD (T,3,3,1,S(ILOC+1),3,1,0,AFE(ILOC+1))
      GO TO 2530
C
 2510 DO 2520 I = 1,3
 2520 AFE(ILOC+I) = S(ILOC+I)
C
 2530 CONTINUE
      IF (.NOT.GRAV) GO TO 2610
C
C     GRAVITATIONAL STIFFNESS MATRIX
C
      JLOC = 9*(IROW-1)
C
      DO 2600 ICOL = 1,NGRIDS
      ILOC = 9*NGRIDS*(ICOL-1) + JLOC
C
      IF (IZ(GSI) .EQ. 0) GO TO 2540
      CALL GMMATD (T,3,3,1, KGE(ILOC+1),3,3,0, KIK)
      GO TO 2570
C
 2540 KLOC = ILOC
      DO 2550 I = 1,9
 2550 KIK(I) = KGE(KLOC+I)
C
 2570 GSJ = IBGPDT + (FBREC(ICOL+2)-1)*4
      IF (IZ(GSJ) .EQ. 0) GO TO 2580
      CALL TRANSD (Z(GSJ),T)
      CALL GMMATD (KIK,3,3,0, T,3,3,0, KII(ILOC+1))
      GO TO 2600
C
 2580 KLOC = ILOC
      DO 2590 I = 1,9
 2590 KII(KLOC+I) = KIK(I)
 2600 CONTINUE
 2610 CONTINUE
C
C     REARANGE THE STORAGE OF THE GRAVITATIONAL STIFFNESS MATRIX
C     TO COLUMNWISE FOR THE USE WITH THE ASSEMBLER
C
      IF (.NOT.GRAV) RETURN
C
      DO 2630 ICOL = 1,NGRIDS
      JLOC = 9*NGRIDS*(ICOL-1)
C
      DO 2630 IROW = 1,NGRIDS
      ILOC = JLOC + 9*(IROW-1)
      KLOC = JLOC + 3*(IROW-1)
C
      DO 2620 I = 1,3
      KGE(KLOC+1) = KII(ILOC+1)
      KGE(KLOC+2) = KII(ILOC+4)
      KGE(KLOC+3) = KII(ILOC+7)
      KLOC = KLOC + 3*NGRIDS
 2620 ILOC = ILOC + 1
 2630 CONTINUE
      RETURN
C
C     ERROR CONDITIONS
C
 8005 WRITE (NOUT,9005) UFM,FBREC(2)
      ERROR = .TRUE.
      GO TO 9000
 8013 WRITE (NOUT,9013) UFM,FBREC(1),FBREC(7)
      ERROR = .TRUE.
      GO TO 9000
 8014 WRITE (NOUT,9014) UWM,FBREC(1),FBREC(2)
 9000 RETURN
C
 9005 FORMAT (A23,' 8005. BAD GEOMETRY DEFINED FOR STRUCTURAL ELEMENT ',
     1       I8)
C
 9013 FORMAT (A23,' 8013, FLUID ELEMENT',I9,' ON A CFLSTR CARD ',
     1       'REFERENCES UNDEFINED GRAVITY ID',I9)
C
 9014 FORMAT (A25,' 8014, FLUID ELEMENT',I9,' AND STRUCTURE ELEMENT',I9,
     1       ' ARE DISJOINT. CHECK CFLSTR CARDS.')
      END
