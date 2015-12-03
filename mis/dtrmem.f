      SUBROUTINE DTRMEM( IOPT )
C
C     DIFFERENTIAL STIFFNESS CALCULATIONS FOR THE TRIANGULAR MEMBRANE
C     ELEMENT.  THREE 6X6 MATRICES FOR THE PIVOT POINT ARE INSERTED.
C     IF THIS ROUTINE IS CALLED FROM DTRIA OR DQUAD ONLY THE IN PLANE
C     EFFECTS ARE GENERATED AND THE STRESS VALUES ARE RETURNED.
C
C     THE VALUE OF IOPT TELLS US WHICH ROUTINE IS CALLING DTRMEM.
C      THE OPTIONS ARE
C                IOPT        ROUTINE
C               ******       *******
C                 0            DSIA
C                 1            DQDMEM
C                 2            DTRIA
C                 3            DQUAD
C
C
C     THIS ROUTINE COMPUTES AN E-MATRIX UNIQUE TO THIS ROUTINE.
C
C                       IX  IY  IZ
C                  E =  JX  JY  JZ
C                       KX  KY  KZ
C
      DOUBLE PRECISION   E             ,C
     1                  ,KD            ,SIGX
     2                  ,SIGY          ,SIGXY
     3                  ,TEMP1         ,TEMP2
     4                                 ,KIJ
     5                  ,G             ,XSUBB
     6                  ,XSUBC         ,YSUBC
     7                  ,SUM           ,MU
     8                  ,LAMDA         ,DELTA
     9                  ,TEMP          ,GAMMA1
     T                  ,GAMMA2        ,GAMMA3
     1                  ,DISP          ,T
      DOUBLE PRECISION   AREAT         ,DUMDP
C
      DIMENSION          SUM(3)        ,NECPT(6)      ,KIJ(36)
C
C
C     INTERFACE DATA BLOCKS
C
      COMMON /CONDAS/ CONSTS(5)
      COMMON /MATIN / MATID, INFLAG, ELTEMP, STRESS, SINTH, COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALPH12,
     1       TSUB0,GSUBE,SIGTEN,SIGCOM,SIGSHE,G2X211,G2X212,G2X222
      COMMON /DS1AAA/ NPVT, ICSTM, NCSTM
      COMMON /DS1AET/ ECPT(21),ELDEF,LDTEMP,SDISP(9)
      COMMON /DS1ADP/    E(9)          ,C(54)
     1                  ,KD(36)        ,TEMP1(18)
     2                  ,TEMP2(18)
     3                  ,G(9)          ,T(9)
     4                  ,DISP(9)       ,MU
     5                  ,LAMDA         ,DELTA
     6                  ,TEMP          ,GAMMA1
     7                  ,GAMMA2        ,GAMMA3
     8                  ,AREAT         ,XSUBB
     9                  ,XSUBC         ,YSUBC
     T                  ,DUMDP(12)     ,THETA
     1                  ,ICSTM1        ,NPIVOT
     2                  ,IDUM
     3                  ,SIGX          ,SIGY           ,SIGXY
C
      EQUIVALENCE ( CONSTS(4) , DEGRA  )
      EQUIVALENCE (LDTEMP,FTEMP),(NECPT(1),ECPT(1)),(SUM(1),SIGX)
      EQUIVALENCE        (KIJ(1),KD(1))
C
C
C     ******************************************************************
C     ECPT( 1) = ELEMENT ID
C     ECPT( 2) = GRID POINT A OR 1
C     ECPT( 3) = GRID POINT B OR 2
C     ECPT( 4) = GRID POINT C OR 3
C     ECPT( 5) = THETA = ANGLE OF MATERIAL CUT IF ANISOTROPIC
C     ECPT( 6) = MATERIAL ID
C     ECPT( 7) = THICKNESS
C     ECPT( 8) = NON-STRUCTURAL MASS
C     ECPT( 9) = COORD. SYSTEM ID 1
C     ECPT(10) = X1
C     ECPT(11) = Y1
C     ECPT(12) = Z1
C     ECPT(13) = COORD. SYSTEM ID 2
C     ECPT(14) = X2
C     ECPT(15) = Y2
C     ECPT(16) = Z2
C     ECPT(17) = COORD. SYSTEM ID 3
C     ECPT(18) = X3
C     ECPT(19) = Y3
C     ECPT(20) = Z3
C     ECPT(21) = ELEMENT TEMPERATURE
C     ECPT(22) = ELEMENT DEFORMATION DELTA
C     ECPT(23) = AVG. LOADING TEMPERATURE =(-1) IF NO LOADING TEMP.
C     ECPT(24) = X-TRANS POINT 1
C     ECPT(25) = Y-TRANS POINT 1
C     ECPT(26) = Z-TRANS POINT 1
C     ECPT(27) = X-TRANS POINT 2
C     ECPT(28) = Y-TRANS POINT 2
C     ECPT(29) = Z-TRANS POINT 2
C     ECPT(30) = X-TRANS POINT 3
C     ECPT(31) = Y-TRANS POINT 3
C     ECPT(32) = Z-TRANS POINT 3
C     ******************************************************************
C//////
C     CALL BUG(4HTMET,0,ECPT,32)
C//////
C
      SIGX=0.0D0
      SIGY=0.0D0
      SIGXY=0.0D0
      IF(ECPT(7) .EQ. 0.0 .OR. NECPT(6) .EQ. 0 ) RETURN
C     FILL ELEMENT TO GLOBAL E-TRANSFORMATION MATRIX
C
C     IVEC = E(1). . .E(3)
C     JVEC = E(4). . .E(6)
C     KVEC = E(7). . .E(9)
C
      DO 10 I=1,3
   10 E(I) = DBLE( ECPT(I+13) ) - DBLE( ECPT(I+9) )
C
C     LENGTH THEN = XSUBB
C
      XSUBB = DSQRT( E(1)**2 + E(2)**2 + E(3)**2 )
C
C     R  - R   (INTERMEDIATE STEP) AND NOMALIZE IVECTOR = E(1). . .E(3)
C      C    A
C
      DO 20 I=1,3
      E(I+3) = DBLE( ECPT(I+17) ) - DBLE( ECPT(I+9) )
   20 E(I) = E(I) / XSUBB
C
C     XSUBC = I DOT (R  - R )
C                     C    A
C
      XSUBC = E(1) * E(4)  +  E(2) * E(5)  +  E(3) * E(6)
C
C     KVEC = IVEC CROSS (R  - R )
C                         C    A
C
      E(7) = E(2) * E(6)  -  E(3) * E(5)
      E(8) = E(3) * E(4)  -  E(1) * E(6)
      E(9) = E(1) * E(5)  -  E(2) * E(4)
C
C     LENGTH = YSUBC
C
      YSUBC = DSQRT(E(7)**2 + E(8)**2 + E(9)**2 )
C
C     NORMALIZE KVECTOR
      E(7) = E(7) / YSUBC
      E(8) = E(8) / YSUBC
      E(9) = E(9) / YSUBC
C
C     JVECTOR = I CROSS K
C
      E(4) = E(3) * E(8)  -  E(2) * E(9)
      E(5) = E(1) * E(9)  -  E(3) * E(7)
      E(6) = E(2) * E(7)  -  E(1) * E(8)
C
C     NORMALIZE JVECTOR TO MAKE SURE
      TEMP = DSQRT( E(4)**2 + E(5)**2 + E(6)**2 )
      E(4) = E(4) / TEMP
      E(5) = E(5) / TEMP
      E(6) = E(6) / TEMP
C
C     MU, LAMDA, AND DELTA
C
      MU    = 1.0D0 / XSUBB
      LAMDA = 1.0D0 / YSUBC
      DELTA =(XSUBC/XSUBB) - 1.0D0
      AREAT = XSUBB * YSUBC * 0.50D0 * DBLE( ECPT(7) )
C
C     C MATRIX    C  =(3X2) STORED C( 1). . .C( 6)
C                  A
C                 C  =(3X2) STORED C( 7). . .C(12)
C                  B
C                 C  =(3X2) STORED C(13). . .C(18)
C                  C
C
      C( 1) = -MU
      C( 2) =  0.0D0
      C( 3) =  0.0D0
      C( 4) =  LAMDA * DELTA
      C( 5) =  C(4)
      C( 6) = -MU
      C( 7) =  MU
      C( 8) =  0.0D0
      C( 9) =  0.0D0
      C(10) = -LAMDA * MU * XSUBC
      C(11) =  C(10)
      C(12) =  MU
      C(13) =  0.0D0
      C(14) =  0.0D0
      C(15) =  0.0D0
      C(16) =  LAMDA
      C(17) =  LAMDA
      C(18) =  0.0D0
C
      IF( IOPT .GE. 1 ) GO TO 30
C     THE REASON FOR THIS IS THAT IF THE DQDMEM ROUTINE IS CALLING,
C     EACH INDIVIDUAL SUBTRIANGLE WILL ALREADY HAVE A SINTH AND COSTH.
C
      THETA = ECPT(5) * DEGRA
      SINTH = SIN( THETA )
      COSTH = COS( THETA )
   30 IF( ABS(SINTH) .LT. 1.0E-06 ) SINTH = 0.0E0
C
      ELTEMP = ECPT(21)
      MATID = NECPT(6)
      INFLAG = 2
      CALL MAT( ECPT(1) )
C
C     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE.
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
C     G, E, C MATRICES ARE COMPLETE
C
C     FOLLOWING COMPUTES SIG , SIG , SIG      (3X1) VECTOR
C                           X     Y     XY
C
C         I=3
C      = (SUM (G)(C )(E)(T )(DISP )) - (S )(LDTEMP - T )
C         I=1      I      I      I       T            0
C
C        WHERE  S  =(G)(ALPHAS)   (3X1)
C                T
C
      SUM(1) = 0.0E0
      SUM(2) = 0.0E0
      SUM(3) = 0.0E0
C
C     MAKE DISPLACEMENT VECTOR DOUBLE PRECISION
C
      DO 40 I=1,9
   40 DISP(I) = SDISP(I)
C
      DO 90 I=1,3
C     DO WE NEED TRANSFORMATIONS
C
      IF(NECPT(4*I+5)) 50,60,50
   50 CALL TRANSD( NECPT(4*I+5),T(1))
      CALL GMMATD( T(1),3,3,0, DISP(3*I-2),3,1,0, TEMP1(1))
      GO TO 80
C
   60 DO 70 J=1,3
      IDUM=  3*(I-1)+J
   70 TEMP1(J) = DISP(IDUM)
C
   80 CALL GMMATD( E(1),2,3,0,TEMP1(1),3,1,0, TEMP2(1)  )
      CALL GMMATD( C(6*I-5),3,2,0,  TEMP2(1),2,1,0,  TEMP1(1) )
      CALL GMMATD( G(1),3,3,0,    TEMP1(1),3,1,0,    TEMP2(1) )
C
      SUM(1) = SUM(1) + TEMP2(1)
      SUM(2) = SUM(2) + TEMP2(2)
      SUM(3) = SUM(3) + TEMP2(3)
C
   90 CONTINUE
C
      IF( LDTEMP .EQ. (-1) ) GO TO 110
C     COMPUTE S MATRIX
C               T
C
      TEMP2(1) = ALPHA1
      TEMP2(2) = ALPHA2
      TEMP2(3) = ALPH12
C     ABOVE IS FOR SINGLE TO DOUBLE PRECISION.
C
      CALL GMMATD( G(1),3,3,0,  TEMP2(1),3,1,0,  TEMP1(1) )
      TEMP = FTEMP - TSUB0
      DO 100 I=1,3
  100 SUM(I) = SUM(I) - TEMP1(I) * TEMP
C
C//////
C     CALL BUG(4HSUMS,90,SUM,6)
C//////
C  90 AT 90 SIG = SUM(1),  SIG = SUM(2),  SIG   = SUM(3)
C              X              Y              XY
C
C     ABOVE SIMULATES SMA,SDR2-PHASE I+II
C     FROM ABOVE THE E MATRIX (3X3), AND THE SUM (3X1) MATRIX ALONG WITH
C     XSUBB, XSUBC, AND YSUBC ARE NOW USED...
  110 DO 120  I =1,36
  120 KD(I) =0.0D0
C
      IF( IOPT.EQ. 3 ) AREAT=AREAT/2.0D0
C
      MU = SIGX*AREAT
      LAMDA = SIGY*AREAT
      DELTA = SIGXY *AREAT
C
      IF ( IOPT .GE. 2)  GO TO 130
      KD(1) = LAMDA
      KD(2) =-DELTA
      KD(7) = KD(2)
      KD(8) = MU
  130 KD(15) = MU+LAMDA
      KD(16) =-DELTA
      KD(17) = DELTA
      KD(18) = MU -LAMDA
      KD(21) = KD(16)
      KD(27) = KD(17)
      KD(33) = KD(18)
C
C     GENERATE C MATRICES
C
      DO 140 I=1,54
  140 C(I) =0.0D0
C
C     FILL NON ZERO TERMS
C
      GAMMA1 = 1.0D0 /XSUBB
      GAMMA2 = 1.0D0 /YSUBC
      GAMMA3 = XSUBC /( XSUBB*YSUBC)
      C(3) = GAMMA3 -GAMMA2
      C(6) = GAMMA1
      C(7) =-C(3)/2.0D0
      C(8) =-GAMMA1/2.0D0
      C(10)=-GAMMA1
      C(14)= C(3)
      C(16)=-C(7)
      C(17)= C(8)
C
      C(21)=-GAMMA3
      C(24)=-GAMMA1
      C(25)= GAMMA3/2.0D0
      C(26)=-C(8)
      C(28)= GAMMA1
      C(32)=-GAMMA3
      C(34)=-C(25)
      C(35)= C(26)
C
      C(39)=GAMMA2
      C(43)=-GAMMA2/2.0D0
      C(50)= GAMMA2
      C(52)=-C(43)
C
C     REPLACE C MATRICES BY  (C)(E )(T) FOR EACH POINT
      DO 200 I =1,3
      IF( NECPT(4*I+5)) 150,160,150
C
C     GLOBAL TO BASIC MATRIX T IS GENERATED AGAIN HERE
C
  150 CALL TRANSD( NECPT(4*I+5),T(1))
      CALL GMMATD( E(1),3,3,0,  T(1),3,3,0, TEMP1(1) )
      GO TO 180
  160 DO 170 J =1,9
  170 TEMP1(J) = E(J)
C
  180 CALL GMMATD( C(18*I-17),6,3,0,  TEMP1(1),3,3,0,  TEMP2(1))
      DO 190  J=1,18
      IDUM =  18*(I-1) +J
  190 C(IDUM) = TEMP2(J)
C
  200 CONTINUE
C
      DO 210 I =1,3
      IF(NECPT(I+1) .NE. NPVT) GO TO 210
      NPIVOT= I
      GO TO 220
  210 CONTINUE
      RETURN
  220 CALL GMMATD( C(18*NPIVOT-17),6,3,1, KD(1),6,6,0, TEMP1(1))
C
C     TEMP1 NOW CONTAINS                   T
C                           ( (C )(E)(T ) ) ( KD)
C                               J      J
C     WHERE J IS THE PIVOT POINT
C
C     GENERATE THE THREE BY THREE PARTITIONS IN GLOBAL COORDINATES HERE
C
      DO 240 I=1,3
      CALL GMMATD( TEMP1,3,6,0, C(18*I-17),6,3,0,TEMP2(1)  )
C//////
C     CALL BUG(4HTRMK,260,TEMP2,18)
C//////
      DO 230 J=1,36
  230 KIJ(J) = 0.0D0
      KIJ( 1) = TEMP2(1)
      KIJ( 2) = TEMP2(2)
      KIJ( 3) = TEMP2(3)
      KIJ( 7) = TEMP2(4)
      KIJ( 8) = TEMP2(5)
      KIJ( 9) = TEMP2(6)
      KIJ(13) = TEMP2(7)
      KIJ(14) = TEMP2(8)
      KIJ(15) = TEMP2(9)
C
      CALL DS1B( KIJ(1), NECPT(I+1) )
C
  240 CONTINUE
      RETURN
      END
