      SUBROUTINE TETRA (TEMPS,PG,IOPT)
C
C     ELEMENT THERMAL LOAD GENERATOR FOR THE TETRAHEDRON SOLID ELEMENT
C
C     LOOKING DOWN ON THIS ELEMENT, GRIDS 1,2,3 ARE THE BASE AND MUST BE
C     LABELED COUNTERCLOCKWISE. GRID 4 MUST BE ABOVE THE PLANE FORMED BY
C     GRIDS 1,2,3 AND CLOSEST TO THIS OBSERVER.
C
C     ECPT FOR THE TETRAHEDRON SOLID ELEMENT
C
C     ECPT( 1) = ELEMENT ID
C     ECPT( 4) = SIL GRID POINT 3
C     ECPT( 5) = SIL GRID POINT 4
C     ECPT( 2) = MATERIAL ID (MAT1 MATERIAL TYPE)
C     ECPT( 3) = SIL GRID POINT 1
C     ECPT( 4) = SIL GRID POINT 2
C     ECPT( 5) = SIL GRID POINT 3
C     ECPT( 6) = SIL GRID POINT 4
C     ECPT( 7) = COORD SYS ID GRID PT 1
C     ECPT( 8) = X1
C     ECPT( 9) = Y1
C     ECPT(10) = Z1
C     ECPT(11) = COORD SYS ID GRID PT 2
C     ECPT(12) = X2
C     ECPT(13) = Y2
C     ECPT(14) = Z2
C     ECPT(15) = COORD SYS ID GRID PT 3
C     ECPT(16) = X3
C     ECPT(17) = Y3
C     ECPT(18) = Z3
C     ECPT(19) = COORD SYS ID GRID PT 4
C     ECPT(20) = X4
C     ECPT(21) = Y4
C     ECPT(22) = Z4
C     ECPT(23) = ELEMENT TEMPERATURE
C
      INTEGER         NECPT(2) ,OUT
      REAL            TEMPS(4) ,PG(6)   ,P(6)    ,C(72)    ,G(36)    ,
     1                H(16)    ,CTG(18) ,NU      ,ALFA(6)  ,TEMP(12)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ SYSBUF   ,OUT
      COMMON /TRIMEX/ ECPT(23)
      COMMON /MATIN / MATID    ,INFLAG  ,ELTEMP
      COMMON /MATOUT/ E        ,GG      ,NU      ,RHO       ,ALPHA   ,
     1                TSUB0    ,GSUBE   ,SIGT    ,SIGC      ,SIGS
      EQUIVALENCE     (NECPT(1),ECPT(1))
C
C     FILL THE 4 X 4 H MATRIX.
C
      H( 1) = 1.0
      H( 2) = ECPT( 8)
      H( 3) = ECPT( 9)
      H( 4) = ECPT(10)
      H( 5) = 1.0
      H( 6) = ECPT(12)
      H( 7) = ECPT(13)
      H( 8) = ECPT(14)
      H( 9) = 1.0
      H(10) = ECPT(16)
      H(11) = ECPT(17)
      H(12) = ECPT(18)
      H(13) = 1.0
      H(14) = ECPT(20)
      H(15) = ECPT(21)
      H(16) = ECPT(22)
C
C     INVERT H AND GET THE DETERMINANT
C
      ISING = 0
C
      CALL INVERS (4,H(1),4,DUM,0,HDETER,ISING,TEMP(1))
C
C     IF THE MATRIX IS SINGULAR TETRAHEDRON IS BAD
C
      HDETER = ABS(HDETER)
      IF (ISING .NE. 2) GO TO 200
      WRITE  (OUT,150) UFM,NECPT(1)
  150 FORMAT (A23,' 4002, MODULE SSG1 DETECTS BAD OR REVERSE GEOMETRY ',
     1       'FOR ELEMENT ID =',I9)
      GO TO 900
C
C     GET THE MATERIAL DATA AND FILL THE 6X6 G MATERIAL STRESS-STRAIN
C     MATRIX.
C
  200 INFLAG = 1
      MATID  = NECPT(2)
      ELTEMP = ECPT(23)
      CALL MAT (NECPT(1))
      DO 210 I = 1,36
  210 G(I)  = 0.0
      TEMP1 = (1.0+NU)*(1.0-2.0*NU)
      IF (TEMP1 .NE. 0.0) GO TO 240
      WRITE  (OUT,230) UFM,MATID,ECPT(1)
  230 FORMAT (A23,' 4003, AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED ',
     1       'UNDER MATERIAL ID =',I9,' FOR ELEMENT ID =',I9)
      GO TO 900
  240 G( 1) = E*(1.0-NU)/TEMP1
      G( 8) = G(1)
      G(15) = G(1)
      G( 2) = E*NU/TEMP1
      G( 3) = G(2)
      G( 7) = G(2)
      G( 9) = G(2)
      G(13) = G(2)
      G(14) = G(2)
      G(22) = GG
      G(29) = GG
      G(36) = GG
C
C     FILL 4 C-MATRICES. (6X3) EACH.
C
      DO 400 I = 1,72
  400 C(I) = 0.0
      DO 500 I = 1,4
      J = 18*I - 18
      C(J+ 1) = H(I+ 4)
      C(J+ 5) = H(I+ 8)
      C(J+ 9) = H(I+12)
      C(J+11) = H(I+12)
      C(J+12) = H(I+ 8)
      C(J+13) = H(I+12)
      C(J+15) = H(I+ 4)
      C(J+16) = H(I+ 8)
      C(J+17) = H(I+ 4)
  500 CONTINUE
C
C     DIVIDE DETERMINANT BY 6.0, AND BY AN ADDITIONAL 2.0 IF A SUB-TETRA
C     FOR THE HEXA-10 ELEMENT.
C
      IF (IOPT) 602,601,602
  601 HDETER = HDETER/6.0
      GO TO 610
  602 HDETER = HDETER/12.0
C
C     INTRODUCE TBAR AND ALPHA
C
  610 HDETER = HDETER*(0.25*(TEMPS(1)+TEMPS(2)+TEMPS(3)+TEMPS(4))-TSUB0)
     1         *ALPHA
C
C     FILL ALPHA VECTOR
C
      ALFA(1) = HDETER
      ALFA(2) = HDETER
      ALFA(3) = HDETER
      ALFA(4) = 0.0
      ALFA(5) = 0.0
      ALFA(6) = 0.0
C
C     LOOP FOR THE FOUR GRID POINTS
C
      DO 800 I = 1,4
      CALL GMMATS (C(18*I-17),6,3,1, G(1),6,6,0, CTG(1))
      CALL GMMATS (CTG(1),3,6,0, ALFA(1),6,1,0, P(1))
C
C     TRANSFORM TO GLOBAL
C
      P(4) = 0.0
      P(5) = 0.0
      P(6) = 0.0
      K    = 4*I + 3
      IF (NECPT(K) .NE. 0) CALL BASGLB (P(1),P(1),NECPT(K+1),NECPT(K))
C
C     INSERT LOAD VECTOR FOR GRID POINT
C
      L = NECPT(I+2) - 1
      DO 790 J = 1,3
      L = L + 1
      PG(L) = PG(L) + P(J)
  790 CONTINUE
  800 CONTINUE
      RETURN
C
  900 CALL MESAGE (-61,0,0)
      RETURN
      END
