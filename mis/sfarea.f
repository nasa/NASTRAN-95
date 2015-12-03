      SUBROUTINE SFAREA (NGPT,V,G)
C
C     THIS SUBROUTINE IS CALLED ONLY BY EMGFIN TO COMPUTE THE SURFACE
C     AREAS OF THE SOLID AND PLATE ELEMENTS
C     NOTE - THE INPUT VALUE OF NGPT (NO. OF GRID POINTS) WILL BE
C     CHANGED TO NO. OF SURFACE AREAS (OUTPUT) BY THIS ROUTINE
C
C     DEFINITION OF SURFACES ( 1 THRU 6)
C
C     LET CORNER POINTS 1, 2, 3, 4 ON THE BOTTOM SURFACE OF A CUBE,
C     AND 5, 6, 7, AND 8 ARE ON TOP SURFACE. CORNER POINT 5 IS ON TOP
C     OF POINT 1, THEN FOR SOLID (BRICK) ELEMENTS -
C
C       FACE       CORNER POINTS
C     -------     ---------------
C        1          1  2  3  4
C        2          1  2  6  5
C        3          2  3  7  6
C        4          3  4  8  7
C        5          4  1  5  8
C        6          5  6  7  8
C
C     IN WEDGE AND TETRA, FACE 1 CONTAINS CORNER POINTS 1, 2, AND 3,
C     FACE 2 IS MADE UP OF 1, 2, AND M, WHERE M IS A CORNER POINT NOT
C     ON FACE 1, AND SIMILARLY, FACE 3 HOLDS CONNER POINTS 2, 3, AND N,
C     AND SO ON.
C
C     PLATE (TRIANG AND QUAD) ELEMENTS HAVE ONE SURFACE. MASS AND VOLUME
C     ARE COMPUTED HERE FOR THESE ELEMENTS.
C
      INTEGER         SUB(2),   JX(6),    KX(12),   TYPE(6)
      REAL            V(1),     G(1),     A(6)
      COMMON /BLANK/  DUMMY(16),VOLUME,   SURFAC
      DATA    TETRA,  S2D8,     TRIM6,    TRPL1,    TRSHL  /
     1        4HCTET, 4HCIS2,   4HCTRI,   4HCTRP,   4HCTRS /
      DATA     JX  /  129, 133, 137, 141, 145, 149 /
      DATA    TYPE /    4,   3,   8,   6,  20,  32 /
      DATA     KX  /  9, 33, 5, 89, 17, 101, 29, 113, 41, 125, 89, 113 /
      DATA     SUB /  4HSFAR,   4HEA      /
C
C     AREA(I,J,K)=.5*SQRT(
C    1 ((G(J+2)-G(I+2))*(G(K+3)-G(I+3))-(G(J+3)-G(I+3))*(G(K+2)-G(I+2)))
C    2 **2
C    3+((G(J+3)-G(I+3))*(G(K+1)-G(I+1))-(G(J+1)-G(I+1))*(G(K+3)-G(I+3)))
C    4 **2
C    5+((G(J+1)-G(I+1))*(G(K+2)-G(I+2))-(G(J+2)-G(I+2))*(G(K+1)-G(I+1)))
C    6 **2)
C
C     (THE ABOVE FUNCTION MAY BE TOO LONG FOR SOME MACHINE THAT
C      WOULD CREATE PROBLEM IN COMPILING. SO MOVE IT OUT AND MAKE
C      IT AN EXTERNAL FUNCTION. AND ADD A 'G,' INSIDE ARG. LIST)
C
C
C     1 2 3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 - GRID PT
C     1 5 9 13 17 21 25 29 33 37 41 45 49 53 57 61 65 69 73 77 - POINTER
C
C     21 22 23 24 25  26  27  28  29  30  31  32  C1  C2  C3  C4  C5  C6
C     81 85 89 93 97 101 105 109 113 117 121 125 129 133 137 141 145 149
C     (WHERE C1, C2, ..., C6 ARE CENTER POINTS ON FACES 1, 2, ..., 6)
C
      DO 10 L=1,6
      IF (NGPT .EQ. TYPE(L)) GO TO (120,130,40,30,50,60), L
C                   NO. OF GRID PT =  4,  3, 8, 6,20,32
 10   CONTINUE
      CALL MESAGE (-37,0,SUB)
C
C     4-GRID ELEMENT (TETRA)
C
 20   A(1) = AREA(G,1,5,9 )
      A(2) = AREA(G,1,5,13)
      A(3) = AREA(G,5,9,13)
      A(4) = AREA(G,1,9,13)
      NAREA = 4
      GO TO 200
C
C     6-GRID ELEMENT (WEDGE)
C
 30   IF (V(1).EQ.TRIM6 .OR. V(1).EQ.TRPL1 .OR. V(1).EQ.TRSHL) GO TO 150
      A(1) = AREA(G,1,5,9)
      A(2) = AREA(G,1,5,13) + AREA(G,13,17,5)
      A(3) = AREA(G,5,9,17) + AREA(G,17,21,9)
      A(4) = AREA(G,1,9,13) + AREA(G,9,13,21)
      A(5) = AREA(G,13,17,21)
      NAREA = 5
      GO TO 200
C
C     8-GIRD ELEMENT
C
 40   IF (V(1) .EQ. S2D8) GO TO 140
      A(1) = AREA(G,1,5,9)   + AREA(G,1,9,13)
      A(2) = AREA(G,1,5,17)  + AREA(G,5,17,21)
      A(3) = AREA(G,5,21,25) + AREA(G,5,25,9)
      A(4) = AREA(G,9,25,29) + AREA(G,9,29,13)
      A(5) = AREA(G,13,1,29) + AREA(G,1,29,17)
      A(6) = AREA(G,17,21,25)+ AREA(G,17,25,29)
      GO TO 110
C
C     20-GRID ELEMENT
C
 50   A(1) = AREA(G, 1, 5,29) + AREA(G,29, 5,13) + AREA(G,13, 5, 9) +
     1       AREA(G,13,17,21) + AREA(G,13,21,29) + AREA(G,29,25,21)
      A(2) = AREA(G, 1, 5,33) + AREA(G,33, 5,53) + AREA(G,53,33,49) +
     1       AREA(G,37,57,53) + AREA(G,53,37, 5) + AREA(G, 5, 9,37)
      A(3) = AREA(G, 9,37,13) + AREA(G,13,37,61) + AREA(G,61,37,57) +
     1       AREA(G,61,65,41) + AREA(G,41,61,13) + AREA(G,13,41,17)
      A(4) = AREA(G,17,41,21) + AREA(G,21,69,41) + AREA(G,41,65,69) +
     1       AREA(G,69,73,45) + AREA(G,45,69,21) + AREA(G,21,45,25)
      A(5) = AREA(G, 1,33,29) + AREA(G,29,77,33) + AREA(G,33,49,77) +
     1       AREA(G,77,73,45) + AREA(G,45,77,29) + AREA(G,29,45,25)
      A(6) = AREA(G,49,53,77) + AREA(G,77,53,61) + AREA(G,61,53,57) +
     1       AREA(G,61,65,69) + AREA(G,69,61,77) + AREA(G,77,69,73)
      GO TO 110
C
C     32-GRID ELEMENT
C
 60   DO 70 L=1,6
 70   A(L) = 0.0
      KK = 1
      DO 90 L=129,152,4
      M = KX(KK  )
      N = KX(KK+1)
      DO 80 JJ=1,3
 80   G(L+JJ) = 0.5*(G(M+JJ)+G(N+JJ))
 90   KK = KK+2
      JJ= 2
      DO 100 L=1,12
      M = L*4-3
      N = M+4
      IF (N .GT. 48) N=1
      A( 1) = A( 1) + AREA(G,M,N,JX( 1))
      A(JJ) = A(JJ) + AREA(G,M,N,JX(JJ))
      M = (L+20)*4-3
      N = M+4
      IF (N .GT. 128) N=81
      A( 6) = A( 6) + AREA(G,M,N,JX( 6))
      A(JJ) = A(JJ) + AREA(G,M,N,JX(JJ))
      IF (MOD(L,3) .EQ. 0) JJ = JJ+1
 100  CONTINUE
      A(2)=A(2)+AREA(G, 1, 49,133)+AREA(G,49, 65,133)+AREA(G,65, 81,133)
     1         +AREA(G,13, 53,133)+AREA(G,53, 69,133)+AREA(G,69, 93,133)
      A(3)=A(3)+AREA(G,13, 53,137)+AREA(G,53, 69,137)+AREA(G,69, 93,137)
     1         +AREA(G,25, 57,137)+AREA(G,57, 73,137)+AREA(G,73,105,137)
      A(4)=A(4)+AREA(G,25, 57,141)+AREA(G,57, 73,141)+AREA(G,73,105,141)
     1         +AREA(G,37, 61,141)+AREA(G,61, 77,141)+AREA(G,77,117,141)
      A(5)=A(5)+AREA(G,37, 61,145)+AREA(G,61, 77,145)+AREA(G,77,117,145)
     1         +AREA(G, 1, 49,145)+AREA(G,49, 65,145)+AREA(G,65, 81,145)
 110  NAREA = 6
      GO TO 200
C
C     4-GRID ELEMENT (QUAD)
C
 120  IF (V(1) .EQ. TETRA) GO TO 20
      A(1)=AREA(G,1,5,9) + AREA(G,1,5,13)
      GO TO 160
C
C     3-GRID ELEMENT
C
 130  A(1)=AREA(G,1,5,9)
      GO TO 160
C
C     8-GRID ELEMENT (IS2D8)
C
 140  J=33
      A(1) = G(J)
      GO TO 160
C
C     6-GRID TRIANGULAR ELEMENTS (TRIM6, TRPLT1, TRSHL)
C
 150  I=129
      J=21
      K=9
      DO 155 L=1,3
 155  G(L+I)=G(L+J) + (G(L+K)-G(L+J))*.33333
      A(1) = AREA(G, 1, 5,129) + AREA(G, 5, 9,129) + AREA(G, 9,13,129) +
     1       AREA(G,13,17,129) + AREA(G,17,21,129) + AREA(G,21, 1,129)
 160  NAREA=1
C
C     AT THIS POINT, V(4) AND V(5) ARE THICKNESS AND DENSITY OF THE
C     PLATE. COMPUTE VOLUME AND MASS AND PUT THEM BACK IN V(4) AND V(5)
C
      IF (VOLUME .LE. 0.0) GO TO 200
      J=4
      V(J+1) = A(1)*V(J)*V(J+1)
      V(J) = A(1)*V(J)*VOLUME
C
 200  NGPT = NAREA
      DO 210 L=1,NAREA
 210  V(L+5) = A(L)*SURFAC
      RETURN
      END
