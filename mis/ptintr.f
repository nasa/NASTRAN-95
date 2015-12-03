      SUBROUTINE PTINTR (A,AA,B,BB,S,K,EPS)
C
C     RETURNS DOUBLE PRECISION VALUES OF X,Y COORDINATES (S) OF
C         POINT OF INTERSECTION (IF ANY) OF LINE SEGMENTS
C         FROM A TO AA AND B TO BB
C           A .NE. AA  AND  B .NE. BB
C     K IS CONDITION FLAG RETURNED --
C         K = 1   LINES INTERSECT AT S
C         K = 0   LINES INTERSECT AT S, AN ENDPOINT OF ONE LINE SEGMENT
C         K =-1   LINES DO NOT INTERSECT
C
      DOUBLE PRECISION A(2),AA(2),B(2),BB(2),P(2),S(2)
      DOUBLE PRECISION AX,AY,BX,BY,AAA,PA,PAA,BBB,PB,PBB
      DOUBLE PRECISION EPS(2),D
      DOUBLE PRECISION DIST,X,Y,U,V
C
C     EPS ARRAY FOR SIGNIFICANCE TESTING
C         EPS(1) IS AREA, ANGLE LIMIT
C         EPS(2) IS LENGTH LIMIT
C
C
C     DOUBLE PRECISION FUNCTION FOR DISTANCE BETWEEN 2 POINTS
C
      DIST(X,Y,U,V) = (X-U)**2 +(Y-V)**2
C
      X    = 0.D0
      Y    = X
      U    = X
      V    = X
      P(1) = 0.D0
      P(2) = 0.D0
      S(1) = 0.D0
      S(2) = 0.D0
C
      K  =-1
C
      AX = AA(1) - A(1)
      AY = AA(2) - A(2)
      BX = BB(1) - B(1)
      BY = BB(2) - B(2)
C
      AAA= AX**2 + AY**2
      BBB= BX**2 + BY**2
      D  = BX*AY - AX*BY
C
C     IS EITHER LINE TOO SHORT?
C
      IF (AAA.LE.EPS(1) .OR. BBB.LE.EPS(1)) RETURN
C
C     ARE A AND B PARALLEL?
C
      IF (DABS(D) .GT. EPS(1)) GO TO 80
C
C     A AND B ARE PARALLEL -- ARE THEY SAME LINE?
C
      P(1) = B(1)
      P(2) = B(2)
      IF (DIST(B(1),B(2), A(1), A(2)) .LE. EPS(1) .OR.
     1    DIST(B(1),B(2),AA(1),AA(2)) .LE. EPS(1)) GO TO 100
      P(1) = BB(1)
      P(2) = BB(2)
      IF (DIST(BB(1),BB(2), A(1), A(2)) .LE. EPS(1) .OR.
     1    DIST(BB(1),BB(2),AA(1),AA(2)) .LE. EPS(1)) GO TO 100
C
C     A PARALLEL TO B AND NOT SAME LINE
C
      RETURN
C
C     IS A PARALLEL TO Y AXIS?
C
   80 IF (DABS(AX) .GT. EPS(2)) GO TO 90
      P(1) = A(1)
      P(2) = B(2) + (P(1)-B(1))*BY/BX
      GO TO 100
   90 P(1) = ((B(2)-A(2))*AX*BX + A(1)*AY*BX-B(1)*AX*BY)/D
      P(2) =  A(2) + (P(1)-A(1))*AY/AX
C
  100 AAA = AAA + EPS(1)
      BBB = BBB + EPS(1)
      PA  = DIST(P(1),P(2), A(1), A(2))
      PB  = DIST(P(1),P(2), B(1), B(2))
      PAA = DIST(P(1),P(2),AA(1),AA(2))
      PBB = DIST(P(1),P(2),BB(1),BB(2))
C
C     POINT OF INTERSECTION NOT ON EITHER SEGMENT
C
      IF (PA.GT.AAA .OR. PAA.GT.AAA .OR. PB.GT.BBB .OR. PBB.GT.BBB)
     1    RETURN
C
C     LINES INTERSECT AT P
C
      K    = 1
      S(1) = P(1)
      S(2) = P(2)
C
C     LINES INTERSECT AT P, AN ENDPOINT OF ONE SEGMENT
C
      IF ((PA.LT.EPS(2) .OR. PAA.LT.EPS(2)) .OR.
     1    (PB.LT.EPS(2) .OR. PBB.LT.EPS(2))) K= 0
      RETURN
      END
