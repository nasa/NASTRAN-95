      SUBROUTINE HDSOLV (IXR,J,XXX,CCC,II,NNO,NIT,X21,Y21,Z21,IIA,NC,
     1                   ZM,ZMI,LZ)
C
C     THIS SUBROUTINE SOLVES FOR THE LINES OF INTERSECTION RESULTING
C     FROM THE INTERSECTIONS OF THE JTH ELEMENT WITH THE OTHER
C     RELEVANT ELEMENTS.
C
C
C
      INTEGER         XCC,XASOLV,YASOLV,ZASOLV
      DIMENSION       XXX(1),CCC(1),NNO(1),ZM(1),ZMI(1),
     1                X21(1),Y21(1),Z21(1),IIA(1),IV(2)
      COMMON /HDPTRS/ XDUM,XCC,XASOLV,YASOLV,ZASOLV
      COMMON /ZZZZZZ/ RZ(1)
      COMMON /GO3   / L0,L1,L00,L01,L2,L3,L4,L5,L6,L7,L8,L9,L10,
     1                L11,L12,L13
C
      ERS = .015
      ER  =  ERS
      EXX = .015
      EXP = .015
      JT  = L12 + (J-1)*5
      JB  = L13 + (J-1)*LZ
      IF (II .EQ. 0) GO TO 80
      A3 = XXX(1+JT)
      B3 = XXX(2+JT)
      C3 = XXX(3+JT)
      D3 = XXX(4+JT)
      IF (XXX(JT+3) .EQ. 0.) GO TO 80
      DO 70 L = 1,II
      K  = NNO(L4+L)
C
C     CHECKS TO SEE IF THIS RELEVANT ELEMENT IS TO BE CONSIDERED FOR
C     INTERSECTION
C
      IF (K .GT. 0) GO TO 9
      GO TO 70
    9 CONTINUE
      IF (K .LT. J) GO TO 70
      JX = L12 + (K-1)*5
      IF (ZM(L2+J) .LT. ZMI(L3+K)) GO TO 70
      IF (ABS(XXX(3+JX)) .LT. ERS) GO TO 70
      MT = 0
      A4 = XXX(1+JX)
      B4 = XXX(2+JX)
      C4 = XXX(3+JX)
      D4 = XXX(4+JX)
C
C     DETERMINES THE EQUATION OF LINE OF INTERSECTION.
C
      B = A3*C4 - A4*C3
      A = B3*C4 - B4*C3
      C = D3*C4 - D4*C3
      IF (A.EQ.0. .AND. B.EQ.0.) GO TO 70
      IF (A .NE. 0.) GO TO 10
      A = 0
      C = C/B
      B = 1
      GO TO 20
   10 CONTINUE
      B = B/A
      C = C/A
      A = 1
   20 CONTINUE
      IV(1) = J
      IV(2) = K
      DO 60 M = 1,2
      JV = 1
      I  = IV(M)
      JJ = L13 + (I-1)*LZ
      IG = L12 + (I-1)*5 + 5
      NK = XXX(IG)
      DO 50 IX = 1,NK
      A1 = CCC(JV+  JJ)
      B1 = CCC(JV+1+JJ)
      C1 = CCC(JV+2+JJ)
C
C     CHECK TO BE SURE LINE OF INTERSECTION IS NOT BOUNDARY LINE
C     OF THE JTH SET.
C
      S  = A1 + B1 + C1
      S1 = A  + B  + C
      E  = ABS(S-S1)
      S  = A1*50 + B1*50 + C1
      S1 = A *50 + B *50 + C
      F  = ABS(S-S1)
      IF (F.LT.EXP .AND. E.LT.EXP) GO TO 70
C
C
C     DETERMINES THE POINTS OF INTERSECTIONS OF THE LINE OF INTERSECTION
C     WITH OTHER LINES OF RELEVANT ELEMENTS.
C
C
      T = A1*B - B1*A
      IF (ABS(T) .LT. ER) GO TO 50
      XO = (C1*A-C*A1)/T
      IF (A .NE. 0.) GO TO 30
      YO = -C1 - B1*XO
      GO TO 40
   30 CONTINUE
      YO = -C - B*XO
   40 CONTINUE
      T = XO
      IF (A1 .EQ. 0.) T = YO
      S  = T - CCC(JV+4+JJ)
      S1 = T - CCC(JV+3+JJ)
      IF (S*S1 .GT. 0.) GO TO 50
      MT = MT + 1
C
C     STORE THE PTS OF INTERSECTIONS.
C
      RZ(XASOLV-1+MT) = XO
      RZ(YASOLV-1+MT) = YO
      RZ(ZASOLV-1+MT) =-(D3+A3*XO+B3*YO)/C3
      ZT = -(D4+A4*XO+B4*YO)/C4
      IF (ABS(ZT-RZ(ZASOLV-1+MT)) .GT. EXX) GO TO 70
   50 JV = JV + 5
   60 CONTINUE
      CALL HDSTAT (MT,NIT,IXR,X21,Y21,Z21,IIA,IV,A,B,C,J,
     1             RZ(XASOLV),RZ(YASOLV),RZ(ZASOLV),CCC,XXX,LZ)
   70 CONTINUE
   80 CONTINUE
      NR = 5*XXX(5+JT)
      DO 90 IS = 1,NR
      RZ(XCC-1+IS) = CCC(IS+JB)
   90 CONTINUE
      XXX(5+JT) = XXX(5+JT) + NIT
      RETURN
      END
