      SUBROUTINE HDSKET (X,Y,Z,NP,NC)
C
C     THIS SUBROUTINE SETS UP PEN MOTION INDICATORS.
C
      INTEGER         XCC,X1SKT,Y1SKT,Z1SKT,X21,Y21,Z21,XE,YE,XU,YU,
     1                XI,YI,ZI,DI,W,IZ(1)
      DIMENSION       X(1),Y(1),Z(1)
      COMMON /HDPTRS/ XDUM,XCC,XASOLV,YASOLV,ZASOLV,X1SKT,Y1SKT,Z1SKT,
     1                ZCOEF1,ZCOEF,ICOUNT,IRCT,X21,Y21,Z21,IIA,XE,YE,
     2                XU,YU,XI,YI,ZI,DI,IBEG,IEND,ICT,ICCT,W
      COMMON /ZZZZZZ/ RZ(1)
      COMMON /HDSC  / SCX,YAW,ROL,PIT,LZ,VP,JJJ,ICORE
      EQUIVALENCE     (IZ(1),RZ(1))
C
      L   = NP
      LI  = NP
      IF (L .LE. 2) GO TO 50
      LX  = 1
      NPX = NP
    1 NPX = NPX-1
      I   = LX
      DO 8 M = I,NPX
      RX = 0
      A  = X(M+1) - X(M)
      B  = Y(M+1) - Y(M)
      C  = Z(M+1) - Z(M)
      IF (A .NE. 0.) GO TO 8
      IF (B .NE. 0.) GO TO 8
      IF (C .NE. 0.) GO TO 8
      IX = M
      IX1SKT = NPX
      DO 4 MX = IX,IX1SKT
      X(MX) = X(MX+1)
      Y(MX) = Y(MX+1)
      Z(MX) = Z(MX+1)
    4 CONTINUE
      RX = 1
      LX = M
      IF (LX .EQ. NPX) GO TO 10
      GO TO 1
    8 CONTINUE
   10 CONTINUE
      IF (RX .EQ. 1.) NPX = NPX - 1
      NP = NPX + 1
      LI = NP
      IF (NP .LE. 2) GO TO 50
      IX = 0
      M1 = 0
      M  = 1
      IS = NP - 1
   20 CONTINUE
      M  = M  + IX
      M1 = M1 + IX + 1
      IF (M-1 .EQ. LI) GO TO 70
C
C     SEARCH FOR MATCHING COORDINATES.
C
      DO 40 J = M,IS
      T = X(J+1) - X(M)
      U = Z(J+1) - Z(M)
      V = Y(J+1) - Y(M)
      IF (T .NE. 0.) GO TO 40
      IF (V .NE. 0.) GO TO 40
      IF (U .NE. 0.) GO TO 40
      NP = NP + 1
C
C     MATCH FOUND.....STORE COORDINATES AND SET SWITCH TO LIFT PEN
C     AND/OR END SET.
C
      IX = J + 2 - M
      IX1SKT = J - IS + 1
      DO 30 IK = 1,IX
      RZ(X1SKT+M1-2+IK) = X(M-1+IK)
      RZ(Y1SKT+M1-2+IK) = Y(M-1+IK)
      RZ(Z1SKT+M1-2+IK) = Z(M-1+IK)
   30 CONTINUE
      RZ(Z1SKT-1+M1+IX) = -ISIGN(1,IX1SKT)*9999.
      GO TO 20
   40 CONTINUE
   50 CONTINUE
      DO 60 J = 1,LI
      RZ(X1SKT-1+J) = X(J)
      RZ(Y1SKT-1+J) = Y(J)
      RZ(Z1SKT-1+J) = Z(J)
   60 CONTINUE
      NP = NP + 1
      RZ(Z1SKT-1+NP) = -9999.
   70 CONTINUE
      CALL HDLIN (RZ(X1SKT),RZ(Y1SKT),RZ(Z1SKT),NP,NC,
     1     RZ(XCC),IZ(ICOUNT),IZ(IRCT),RZ(X21),RZ(Y21),RZ(Z21),
     2     IZ(IIA),RZ(XE),RZ(YE),RZ(XU),RZ(YU),RZ(XI),RZ(YI),RZ(ZI),
     3     RZ(DI),IZ(IBEG),IZ(IEND),IZ(ICT),IZ(ICCT),
     4     IZ(W),IZ(W),RZ(W),RZ(W),IZ(W),IZ(W),IZ(W),RZ(W),RZ(W),RZ(W),
     5     RZ(W),RZ(W),RZ(W),RZ(W),IZ(W),IZ(W),RZ(W),RZ(W),RZ(W),RZ(W),
     6     IZ(W),IZ(W))
      NP = L
C
C     RESET VALUE FOR MAXIMUM NUMBER OF EDGES IF ARGUMENT IS COMPLETED.
C
      IF (VP .GT. 0.) LZ = LZ/5
      RETURN
      END
