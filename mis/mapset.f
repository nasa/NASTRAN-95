      SUBROUTINE MAPSET(X1,Y1,X2,Y2,KI1,KJ1,KI2,KJ2,L)
C
C     POINT 1 IS LOWER LEFT CORNER OF FRAME
C     POINT 2 IS UPPER RIGHT CORNER OF FRAME
C     I,J ARE IN PLOTTER UNITS
C     X,Y ARE IN PHYSICAL UNITS
C     L IS OUTPUT FLAG, 1=I,J ARE INTEGER, 2=I,J ARE REAL
C
      EQUIVALENCE (I1,ZI1),(J1,ZJ1),(I2,ZI2),(J2,ZJ2)
      EQUIVALENCE (I,ZI),(J,ZJ)
C
      I1=KI1
      J1=KJ1
      I2=KI2
      J2=KJ2
      LL=L
C
      IF(L.EQ.2) GO TO 100
      A=FLOAT(I2-I1)/(X2-X1)
      B=FLOAT(I1)-A*X1
      C=FLOAT(J2-J1)/(Y2-Y1)
      D=FLOAT(J1)-C*Y1
      RETURN
  100 A=(ZI2-ZI1)/(X2-X1)
      B=ZI1-A*X1
      C=(ZJ2-ZJ1)/(Y2-Y1)
      D=ZJ1-C*Y1
      RETURN
C
C
C***********************************************************************
C
      ENTRY MAP(X,Y,KI,KJ)
      IF(LL.EQ.2) GO TO 200
      I=A*X+B+0.5
      J=C*Y+D+0.5
      GO TO 300
  200 ZI=A*X+B
      ZJ=C*Y+D
  300 KI=I
      KJ=J
      RETURN
C
      END
