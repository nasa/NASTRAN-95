      SUBROUTINE FORMG2(IG,JR,JD,IR,ID)
C
C     FORMGG FORMS THE GG MATRIX FOR EACH RIGID ELEMENT DEGREE OF
C     FREEDOM.  IG IS THE START OF THE ROW STORED GG MATRIX - 1
C     JR IS THE START OF THE TA MATRIX - 1.
C     JD IS THE START OF THE TB MATRIX - 1.
C     IR IS THE START OF THE BGPDT INFORMATION FOR REFERENCE POINT
C     ID IS THE START OF THE BGPDT INFORMATION FOR DEPENDENT POINT
C
      DOUBLE PRECISION XD,YD,ZD,ZZ(1)
      DIMENSION ZR(1)
      INTEGER Z
      COMMON/ZZZZZZ/Z(1)
      EQUIVALENCE (ZZ(1),ZR(1))
      EQUIVALENCE (ZZ(1),Z(1))
C
C     CALCULATE THE X,Y,AND Z DIRECTED DISTANCES WITH RESPECT TO THE
C     REFERENCE GRID POINT
C
      XD = ZR(ID+1) - ZR(IR+1)
      YD = ZR(ID+2) - ZR(IR+2)
      ZD = ZR(ID+3) - ZR(IR+3)
C
C     IF NO TRANSFORMATION IS NECESSARY, GO TO 30
C
      IF (Z(IR).EQ.0.AND.Z(ID).EQ.0) GO TO 30
C
C     IF ONLY DEPENDENT GRID POINT HAS A TRANSFORMATION, GO TO 20
C
      IF (Z(IR).EQ.0) GO TO 20
C
C     IF BOTH HAVE TRANSFORMATIONS, GO TO 10
C
C
      IF (Z(ID).NE.0) GO TO 10
C
C     ONLY REFERENCE GRID POINT HAS A TRANSFORMATION
C
      ZZ(IG+ 1) = ZZ(JR+1)
      ZZ(IG+ 2) = ZZ(JR+2)
      ZZ(IG+ 3) = ZZ(JR+3)
      ZZ(IG+ 4) =ZD * ZZ(JR+4) - YD * ZZ(JR+7)
      ZZ(IG+ 5) =ZD * ZZ(JR+5) - YD * ZZ(JR+8)
      ZZ(IG+ 6) =ZD * ZZ(JR+6) - YD * ZZ(JR+9)
      ZZ(IG+ 7) = ZZ(JR+4)
      ZZ(IG+ 8) = ZZ(JR+5)
      ZZ(IG+ 9) = ZZ(JR+6)
      ZZ(IG+10) =XD * ZZ(JR+7) - ZD * ZZ(JR+1)
      ZZ(IG+11) =XD * ZZ(JR+8) - ZD * ZZ(JR+2)
      ZZ(IG+12) =XD * ZZ(JR+9) - ZD * ZZ(JR+3)
      ZZ(IG+13) = ZZ(JR+7)
      ZZ(IG+14) = ZZ(JR+8)
      ZZ(IG+15) = ZZ(JR+9)
      ZZ(IG+16) =YD * ZZ(JR+1) - XD * ZZ(JR+4)
      ZZ(IG+17) =YD * ZZ(JR+2) - XD * ZZ(JR+5)
      ZZ(IG+18) =YD * ZZ(JR+3) - XD * ZZ(JR+6)
      ZZ(IG+19) = 0.0
      ZZ(IG+20) = 0.0
      ZZ(IG+21) = 0.0
      ZZ(IG+22) = ZZ(IG+ 1)
      ZZ(IG+23) = ZZ(IG+ 2)
      ZZ(IG+24) = ZZ(IG+ 3)
      ZZ(IG+25) = 0.0
      ZZ(IG+26) = 0.0
      ZZ(IG+27) = 0.0
      ZZ(IG+28) = ZZ(IG+ 7)
      ZZ(IG+29) = ZZ(IG+ 8)
      ZZ(IG+30) = ZZ(IG+ 9)
      ZZ(IG+31) = 0.0
      ZZ(IG+32) = 0.0
      ZZ(IG+33) = 0.0
      ZZ(IG+34) = ZZ(IG+13)
      ZZ(IG+35) = ZZ(IG+14)
      ZZ(IG+36) = ZZ(IG+15)
      RETURN
   10 CONTINUE
C
C     BOTH HAVE TRANSFORMATIONS
C
      ZZ(IG+ 1) = ZZ(JD+1)*ZZ(JR+1) + ZZ(JD+4)*ZZ(JR+4) +
     1 ZZ(JD+7)*ZZ(JR+7)
      ZZ(IG+ 2) = ZZ(JD+1)*ZZ(JR+2) + ZZ(JD+4)*ZZ(JR+5) +
     2 ZZ(JD+7)*ZZ(JR+8)
      ZZ(IG+ 3) = ZZ(JD+1)*ZZ(JR+3) + ZZ(JD+4)*ZZ(JR+6) +
     3 ZZ(JD+7)*ZZ(JR+9)
      ZZ(IG+ 4) = ZZ(JD+1)*ZD*ZZ(JR+4)-ZZ(JD+1)*YD*ZZ(JR+7) +
     1 ZZ(JD+4)*XD*ZZ(JR+7) - ZZ(JD+4)*ZD*ZZ(JR+1) +
     1 ZZ(JD+7)*YD*ZZ(JR+1) - ZZ(JD+7)*XD*ZZ(JR+4)
      ZZ(IG+ 5) = ZZ(JD+1)*ZD*ZZ(JR+5)-ZZ(JD+1)*YD*ZZ(JR+8) +
     2 ZZ(JD+4)*XD*ZZ(JR+8) - ZZ(JD+4)*ZD*ZZ(JR+2) +
     2 ZZ(JD+7)*YD*ZZ(JR+2) - ZZ(JD+7)*XD*ZZ(JR+5)
      ZZ(IG+ 6) = ZZ(JD+1)*ZD*ZZ(JR+6)-ZZ(JD+1)*YD*ZZ(JR+9) +
     3 ZZ(JD+4)*XD*ZZ(JR+9) - ZZ(JD+4)*ZD*ZZ(JR+3) +
     3 ZZ(JD+7)*YD*ZZ(JR+3) - ZZ(JD+7)*XD*ZZ(JR+6)
      ZZ(IG+ 7) = ZZ(JD+2)*ZZ(JR+1) + ZZ(JD+5)*ZZ(JR+4) +
     4 ZZ(JD+8)*ZZ(JR+7)
      ZZ(IG+ 8) = ZZ(JD+2)*ZZ(JR+2) + ZZ(JD+5)*ZZ(JR+5) +
     5 ZZ(JD+8)*ZZ(JR+8)
      ZZ(IG+ 9) = ZZ(JD+2)*ZZ(JR+3) + ZZ(JD+5)*ZZ(JR+6) +
     6 ZZ(JD+8)*ZZ(JR+9)
      ZZ(IG+10) = ZZ(JD+2)*ZD*ZZ(JR+4)-ZZ(JD+2)*YD*ZZ(JR+7) +
     4 ZZ(JD+5)*XD*ZZ(JR+7) - ZZ(JD+5)*ZD*ZZ(JR+1) +
     4 ZZ(JD+8)*YD*ZZ(JR+1) - ZZ(JD+8)*XD*ZZ(JR+4)
      ZZ(IG+11) = ZZ(JD+2)*ZD*ZZ(JR+5)-ZZ(JD+2)*YD*ZZ(JR+8) +
     5 ZZ(JD+5)*XD*ZZ(JR+8) - ZZ(JD+5)*ZD*ZZ(JR+2) +
     5 ZZ(JD+8)*YD*ZZ(JR+2) - ZZ(JD+8)*XD*ZZ(JR+5)
      ZZ(IG+12) = ZZ(JD+2)*ZD*ZZ(JR+6)-ZZ(JD+2)*YD*ZZ(JR+9) +
     6 ZZ(JD+5)*XD*ZZ(JR+9) - ZZ(JD+5)*ZD*ZZ(JR+3) +
     6 ZZ(JD+8)*YD*ZZ(JR+3) - ZZ(JD+8)*XD*ZZ(JR+6)
      ZZ(IG+13) = ZZ(JD+3)*ZZ(JR+1) + ZZ(JD+6)*ZZ(JR+4) +
     7 ZZ(JD+9)*ZZ(JR+7)
      ZZ(IG+14) = ZZ(JD+3)*ZZ(JR+2) + ZZ(JD+6)*ZZ(JR+5) +
     8 ZZ(JD+9)*ZZ(JR+8)
      ZZ(IG+15) = ZZ(JD+3)*ZZ(JR+3) + ZZ(JD+6)*ZZ(JR+6) +
     9 ZZ(JD+9)*ZZ(JR+9)
      ZZ(IG+16) = ZZ(JD+3)*ZD*ZZ(JR+4)-ZZ(JD+3)*YD*ZZ(JR+7) +
     7 ZZ(JD+6)*XD*ZZ(JR+7) - ZZ(JD+6)*ZD*ZZ(JR+1) +
     7 ZZ(JD+9)*YD*ZZ(JR+1) - ZZ(JD+9)*XD*ZZ(JR+4)
      ZZ(IG+17) = ZZ(JD+3)*ZD*ZZ(JR+5)-ZZ(JD+3)*YD*ZZ(JR+8) +
     8 ZZ(JD+6)*XD*ZZ(JR+8) - ZZ(JD+6)*ZD*ZZ(JR+2) +
     8 ZZ(JD+9)*YD*ZZ(JR+2) - ZZ(JD+9)*XD*ZZ(JR+5)
      ZZ(IG+18) = ZZ(JD+3)*ZD*ZZ(JR+6)-ZZ(JD+3)*YD*ZZ(JR+9) +
     9 ZZ(JD+6)*XD*ZZ(JR+9) - ZZ(JD+6)*ZD*ZZ(JR+3) +
     9 ZZ(JD+9)*YD*ZZ(JR+3) - ZZ(JD+9)*XD*ZZ(JR+6)
      ZZ(IG+19) = 0.0
      ZZ(IG+20) = 0.0
      ZZ(IG+21) = 0.0
      ZZ(IG+22) = ZZ(IG+ 1)
      ZZ(IG+23) = ZZ(IG+ 2)
      ZZ(IG+24) = ZZ(IG+ 3)
      ZZ(IG+25) = 0.0
      ZZ(IG+26) = 0.0
      ZZ(IG+27) = 0.0
      ZZ(IG+28) = ZZ(IG+ 7)
      ZZ(IG+29) = ZZ(IG+ 8)
      ZZ(IG+30) = ZZ(IG+ 9)
      ZZ(IG+31) = 0.0
      ZZ(IG+32) = 0.0
      ZZ(IG+33) = 0.0
      ZZ(IG+34) = ZZ(IG+13)
      ZZ(IG+35) = ZZ(IG+14)
      ZZ(IG+36) = ZZ(IG+15)
      RETURN
   20 CONTINUE
C
C     DEPENDENT GRID POINT HAS TRANSFORMATION
C
      ZZ(IG+ 1) = ZZ(JD+1)
      ZZ(IG+ 2) = ZZ(JD+4)
      ZZ(IG+ 3) = ZZ(JD+7)
      ZZ(IG+ 4) = ZZ(JD+7)*YD - ZZ(JD+4)*ZD
      ZZ(IG+ 5) = ZZ(JD+1)*ZD - ZZ(JD+7)*XD
      ZZ(IG+ 6) = ZZ(JD+4)*XD - ZZ(JD+1)*YD
      ZZ(IG+ 7) = ZZ(JD+2)
      ZZ(IG+ 8) = ZZ(JD+5)
      ZZ(IG+ 9) = ZZ(JD+8)
      ZZ(IG+10) = ZZ(JD+8)*YD - ZZ(JD+5)*ZD
      ZZ(IG+11) = ZZ(JD+2)*ZD - ZZ(JD+8)*XD
      ZZ(IG+12) = ZZ(JD+5)*XD - ZZ(JD+2)*YD
      ZZ(IG+13) = ZZ(JD+3)
      ZZ(IG+14) = ZZ(JD+6)
      ZZ(IG+15) = ZZ(JD+9)
      ZZ(IG+16) = ZZ(JD+9)*YD - ZZ(JD+6)*ZD
      ZZ(IG+17) = ZZ(JD+3)*ZD - ZZ(JD+9)*XD
      ZZ(IG+18) = ZZ(JD+6)*XD - ZZ(JD+3)*YD
      ZZ(IG+19) = 0.0
      ZZ(IG+20) = 0.0
      ZZ(IG+21) = 0.0
      ZZ(IG+22) = ZZ(IG+ 1)
      ZZ(IG+23) = ZZ(IG+ 2)
      ZZ(IG+24) = ZZ(IG+ 3)
      ZZ(IG+25) = 0.0
      ZZ(IG+26) = 0.0
      ZZ(IG+27) = 0.0
      ZZ(IG+28) = ZZ(IG+ 7)
      ZZ(IG+29) = ZZ(IG+ 8)
      ZZ(IG+30) = ZZ(IG+ 9)
      ZZ(IG+31) = 0.0
      ZZ(IG+32) = 0.0
      ZZ(IG+33) = 0.0
      ZZ(IG+34) = ZZ(IG+13)
      ZZ(IG+35) = ZZ(IG+14)
      ZZ(IG+36) = ZZ(IG+15)
      RETURN
   30 CONTINUE
C
C     NO TRANSFORMATIONS
C
      DO 40 I = 1,36
      ZZ(IG+I) = 0.0
   40 CONTINUE
      ZZ(IG+ 1) = 1.0
      ZZ(IG+ 8) = 1.0
      ZZ(IG+15) = 1.0
      ZZ(IG+22) = 1.0
      ZZ(IG+29) = 1.0
      ZZ(IG+36) = 1.0
      ZZ(IG+ 5) =  ZD
      ZZ(IG+ 6) = -YD
      ZZ(IG+10) = -ZD
      ZZ(IG+12) =  XD
      ZZ(IG+16) =  YD
      ZZ(IG+17) = -XD
      RETURN
      END
