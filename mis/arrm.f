      SUBROUTINE ARRM(P,D,ND)
C
C     SCALED ARITHMETIC ROUTINES--ARRANGING ROUTINE
C
      DOUBLE PRECISION DX,D,P,PX
      DIMENSION P(3),D(3),ND(3)
      DO 30 I=1,3
      IF(D(I) .EQ. 0.0D0) GO TO 30
   10 IF(DABS(D(I)) .GE. 1.0) GO TO 20
      D(I) = D(I)*10.0
      ND(I) = ND(I)-1
      GO TO 10
   20 IF(DABS(D(I)) .LT. 10.0) GO TO 30
      D(I) = D(I)*0.1
      ND(I) = ND(I)+1
      GO TO 20
   30 CONTINUE
      IF(ND(1) .GT. ND(2) .AND. ND(2) .GT. ND(3)) RETURN
      IF(ND(1) .GT. ND(2) .AND. ND(1) .GT. ND(3)) GO TO 110
      IF(ND(2) - ND(3)) 50,40,80
   40 IF(DABS(D(2)) .GE. DABS(D(3))) GO TO 80
   50 IF(ND(1) - ND(3)) 70,60,110
   60 IF(DABS(D(1)) .GE. DABS(D(3))) GO TO 110
   70 NX=ND(1)
      DX=D(1)
      PX=P(1)
      ND(1)=ND(3)
      D(1)=D(3)
      P(1)=P(3)
      ND(3)=NX
      D(3)=DX
      P(3)=PX
      GO TO 110
   80 IF(ND(1) - ND(2)) 100,90,110
   90 IF(DABS(D(1)) .GE. DABS(D(2))) GO TO 110
  100 NX=ND(1)
      DX=D(1)
      PX=P(1)
      ND(1)=ND(2)
      D(1)=D(2)
      P(1)=P(2)
      ND(2)=NX
      D(2)=DX
      P(2)=PX
  110 IF(ND(2) - ND(3)) 130,120,140
  120 IF(DABS(D(2)) .GE. DABS(D(3))) RETURN
  130 NX=ND(2)
      DX=D(2)
      PX=P(2)
      ND(2)=ND(3)
      D(2)=D(3)
      P(2)=P(3)
      ND(3)=NX
      D(3)=DX
      P(3)=PX
  140 RETURN
      END
