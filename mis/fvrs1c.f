      SUBROUTINE FVRS1C (Z,W1,OMEGA,NF)
C----------------------------------------------------------------------
      COMPLEX Z(3,NF),P,Z1,PY1A,PY1B,PZ1A,PZ1B,PY2A,PY2B,PZ2A,PZ2B
C
      DIMENSION W1(NF)
C
      COMMON /CONDAS/ PI,TWOPI,RADEG,DEGRA,S4PIQ
      COMMON /BLANK / DUM(5),IXT,IXP,IYT,IYP,IZT,IZP,DUM1(3)
C----------------------------------------------------------------------
      LL=1
      DO 400 KKK=1,NF
      IF(W1(KKK).EQ.0.0)GO TO 30
      A=1.0
      IF(W1(KKK)-OMEGA.LT.0.0)A=-1.0
      B=1.0
      IF(W1(KKK)+OMEGA.LT.0.0)B=-1.0
C
C     COMPUTE BASE(FI)(3X3)  IF W.NE.0--MODFRL=TRUE
C
C     ZERO OUT MATRIX
C
      KK=LL+2
      DO 1 I=1,3
      DO 1 J=LL,KK
  1   Z(I,J)=(0.0,0.0)
      F=W1(KKK)/TWOPI
      IF(IXT.EQ.-1) GO TO 9
      CALL TAB(IXT,F,XO)
      IF(IXP.EQ.-1)GO TO 4
      CALL TAB(IXP,F,PHI)
      RAD=PHI*DEGRA
      Z1=CMPLX(0.0,RAD)
      P=CEXP(Z1)
      GO TO 5
 4    P=(1.0,0.0)
 5    Z(1,LL+1)=XO*P
 9    IF(IYT.EQ.-1)GO TO 11
      CALL TAB(IYT,F,YO)
      GO TO 12
 11   YO=0.0
 12   IF(IZT.EQ.-1)GO TO 13
      CALL TAB(IZT,F,ZO)
      GO TO 14
 13   ZO=0.0
 14   IF(IYP.EQ.-1)GO TO 15
      CALL TAB(IYP,F,PHI)
      GO TO 16
 15   PHI=0.0
 16   RAD=PHI*DEGRA
      Z1=CMPLX(0.0,RAD)
      PY1A=CEXP(A*Z1)
      PY1B=CEXP(B*Z1)
      Z1=CMPLX(0.0,RAD-0.5*PI*A)
      PY2A=CEXP(A*Z1)
      Z1=CMPLX(0.0,RAD-0.5*PI*B)
      PY2B=CEXP(B*Z1)
      IF(IZP.EQ.-1)GO TO 17
      CALL TAB(IZP,F,PHI)
      GO TO 18
 17   PHI=0.0
 18   RAD=PHI*DEGRA
      Z1=CMPLX(0.0,RAD)
      PZ1A=CEXP(A*Z1)
      PZ1B=CEXP(B*Z1)
      Z1=CMPLX(0.0,RAD-0.5*PI*A)
      PZ2A=CEXP(A*Z1)
      Z1=CMPLX(0.0,RAD-0.5*PI*B)
      PZ2B=CEXP(B*Z1)
      Z(2,LL)=(YO*PY1A-A*ZO*PZ2A)*0.5
      Z(3,LL)=(A*YO*PY2A+ZO*PZ1A)*0.5
      Z(2,LL+2)=(YO*PY1B+B*ZO*PZ2B)*0.5
      Z(3,LL+2)=(-B*YO*PY2B+ZO*PZ1B)*0.5
      LL=LL+3
      GO TO 400
 30   CONTINUE
C
C     COMPUTE BASE(FI)(3X2) IF W1=0.0, FOR MODFRL=TRUE
C
      A=1.0
      IF(OMEGA.LT.0.0)A=-1.0
C------ZERO OUT MATRIX(3X2)
      KK=LL+1
      DO 32 I=1,3
      DO 32 J=LL,KK
 32   Z(I,J)=(0.0,0.0)
      F=W1(KKK)/TWOPI
      IF(IXT.EQ.-1)GO TO 90
      CALL TAB(IXT,F,XO)
      IF(IXP.EQ.-1)GO TO 40
      CALL TAB(IXP,F,PHI)
      RAD=PHI*DEGRA
      Z1=CMPLX(0.0,RAD)
      P=CEXP(Z1)
      GO TO 50
 40   P=(1.0,0.0)
 50   Z(1,LL)=XO*P
      GO TO 100
 90   Z(1,LL)=(0.0,0.0)
 100  IF(IYT.EQ.-1)GO TO 190
      CALL TAB(IYT,F,YO)
      IF(IYP.EQ.-1) GO TO 140
      CALL TAB(IYP,F,PHI)
      RAD=PHI*DEGRA
      CY=COS(RAD)
      GO TO 150
 140  CY=1.0
 150  YY=YO*CY
      GO TO 200
 190  YY=0.0
 200  IF(IZT.EQ.-1)GO TO 290
      CALL TAB(IZT,F,ZO)
      IF(IZP.EQ.-1) GO TO 240
      CALL TAB(IZP,F,PHI)
      RAD=PHI*DEGRA
      CZ=COS(RAD)
      GO TO 250
 240  CZ=1.0
 250  ZZ=ZO*CZ
      GO TO 300
 290  ZZ=0.0
 300  Z(2,KK)=YY-A*CMPLX(0.0,ZZ)
      Z(3,KK)=ZZ+A*CMPLX(0.0,YY)
      LL=LL+2
 400  CONTINUE
      RETURN
      END
