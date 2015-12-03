      SUBROUTINE TSPL2S (TS7)
C
C    TRANSVERSE SHEAR ROUTINE2 FOR CTRPLT1 - SINGLE PRECISION VERSION
C
      DIMENSION TS7(60)
      COMMON /SMA1IO/ X,Y
      DO 105 I=1,60
      TS7(I)=0.0
  105 CONTINUE
      X2=X*X
      XY=X*Y
      Y2=Y*Y
      X3=X2*X
      X2Y=X2*Y
      XY2=X*Y2
      Y3=Y2*Y
      TS7(   4)=2.0
      TS7(   7)=6.0*X
      TS7(   8)=2.0*Y
      TS7(  11)=12.0*X2
      TS7(  12)=6.0*XY
      TS7(  13)=2.0*Y2
      TS7(  16)=20.0*X3
      TS7(  17)=6.0*XY2
      TS7(  18)=2.0*Y3
      TS7(  26)=2.0
      TS7(  29)=2.0*X
      TS7(  30)=6.0*Y
      TS7(  33)=2.0*X2
      TS7(  34)=TS7(12)
      TS7(  35)=12.0*Y2
      TS7(  37)=2.0*X3
      TS7(  38)=6.0*X2Y
      TS7(  39)=12.0*XY2
      TS7(  40)=20.0*Y3
      TS7(  45)=2.0
      TS7(  48)=4.0*X
      TS7(  49)=4.0*Y
      TS7(  52)=6.0*X2
      TS7(  53)=8.0*XY
      TS7(  54)=6.0*Y2
      TS7(  57)=12.0*X2Y
      TS7(  58)=TS7(39)
      TS7(  59)=8.0*Y3
      RETURN
      END
