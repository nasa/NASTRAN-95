      SUBROUTINE TLODT2 (TS1,TS2)
C
C    CALCULATION OF PTGEN2 - GEN THERMAL LOAD VECTOR DUE TO TRANSVERSE S
C
      DIMENSION       TS1(60),TS2(20),TS3(20)
      REAL            J11,J12,J22
      COMMON /EMGEST/ EST(100)
      COMMON /SSGWRK/ X,Y,Z,DISTA,DISTB,DISTC,A1,A2,A3,B1,B2,B3,G1(3),
     1                D(3)
      COMMON /MATOUT/ EM(6),DUM6(9),RJ11,RJ12,RJ22
      DIMENSION       BE(7),GA(7),WT(7),CONS(2)
      DATA       BE / 0.33333333333333, 0.47014206   , 0.05971588  ,
     1                0.47014206      , 0.101286505  , 0.79742699  ,
     2                0.101286505     /
      DATA       GA / 0.33333333333333, 2*.47014206  , 0.05971588  ,
     1                2*0.101286505   , 0.79742699   /
      DATA       WT / 0.1125          , 3*0.066197075, 3*0.06296959/
C
      CONS(1)=DISTA*DISTC
      CONS(2)=DISTB*DISTC
      DO 104 I=1,60
      TS1(I)=0.0
  104 CONTINUE
      DO 106 I=1,20
      TS3(I)=0.0
  106 CONTINUE
      DO 150 K=1,7
      DO 145 KASE=1,2
      IF (KASE.EQ.1) X= BE(K)*DISTA
      IF (KASE.EQ.2) X=-BE(K)*DISTB
      Y=GA(K)*DISTC
      CONS1=WT(K)*CONS(KASE)
      THK=A1+A2*X+A3*Y
      TEMP=D(1)+D(2)*X+D(3)*Y
      THK1=THK**3/12.0
      D11=EM(1)*THK1
      D12=EM(2)*THK1
      D13=EM(3)*THK1
      D22=EM(4)*THK1
      D23=EM(5)*THK1
      D33=EM(6)*THK1
      D21=D12
      D31=D13
      D32=D23
      J11=1.0/(EM(6)*THK)
      J22=J11
      J12=0.0
      A11=-(J11*D11+J12*D13)
      A12=-(J11*D12+J12*D23)
      A13=-(J11*D13+J12*D33)
      A14=-(J11*D31+J12*D21)
      A15=-(J11*D32+J12*D22)
      A16=-(J11*D33+J12*D23)
      A21=-(J12*D11+J22*D13)
      A22=-(J12*D12+J22*D23)
      A23=-(J12*D13+J22*D33)
      A24=-(J12*D13+J22*D12)
      A25=-(J12*D23+J22*D22)
      A26=-(J12*D33+J22*D32)
      A31= A14+2.0*A13
      A32= A12+2.0*A16
      A33= A24+2.0*A23
      A34= A22+2.0*A26
      A35= A33+A11
      A36= A34+A31
      A37= A25+A32
      TS1(31)  =-24.0*A11
      TS1(33)  =-24.0*A21
      TS1(34)  =-6.0*A31
      TS1(35)  =-6.0*A21
      TS1(36)  =-6.0*A35
      TS1(37)  =-4.0*A32
      TS1(38)  =-4.0*A33
      TS1(39)  =-4.0*A36
      TS1(40)  =-6.0*A15
      TS1(41)  =-6.0*A34
      TS1(42)  =-6.0*A37
      TS1(44)  =-24.0*A25
      TS1(45)  =-24.0*A15
      TS1(46)  =-120.0*A11*X
      TS1(48)  =-120.0*A21*X
      TS1(49)  =-12.0*(A32*X+A31*Y)
      TS1(50)  =-12.0*(A33*X+A21*Y)
      TS1(51)  =-12.0*(A36*X+A35*Y)
      TS1(52)  =-12.0*(A15*X+A32*Y)
      TS1(53)  =-12.0*(A34*X+A33*Y)
      TS1(54)  =-12.0*(A37*X+A36*Y)
      TS1(55)  =-24.0*A15*Y
      TS1(56)  =-24.0*(A25*X+A34*Y)
      TS1(57)  =-24.0*(A15*X+A37*Y)
      TS1(59)  =-120.0*A25*Y
      TS1(60)  =-120.0*A15*Y
C
C
      CALL GMMATS (TS1,20,3,0,G 1,3,1,0,TS2)
      DO 112 I=1,20
      TS2(I)=TS2(I)*TEMP*THK1*CONS1
      TS3(I)=TS3(I)+TS2(I)
  112 CONTINUE
  145 CONTINUE
  150 CONTINUE
      DO 160 I=1,20
      TS2(I)=TS3(I)
  160 CONTINUE
      RETURN
      END
