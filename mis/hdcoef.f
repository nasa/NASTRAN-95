      SUBROUTINE HDCOEF(X,Y,Z,XXX,JXX,NS,CCC,LZ)
C
C
C     THIS SUBROUTINE DETERMINES EQUATION OF LINES AND PLANES.
C
C
      INTEGER   ZCOEF1,ZCOEF,IBCOEF(5)
      DIMENSION CCC(1),XXX(1),X(1),Y(1),Z(1),COE(8)
      COMMON/ZZZZZZ/RZ(1)
      COMMON/HDPTRS/XDUM,XCC,XASOLV,YASOLV,ZASOLV,X1SKT,Y1SKT,Z1SKT,
     1              ZCOEF1,ZCOEF
      COMMON/GO3/L0,L1,L00,L01,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13
      DATA EPSI / 1.0E-5 /
      LE=0
      JA=L13+(JXX-1)*LZ
      JF=L12+(JXX-1)*5
      I=0
      J=1
   10 CONTINUE
C
C
C     SEARCH FOR MATCHING COORDINATES.
C
C
      I=I+1
      T=X(I+1)-X(I)
      S=Y(I+1)-Y(I)
      U=Z(I+1)-Z(I)
      IF (ABS(T) .GT. EPSI)  GO TO 20
      IF (ABS(S) .GT. EPSI)  GO TO 20
      IF (ABS(U) .GT. EPSI)  GO TO 20
C
C
C     MATCH FOUND.....PROCEED IF LIST IS NOT EXHAUSTED.
C
C
      I=I+2
   20 CONTINUE
      IF(I.GT.NS)GO TO 70
C
C
C     DETERMINE EQUATION OF LINE-SEGMENTS.
C
C
      T=X(I+1)-X(I)
      T1=Y(I+1)-Y(I)
      IF ((ABS(T1) .LT. EPSI) .AND. (ABS(T) .LT. EPSI))  GO TO 10
      IF (ABS(T) .GT. EPSI)  GO TO 30
   29 CONTINUE
      CCC(J+JA)=0
      CCC(J+1+JA)=1
      CCC(J+2+JA)=-X(I)
      GO TO 40
   30 CONTINUE
      CCC(J+JA)=1
      E=(Y(I+1)-Y(I))/(X(I+1)-X(I))
      IF(ABS(E).GT.100000.)GO TO 29
      F=(E*X(I))-Y(I)
      CCC(J+1+JA)=-E
      CCC(J+2+JA)=F
   40 CONTINUE
      IF (ABS(CCC(J+JA)) .GT. EPSI)  GO TO 50
      CCC(J+3+JA)=Y(I)
      CCC(J+4+JA)=Y(I+1)
      GO TO 60
   50 CONTINUE
      CCC(J+3+JA)=X(I)
      CCC(J+4+JA)=X(I+1)
   60 CONTINUE
      J=J+5
      RZ(ZCOEF1+LE)=Z(I)
      RZ(ZCOEF+LE)=Z(I+1)
      LE=LE+1
      IF(LE.GT.3)GO TO 10
      IBCOEF(LE)=I
      GO TO 10
   70 CONTINUE
C
C
C     DETERMINE EQUATION OF PLANE.
C
      J=(J-1)/5
      XXX(JF+5)=J
      IF(NS.LE.3)GO TO 120
      K1=1
      K2=2
      K3=3
      A1=X(K3)-X(K1)
      B1=Y(K3)-Y(K1)
      C1=Z(K3)-Z(K1)
      A2=X(K2)-X(K1)
      B2=Y(K2)-Y(K1)
      C2=Z(K2)-Z(K1)
      COE(1)=B1*C2-B2*C1
      COE(2)=C1*A2-C2*A1
      COE(3)=A1*B2-A2*B1
      COE(4)=COE(1)*X(1)+COE(2)*Y(1)+COE(3)*Z(1)
      COE(4)=-COE(4)
      DO 110 J=1,4
  110 XXX(JF+J)=COE(J)
      IF (ABS(COE(3)) .GT. EPSI)  GO TO 140
      J=1
      DO 25 K=1,LE
      JAPJ=JA+J
      CCC(JAPJ)=RZ(ZCOEF1-1+K)
      CCC(JAPJ+1)=RZ(ZCOEF-1+K)
      J=J+5
   25 CONTINUE
      IF (ABS(COE(1)) .GT. EPSI)  I=1
      IF (ABS(COE(2)) .GT. EPSI)  I=2
      P=COE(I)
      IF (ABS(P) .LT. EPSI)  P = EPSI
      DO 26 K=1,4
      JFPK=JF+K
   26 XXX(JFPK)=XXX(JFPK)/P
      GO TO 140
  120 CONTINUE
      XXX(JF+5)=1
      DO 130 IX=1,2
  130 XXX(JF+IX)=Z(IX)
      XXX(JF+3)=0
  140 CONTINUE
      RETURN
      END
