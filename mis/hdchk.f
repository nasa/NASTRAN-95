      SUBROUTINE HDCHK(XXX,CCC,NNO,II,XI,YI,NGX,ZM,ZMI,
     1           RV,RVI,TGM,TGI,ZI,LZ,XCC)
C
C
C
C     THIS SUBROUTINE SOLVES FOR THE POINTS OF INTERSECTION ON THE
C     LINES OF THE JTH ELEMENT WITH OTHER LINES AND PLANES(RELEVANT)
C
C
      DIMENSION CCC(1),XXX(1)
      DIMENSION RV(1),RVI(1),TGM(1),TGI(1),ZM(1),ZMI(1),
     1          NNO(1),NGX(1),XCC(1),XI(1),YI(1),ZI(1)
      COMMON/HEDG/JS,M,JT,VX,VX1,VX2,VX3,NN
      COMMON/GO3/L0,L1,L00,L01,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13
      JM=1
      EEX=.015
      EXP=.005
      NGX(1)=0
      IF(II.EQ.0)GO TO 190
      IF(NN .EQ. 1) GO TO 5
      IF(VX3 .NE. 0.)GO TO 5
      A=XXX(JT+2)
      B=XXX(JT+1)
      C=XXX(JT+4)
      Z1=XCC(JS)
      Z2=XCC(JS+1)
      IF(A.EQ.0.) GO TO 1
      Y1=-XCC(JS+3)*B-C
      Y2=-XCC(JS+4)*B-C
      X1=XCC(JS+3)
      X2=XCC(JS+4)
      GO TO 50
    1 CONTINUE
      Y1=XCC(JS+3)
      Y2=XCC(JS+4)
      X1=-C
      X2=X1
      GO TO 50
    5 CONTINUE
      A=XCC(JS)
      B=XCC(JS+1)
      C=XCC(JS+2)
      IF(A.EQ.0.)GO TO 20
      Y1=-XCC(JS+3)*XCC(JS+1)-XCC(JS+2)
      Y2=-XCC(JS+4)*XCC(JS+1)-XCC(JS+2)
      X1=XCC(JS+3)
      X2=XCC(JS+4)
      GO TO 30
   20 CONTINUE
      Y1=XCC(JS+3)
      Y2=XCC(JS+4)
      X1=-XCC(JS+2)
      X2=X1
   30 CONTINUE
      IF(NN.NE.1)GO TO 40
      Z1=XXX(1+JT)
      Z2=XXX(2+JT)
      GO TO 50
   40 CONTINUE
      Z1=-(VX+VX1*Y1+VX2*X1)/VX3
      Z2=-(VX+VX1*Y2+VX2*X2)/VX3
   50 CONTINUE
      AL=X2-X1
      BL=Y2-Y1
      CL=Z2-Z1
      EG=AMIN1(Z1,Z2)
      EGX=AMAX1(X1,X2)
      EGX1=AMIN1(X1,X2)
      EGY=AMAX1(Y1,Y2)
      EGY1=AMIN1(Y1,Y2)
C
C
C     THIS CODE DETERMINES THE POINTS OF INTERSECTIONS ON THE LINES OF
C     JTH ELEMENT RESULTING FROM THE INTERSECTION OF THE PLANES WITH
C     THESE LINES.
C
C
      DO 170 JR=1,II
      LG=NNO(L4+JR)
      NNO(L4+JR)=IABS(NNO(JR+L4))
      LE=NNO(L4+JR)
      JE=L13+LZ*(LE-1)
      JU=L12+5*(LE-1)
      NK=XXX(5+JU)
      JV=1
      AC=XXX(1+JU)
      BC=XXX(2+JU)
      CC=XXX(3+JU)
      D=XXX(4+JU)
      IF(EGX.LT.TGM(L5+LE))GO TO 170
      IF(EGX1.GT.TGI(L6+LE))GO TO 170
      IF(EGY.LT.RVI(L8+LE))GO TO 170
      IF(EGY1.GT.RV(L7+LE))GO TO 170
      IF(EG.GT.ZM(L2+LE))GO TO 170
      IF(LG.LT.0)GO TO 80
      IF((AL.EQ.0.).AND.(BL.EQ.0.))GO TO 80
      IF(AL.EQ.0.)GO TO 60
      XP=((BC*BL)/AL)*X1+(CC*CL/AL)*X1-D
      XP=XP-BC*Y1-CC*Z1
      VU=AC+(BC*BL/AL)+(CC*CL/AL)
      IF(VU.EQ.0.)GO TO 80
      XP=XP/VU
      T=(XP-X1)/AL
      YP=T*BL+Y1
      ZP=T*CL+Z1
      GO TO 70
   60 CONTINUE
      YP=(AC*AL/BL)*Y1+(CC*CL/BL)*Y1-D
      YP=YP-CC*Z1-AC*X1
      VU=BC+(AC*AL/BL)+(CC*CL/BL)
      IF(VU.EQ.0.)GO TO 80
      YP=YP/VU
      T=(YP-Y1)/BL
      XP=T*AL+X1
      ZP=T*CL+Z1
   70 CONTINUE
      S=ZP-ZM(L2+LE)
      S1=ZP-ZMI(L3+LE)
      IF((ABS(S).LT.EEX).OR.(ABS(S1).LT.EEX))GO TO 56
      IF(S*S1.GT.0.)GO TO 80
   56 CONTINUE
      S=XP-TGM(L5+LE)
      S1=XP-TGI(L6+LE)
      IF(S*S1.GT.0.)GO TO 80
      S=YP-RV(L7+LE)
      S1=YP-RVI(L8+LE)
      IF(S*S1.GT.0.)GO TO 80
      T=XP
      IF(A.EQ.0.)T=YP
      S=T-XCC(JS+3)
      S1=T-XCC(JS+4)
      IF(S*S1.GE.0.)GO TO 80
      M=M+1
C
C     STORES INTERSECTIONS.
C
      XI(M+1)=XP
      YI(M+1)=YP
      ZI(M+1)=ZP
   80 CONTINUE
C
C     THIS CODE DETERMINES INTERSECTION POINTS OF LINES WITH LINES.
C
      DO 160 JC=1,NK
      B1=CCC(JV+1+JE)
      A1=CCC(JV+JE)
      C1=CCC(JV+2+JE)
      T=A1*B-B1*A
      IF(T.EQ.0.)GO TO 160
      XO=(C1*A-C*A1)/T
      IF((ABS(B).LE.50.).AND.(A.NE.0.))GO TO 90
      YO=-C1-B1*XO
      GO TO 100
   90 CONTINUE
      YO=-C-B*XO
  100 CONTINUE
      T=XO
      IF(A.EQ.0.)T=YO
      S=T-XCC(JS+3)
      S1=T-XCC(JS+4)
      IF(S*S1.GE.0.)GO TO 160
      T=XO
      IF(A1.EQ.0.)T=YO
      S1=T-CCC(JV+4+JE)
      S=T-CCC(JV+3+JE)
      IF((ABS(S).LE.EEX).OR.(ABS(S1).LE.EEX))GO TO 110
      IF(S*S1.GT.0.)GO TO 160
  110 CONTINUE
      IF(CC.EQ.0.)GO TO 160
      ZX=-(AC*XO+BC*YO+D)/CC
      IF(NN.NE.1 .AND. VX3 .NE. 0.)GO TO 130
      TSZ=Z2-Z1
      TSX=X2-X1
      VT=XO-X1
      IF(TSX.NE.0.)GO TO 120
      VT=YO-Y1
      TSX=Y2-Y1
  120 CONTINUE
      ZX1=(TSZ/TSX)*VT+Z1
      GO TO 140
  130 CONTINUE
      ZX1=-(VX+VX1*YO+VX2*XO)/VX3
  140 CONTINUE
      IF(ABS(ZX-ZX1).LE.EXP)GO TO 150
      IF(ZX1.GT.ZX)GO TO 160
  150 CONTINUE
      M=M+1
C
C     STORES INTERSECTIONS.
C
      XI(M+1)=XO
      YI(M+1)=YO
      ZI(M+1)=ZX1
  160 JV=JV+5
  170 CONTINUE
      NGX(1)=M
  190 RETURN
      END
