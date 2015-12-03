      SUBROUTINE HDSTUS (OJ,TMJ,XXX,TGM,RV,RVI,TGI,ZM,NNO,II,H,IM,JXT,
     1                   ZJ,NC,ZMI,CCC,LZ)
C
C     THIS SUBROUTINE DETERMINES THE VISIBILITY OF AN ARBITRARY POINT
C     BY DRAWING A LINE FROM THE POINT IN QUESTION TO INFINITY AND
C     COUNTING THE NUMBER OF TIMES IT CROSSES THE BOUNDARIES OF A
C     RELEVANT ELEMENT.
C
      DIMENSION    CCC(1),XXX(1),ZMI(1),TGM(1),RV(1),RVI(1),TGI(1),
     1             ZM(1),NNO(1),H(8)
      COMMON /GO3/ L0,L1,L00,L01,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13
C
      GGK = .015
      EI  = 0
      IM  = 0
   10 CONTINUE
      IF (EI .GE. 1.) GO TO 70
      EI  = EI + .2
      D   = EI*OJ - TMJ
      DO 60 JO = 1,II
      GGK = .015
      I   = 0
      JG  = NNO(L4+JO)
      JS  = L13 + (JG-1)*LZ
      JT  = L12 + (JG-1)*5
C
C     PRELIMINARY CHECK TO SEE IF THE POINT IS OUTSIDE THE BOUNDARY
C     BOXES IN THE X,Y,Z DIMENSIONS.
C
      IF (TMJ.GE.RV(L7+JG) .OR. TMJ.LE.RVI(L8+JG)) GO TO 60
      IF (OJ.GE.TGI(L6+JG) .OR. OJ.LE.TGM(L5+JG) ) GO TO 60
      IF (ZJ .GE. ZM(L2+JG)) GO TO 60
      VX  = XXX(4+JT)
      VX1 = XXX(2+JT)*TMJ
      VX2 = XXX(1+JT)*OJ
      ZS  =-(VX+VX1+VX2)/XXX(3+JT)
      IF (ABS(ZJ-ZS) .LT. GGK) GO TO 60
      IF (ZJ .GE. ZS) GO TO 60
      NS  = XXX(5+JT)
      IB  = NS*5
      IF (H(8) .EQ. 1.) GO TO 25
      DO 20 J = 1,IB,5
      GGK = .015
      NSUB= J + 1 + JS
      IF (ABS(CCC(NSUB)) .GE. 100.) GGK = ALOG10(ABS(CCC(NSUB)))
      VE  = OJ
      IF (CCC(J+JS) .EQ. 0.) VE = TMJ
      S   = VE - CCC(J+3+JS)
      S1  = VE - CCC(J+4+JS)
      YG  = TMJ
      IF (CCC(J+JS) .NE. 0.) GO TO 15
      DY  =-CCC(J+2+JS)/CCC(J+1+JS)
      YG  = OJ
      GO TO 16
   15 CONTINUE
      DY  =-CCC(J+2+JS) - CCC(J+1+JS)*OJ
   16 CONTINUE
      IF (ABS(YG-DY).LT.GGK .AND. S*S1.LE.0.) GO TO 60
   20 CONTINUE
   25 CONTINUE
C
C     THE FOLLOWING CODE COUNTS THE INTERSECTIONS OF BOUNDARIES
C     OF A GIVEN ELEMENT WITH THE INFINITE LINE AND CHECKS,IF INSIDE
C     OF THE BOUNDARY, WHETHER OR NOT THE POINT IS BEHIND OR IN FRONT
C     OF THE ELEMENT.
C
      DO 40 J = 1,IB,5
      T  =-CCC(J+2+JS)  + CCC(J+JS)*D
      R  = EI*CCC(J+JS) + CCC(J+1+JS)
      IF (R .EQ. 0.) GO TO 40
      T  = T/R
      IF (T .LT. OJ) GO TO 40
      IF (CCC(J+JS) .NE. 0.) GO TO 30
      T  = EI*T - D
   30 CONTINUE
      S  = T - CCC(J+3+JS)
      S1 = T - CCC(J+4+JS)
      IF (S.EQ.0. .OR. S1.EQ.0.) GO TO 10
      IF (S*S1 .GE. 0.) GO TO 40
      I  = I + 1
   40 CONTINUE
      IF (MOD(I,2) .EQ. 0) GO TO 60
      IM = 1
      GO TO 70
   60 CONTINUE
      IM = 0
   70 CONTINUE
      RETURN
      END
