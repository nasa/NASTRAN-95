      SUBROUTINE MBCTR (ICTR,IL1,IR1,NCN,NC1,NWN,NW1,PAREA)
C
C     CONTROL1 SURFACE
C
C     CONVEX ONLY:
C     MUST COMPILE WITH O1 OR LOWER OPTIMIZATION OPTION. IF O2 IS USED,
C     THE COMPILER WOULD GO INTO INFINITE LOOP.
C
      LOGICAL        CNTRL2,CNTRL1,CRANK1,CRANK2,ASYM
      DIMENSION      NCN(1),NC1(1),NWN(1),NW1(1),PAREA(50,50,3),
     1               X(5),Y(5),TANG(5),COTANG(5)
      COMMON /MBOXA/ XX(12),YY(12),TG(10),ANG(10),COTG(10)
      COMMON /MBOXC/ NJJ ,CRANK1,CRANK2,CNTRL1,CNTRL2,NBOX,NPTS0,NPTS1,
     1               NPTS2,ASYM,GC,CR,MACH,BETA,EK,EKBAR,EKM,BOXL,BOXW,
     2               BOXA ,NCB,NSB,NSBD,NTOTE,KC,KC1,KC2,KCT,KC1T,KC2T
C
      IF (ICTR .EQ. 2) GO TO 1000
      X(1) = XX( 7)
      X(2) = XX( 8)
      X(3) = XX( 9)
      X(4) = XX(11)
      Y(1) = YY( 7)
      Y(2) = YY( 8)
      Y(3) = YY( 9)
      Y(4) = YY(11)
      TANG(1) = TG(6)
      TANG(2) = TG(7)
      TANG(3) = TG(8)
      COTANG(1) = COTG(6)
      COTANG(2) = COTG(7)
      COTANG(3) = COTG(8)
      GO TO 2000
C
C     CONTROL2 SURFACE
C
 1000 X(1) = XX(11)
      X(2) = XX( 9)
      X(3) = XX(10)
      X(4) = XX(12)
      Y(1) = YY(11)
      Y(2) = YY( 9)
      Y(3) = YY(10)
      Y(4) = YY(12)
      TANG(1) = TG( 8)
      TANG(2) = TG(10)
      TANG(3) = TG( 9)
      COTANG(1) = COTG( 8)
      COTANG(2) = COTG(10)
      COTANG(3) = COTG( 9)
C
 2000 X(5) = XX(5)
      Y(5) = YY(5)
      TANG(4) = TG(4)
      TANG(5) = TG(5)
      COTANG(4) = COTG(4)
      COTANG(5) = COTG(5)
C
      IL1 = AMIN1(Y(2),Y(1))/BOXW + 1.5
      IR1 = AMAX1(Y(3),Y(4))/BOXW + 1.4999
      DO 6000 I = IL1,IR1
      YR  = (FLOAT(I)-0.5)*BOXW
      YL  = YR-BOXW
C
      XLL = (YL-Y(2))*TANG(1) + X(2)
      XRL = (YR-Y(2))*TANG(1) + X(2)
      XLH = (YL-Y(2))*TANG(2) + X(2)
      XRH = (YR-Y(2))*TANG(2) + X(2)
      XLR = (YL-Y(3))*TANG(3) + X(3)
      XRR = (YR-Y(3))*TANG(3) + X(3)
C
      IF (CRANK2 .AND. Y(5).LE.Y(1)) GO TO 4515
C
      XLT = (YL-Y(1))*TANG(4) + X(1)
      XRT = (YR-Y(1))*TANG(4) + X(1)
      GO TO 4520
C
 4515 XLT = (YL-Y(1))*TANG(5) + X(1)
      XRT = (YR-Y(1))*TANG(5) + X(1)
C
 4520 IF (YL.LE.Y(2) .AND. YR.GE.Y(2)) GO TO 4525
C
      IF (YR .LT. Y(2)) GO TO 4550
      JT = (XLH-AMOD(XLH,BOXL)+BOXL)/BOXL + 0.01
      GO TO 4600
C
 4525 JT = (X(2)-AMOD(X(2),BOXL)+BOXL)/BOXL + 0.01
      GO TO 4600
C
 4550 JT = (XRL-AMOD(XRL,BOXL)+BOXL)/BOXL + 0.01
C
 4600 IF (YL.LT.Y(4) .AND. YR.GE.Y(4).AND.XRT.GE.XLT) GO TO 4625
      IF (YL .GE. Y(4)) GO TO 4650
C
      JB = (AMAX1(XLT,XRT)-AMOD(AMAX1(XLT,XRT),BOXL)+BOXL)/BOXL + 0.01
      GO TO 4700
 4625 JB = (X(4)-AMOD(X(4),BOXL)+BOXL)/BOXL + 0.01
      GO TO 4700
C
 4650 JB = (XLR-AMOD(XLR,BOXL)+BOXL)/BOXL + 0.01
C
 4700 DO 5400 J = JT,JB
C
      XB = FLOAT(J)*BOXL
      XT = XB - BOXL
C
      YTL = (XT-X(2))*COTANG(1) + Y(2)
      YBL = (XB-X(2))*COTANG(1) + Y(2)
      YTH = (XT-X(2))*COTANG(2) + Y(2)
      YBH = (XB-X(2))*COTANG(2) + Y(2)
      YTR = (XT-X(3))*COTANG(3) + Y(3)
      YBR = (XB-X(3))*COTANG(3) + Y(3)
C
      IF (CRANK2 .AND. Y(5).LE.Y(1)) GO TO 4706
C
      YTT = (XT-X(1))*COTANG(4) + Y(1)
      YBT = (XB-X(1))*COTANG(4) + Y(1)
      GO TO 4708
C
 4706 YTT = (XT-X(1))*COTANG(5) + Y(1)
      YBT = (XB-X(1))*COTANG(5) + Y(1)
C
C     FULL BOXES
C
 4708 IF (YL.GE.YTL .AND. XT.GE.XRH .AND. YR.LT.YBR .AND.
     1    XB.LT.XRT .AND. XB.LT.XLT) GO TO 4900
C
C     DOUBLE CORNER BOXES
C
      IF (YL.LE.Y(2) .AND. YR.GE.Y(2) .AND. XT.LT.X(2) .AND.
     1    XB.GE.X(2) .AND. YL.LE.Y(1) .AND. YR.GE.Y(1) .AND.
     2    XT.LT.X(1) .AND. XB.GE.X(1)) GO TO 4820
C
      IF (YL.LT.Y(3) .AND. YR.GE.Y(3) .AND. XT.LT.X(3) .AND.
     1    XB.GE.X(3) .AND. YL.LT.Y(4) .AND. YR.GE.Y(4) .AND.
     2    XT.LT.X(4) .AND. XB.GE.X(4)) GO TO 4840
C
C     SINGLE CORNER BOXES
C
      IF (YL.LE.Y(2) .AND. YR.GE.Y(2) .AND. XT.LT.X(2) .AND.
     1    XB.GE.X(2)) GO TO 4710
      IF (YL.LE.Y(1) .AND. YR.GE.Y(1) .AND. XT.LT.X(1) .AND.
     1    XB.GE.X(1)) GO TO 4730
      IF (YL.LT.Y(3) .AND. YR.GE.Y(3) .AND. XT.LT.X(3) .AND.
     1    XB.GE.X(3)) GO TO 4750
      IF (YL.LT.Y(4) .AND. YR.GE.Y(4) .AND. XT.LT.X(4) .AND.
     1    XB.GE.X(4)) GO TO 4770
C
C     HINGE + T. E. BOXES
C
      IF (XT.LT.XRH .AND. (XB.GE.XLT .OR. XB.GE.XRT)) GO TO 4788
C
C     SIDE BOXES
C
      IF (XB.GE.XLH .AND. XT.LT.XRH .AND. YL.GE.YTL .AND. (XB.LT.X(3)
     1   .OR. YR.LT.Y(3))) GO TO 4745
      IF (YL.LE.YTL .AND. YR.GE.YBL .AND. XB.GE.X(2) .AND. XT.LT.X(1))
     1    GO TO 4740
      IF (YL.LT.YTR .AND. YR.GE.YBR .AND. XB.GE.X(3) .AND. XT.LT.X(4)
     1   .AND. YR.GE.Y(4)) GO TO 4765
      GO TO 4747
C
C     FWD LH CORNER
C
 4710 IF (YL.LE.YBL .AND. YR.GE.YBL .AND. XT.LT.XRH .AND. XB.GE.XRH)
     1   PA = .5*((Y(2)-YBL)*(XB-X(2))+(2.*XB-X(2)-XRH)*(YR-Y(2)))/BOXA
      IF (XT.LT.XLL .AND. XB.GE.XLL .AND. XT.LT.XRH .AND. XB.GE.XRH)
     1   PA = .5*((2.*XB-XLL-X(2))*(Y(2)-YL)+(2.*XB-XRH-X(2))*(YR-Y(2)))
     2        /BOXA
      IF (XT.LT.XLL .AND. XB.GE.XLL .AND. YL.LT.YBH .AND. YR.GE.YBH)
     1   PA = .5*((2.*XB-X(2)-XLL)*(Y(2)-YL)+(YBH-Y(2))*(XB-X(2)))/BOXA
      IF (YL.LE.YBL .AND. YR.GE.YBL .AND. YL.LT.YBH .AND. YR.GE.YBH)
     1   PA = 0.5*(XB-X(2))*(YBH-YBL)/BOXA
      IF (I-1) 4799,5000,4799
C
 4720 IF (YL.LE.YTL .AND. YR.GE.YTL .AND. YL.LT.YTH .AND. YR.GE.YTH .AND
     1.   XT.LT.XLL .AND. XB.GE.XLL .AND. XT.LT.XRH .AND. XB.GE.XRH)
     2    PA = 1.0 - 0.5*((XLL-XT)*(YTL-YL)+(YR-YTH)*(XRH-XT))/BOXA
      IF (YL.LE.YBL .AND. YR.GE.YBL .AND. YL.LE.YTL .AND. YR.GE.YTL .AND
     1.   YL.LT.YTH .AND. YR.GE.YTH .AND. XT.LT.XRH .AND. XB.GE.XRH)
     2    PA = 1.0 - 0.5*((YTL+YBL-2.0*YL)*BOXL+(YR-YTH)*(XRH-XT))/BOXA
      IF (YL.LT.YBH .AND. YR.GE.YBH .AND. XT.LT.XLL .AND. XB.GE.XLL .AND
     1.   YL.LE.YTL .AND. YR.GE.YTL .AND. YL.LT.YTH .AND. YR.GE.YTH)
     2    PA = 1.0 - 0.5*((YTL-YL)*(XLL-XT)+(2.0*YR-YTH-YBH)*BOXL)/BOXA
      IF (YL.LE.YTL .AND. YR.GE.YTL .AND. YL.LT.YTH .AND. YR.GE.YTH .AND
     1.   YL.LE.YBL .AND. YR.GE.YBL .AND. YL.LT.YBH .AND. YR.GE.YBH)
     2    PA = 0.5*(YTH+YBH-YTL-YBL)/BOXW
      IF (YL.LE.YTL .AND. YR.GE.YTL .AND. YL.LE.YBL .AND. YR.GE.YBL .AND
     1.   YL.LT.YBT .AND. YR.GE.YBT .AND. XT.LT.XRT .AND. XB.GE.XRT)
     2    PA = 0.5*((2.0*YR-YTL-YBL)*BOXL-(YR-YBT)*(XB-XRT))/BOXA
      IF (YL.LT.YTL .AND. YR.GE.YTL .AND. XT.LT.XLL .AND. XB.GE.XLL .AND
     1.   XT.LT.XLT .AND. XB.GE.XLT .AND. YL.LT.YBT .AND. YR.GE.YBT)
     2    PA = 1.0 - 0.5*((YTL-YL)*(XLL-XT)+(XB-XLT)*(YBT-YL))/BOXA
      IF (XT.LT.XLL .AND. XB.GE.XLL .AND. XT.LT.XRL .AND. XB.GE.XRL .AND
     1.   YL.LT.YBT .AND. YR.GE.YBT .AND. XT.LT.XLT .AND. XB.GE.XLT)
     2    PA = 0.5*((2.0*XB-XLL-XRL)*BOXW-(XB-XLT)*(YBT-YL))/BOXA
      IF (YL.LT.YTL .AND. YR.GE.YTL .AND. XT.LT.XLL .AND. XB.GE.XLL .AND
     1.   XT.LT.XLT .AND. XB.GE.XLT .AND. XT.LT.XRT .AND. XB.GE.XRT)
     2    PA = 0.5*((XLT+XRT-2.0*XB)*BOXW-(YTL-YL)*(XLL-XT))/BOXA
      IF (XT.LT.XLL .AND. XB.GE.XLL .AND. XT.LT.XLT .AND. XB.GE.XLT .AND
     1.   XT.LT.XRL .AND. XB.GE.XRL .AND. XT.LT.XRT .AND. XB.GE.XRT)
     2    PA = 0.5*(XLT+XRT-XLL-XRL)/BOXL
      IF (I-1) 4799,5000,4799
C
C     AFT LH CORNER
C
 4730 IF (X(1) .GE. X(4)) GO TO 4735
      IF (YL.LT.YBT .AND. YR.GE.YBT .AND. YL.LE.YTL .AND. YR.GE.YTL)
     1   PA = .5*((2.*YR-YTL-Y(1))*(X(1)-XT)+(2.*YR-Y(1)-YBT)*(XB-X(1)))
     2        /BOXA
 4735 IF (YL.LE.YTL .AND. YR.GE.YTL .AND. XT.LT.XRT .AND. XB.GE.XRT)
     1   PA = .5*((Y(1)-YTL)*(X(1)-XT)+(X(1)+XRT-2.*XT)*(YR-Y(1)))/BOXA
      IF (YL.LT.YTT .AND. YR.GE.YTT .AND. YL.LE.YTL .AND. YR.GE.YTL .AND
     1.   YTT.GE.YTL) PA = 0.5*(YTT-YTL)*(X(1)-XT)/BOXA
      IF (XT.LT.XRL .AND. XB.GE.XRL .AND. XT.LT.XRT .AND. XB.GE.XRT)
     1   PA = 0.5*(XRT-XRL)*(YR-Y(1))/BOXA
      IF (YL.LT.YBT .AND. YR.GE.YBT .AND. XT.LT.XRL .AND. XB.GE.XRL)
     1   PA = .5*((X(1)-XRL)*(YR-Y(1))+(2.*YR-Y(1)-YBT)*(XB-X(1)))/BOXA
      IF (I-1) 4799,5000,4799
C
C     LH EDGE
C
 4740 IF (I .EQ. 1) GO TO 4800
C
      IF (XT.LT.XLL .AND. XB.GE.XLL .AND. XT.LT.XRL .AND. XB.GE.XRL)
     1   PA = 0.5*(2.0*XB-XLL-XRL)/BOXL
      IF (YL.LE.YBL .AND. YR.GE.YBL .AND. XT.LT.XRL .AND. XB.GE.XRL)
     1   PA = 0.5*(YR-YBL)*(XB-XRL)/BOXA
      IF (YL.LE.YTL .AND. YR.GE.YTL .AND. YL.LE.YBL .AND. YR.GE.YBL)
     1   PA = 0.5*(2.0*YR-YTL-YBL)/BOXW
      IF (YL.LE.YTL .AND. YR.GE.YTL .AND. XT.LT.XLL .AND. XB.GE.XLL)
     1   PA = 1.0 - 0.5*(XLL-XT)*(YTL-YL)/BOXA
      GO TO 4720
C
C     HINGE LINE
C
 4745 IF (YL.LT.YTH .AND. YR.GE.YTH .AND. XT.LT.XRH .AND. XB.GE.XRH)
     1   PA = 1.0 - 0.5*(YR-YTH)*(XRH-XT)/BOXA
      IF (XT.LT.XLH .AND. XB.GE.XLH .AND. XT.LT.XRH .AND. XB.GE.XRH)
     1   PA = 0.5*(2.0*XB-XLH-XRH)/BOXL
      IF (YL.LT.YBH .AND. YR.GE.YBH .AND. XT.LT.XLH .AND. XB.GE.XLH)
     1   PA = 0.5*(XB-XLH)*(YBH-YL)/BOXA
      IF (YL.LT.YTH .AND. YR.GE.YTH .AND. YL.LT.YBH .AND. YR.GE.YBH)
     1   PA = 0.5*(YTH+YBH-2.0*YL)/BOXW
      GO TO 4760
C
C     TRAILING EDGE
C
 4747 IF (YL.LT.YTT .AND. YR.GE.YTT .AND. XT.LT.XRT .AND. XB.GE.XRT)
     1   PA = 0.5*(YR-YTT)*(XRT-XT)/BOXA
      IF (XT.LT.XLT .AND. XB.GE.XLT .AND. XT.LT.XRT .AND. XB.GE.XRT)
     1   PA = 0.5*(XLT+XRT-2.0*XT)/BOXL
      IF (XT.LT.XLT .AND. XB.GE.XLT .AND. YL.LT.YBT .AND. YR.GE.YBT)
     1   PA = 1.0 - 0.5*(XB-XLT)*(YBT-YL)/BOXA
      IF (YL.LT.YTT .AND. YR.GE.YTT .AND. YL.LT.YBT .AND. YR.GE.YBT)
     1   PA = 0.5*(2.0*YR-YTT-YBT)/BOXW
      IF (YL.LT.YBT .AND. YR.GE.YBT .AND. XT.LT.XRT .AND. XB.GE.XRT)
     1   PA = 1.0 - 0.5*(YR-YBT)*(XB-XRT)/BOXA
      IF (XT.LT.XLT .AND. XB.GE.XLT .AND. YL.LT.YTT .AND. YR.GE.YTT)
     1   PA = 0.5*(XLT-XT)*(YTT-YL)/BOXA
      GO TO 4799
C
C     FWD RH CORNER
C
 4750 IF (YR.GE.YBR .AND. YL.LT.YBR .AND. XT.LT.XLH .AND. XB.GE.XLH)
     1   PA = 0.5*((X(3)-XLH)*(Y(3)-YL)+(Y(3)+YBR-2.0*YL)*(XB-X(3)))
     2        /BOXA
      IF (XT.LT.XLH .AND. XB.GE.XLH .AND. XT.LT.XLR .AND. XB.GE.XLR)
     1   PA = 0.5*(XLR-XLH)*(Y(3)-YL)/BOXA
      IF (YR.GE.YTH .AND. YL.LT.YTH .AND. XT.LT.XLR .AND. XB.GE.XLR)
     1   PA = .5*((YTH+Y(3)-2.*YL)*(X(3)-XT)+(Y(3)-YL)*(XLR-X(3)))/BOXA
      IF (YR.GE.YTH .AND. YL.LT.YTH .AND. YR.GE.YBR .AND. YL.LT.YBR)
     1   PA = 0.5*((YTH+Y(3)-2.0*YL)*(X(3)-XT)+(Y(3)+YBR-2.0*YL)*
     2        (XB-X(3)))/BOXA
      GO TO 4799
C
 4760 IF (YR.GE.YBR .AND. YL.LT.YBR .AND. XT.LT.XRR .AND. XB.GE.XRR .AND
     1.   XT.LT.XRH .AND. XB.GE.XRH .AND. YL.LT.YTH .AND. YR.GE.YTH)
     2    PA = 1.0 - 0.5*((YR-YTH)*(XRH-XT)+(XB-XRR)*(YR-YBR))/BOXA
      IF (YR.GE.YBR .AND. YL.LT.YBR .AND. XT.LT.XRR .AND. XB.GE.XRR .AND
     1.   XT.LT.XRH .AND. XB.GE.XRH .AND. XT.LT.XLH .AND. XB.GE.XLH)
     2    PA = 0.5*((2.0*XB-XLH-XRH)*BOXW-(YR-YBR)*(XB-XRR))/BOXA
      IF (YL.LT.YTH .AND. YR.GE.YTH .AND. XT.LT.XRH .AND. XB.GE.XRH .AND
     1.   XT.LT.XRR .AND. XB.GE.XRR .AND. XT.LT.XLR .AND. XB.GE.XLR)
     2    PA = 0.5*((XLR+XRR-2.0*XT)*BOXW-(YR-YTH)*(XRH-XT))/BOXA
      IF (XT.LT.XLH .AND. XB.GE.XLH .AND. XT.LT.XLR .AND. XB.GE.XLR .AND
     1.   XT.LT.XRH .AND. XB.GE.XRH .AND. XT.LT.XRR .AND. XB.GE.XRR)
     2    PA = 0.5*(XLR+XRR-XLH-XRH)/BOXL
      GO TO 4799
C
C     RH EDGE
C
 4765 IF (XT.LT.XRR .AND. XB.GE.XRR .AND. XT.LT.XLR .AND. XB.GE.XLR)
     1   PA = 0.5*(XLR+XRR-2.0*XT)/BOXL
      IF (YR.GE.YTR .AND. YL.LT.YTR .AND. XT.LT.XLR .AND. XB.GE.XLR)
     1   PA = 0.5*(YTR-YL)*(XLR-XT)/BOXA
      IF (YR.GE.YTR .AND. YL.LT.YTR .AND. YR.GE.YBR .AND. YL.LT.YBR)
     1   PA = 0.5*(YTR+YBR-2.0*YL)/BOXW
      IF (YR.GE.YBR .AND. YL.LT.YBR .AND. XT.LT.XRR .AND. XB.GE.XRR)
     1   PA = 1.0 - 0.5*(XB-XRR)*(YR-YBR)/BOXA
      GO TO 4780
C
C     AFT RH CORNER
C
 4770 IF (X(4) .GE. X(1)) GO TO 4775
      IF (YR.GE.YTR .AND. YL.LT.YTR .AND. YL.LT.YBT .AND. YR.GE.YBT)
     1   PA = 0.5*((YTR+Y(4)-2.0*YL)*(X(4)-XT)+(Y(4)+YBT-2.0*YL)*
     2   (XB-X(4)))/BOXA
 4775 IF (YR.GE.YTR .AND. YL.LT.YTR .AND. XT.LT.XLT .AND. XB.GE.XLT)
     1   PA = .5*((XLT-X(4))*(Y(4)-YL)+(YTR+Y(4)-2.*YL)*(X(4)-XT))/BOXA
      IF (YL.LT.YTT .AND. YR.GE.YTT .AND. YR.GE.YTR .AND. YL.LT.YTR)
     1   PA = 0.5*(YTR-YTT)*(X(4)-XT)/BOXA
      IF (XT.LT.XLT .AND. XB.GE.XLT .AND. XT.LT.XRR .AND. XB.GE.XRR)
     1   PA = 0.5*((XLT+X(4)-2.0*XT)*(Y(4)-YL)+(X(4)+XRR-2.0*XT)*
     2   (YR-Y(4)))/BOXA
      IF (YL.LT.YTT .AND. YR.GE.YTT .AND. XT.LT.XRR .AND. XB.GE.XRR)
     1   PA = .5*((Y(4)-YTT)*(X(4)-XT)+(X(4)+XRR-2.*XT)*(YR-Y(4)))/BOXA
      GO TO 4799
C
 4780 IF (XT.LT.XLT .AND. XB.GE.XLT .AND. YR.GE.YBR .AND. YL.LT.YBR .AND
     1.   YL.LT.YBT .AND. YR.GE.YBT .AND. XT.LT.XRR .AND. XB.GE.XRR)
     2    PA = 1.0 - 0.5*((XB-XLT)*(YBT-YL)+(YR-YBR)*(XB-XRR))/BOXA
      IF (YL.LT.YTT .AND. YR.GE.YTT .AND. YL.LT.YBT .AND. YR.GE.YBT .AND
     1.   YR.GE.YBR .AND. YL.LT.YBR .AND. XT.LT.XRR .AND. XB.GE.XRR)
     2    PA = 0.5*((2.0*YR-YTT-YBT)*BOXL-(YR-YBR)*(XB-XRR))/BOXA
      IF (YR.GE.YTR .AND. YL.LT.YTR .AND. YR.GE.YBR .AND. YL.LT.YBR .AND
     1.   XT.LT.XLT .AND. XB.GE.XLT .AND. YL.LT.YBT .AND. YR.GE.YBT)
     2    PA = 0.5*((YTR+YBR-2.0*YL)*BOXL-(XB-XLT)*(YBT-YL))/BOXA
      IF (YL.LT.YTT .AND. YR.GE.YTT .AND. YR.GE.YTR .AND. YL.LT.YTR .AND
     1.   YL.LT.YBT .AND. YR.GE.YBT .AND. YR.GE.YBR .AND. YL.LT.YBR)
     2    PA = 0.5*(YTR-YTT+YBR-YBT)/BOXW
      GO TO 4799
C
 4788 IF (XB.GE.XLT .AND. YR.GE.YBT .AND. YL.LT.YTH)
     1    PA = 1.0 - 0.5*((YR-YTH)*(XRH-XT)+(XB-XLT)*(YBT-YL))/BOXA
      IF (XB.GE.XRT .AND. YL.LT.YBT .AND. YL.LT.YTH)
     1    PA = 1.0 - 0.5*((YR-YTH)*(XRH-XT)+(XB-XRT)*(YR-YBT))/BOXA
      IF (XT.LT.XLH .AND. XB.GE.XLT .AND. XB.GE.XRT)
     1    PA = 0.5*(XRT-XRH+XLT-XLH)/BOXL
      IF (XT.LT.XLH .AND. YL.LT.YBT .AND. XB.GE.XRT)
     1    PA = 1.0 - 0.5*((XLH+XRH-2.0*XT)*BOXW+(XB-XRT)*(YR-YBT))/BOXA
      IF (YL.LT.YTH .AND. XB.GE.XLT .AND. XB.GE.XRT)
     1    PA = 1.0 - 0.5*((2.0*XB-XLT-XRT)*BOXW+(XRH-XT)*(YR-YTH))/BOXA
      IF (XT.LT.XLH .AND. YR.GE.YBT .AND. XB.GE.XLT)
     1    PA = 1.0 - 0.5*((XLH+XRH-2.0*XT)*BOXW+(XB-XLT)*(YBT-YL))/BOXA
C
 4799 PAREA(J,I,2) = PA
      PAREA(J,I,1) = PAREA(J,I,1) - PA
      GO TO 5400
C
 4800 YL1  = 0.0
      XLL1 = (YL1-Y(2))*TANG(1) + X(2)
C
      IF (XT.LT.XLL1 .AND. XB.GE.XLL1 .AND. XT.LT.XRL .AND. XB.GE.XRL)
     1   PA = 0.5*(2.0*XB-XLL1-XRL)*(YR-YL1)/BOXA
      IF (YL1.LE.YBL .AND. YR.GE.YBL .AND. XT.LT.XRL .AND. XB.GE.XRL)
     1   PA = 0.5*(XB-XRL)*(YR-YBL)/BOXA
      IF (YL1.LE.YTL .AND. YR.GE.YTL .AND. XT.LT.XLL .AND. XB.GE.XLL)
     1   PA = 1.0 - 0.5*(YTL-YL1)*(XLL1-XT)/BOXA
      IF (YL1.LE.YTL .AND. YR.GE.YTL .AND. YL1.LE.YBL .AND. YR.GE.YBL)
     1   PA = 0.5*(2.0*YR-YTL-YBL)/BOXW
      GO TO 4720
C
C     LH CORNERS
C
 4820 IF (YL.LE.YTH .AND. YR.GE.YTH .AND. YL.LE.YBT .AND. YR.GE.YBT)
     1    PA = 0.5*((2.0*YR-YTH-Y(2))*(X(2)-XT)+(2.0*YR-Y(2)-Y(1))*
     2         (X(1)-X(2))+(2.0*YR-Y(1)-YBT)*(XB-X(1)))/BOXA
      IF (XT.LT.XRH .AND. XB.GE.XRH .AND. YL.LE.YBT .AND. YR.GE.YBT)
     1    PA = 0.5*((X(2)-XRH)*(YR-Y(2))+(2.0*YR-Y(2)-Y(1))*(X(1)-X(2))
     2         +(2.0*YR-Y(1)-YBT)*(XB-X(1)))/BOXA
      IF (YL.LE.YTH .AND. YR.GE.YTH .AND. XT.LT.XRT .AND. XB.GE.XRT)
     1    PA = 0.5*((2.0*YR-YTH-Y(2))*(X(2)-XT)+(2.0*YR-Y(2)-Y(1))*
     2         (X(1)-X(2))+(XRT-X(1))*(YR-Y(1)))/BOXA
      IF (XT.LT.XRH .AND. XB.GE.XRH .AND. XT.LT.XRT .AND. XB.GE.XRT)
     1    PA = 0.5*((X(2)-XRH)*(YR-Y(2))+(2.0*YR-Y(2)-Y(1))*(X(1)-X(2))
     2         +(XRT-X(1))*(YR-Y(1)))/BOXA
      IF (I-1) 4799,5000,4799
C
C     RH CORNERS
C
 4840 IF (YL.LT.YTH .AND. YR.GE.YTH .AND. YL.LT.YBT .AND. YR.GE.YBT)
     1   PA = 0.5*((YTH+Y(3)-2.0*YL)*(X(3)-XT)+(Y(3)+Y(4)-2.0*YL)*(X(4)
     2        -X(3))+(Y(4)+YBT-2.0*YL)*(XB-X(4)))/BOXA
      IF (XT.LT.XLH .AND. XB.GE.XLH .AND. YL.LT.YBT .AND. YR.GE.YBT)
     1   PA = 0.5*((X(3)-XLH)*(Y(3)-YL)+(Y(3)+Y(4)-2.0*YL)*(X(4)-X(3))
     2        +(Y(4)+YBT-2.0*YL)*(XB-X(4)))/BOXA
      IF (YL.LT.YTH .AND. YR.GE.YTH .AND. XT.LT.XLT .AND. XB.GE.XLT)
     1   PA = 0.5*((YTH+Y(3)-2.0*YL)*(X(3)-XT)+(Y(3)+Y(4)-2.0*YL)*(X(4)
     2        -X(3))+(XLT-X(4))*(Y(4)-YL))/BOXA
      IF (XT.LT.XLH .AND. XB.GE.XLH .AND. XT.LT.XLT .AND. XB.GE.XLT)
     1   PA = 0.5*((X(3)-XLH)*(Y(3)-YL)+(Y(3)+Y(4)-2.0*YL)*(X(4)-X(3))
     2        +(XLT-X(4))*(Y(4)-YL))/BOXA
      GO TO 4799
C
 4900 PAREA(J,I,2) = 1.0
      PAREA(J,I,1) = 0.0
      GO TO 5400
C
 5000 PAREA(J,I,2) = 2.0*PA
      PAREA(J,I,1) = PAREA(J,I,1) - PAREA(J,I,2)
C
 5400 CONTINUE
C
      YC = YR - BOXW/2.0
      XF = (YL-Y(2))*TANG(2) + X(2)
C
      IF (YC .LT. Y(2)) GO TO 5600
      IF (YC .LT. Y(3)) GO TO 5800
      IF (YC .GE. Y(4)) GO TO 6000
      XF2 = (YR-Y(3))*TANG(3) + X(3)
      NC1(I) = XF2/BOXL + 1.0
      IF (YC .LT. Y(1)) GO TO 5900
 5500 NCN(I) =  NWN(I)
      GO TO 6000
 5600 IF (YC .LT. Y(1)) GO TO 6000
      XF1 = (YR-Y(2))*TANG(1) + X(2)
      NC1(I) = XF1/BOXL + 1.0
 5700 NC1(I) = MAX0(NC1(I),NW1(I))
      IF (YC .LT. Y(4)) GO TO 5500
      XF2 = (YR-Y(3))*TANG(3) + X(3)
      NCN(I) = XF2/BOXL + 1.0
      GO TO 6000
 5800 NC1(I) = XF/BOXL + 1.0
      IF (YC .GE. Y(1)) GO TO 5700
 5900 XF1 = (YR-Y(2))*TANG(1) + X(2)
      NCN(I) = XF1/BOXL + 1.0
 6000 CONTINUE
C
      RETURN
      END
