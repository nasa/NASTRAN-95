      SUBROUTINE MBREG (IREG,NW1,NWN,NC21,NC2N,NC1,NCN,ND1,NDN,XK,YK,
     1                  XK1,YK1,XK2,YK2,XWTE,YWTE,KTE,KTE1,KTE2,PAREA)
C
C     SUBROUTINE TO COMPUTE LIMITS OF REGION AND PERCENTAGE OF BOX IN
C     EACH
C
      LOGICAL         CNTRL2,CNTRL1,CRANK1,CRANK2,ASYM,DEBUG
      DIMENSION       NW1(1),NWN(1),NC21(1),NC2N(1),NC1(1),NCN(1),
     1                ND1(1),NDN(1),XK(1),YK(1),XK1(1),YK1(1),XK2(1),
     2                YK2(1),XWTE(1),YWTE(1),KTE(1),KTE1(1),KTE2(1)
      DIMENSION PAREA(50,50,3)
      COMMON /SYSTEM/ SYS,N6
      COMMON /MBOXA / X(12),Y(12),TANG(10),ANG(10),COTANG(10)
      COMMON /MBOXC / NJJ ,CRANK1,CRANK2,CNTRL1,CNTRL2,NBOX,NPTS0,NPTS1,
     1                NPTS2,ASYM,GC,CR,MACH,BETA,EK,EKBAR,EKM,BOXL,BOXW,
     2                BOXA ,NCB,NSB,NSBD,NTOTE,KC,KC1,KC2,KCT,KC1T,KC2T
      DATA    DEBUG / .FALSE./
C
      IPRINT = 0
      IF (DEBUG) IPRINT = 1
C
      IREG = 1
      BOXA = BOXL*BOXW
      KPT  = 0
      KC1T = 0
      KC2T = 0
      DO 20 I = 1,50
      NW1(I)  = 0
      NWN(I)  = 0
      NC1(I)  = 0
      NCN(I)  = 0
      NC21(I) = 0
      NC2N(I) = 0
      ND1(I)  = 0
      NDN(I)  = 0
      KTE(I)  = 0
      KTE1(I) = 0
      KTE2(I) = 0
      XWTE(I) = 0.0
      YWTE(I) = 0.0
      DO 10 J = 1,50
      DO 10 KP = 1,3
      PAREA(I,J,KP) = 0.
   10 CONTINUE
   20 CONTINUE
      DO 30 I = 1,200
      XK(I) = 0.0
   30 YK(I) = 0.0
      DO 40 I = 1,125
      XK1(I) = 0.0
      YK1(I) = 0.0
      XK2(I) = 0.0
   40 YK2(I) = 0.0
C
C     LEADING EDGE OF MAIN
C
      XRE = 0.0
      YBE = 0.0
      K   = 1
      YR  = -0.5*BOXW
      DO 220 I = 1,NSB
      YL  = YR
      YR  = (FLOAT(I)-0.5)*BOXW
      XLE = XRE
      IF (YR .GT. Y(K+1)) GO TO 50
      XRE = (YR-Y(K))*TANG(K) + X(K)
      GO TO 60
   50 XRE = (YR-Y(K+1))*TANG(K+1) + X(K+1)
      KPT = 1
   60 XT  = XLE - AMOD(XLE,BOXL)
      XB  = XT + BOXL
      J1  = XB/BOXL + 0.01
C
C
      DO 170 J = J1,NCB
      IF (XRE .GT. XB) GO TO 100
      IF (XLE .GT. XT) GO TO 90
C
      IF (XT .LE. X(K+1)) GO TO 70
      YTE  = (XT-X(K+1))*COTANG(K+1) + Y(K+1)
      GO TO 80
   70 YTE  = (XT-X(K))*COTANG(K) + Y(K)
      IF (KPT .EQ. 1) KPT =  2
   80 A = 0.5*(YR-YTE)*(XRE-XT)
      IF (KPT .EQ. 2) A = A + (XT*(Y(K+1)-YR) - YTE*(X(K+1)-XRE) +
     1                    X(K+1)*YR - XRE*Y(K+1))/2.0
      PA = 1.0 - A/BOXA
      GO TO 180
C
   90 A  = 0.5*(XLE+XRE-2.0*XT)*(YR-YL)
      IF (KPT .GT. 0) A = A + (XLE*(Y(K+1)-YR) - YL*(X(K+1)-XRE) +
     1                    X(K+1)*YR - XRE*Y(K+1))/2.0
      PA = 1.0 - A/BOXA
      GO TO 180
C
  100 IF (XLE .GT. XT) GO TO 130
C
      YTE = YBE
      IF (XB .GT. X(K+1)) GO TO 110
      YBE = (XB-X(K))*COTANG(K) + Y(K)
      GO TO 120
  110 YBE = (XB-X(K+1))*COTANG(K+1) + Y(K+1)
      IF (KPT .EQ. 1) KPT  =  2
  120 A = 0.5*(YTE+YBE-2.0*YL)*BOXL
      IF (KPT .EQ. 2) A = A + (XT*(YBE-Y(K+1)) - YTE*(XB-X(K+1)) +
     1                    XB*Y(K+1) - YBE*X(K+1))/2.0
      PA = A/BOXA
      GO TO 160
C
  130 IF (XB .GT. X(K+1)) GO TO 140
      YBE = (XB-X(K))*COTANG(K) + Y(K)
      GO TO 150
  140 YBE = (XB-X(K+1))*COTANG(K+1) + Y(K+1)
      IF (KPT .EQ. 1) KPT =  2
  150 A   = 0.5*(XB-XLE)*(YBE-YL)
      IF (KPT .EQ. 2) A = A + (XLE*(YBE-Y(K+1)) - YL*(XB-X(K+1)) +
     1                    XB*Y(K+1) - YBE*X(K+1))/2.0
      PA  = A/BOXA
  160 XT  = XB
      XB  = FLOAT(J+1)*BOXL
      IF (KPT .EQ. 2) KPT = 3
      IF (I .EQ. 1) PA = 2.0*PA - 1.0
      PAREA(J,I,1) = PA
  170 CONTINUE
      GO TO 190
C
  180 IF (I .EQ. 1) PA = 2.0*PA - 1.0
      PAREA(J,I,1) = PA
  190 YC  = YR - 0.5*BOXW
      IF (KPT .LE. 0) GO TO 200
      IF (YC .LE. Y(K+1)) GO TO 200
      XC  = (YC-Y(K+1))*TANG(K+1) + X(K+1)
      GO TO 210
  200 XC  =  (YC-Y(K))*TANG(K) + X(K)
  210 NW1(I) = XLE/BOXL + 1.0001
      IF (KPT .GT. 0) K = K + 1
      KPT = 0
  220 CONTINUE
      IF (IPRINT .LE. 0) GO TO 250
      WRITE  (N6,240)
      WRITE  (N6,230) (NW1(I),I=1,NSB)
  230 FORMAT (10I12)
  240 FORMAT (4H NW1)
  250 CONTINUE
C
C     TRAILING EDGE OF MAIN
C
      XRE = X(4)
      K   = 4
      YR  = 0.0
      DO 460 I = 1,NSB
      YL  = YR
      YR  = (FLOAT(I)-0.5)*BOXW
      XLE = XRE
      IF (YR .GT. Y(K+1)) GO TO 260
      XRE = (YR-Y(K))*TANG(K) + X(K)
      GO TO 270
  260 XRE = (YR-Y(K+1))*TANG(K+1) + X(K+1)
      KPT = 1
  270 XT  = XLE - AMOD(XLE,BOXL)
      XB  = XT + BOXL
      J   = XB/BOXL + 0.01
      IF (J .GT. 50) GO TO 410
      IPT = 0
      IF (XRE.GT.XB .OR. XRE.LT.XT) GO TO 280
      A   = 0.5*(XLE+XRE-2.0*XT)*(YR-YL)
      IF (KPT .GT. 0) A = A + (XLE*(Y(K+1)-YR) - YL*(X(K+1)-XRE) +
     1                    X(K+1)*YR - Y(K+1)*XRE)/2.0
      IF (I .EQ. 1) A = 2.0*A
      GO TO 430
C
  280 IPT = 1
      IF (XLE .LT. XRE) GO TO 340
      IPT = -1
  290 IF (XRE .LT. XT) GO TO 300
C
      A  =  0.5*(XB-XRE)*(YR-YTE)
      IF (KPT.GT.0 .AND. KPT.LT.3) A = A - (XRE*(YTE - Y(K+1)) -
     1                   YR*(XB-X(K+1)) + XB*Y(K+1) - YTE*X(K+1))/2.0
      IF (I .EQ. 1) A = 2.0*A
      GO TO 420
C
  300 YBE = YTE
      IF (XT .LT. X(K+1)) GO TO 310
      YTE = (XT-X(K))*COTANG(K) + Y(K)
      GO TO 320
  310 YTE = (XT-X(K+1))*COTANG(K+1) + Y(K+1)
      IF (KPT .EQ. 1) KPT =  2
  320 IF (XLE .GT. XB) GO TO 330
C
      A = 0.5*(XLE-XT)*(YTE-YL)
      IF (KPT .EQ. 2) A = A + (XT*(YL-Y(K+1)) - YTE*(XLE-X(K+1)) +
     1                    XLE*Y(K+1) - YL*X(K+1))/2.0
      GO TO 390
C
  330 A = 0.5*BOXL*(YTE+YBE - 2.0*YL)
      IF (KPT .EQ. 2) A = A + (XT*(YBE-Y(K+1)) - YTE*(XB- X(K+1)) +
     1                    XB*Y(K+1) - YBE*X(K+1))/2.0
      GO TO 390
C
  340 IF (XRE .GT. XB) GO TO 350
C
      A = 0.5*(YR-YBE)*(XRE-XT)
      IF (KPT.GT.0 .AND. KPT.LT.3) A = A + (XT*(Y(K+1)-YR) -
     1                   YBE*(X(K+1)-XRE ) + X(K+1)*YR - Y(K+1)*XRE)/2.0
      IF (I .EQ. 1) A = 2.0*A
      GO TO 430
C
  350 YTE = YBE
      IF (XB .GT. X(K+1)) GO TO 360
      YBE = (XB-X(K))*COTANG(K) + Y(K)
      GO TO 370
  360 YBE = (XB-X(K+1))*COTANG(K+1) + Y(K+1)
      IF (KPT .EQ.  1) KPT = 2
  370 IF (XLE .LT. XT) GO TO 380
C
      A  = 0.5*(XB-XLE)*(YBE-YL)
      IF (KPT .EQ. 2) A = A - (XLE*(Y(K+1)-YBE) - YL* (X(K+1)-XB) +
     1                    X(K+1)*YBE - Y(K+1)*XB)/2.0
      IF (I .EQ. 1) A = 2.0*A
      A  =  BOXA - A
      GO TO 400
C
  380 A  =  0.5*BOXL*(2.0*YR - YTE - YBE)
      IF (KPT .EQ. 2) A = A + (XT*(Y(K+1)-YBE) - YTE*(X(K+1)-XB) +
     1                    X(K+1)*YBE - Y(K+1)*XB)/2.0
C
  390 IF (I .EQ. 1) A = 2.0*A
  400 PA = A/BOXA
      A  = 1.0
      IF (PAREA(J,I,1) .GT. 0.0) A = PAREA(J,I,1)
      PAREA(J,I,1) = PA*A
      J  = J + IPT
      IF (J .GT. 50) GO TO 410
      XB = FLOAT(J)*BOXL
      XT = XB - BOXL
      IF (KPT .EQ. 2) KPT = 3
      IF (IPT .GT. 0) GO TO 340
      GO TO 290
  410 IREG = 2
      RETURN
C
  420 A  = BOXA - A
  430 YC = YR - 0.5*BOXW
      IF (KPT .LE. 0) GO TO 440
      IF (YC .LE. Y(K+1)) GO TO 440
      XC = (YC-Y(K+1))*TANG(K+1) + X(K+1)
      GO TO 450
  440 XC = (YC-Y(K))*TANG(K) + X(K)
  450 NWN(I) = AMAX1(XLE,XRE)/BOXL + 0.9999
      PA = A/BOXA
      A  = 1.0
      IF (PAREA(J,I,1) .GT. 0.0) A = PAREA(J,I,1)
      PAREA(J,I,1) = PA*A
      XWTE(I) = XC
      YWTE(I) = YC
      IF (KPT .GT. 0) K = K + 1
      KPT =  0
  460 CONTINUE
      IF (IPRINT .LE. 0) GO TO 480
      WRITE  (N6,470)
      WRITE  (N6,230) (NWN(I),I=1,NSB)
  470 FORMAT (4H NWN)
  480 CONTINUE
      NTOTE = NSB
C
C     FILL IN MAIN PERCENTAGES
C
      DO 520 I = 1,NSB
      N1 = NW1(I)
      NN = NWN(I)
      DO 490 J = N1,NN
      IF (PAREA(J,I,1) .LE. 0.0) PAREA(J,I,1) = 1.0
  490 CONTINUE
C
C     DIAPHRAGM INDEX
C
      IF (I .NE. 1) GO TO 500
      ND1(1) = NW1(1)
      NDN(1) = NWN(1)
      GO TO 520
  500 ND1(I) = MIN0(NW1(I),ND1(I-1)+1)
      NDN(I) = MAX0(NWN(I),NDN(I-1)-1)
      IF (NDN(I) .LE. NDN(I-1)+1) GO TO 520
      DO 510 K = 2,I
      KK = I - K + 1
      IF (NDN(KK) .GE. NDN(KK+1)-1) GO TO 520
      NDN(KK) = MAX0(NDN(KK),NDN(KK+1)-1)
  510 CONTINUE
  520 CONTINUE
      J  =  NSB + 1
  530 IF (ND1(J-1) .GE. NDN(J-1)-1) GO TO 540
      ND1(J) = ND1(J-1) + 1
      NDN(J) = NDN(J-1) - 1
      J  = J + 1
      IF (J .LE. 50) GO TO 530
      IREG =  2
      RETURN
C
  540 NSBD =  J - 1
      IF (IPRINT .LE. 0) GO TO 580
      WRITE  (N6,550)
      WRITE  (N6,230) (ND1(I),I=1,NSBD)
  550 FORMAT (4H ND1)
      WRITE  (N6,560)
      WRITE  (N6,230) (NDN(I),I=1,NSBD)
  560 FORMAT (4H NDN)
C
      WRITE  (N6,610)
      DO 570 I = 1,NCB
      WRITE (N6,640) I
      WRITE (N6,600) (PAREA(I,J,1),J=1,NSB)
  570 CONTINUE
  580 IF (CNTRL1) CALL MBCTR (1,IL1,IR1,NCN,NC1,NWN,NW1,PAREA)
      IF (CNTRL2) CALL MBCTR (2,IL2,IR2,NC2N,NC21,NWN,NW1,PAREA)
      IF (IPRINT .EQ. 0) GO TO 650
      DO 590 KXYZ = 1,3
      IF (KXYZ .EQ. 1) WRITE (N6,610)
      IF (KXYZ .EQ. 2) WRITE (N6,620)
      IF (KXYZ .EQ.3 ) WRITE (N6,630)
      DO 590 I = 1,NCB
      WRITE (N6,640) I
      WRITE (N6,600) (PAREA(I,J,KXYZ),J=1,NSB)
  590 CONTINUE
  600 FORMAT (5X,10F9.5)
  610 FORMAT (12H PAREA, MAIN)
  620 FORMAT (14H PAREA, CNTRL1)
  630 FORMAT (14H PAREA, CNTRL2)
  640 FORMAT (4H ROW,I4)
C
C     MAIN BOX CTR. COORDINATES
C
  650 KC = 0
      DO 660 I = 1,NCB
      IXR = I - 1
      DO 660 J = 1,NSB
      IF (.NOT.(I.GE.(ND1(J)) .AND. I.LE.(NDN(J)))) GO TO 660
      IF (PAREA(I,J,1) .LT. 0.005) GO TO 660
      JXR = J  - 1
      KC  = KC + 1
      IF (KC .GE. 200) GO TO 830
      XK(KC) = BOXL*(FLOAT(IXR)+0.5)
      YK(KC) = BOXW*FLOAT(JXR)
  660 CONTINUE
      DO 670 J = 1,NSB
      KC = KC + 1
      IF (KC .GE. 200) GO TO 830
      KTE(J) = KC
      XK(KC) = XWTE(J)
      YK(KC) = YWTE(J)
  670 CONTINUE
      KCT = KC
      IF (IPRINT .LE. 0) GO TO 700
      WRITE  (N6,680) (I,XK(I),I=1,KC)
  680 FORMAT (1H1,23H MAIN BOX CTR. X COORD.,/(10(1X,I4,F8.2)))
      WRITE  (N6,690) (I,YK(I),I=1,KC)
  690 FORMAT (1H1,23H MAIN BOX CTR. Y COORD.,/(10(1X,I4,F8.2)))
  700 CONTINUE
C
C     CNTRL1 BOX CTR. COORDINATES
C
      IF (.NOT.CNTRL1) GO TO 750
      KC1 = 0
      DO 710 I = 1,NCB
      IXR = I - 1
      DO 710 J = IL1,IR1
      IF (PAREA(I,J,2) .LT. 0.005) GO TO 710
      JXR = J - 1
      KC1 = KC1 + 1
      IF (KC1 .GE. 125) GO TO 830
      XK1(KC1) = BOXL*(FLOAT(IXR)+0.5)
      YK1(KC1) = BOXW*FLOAT(JXR)
  710 CONTINUE
      DO 720 J = IL1,IR1
      KC1 = KC1 + 1
      IF (KC1 .GE. 125) GO TO 830
      KTE1(J)  = KC1
      XK1(KC1) = XWTE(J)
      YK1(KC1) = YWTE(J)
  720 CONTINUE
      KC1T = KC1
      IF (IPRINT .LE. 0) GO TO 750
      WRITE  (N6,730) (I,XK1(I),I=1,KC1)
  730 FORMAT (1H1,25H CNTRL1 BOX CTR. X COORD.,/(10(1X,I4,F8.2)))
      WRITE  (N6,740) (I,YK1(I),I=1,KC1)
  740 FORMAT (1H1,25H CNTRL1 BOX CTR. Y COORD.,/(10(1X,I4,F8.2)))
C
C     CNTRL2 BOX CTR. COORDINATES
C
  750 IF (.NOT.CNTRL2) GO TO 800
      KC2 = 0
      DO 760 I = 1,NCB
      IXR = I - 1
      DO 760 J = IL2,IR2
      IF (PAREA(I,J,3) .LT. 0.005) GO TO 760
      JXR = J - 1
      KC2 = KC2 + 1
      IF (KC2 .GE. 125) GO TO 830
      XK2(KC2) = BOXL*(FLOAT(IXR)+0.5)
      YK2(KC2) = BOXW*FLOAT(JXR)
  760 CONTINUE
      DO 770 J = IL2,IR2
      KC2 = KC2 + 1
      IF (KC2 .GE. 125) GO TO 830
      KTE2(J)  = KC2
      XK2(KC2) = XWTE(J)
      YK2(KC2) = YWTE(J)
  770 CONTINUE
      KC2T = KC2
      IF (IPRINT .LE. 0) GO TO 800
      WRITE  (N6,780) (I,XK2(I),I=1,KC2)
  780 FORMAT (1H1,25H CNTRL2 BOX CTR. X COORD.,/(10(1X,I4,F8.2)))
      WRITE  (N6,790) (I,YK2(I),I=1,KC2)
  790 FORMAT (1H1,25H CNTRL2 BOX CTR. Y COORD.,/(10(1X,I4,F8.2)))
  800 CONTINUE
      BOXL = BOXL/CR
      BOXW = BOXW/CR
      BOXA = BOXA/CR**2
      DO 810 I = 1,12
      X(I) = X(I)/CR
  810 Y(I) = Y(I)/CR
      DO 820 I = 1,50
      XWTE(I) = XWTE(I)/CR
  820 YWTE(I) = YWTE(I)/CR
      GO TO 840
  830 IREG = 2
  840 RETURN
      END
