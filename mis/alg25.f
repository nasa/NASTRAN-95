      SUBROUTINE ALG25 (IX,LX,LOG1,X,Y1)
C
      REAL        LINE
      DIMENSION   X(1),Y1(1),SYMBOL(1),LINE(121),XNUM(13)
      DATA SYMBOL/1H*/, DASH/1H-/, CROSS/1H+/, BLANK/1H /, XI/1HI/
C
      YMIN = Y1(1)
      YMAX = YMIN
      DO 200 I = 1,IX
      IF (Y1(I) .LT. YMIN) YMIN = Y1(I)
      IF (Y1(I) .GT. YMAX) YMAX = Y1(I)
200   CONTINUE
      IF (YMIN .EQ. YMAX) GO TO 900
      YH   = YMAX + (YMAX-YMIN)/18.0
      YL   = YMIN - (YMAX-YMIN)/18.0
      XH   = 60.0
      IF (LX .GT. 59) XH = FLOAT(LX) + 1.0
      XL   = XH - 60.0
      XMAX = ABS(XH)
      XMIN = ABS(XL)
      YMIN = ABS(YL)
      YMAX = ABS(YH)
      IF (XMIN .GT. XMAX) XMAX = XMIN
      IF (YMIN .GT. YMAX) YMAX = YMIN
      XMAX = ALOG10(XMAX)
      YMAX = ALOG10(YMAX)
      IF (XMAX .LT. 0.0) XMAX = XMAX - 1.0
      IF (YMAX .LT. 0.0) YMAX = YMAX - 1.0
      MX   = -XMAX
      MY   = -YMAX
      WRITE (LOG1,250) MX,MY
250   FORMAT (20X,46HSCALES - 'X' IS SHOWN TIMES 10 TO THE POWER OF,I3,
     1        40H   'Y' IS SHOWN TIMES 10 TO THE POWER OF,I3,/)
      YINC  = (YH-YL)/54.0
      YINC2 = YINC/2.0
      XRANGE= XH - XL
      DO 750 KLINE = 1,55
      IF (KLINE.EQ.1 .OR. KLINE.EQ.55) GO TO 350
      DO 265 L = 2,120
265   LINE(L) = BLANK
      IF (KLINE.EQ. 7 .OR. KLINE.EQ.13 .OR. KLINE.EQ.19 .OR.
     1    KLINE.EQ.25 .OR. KLINE.EQ.31 .OR. KLINE.EQ.37 .OR.
     2    KLINE.EQ.43 .OR. KLINE.EQ.49) GO TO 300
      LINE(  1) = XI
      LINE(121) = XI
      GO TO 400
300   LINE(  1) = DASH
      LINE(121) = DASH
      GO TO 400
350   DO 360 L = 2,120
360   LINE(L) = DASH
      LINE(1) = CROSS
      LINE(121) = CROSS
      DO 365 L = 11,111,10
365   LINE(L) = XI
      GO TO 650
400   DO 600 I = 1,IX
      IF (Y1(I).GT.YH+YINC2 .OR. Y1(I).LE.YH-YINC2) GO TO 600
      L = (X(I)-XL)/XRANGE*120.0 + 1.5
      LINE(L) = SYMBOL( 1)
600   CONTINUE
      IF (KLINE.EQ. 1 .OR. KLINE.EQ. 7 .OR. KLINE.EQ.13 .OR.
     1    KLINE.EQ.19 .OR. KLINE.EQ.25 .OR. KLINE.EQ.31 .OR.
     2    KLINE.EQ.37 .OR. KLINE.EQ.43 .OR. KLINE.EQ.49 .OR.
     3    KLINE.EQ.55) GO TO 650
      WRITE  (LOG1,610) LINE
610   FORMAT (8X,121A1)
      GO TO 750
650   YNUM = YH*10.0**MY
      WRITE  (LOG1,655) YNUM,LINE
655   FORMAT (1X,F6.3,1X,121A1)
750   YH   = YH - YINC
      XNUM(1) = XL*10.0**MX
      XINC = ((XH-XL)/12.0)*10.0**MX
      DO 800 I = 2,13
800   XNUM(I) = XNUM(I-1) + XINC
      WRITE  (LOG1,820) XNUM
820   FORMAT (6X,12(F6.3,4X),F6.3)
      RETURN
C
900   WRITE  (LOG1,910)
910   FORMAT (//35X,54HNO PLOT HAS BEEN MADE BECAUSE 'X' OR 'Y' RANGE IS
     1 ZERO)
      RETURN
      END
