      SUBROUTINE INVTR (*,X,DX)
C
C     INVTR WILL INVERT A LOWER OR UPPER TRIANGULAR MATRIX
C
C
C     FILEA    =  MATRIX CONTROL BLOCK FOR THE INPUT FILE A
C     FILEB    =  MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX B
C     SCRFIL   =  SCRATCH FILE (NEEDED ONLY FOR AN UPPER TRIANGLE)
C     NX       =  NUMBER OF CELLS OF CORE AVAILABLE AT X
C     PREC     =  DESIRED PRECISION OF ARITHMETIC OPERATIONS
C     X        =  BLOCK OF AVAILABLE CORE
C     DX       =  SAME BLOCK AS X, BUT TYPED DOUBLE PRECISION
C
      INTEGER         RD,RDREW,WRT,WRTREW,REW,NOREW,EOFNRW,RC,TRA2,
     1                CORE,EOL,OUTBUF,TYPEA,TYPEB,PREC,TYPEAR,
     2                FORMA,SYSBUF,BAKSKP,FORSKP,CMPLX,TRA,TRA1,SCRFIL,
     3                FILEA,FILEB,T
      DOUBLE PRECISION DX(1),DA(2),DTEMP
      DIMENSION       X(1),NAME(2),T(7)
      COMMON /TYPE  / PRC(2),NWDS(4),RC(10)
      COMMON /SYSTEM/ SYSBUF
C     COMMON /DESCRP/ LENGTH,MAJOR(1)
      COMMON /ZNTPKX/ A(4),II,EOL
      COMMON /INVTRX/ FILEA(7),FILEB(7),SCRFIL,NX,PREC
      COMMON /NAMES / RD,RDREW,WRT,WRTREW,REW,NOREW,EOFNRW
      COMMON /PACKX / IT1,IT2,IY,JY ,INCRY
      COMMON /UNPAKX/ ITX1,IX,JX ,INCRX
      EQUIVALENCE     (A(1),DA(1)),(FILEA(3),NROW),(FILEA(4),FORMA),
     1                (FILEA(5),TYPEA),(FILEB(5),TYPEB)
      DATA    NAME  / 4HINVT,4HR     /, T /7*0/
C
C     INITIALIZE
C
      TYPEAR = RC(TYPEA)
      TYPEAR = RC(TYPEAR) + PREC - 1
      INCR   = NWDS(TYPEAR)
      IT1    = TYPEAR
      IT2    = TYPEB
      ITX1   = TYPEAR
      INCRX  = 1
      INCRY  = 1
      FILEB(2) = 0
      FILEB(6) = 0
      FILEB(7) = 0
      IOBUF = NX - SYSBUF
      CMPLX = RC(TYPEAR)
      CORE  = IOBUF - 1
      CALL GOPEN (FILEB,X(IOBUF),1)
      CALL CLOSE (FILEB,NOREW)
      IF (FORMA .EQ. 5) GO TO 500
      IF (FORMA .NE. 4) GO TO 1000
C
C     INVERT A LOWER TRIANGULAR MATRIX
C
      BAKSKP = NROW
      FORSKP = 1
      GO TO (1,2,3,4), TYPEAR
    1 ASSIGN 50  TO TRA
      ASSIGN 110 TO TRA1
      GO TO  5
    2 ASSIGN 60  TO TRA
      ASSIGN 120 TO TRA1
      GO TO  5
    3 ASSIGN 70  TO TRA
      ASSIGN 130 TO TRA1
      GO TO  5
    4 ASSIGN 80  TO TRA
      ASSIGN 140 TO TRA1
    5 CONTINUE
C
C     ALLOCATE CORE STORAGE
C
      CALL GOPEN (FILEA,X(IOBUF),0)
      J = 1
C
C     SOLVE QUADRATIC FOR K
C
   10 M  = NROW - J + 1
      L  = 2*M + 1
      K  = M
      IF (L*L .LE. 8/INCR*CORE) GO TO 20
      A1 = L*L - 8/INCR*CORE
      K  = SQRT(A1)
      K  = K + 1
      K  = (L-K)/2
      IF (K .LE. 0) GO TO 1040
C
C     GENERATE COLUMNS J THROUGH J+K OF THE IDENTITY MATRIX (STORE
C     ONLY THE LOWER TRIANGLE IN CORE)
C
   20 L = (M*K-(K*(K-1))/2)*INCR
      DO 30 I = 1,L
   30 X(I) = 0.
      L = 1
      IF (PREC .EQ. 2) GO TO 41
      DO 40 I = 1,K
      X(L) = 1.
   40 L = L + (M-I+1)*INCR
      GO TO 44
   41 DO 42 I = 1,K
      DX(L) = 1.D0
   42 L = L + (M-I+1)*CMPLX
   44 CONTINUE
C
C     READ MATRIX A ONE ELEMENT AT A TIME, ADDING IN TERMS TO THE
C     IDENTITY MATRIX
C
      L  = 1
      LL = 1
C
C     II =  COLUMN INDEX
C     M  =  HEIGTH OF TRAPAZOID
C     K  =  LENGTH OF TRAPAZOID
C
      DO 200 I = J,NROW
      CALL INTPK (*1050,FILEA,0,TYPEAR,0)
   45 IF (EOL) 1050,46,1050
   46 CALL ZNTPKI
      IF (I .NE. II) GO TO 45
      L1 = 0
      DO 90 I1 = 1,LL
      IN1 = (L-1)*CMPLX + 1 + L1
      GO TO TRA, (50,60,70,80)
   50 X(IN1) = X(IN1)/A(1)
      GO TO 90
   60 DX(IN1) = DX(IN1)/DA(1)
      GO TO 90
   70 TEMP     = (A(1)*X(IN1  ) + A(2)*X(IN1+1))/(A(1)*A(1) + A(2)*A(2))
      X(IN1+1) = (A(1)*X(IN1+1) - A(2)*X(IN1  ))/(A(1)*A(1) + A(2)*A(2))
      X(IN1)   = TEMP
      GO TO 90
   80 DTEMP    = (DA(1)*DX(IN1  ) +DA(2)*DX(IN1+1))/(DA(1)**2 +DA(2)**2)
      DX(IN1+1)= (DA(1)*DX(IN1+1) -DA(2)*DX(IN1  ))/(DA(1)**2 +DA(2)**2)
      DX(IN1)  = DTEMP
   90 L1 = L1 + (M-I1)*CMPLX
  100 IF (EOL .EQ. 1) GO TO 190
      CALL ZNTPKI
      L1 = 0
      DO 150 I1 = 1,LL
      IN2 = (L-1)*CMPLX + 1+ L1
      IN1 = IN2 + (II-I)*CMPLX
      GO TO TRA1, (110,120,130,140)
  110 X(IN1) = X(IN1) - A(1)*X(IN2)
      GO TO 150
  120 DX(IN1) = DX(IN1) - DA(1)*DX(IN2)
      GO TO 150
  130 X(IN1  ) = X(IN1  ) - A(1)*X(IN2  ) + A(2)*X(IN2+1)
      X(IN1+1) = X(IN1+1) - A(1)*X(IN2+1) - A(2)*X(IN2  )
      GO TO 150
  140 DX(IN1  ) = DX(IN1  ) - DA(1)*DX(IN2  ) + DA(2)*DX(IN2+1)
      DX(IN1+1) = DX(IN1+1) - DA(1)*DX(IN2+1) - DA(2)*DX(IN2  )
  150 L1 = L1 + (M-I1)*CMPLX
      GO TO 100
  190 LL = LL + 1
      IF (LL .GT. K) LL = K
      L = L + 1
  200 CONTINUE
      FORSKP = FORSKP + K
      BAKSKP = BAKSKP - K
      I1 = REW
      IF (BAKSKP .LT. FORSKP) I1 = NOREW
      CALL CLOSE (FILEA ,I1)
      CALL GOPEN (FILEB,X(IOBUF),WRT)
      L  = 1
      IY = J
      JY = NROW
      DO 205 I = 1,K
      CALL PACK (X(L),FILEB,FILEB)
      IY = IY + 1
  205 L  = L  + (M-I+1)*INCR
      CALL CLOSE (FILEB,NOREW)
      J  = J + K
      IF (J .LE. NROW) GO TO 206
      CALL GOPEN (FILEB,X(IOBUF),WRT)
      CALL CLOSE (FILEB,REW)
      RETURN
C
  206 CONTINUE
      CALL GOPEN (FILEA,X(IOBUF),RD)
      IF (FORSKP .GT. BAKSKP) GO TO 220
      CALL SKPREC (FILEA,FORSKP)
      GO TO 10
  220 CALL SKPREC (FILEA,-BAKSKP)
      GO TO 10
C
C     INVERT UPPER TRIANGULAR MATRIX
C
  500 GO TO (510,520,530,540), TYPEAR
  510 ASSIGN 600 TO TRA
      ASSIGN 700 TO TRA1
      ASSIGN 770 TO TRA2
      GO TO  550
  520 ASSIGN 610 TO TRA
      ASSIGN 710 TO TRA1
      ASSIGN 780 TO TRA2
      GO TO  550
  530 ASSIGN 610 TO TRA
      ASSIGN 720 TO TRA1
      ASSIGN 790 TO TRA2
      GO TO  550
  540 ASSIGN 630 TO TRA
      ASSIGN 730 TO TRA1
      ASSIGN 800 TO TRA2
C
C     REWRITE UPPER TRIANGULAR MATRIX ON SCRATCH FILE
C
  550 INBUF  = IOBUF
      FORSKP = NROW + 1
      BAKSKP = 0
      OUTBUF = INBUF - SYSBUF
      IF (OUTBUF .LT. NROW+1) GO TO 1040
      CALL GOPEN (FILEA,X(IOBUF),0)
C
C     POSITION FILE AT LAST RECORD
C
      CALL SKPREC (FILEA,NROW)
C
C     REWRITE THE INPUT MATRIX ON A SCRATCH FILE WITH THE RECORDS
C     WRITTEN IN THE REVERSE ORDER AND THE COLUMNS INVERTED
C
      CALL GOPEN (SCRFIL,X(OUTBUF),1)
      IT2 = TYPEAR
      DO 645 I = 1,NROW
      IX = 1
      JX = 0
      CALL BCKREC (FILEA)
      CALL UNPACK (*1050,FILEA,X)
      CALL BCKREC (FILEA)
      KK = JX - IX + 1
      K  = KK/2
      IF (K .EQ. 0) GO TO 641
      KK = KK + 1
      DO 640 J = 1,K
      L  = KK - J
      GO TO TRA, (600,610,630)
  600 TEMP = X(J)
      X(J) = X(L)
      X(L) = TEMP
      GO TO 640
  610 DTEMP = DX(J)
      DX(J) = DX(L)
      DX(L) = DTEMP
      GO TO 640
  630 DTEMP = DX(J)
      DX(J) = DX(L)
      DX(L) = DTEMP
      DTEMP = DX(J+1)
      DX(J+1) = DX(L+1)
      DX(L+1) = DTEMP
  640 CONTINUE
  641 CONTINUE
      IY = NROW - JX + 1
      JY = NROW - IX + 1
      CALL PACK (X,SCRFIL,T)
  645 CONTINUE
      IT1 = TYPEAR
      IT2 = TYPEB
      CALL CLOSE (FILEA,REW)
      CALL CLOSE (SCRFIL,EOFNRW)
      CALL GOPEN (SCRFIL,X(IOBUF),0)
      CALL SKPREC (SCRFIL,NROW)
      CALL CLOSE (SCRFIL,NOREW)
C
C     ALLOCATE CORE
C
      J  = 0
  650 M  = J + 1
      CALL GOPEN (SCRFIL,X(IOBUF),RD)
      K  = NROW - J
      IF (K*M+K*(K-1)/2 .LT. CORE/INCR) GO TO 652
      A1 = (2*M-1)**2 + 8*CORE/INCR
      K  = SQRT(A1)
      K  = (-(2*M-1)+K)/2
      IF (K .LE. 0) GO TO 1040
  652 BAKSKP = BAKSKP + K
      FORSKP = FORSKP - K
C
C     POSITION SCRATCH FILE
C
      IF (FORSKP .GT. BAKSKP) GO TO 660
      CALL REWIND (SCRFIL)
      CALL SKPREC (SCRFIL,FORSKP)
      GO TO 665
  660 CALL SKPREC (SCRFIL,-BAKSKP)
  665 CONTINUE
C
C     GENERATE UPPER TRIANGLE OF THE IDENTITY MATRIX
C
      LEND = (M*K+K*(K-1)/2)*INCR
      DO 670 I = 1,LEND
  670 X(I) = 0.
      L = M
      IF (PREC .EQ. 2) GO TO 676
      DO 675 I = 1,K
      X(L) = 1.
  675 L = L + (I+M)*INCR
      GO TO 680
  676 DO 678 I = 1,K
      DX(L) = 1.D0
  678 L = L + (I+M)*CMPLX
  680 CONTINUE
C
C     READ UPPER TRIANGLE ONE ELEMENT AT A TIME, ADDING IN
C     APPROPIATE TERMS TO THE IDENTITY MATRIX
C
      IF (PREC .EQ. 2) LEND = LEND/2
      J = J + K
      L = 1
      DO 901 JJ = 1,J
      CALL INTPK (*1050,SCRFIL,0,TYPEAR,0)
      CALL ZNTPKI
      I = NROW - II + 1
      IF (I .NE. J-JJ+1) GO TO 1050
      L1 = 0
      DO 750 I1 = 1,L
      IN1 = LEND - L*CMPLX - L1 + 1
      GO TO TRA1, (700,710,720,730)
  700 X(IN1) = X(IN1)/A(1)
      GO TO 740
  710 DX(IN1) = DX(IN1)/DA(1)
      GO TO 740
  720 TEMP   = (A(1)*X(IN1  ) + A(2)*X(IN1+1))/(A(1)*A(1) + A(2)*A(2))
      IN2    = IN1 + 1
      X(IN2) = (A(1)*X(IN1+1) - A(2)*X(IN1  ))/(A(1)*A(1) + A(2)*A(2))
      X(IN1) = TEMP
      GO TO 740
  730 DTEMP  = (DA(1)*DX(IN1  ) + DA(2)*DX(IN1+1))/(DA(1)**2 + DA(2)**2)
      IN2    = IN1 + 1
      DX(IN2)= (DA(1)*DX(IN1+1) - DA(2)*DX(IN1  ))/(DA(1)**2 + DA(2)**2)
      DX(IN1)= DTEMP
  740 CONTINUE
  750 L1 = L1 + (M+K-1-I1)*CMPLX
  760 IF (EOL .EQ. 1) GO TO 901
      CALL ZNTPKI
      L1 = 0
      I  = J - JJ - NROW + II
      DO 900 I1 = 1,L
      IN2 = LEND - L*CMPLX - L1 + 1
      IN1 = IN2 - I*CMPLX
      GO TO TRA2, (770,780,790,800)
  770 X(IN1) = X(IN1) - A(1)*X(IN2)
      GO TO 810
  780 DX(IN1) = DX(IN1) - DA(1)*DX(IN2)
      GO TO 810
  790 X(IN1  ) = X(IN1  ) - A(1)*X(IN2  ) + A(2)*X(IN2+1)
      X(IN1+1) = X(IN1+1) - A(1)*X(IN2+1) - A(2)*X(IN2  )
      GO TO 810
  800 DX(IN1  ) = DX(IN1  ) - DA(1)*DX(IN2  ) + DA(2)*DX(IN2+1)
      DX(IN1+1) = DX(IN1+1) - DA(1)*DX(IN2+1) - DA(2)*DX(IN2  )
  810 L1 = L1 + (M+K-1-I1)*CMPLX
  900 CONTINUE
      GO TO 760
  901 L = L + 1
      CALL CLOSE (SCRFIL,NOREW)
      CALL GOPEN (FILEB,X(IOBUF),WRT)
      L = J - K + 1
      LL= 1
      DO 910 I = 1,K
      IY = 1
      JY = L
      CALL PACK (X(LL),FILEB,FILEB)
      L = L + 1
  910 LL = LL + (M+I-1)*INCR
      CALL CLOSE (FILEB,NOREW)
      IF (J .LT. NROW) GO TO 650
      CALL GOPEN (FILEB,X(IOBUF),WRT)
      CALL CLOSE (FILEB,REW)
      CALL GOPEN (SCRFIL,X(IOBUF),RD)
      CALL CLOSE (SCRFIL,REW)
      GO TO 2000
C
 1000 NO = -7
      GO TO 1100
 1040 NO = -8
      GO TO 1100
 1050 RETURN 1
 1100 CALL MESAGE (NO,0,NAME)
C
 2000 RETURN
      END
