      SUBROUTINE GFBS (X,DX)
C
C     GIVEN THE TRIANGULAR FACTORS FOR A GENERAL MATRIX, GFBS WILL
C     PERFORM THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE
C     A SYSTEM OF EQUATIONS
C
C     DEFINITION OF INPUT PARAMETERS
C
C     FILEL    =  MATRIX CONTROL BLOCK FOR THE LOWER TRIANGLE L
C     FILEU    =  MATRIX CONTROL BLOCK FOR THE UPPER TRIANGLE U
C     FILEB    =  MATRIX CONTROL BLOCK FOR THE LOAD   VECTORS B
C     FILEX    =  MATRIX CONTROL BLOCK FOR THE SOLUTION VECTORS X
C     NX       =  NUMBER OF CELLS OF CORE AVAILABLE AT X
C     PREC     =  DESIRED PRECISION OF ARITHMETIC OPERATIONS
C                (1 = SINGLE PRECISION, 2 = DOUBLE PRECISION)
C     ISIGN    =  SIGN TO BE APPLIED TO THE LOAD VECTORS
C     X        =  BLOCK OF CORE AVAILABLE AS WORKING STORAGE
C     DX       =  SAME BLOCK AS X, BUT TYPED DOUBLE PRECISION
C
      INTEGER            FILEL     ,FILEU    ,FILEB    ,FILEX    ,
     1                   TYPEA     ,TYPE1    ,TYPE2    ,FORMB    ,
     2                   SYSBUF    ,PREC     ,EOL      ,TYPEAR   ,
     3                   TYPEX     ,TYPEL    ,RC       ,REW      ,
     4                   TYPEB     ,TRA1     ,TRA2     ,TRA3     ,
     5                   TRA4      ,TRA5     ,PARM(4)  ,CMPLX    ,
     6                   EOFNRW    ,COL      ,FSTCOL   ,CLSOP
      REAL               ZEROS(4)  ,
     1                   SUBNAM(2) ,BUF(2)   ,BEGN     ,END
      DOUBLE PRECISION   DX(1)     ,DA(2)    ,DTEMP
      DIMENSION          X(1)
      CHARACTER          UFM*23    ,UWM*25   ,UIM*29   ,SFM*25
      COMMON   /XMSSG /  UFM       ,UWM      ,UIM      ,SFM
      COMMON   /SYSTEM/  SYSBUF    ,NOUT
      COMMON   /TYPE  /  PRC(2)    ,NWDS(4)  ,RC(10)
C     COMMON   /DESCRP/  LENGTH    ,MAJOR
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP       ,CSP      ,CDP      ,SQR      ,
     3                   RECT      ,DIAG     ,LOWTRI   ,UPRTRI   ,
     4                   SYM       ,ROW      ,IDENTY
      COMMON   /UNPAKX/  TYPEA     ,IXY      ,JXY      ,INCRY
      COMMON   /PACKX /  TYPE1     ,TYPE2    ,IY       ,JY       ,
     1                   INCRX
      COMMON   /ZNTPKX/  A(4)      ,II       ,EOL
      COMMON   /GFBSX /  FILEL(7)  ,FILEU(7) ,FILEB(7) ,FILEX(7) ,
     1                   NX        ,PREC     ,ISIGN
      EQUIVALENCE        (A(1),DA(1))        ,(FILEL(5),TYPEL)   ,
     1                   (FILEL(3),NROW)     ,(FILEX(5),TYPEX)   ,
     2                   (FILEB(4),FORMB)    ,(FILEB(5),TYPEB)
      DATA      PARM(3), PARM(4)  /4HGFBS,4H     /
      DATA      ZEROS /  0., 0., 0., 0. /
      DATA      SUBNAM/  4HGFBS,4H      /, BEGN/ 4HBEGN/, END/ 4HEND /
C
      BUF(1) = SUBNAM(1)
      BUF(2) = BEGN
      CALL CONMSG (BUF,2,0)
C
C     INITIALIZE
C
      IF (FORMB .EQ. IDENTY)TYPEB = 1
      TYPEAR = PREC
      IF (RC(TYPEL)+RC(TYPEB)-1 .GT. 1) TYPEAR = PREC + 2
      INCR  = NWDS(TYPEAR)*NROW
      TYPEA = TYPEAR*ISIGN
      TYPE1 = TYPEAR
      TYPE2 = TYPEX
      INCRX = 1
      INCRY = 1
      CMPLX = RC(TYPEAR)
      IOBUF = NX - SYSBUF
      ICOL  = IOBUF - 1
      COL   = 1
      CLSOP = EOFNRW
C
C     SET UP TRANSFER VECTORS FOR THE ARITHMETIC TYPES
C
      GO TO (10,20,30,40), TYPEAR
   10 ASSIGN 120 TO TRA1
      ASSIGN 240 TO TRA2
      ASSIGN 330 TO TRA3
      ASSIGN 430 TO TRA4
      ASSIGN 540 TO TRA5
      GO TO 50
   20 ASSIGN 130 TO TRA1
      ASSIGN 250 TO TRA2
      ASSIGN 340 TO TRA3
      ASSIGN 440 TO TRA4
      ASSIGN 550 TO TRA5
      GO TO 50
   30 ASSIGN 140 TO TRA1
      ASSIGN 260 TO TRA2
      ASSIGN 350 TO TRA3
      ASSIGN 450 TO TRA4
      ASSIGN 560 TO TRA5
      GO TO 50
   40 ASSIGN 150 TO TRA1
      ASSIGN 270 TO TRA2
      ASSIGN 360 TO TRA3
      ASSIGN 460 TO TRA4
      ASSIGN 570 TO TRA5
   50 CONTINUE
      NM = (IOBUF-1)/INCR
      IF (NM .LE. 0) GO TO 640
      NOLOAD = FILEB(2)
      IF (FORMB .EQ. IDENTY)NOLOAD = NROW
      IDENT  = 1
      LSTLOD = NOLOAD
C
C     WRITE OUTPUT HEADER RECORDS AND INITIALIZE MATRIX CONTROL BLOCKS
C
      CALL GOPEN (FILEX,X(IOBUF),1)
      CALL CLOSE (FILEX(1),NOREW)
      FILEX(2) = 0
      FILEX(6) = 0
      FILEX(7) = 0
      IF (FORMB .EQ. IDENTY) GO TO 100
C
C     OPEN THE LOAD FILE AND FILL CORE WITH LOAD VECTORS
C
      CALL GOPEN (FILEB,X(IOBUF),0)
   60 NN  = 0
      KHR = ICOL
      FSTCOL = COL
      L   = 1
      IXY = 1
      JXY = NROW
   70 IF (L+INCR .GE. KHR) GO TO 85
      CALL UNPACK (*80,FILEB,X(L))
      NN  = NN + 1
      X(KHR) = COL
      KHR = KHR - 1
      L   = L + INCR
   80 IF (COL .EQ. LSTLOD) GO TO 90
      COL = COL + 1
      GO TO 70
   85 COL  = COL - 1
   90 NCOL = KHR
      X(NCOL) = LSTLOD + 1
      LSTCOL  = COL
      IF (LSTCOL .EQ. LSTLOD) CLSOP = REW
      CALL CLOSE (FILEB,CLSOP)
      IF (NN .EQ. 0) GO TO 592
      GO TO 180
C
C     GENERATE COLUMNS OF THE IDENTITY MATRIX
C
  100 NN = MIN0(NM,NOLOAD)
      L  = 1
      DO 170 I = 1,NN
      J1 = L
      J2 = J1 + INCR - 1
      DO 110 K = J1,J2
  110 X(K) = 0.
      K = L + IDENT - 1
      GO TO TRA1, (120,130,140,150)
  120 X(K) = 1.
      GO TO 160
  130 K = (L-1)/2 + IDENT
      DX(K) = 1.D0
      GO TO 160
  140 KK = K + IDENT - 1
      X(KK) = 1.
      GO TO 160
  150 KK = (L-1)/2 + 2*IDENT - 1
      DX(KK) = 1.D0
  160 IDENT  = IDENT + 1
  170 L = L + INCR
      FSTCOL = COL
      COL    = IDENT - 1
      LSTCOL = COL
  180 IJK    = 0
C
C     OPEN FILE FOR THE LOWER TRIANGLE
C
      PARM(2) = FILEL(1)
      CALL GOPEN (FILEL,X(IOBUF),0)
C
C     BEGIN FORWARD PASS
C
      J = 1
  190 CALL INTPK (*380,FILEL(1),0,TYPEAR,0)
  200 IF (EOL) 650,210,650
  210 CALL ZNTPKI
      IF (J-II) 310,220,200
C
C     PERFORM THE REQUIRED ROW INTERCHANGE
C
  220 INTCHN = A(1)
      K = 0
      IF (PREC .EQ. 2) INTCHN = DA(1)
      IN1 = J*CMPLX
      IN2 = IN1 + INTCHN*CMPLX
  230 GO TO TRA2, (240,250,260,270)
  240 TEMP     = X(IN1)
      X(IN1)   = X(IN2)
      X(IN2)   = TEMP
      GO TO 280
  250 DTEMP    = DX(IN1)
      DX(IN1)  = DX(IN2)
      DX(IN2)  = DTEMP
      GO TO 280
  260 TEMP     = X(IN1)
      X(IN1)   = X(IN2)
      X(IN2)   = TEMP
      TEMP     = X(IN1-1)
      X(IN1-1) = X(IN2-1)
      X(IN2-1) = TEMP
      GO TO 280
  270 DTEMP    = DX(IN1)
      DX(IN1)  = DX(IN2)
      DX(IN2)  = DTEMP
      DTEMP    = DX(IN1-1)
      DX(IN1-1) = DX(IN2-1)
      DX(IN2-1) = DTEMP
  280 IN1 = IN1 + NROW*CMPLX
      IN2 = IN2 + NROW*CMPLX
      K   = K + 1
      IF (K-NN) 230,290,290
  290 IF (EOL) 380,300,380
  300 CALL ZNTPKI
  310 K = 0
      IN2 = J*CMPLX
      IN1 = II*CMPLX
  320 K = K + 1
      GO TO TRA3, (330,340,350,360)
  330 X(IN1) = X(IN1) - X(IN2)*A(1)
      GO TO 370
  340 DX(IN1) = DX(IN1) - DX(IN2)*DA(1)
      GO TO 370
  350 X(IN1-1) = X(IN1-1) - A(1)*X(IN2-1) + A(2)*X(IN2  )
      X(IN1  ) = X(IN1  ) - A(1)*X(IN2  ) - A(2)*X(IN2-1)
      GO TO 370
  360 DX(IN1-1) = DX(IN1-1) - DA(1)*DX(IN2-1) + DA(2)*DX(IN2  )
      DX(IN1  ) = DX(IN1  ) - DA(1)*DX(IN2  ) - DA(2)*DX(IN2-1)
  370 IN1 = IN1 + NROW *CMPLX
      IN2 = IN2 + NROW *CMPLX
      IF (K-NN) 320,290,290
  380 J = J + 1
      IF (J .LT. NROW) GO TO 190
      CALL CLOSE (FILEL(1),REW)
C
C     BEGIN BACKWARD PASS
C
      IOFF = FILEU(7)-1
      PARM(2) = FILEU(1)
      CALL GOPEN (FILEU,X(IOBUF),0)
      J = NROW
  390 CALL INTPK (*650,FILEU(1),0,TYPEAR,0)
      IF (EOL) 650,410,650
  410 CALL ZNTPKI
      I = NROW - II + 1
      IF (I .NE. J) GO TO 510
C
C     DIVIDE BY THE DIAGONAL
C
      IN1 = I*CMPLX
      K   = 0
  420 GO TO TRA4, (430,440,450,460)
  430 X(IN1) = X(IN1)/A(1)
      GO TO 470
  440 DX(IN1) = DX(IN1)/DA(1)
      GO TO 470
  450 TEMP   = (A(1)*X(IN1-1) + A(2)*X(IN1  ))/(A(1)*A(1) + A(2)*A(2))
      X(IN1) = (A(1)*X(IN1  ) - A(2)*X(IN1-1))/(A(1)*A(1) + A(2)*A(2))
      X(IN1-1) = TEMP
      GO TO 470
  460 DTEMP   = (DA(1)*DX(IN1-1) + DA(2)*DX(IN1  ))/(DA(1)**2 +DA(2)**2)
      DX(IN1) = (DA(1)*DX(IN1  ) - DA(2)*DX(IN1-1))/(DA(1)**2 +DA(2)**2)
      DX(IN1-1) = DTEMP
  470 K = K + 1
      IN1 = IN1 + NROW*CMPLX
      IF (K-NN) 420,490,490
C
C     SUBTRACT OFF REMAINING TERMS
C
  480 IF (I .GT. J) GO TO 410
  490 IF (EOL) 590,500,590
  500 CALL ZNTPKI
      I   = NROW - II + 1
  510 IN1 = I*CMPLX
      IN2 = J*CMPLX
      IF (I .LT. J) GO TO 520
      K   = IN1
      IN1 = IN2 - IOFF*CMPLX
      IN2 = K
  520 K   = 0
  530 GO TO TRA5, (540,550,560,570)
  540 X(IN1) = X(IN1) - A(1)*X(IN2)
      GO TO 580
  550 DX(IN1) = DX(IN1) - DX(IN2)*DA(1)
      GO TO 580
  560 X(IN1-1) = X(IN1-1) - A(1)*X(IN2-1) + A(2)*X(IN2  )
      X(IN1  ) = X(IN1  ) - A(1)*X(IN2  ) - A(2)*X(IN2-1)
      GO TO 580
  570 DX(IN1-1) = DX(IN1-1) - DA(1)*DX(IN2-1) + DA(2)*DX(IN2  )
      DX(IN1  ) = DX(IN1  ) - DA(1)*DX(IN2  ) - DA(2)*DX(IN2-1)
  580 IN1 = IN1 + NROW*CMPLX
      IN2 = IN2 + NROW*CMPLX
      K   = K + 1
      IF (K-NN) 530,480,480
  590 J = J - 1
      IF (J .GT. 0) GO TO 390
      CALL CLOSE (FILEU(1),REW)
C
C     OUTPUT LOAD VECTORS
C
  592 CONTINUE
      CALL GOPEN (FILEX,X(IOBUF),WRT)
      L  = 1
      IY = 1
      IF (FORMB .NE. IDENTY) NXTNZ = X(ICOL)
      KHR = ICOL
      DO 600 COL = FSTCOL,LSTCOL
      IF (FORMB .EQ. IDENTY) GO TO 595
C 593 CONTINUE
      IF (COL-NXTNZ) 594,595,901
  594 JY = 1
      CALL PACK (ZEROS,FILEX,FILEX)
      GO TO 600
  595 JY = NROW
      CALL PACK (X(L),FILEX,FILEX)
      L   = L + INCR
      KHR = KHR - 1
      IF (FORMB .NE. IDENTY) NXTNZ = X(KHR)
  600 CONTINUE
      IF (FORMB.NE.IDENTY .AND. KHR.NE.NCOL) GO TO 902
      IF (LSTCOL .EQ. LSTLOD) CLSOP = REW
      CALL CLOSE (FILEX,CLSOP)
      NOLOAD = NOLOAD - (LSTCOL-FSTCOL+1)
      IF (LSTCOL .EQ. LSTLOD) GO TO 670
      COL = LSTCOL + 1
      IF (FORMB .EQ. IDENTY) GO TO 100
      CALL GOPEN (FILEB,X(IOBUF),RD)
      GO TO 60
  640 PARM(1) = -8
      GO TO 660
  650 PARM(1) = -5
  660 CALL MESAGE (PARM(1),PARM(2),PARM(3))
  670 IF (FILEX(2) .NE. LSTLOD) GO TO 903
      BUF(1) = SUBNAM(1)
      BUF(2) = END
      CALL CONMSG (BUF,2,0)
      RETURN
C
C     LOGIC ERRORS LAND HERE
C
  901 KERR = 593
      GO TO 997
  902 KERR = 600
      GO TO 997
  903 KERR = 670
      GO TO 997
  997 WRITE  (NOUT,998) SFM,KERR
  998 FORMAT (A25,I4,' - LOGIC ERROR IN GFBS')
      CALL MESAGE (-61,0,0)
      RETURN
      END
