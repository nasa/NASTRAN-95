      SUBROUTINE CDCMPS (*,IX,X,DX)
C
C     CDCMPS IS THE SINGLE PRECISION VERSION OF CDCOMP
C     (THIS ROUTINE IS NOT IN OPERATION YET, 6/87)
C
C     CDCOMP WILL DECOMPOSE A COMPLEX UNSYMETRIC MATRIX INTO A LOWER
C     TRIANGULAR MATRIX AND AN UPPER TRIANGULAR MATRIX, USING PARTIAL
C     PIVOTING WITHIN THE LOWER BAND
C     THE OUTPUT MATRICES ARE PACKED IN S.P. OR D.P. AS SPECIFIED BY
C     THE CALLING ROUTINE VIA TYPEL (=FILEL(5) IN /CDCMPX/)
C
C     DEFINITION OF INPUT PARAMETERS
C
C     FILEA    =  MATRIX CONTROL BLOCK FOR THE INPUT MATRIX A
C     FILEL    =  MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX L
C     FILEU    =  MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX U
C     SR1FIL   =  SCRATCH FILE
C     SR2FIL   =  SCRATCH FILE
C     SR3FIL   =  SCRATCH FILE
C     NX       =  NUMBER OF CELLS OF CORE AVAILABLE AT IX
C     DET      =  CELL WHERE THE DETERMINATE OF A WILL BE STORED
C     POWER    =  SCALE FACTOR TO BE APPLIED TO THE DETERMINATE
C                 ( DETERMINATE = DET*10**POWER )
C     MINDIA   =  CELL WHERE THE VALUE OF THE MINIMUM DIAGONAL WILL BE S
C     IX       =  BLOCK OF CORE AVAILABLE AS WORKING STORAGE TO DECOMP
C     X        =  SAME BLOCK AS IX, BUT TYPED S.P. REAL
C     DX       =  SAME BLOCK AS IX, BUT TYPED S.P. REAL
C
C
      INTEGER            FILEA     ,FILEL    ,FILEU    ,POWER    ,
     1                   SYSBUF    ,FORMA    ,TYPEA    ,RDP      ,
     2                   TYPEL     ,EOL      ,PARM(5)  ,BUFA     ,
     3                   OUTBUF    ,SR1BUF   ,SR2BUF   ,SR3BUF   ,
     4                   B         ,BBAR     ,C        ,CBAR     ,
     5                   BBAR1     ,R        ,CCOUNT   ,CBCNT    ,
     6                   SCRFLG    ,END      ,BBBAR    ,BBBAR1   ,
     7                   COUNT     ,SR2FL    ,SR3FL    ,SR1FIL   ,
     8                   SR2FIL    ,SR3FIL   ,SQR      ,SYM      ,
     9                   FLAG      ,ITRAN(4) ,CSP      ,IX(1)
      REAL               DZ(2)     ,DA(2)    ,MAX(2)   ,X(1)     ,
     1                   DX(1)     ,DTRN(2)  ,DX1      ,DX2      ,
     2                   EPSI
      DOUBLE PRECISION   MINDIA    ,DET
      CHARACTER          UFM*23    ,UWM*25   ,UIM*29
      COMMON   /XMSSG /  UFM       ,UWM      ,UIM
      COMMON   /CDCMPX/  FILEA(7)  ,FILEL(7) ,FILEU(7) ,SR1FIL   ,
     1                   SR2FIL    ,SR3FIL   ,DET(2)   ,POWER    ,
     2                   NX        ,MINDIA   ,B        ,BBAR     ,
     3                   C         ,CBAR     ,R
      COMMON   /SYSTEM/  SYSBUF    ,NOUT
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP       ,CSP      ,CDP      ,SQR      ,
     3                   RECT      ,DIAG     ,LOWTRI   ,UPRTRI   ,
     4                   SYM       ,ROW      ,IDENT
      COMMON   /ZNTPKX/  A(4)      ,II       ,EOL
      COMMON   /DESCRP/  LENGTH    ,MAJOR
      COMMON   /ZBLPKX/  Z(4)      ,JJ
      COMMON   /UNPAKX/  ITYPEX    ,IXY      ,JXY      ,INCRX
      COMMON   /PACKX /  ITYPE1    ,ITYPE2   ,IY       ,JY       ,
     1                   INCRY
      EQUIVALENCE        (DA(1)   ,A(1)    ) ,(DZ(1)   ,Z(1)    ),
     1                   (FORMA   ,FILEA(4)) ,(TYPEA   ,FILEA(5)),
     2                   (NCOL    ,FILEA(3)) ,(TYPEL   ,FILEL(5)),
     3                   (ITRAN(1),ITRN    ) ,(ITRAN(2),JTRN    ),
     4                   (ITRAN(3),DTRN(1) )
      DATA      PARM(3), PARM(4)  /4HCDCM, 4HPS  /
      DATA      IBEGN  , IEND     /4HBEGN, 4HEND /
      DATA      EPSI   , J4, J2   /1.E-38, 2, 1  /
C               WERE...  J4, J2   =        4, 2  IN CDCMPD
C
      WRITE  (NOUT,10) UIM
   10 FORMAT (A29,', COMPLEX MATRIX DECOMP. IS NOW COMPUTED IN SINGLE',
     1       ' PRECISION', /5X,'REVERT TO DOUBLE PRECISION COMPUTATION',
     2       ' BY TURNING ON DIAG 41')
C
C     BUFFER ALLOCATION
C
      BUFA   = NX     - SYSBUF
      IBUFL  = BUFA   - SYSBUF
      OUTBUF = IBUFL  - SYSBUF
      SR1BUF = OUTBUF - SYSBUF
      SR2BUF = SR1BUF - SYSBUF
      SR3BUF = SR2BUF - SYSBUF
      ICRQ   =-SR3BUF
      IF (ICRQ .GT. 0) GO TO 1715
C     ITYPEX = CSP
C     ITYPE2 = TYPEL
      DET(1) = 1.D+0
      DET(2) = 0.D+0
      POWER  = 0
      MINDIA = 1.D+25
      N64    = 4
      ITERM  = 0
      IF (FILEA(1) .LT. 0) ITERM = 1
      FILEA(1) = IABS(FILEA(1))
C
C     WRITE THE HEADER RECORD ON THE OUTPUT TAPES AND INITIALIZE THE
C     TRAILER RECORDS.
C
      CALL GOPEN (FILEL,IX(IBUFL),WRTREW)
      PARM(2)  = SR2FIL
      CALL OPEN (*1680,SR2FIL,IX(OUTBUF),WRTREW)
      CALL FNAME (FILEU(1),X(1))
      CALL WRITE (SR2FIL,X(1),2,1)
      FILEL(2) = 0
      FILEL(3) = NCOL
      FILEL(4) = 4
      FILEL(6) = 0
      FILEL(7) = 0
      FILEU(2) = 0
      FILEU(3) = NCOL
      FILEU(4) = 5
      FILEU(6) = 0
      FILEU(7) = 0
C
C     CALL GENVEC TO PICK B,BBAR,C,CBAR, AND R
C
      IF (B.GT.0 .AND. BBAR.GT.0) GO TO 11
      CALL GENVEC (*1720,IX(BUFA),FILEA(1),NX,IX(1),NCOL,B,BBAR,C,CBAR,
     1             R,2)
   11 CONTINUE
      BBAR1  = BBAR + 1
      BBBAR  = MIN0(B+BBAR,NCOL)
      BBBAR1 = BBBAR - 1
      SCRFLG = 0
      IF (R .LT. BBBAR1) SCRFLG=1
      IF (SCRFLG .EQ. 0) GO TO 20
      ICRQ = (BBBAR1-R)*J4*BBAR
      CALL PAGE2 (2)
      WRITE  (NOUT,15) UIM,ICRQ
   15 FORMAT (A29,' 2177, SPILL WILL OCCUR IN COMPLEX UNSYMMETRIC ',
     1       'DECOMPOSITION.', /I10,' ADDITIONAL WORDS NEEDED TO STAY ',
     2       'IN CORE.')
C
C     INITIALIZE POINTERS TO SPECIFIC AREAS OF CORE
C
   20 I1SP = 1
      I1   = I1SP + (BBBAR/2 +1)*2
      I2   = I1 + 2*BBAR*R
      IPAK = I2
      I3SP = I2 + 2*MIN0(NCOL,BBBAR+BBAR)
      I3   = I3SP + C*2
      I4SP = I3 + 2*BBAR1*C
      I4   = I4SP + CBAR*2
      I5   = I4 + 2*BBBAR*CBAR
      I6SP = I5 + 2*CBAR*C
      I7SP = I6SP + C
      END  = I7SP + CBAR
C
C     DEFINITION OF KEY PROGRAM PARAMETERS
C
C     AREA I
C     I1SP   =  POINTER TO AREA WHERE THE PERMUTATION INDEXES ARE STORED
C     I1     =  POINTER TO AREA WHERE COMPLETED COLUMNS OF L ARE STORED
C     AREA II
C     I2     =  POINTER TO AREA WHERE THE NEXT COLUMN OF A IS STORED
C     IPAK   =  POINTER TO AREA WHERE COLUMNS WILL BE PACKED FROM
C     AREA III
C     I3SP   =  POINTER TO AREA WHERE SEQUENCE ACTIVE COLUMNS INDEXES
C               ARE STORED
C     I3     =  POINTER TO AREA WHERE ACTIVE COLUMNS ARE STORED
C     AREA IV
C     I4SP   =  POINTER TO AREA WHERE SEQUENCE ACTIVE ROW INDEXES ARE
C               STORED
C     I4     =  POINTER TO AREA WHERE ACTIVE ROWS ARE STORED
C     AREA V
C     I5     =  POINTER TO AREA WHERE INTERACTION ELEMENTS ARE STORED
C     AREA VI
C     I6SP   =  POINTER TO AREA WHERE SEQUENCE ACTIVE ROW INDICES ARE
C               STORED
C     AREA VII
C     I7SP   =  POINTER TO AREA WHERE SEQUENCED ACTIVE COLUMN INDICES
C               ARE STORED
C
C     B      =  UPPER HALF-BAND
C     BBAR   =  LOWER HALF-BAND
C     BBAR1  =  BBAR + 1
C     BBBAR  =  B + BBAR
C     BBBAR1 =  B + BBAR - 1
C     C      =  NUMBER OF ACTIVE COLUMNS
C     CBAR   =  NUMBER OF ACTIVE ROWS
C     R      =  NUMBER OF COLUMNS OF L THAT CAN BE STORED IN CORE
C
C     JPOS   =  CURRENT PIVOTAL COLUMN INDEX
C     JPOSL  =  NEXT COLUMN OF L TO BE WRITTEN OUT
C     LCOL   =  NUMBER OF COLUMNS OF L CURRENTLY STORED IN CORE OR ON
C               SCRATCH FILES
C     CCOUNT =  CURRENT NUMBER OF ACTIVE COLUMNS
C     CBCNT  =  CURRENT NUMBER OF ACTIVE ROWS
C     ITRN   =  ROW INDEX OF NEXT ACTIVE COLUMN ELEMENT
C     JTRN   =  COLUMN INDEX  OF NEXT ACTIVE COLUMN ELEMENT
C     IOFF   =  ROW POSITION OF THE FIRST ELEMENT IN AREA II
C     ITERM  =  IF NONZERO, TERMINATE BEFORE THE RE-WRITE
C     NCOL   =  SIZE OF THE INPUT MATRIX
C     SCRFLG =  NONZERO MEANS SPILL
C
      IMHERE = 0
      WRITE (NOUT,1711) IMHERE,
     1          I1SP,I1,I2,I3SP,I3,I4SP,I4,I5,I6SP,I7SP,END,
     2          BBAR,BBAR1,BBBAR,C,CBAR,NCOL
      PARM(5) = IBEGN
      CALL CONMSG (PARM(3),3,0)
C     ****************************************************************
C     RE-WRITE THE UPPER TRIANGLE OF ACTIVE ELEMENTS IN THE TRANSPOSED
C     ORDER
C     ****************************************************************
      PARM(2) = FILEA(1)
      CALL OPEN (*1680,FILEA(1),IX(BUFA),RDREW)
      CCOUNT = 0
      IF (C .EQ. 0) GO TO 40
      CALL CTRNSP (IX(1),X(1),NX,FILEA(1),B,SR1FIL,N64)
C
C     ZERO CORE
C
   40 DO 50 I = 1,END
   50 X(I) = 0.
      IF (C .EQ. 0) GO TO 260
C     ****************************************************************
C     OPEN THE FILE CONTAINING THE TRANSPOSED ACTIVE ELEMENTS AND READ
C     IN THE FIRST BBAR + 1 ROWS
C     ****************************************************************
      PARM(2) = SR1FIL
      CALL OPEN (*1680,SR1FIL,IX(SR1BUF),RD)
      K = 0
   60 CALL READ (*1690,*1700,SR1FIL,ITRAN(1),N64,0,FLAG)
      IF (ITRN .GT. 0) GO TO 70
      CALL CLOSE (SR1FIL,REW)
      GO TO 140
   70 IF (ITRN .GT. K+1) GO TO 130
C
C     DETERMINE IF COLUMN IS ALREADY ACTIVE
C
      IF (JTRN .LE. BBBAR) GO TO 60
      KK  = 0
   80 IN1 = I3SP + KK
      IF (IX(IN1) .EQ. JTRN) GO TO 90
      KK  = KK + 1
      IMHERE = 80
      IF (KK-C) 80,100,1710
C
C     ADD IN ACTIVE ELEMENT TO EXISTING COLUMN
C
   90 IN1 = I3 + 2*KK*BBAR1 + K + K
      DX(IN1  ) = DTRN(1)
      DX(IN1+1) = DTRN(2)
      GO TO 60
C
C     CREATE NEW ACTIVE COLUMN
C
  100 CCOUNT = CCOUNT + 1
      KK  = 0
  110 IN1 = I3SP + KK
      IF (IX(IN1) .EQ. 0) GO TO 120
      KK  = KK + 1
      IMHERE = 120
      IF (KK-C) 110,1710,1710
  120 IX(IN1) = JTRN
      IN1     = IN1 + C
      IX(IN1) = K+1
      IN1     = I3 + 2*KK*BBAR1 + K + K
      DX(IN1  ) = DTRN(1)
      DX(IN1+1) = DTRN(2)
      GO TO 60
  130 K = K + 1
      IMHERE = 130
      IF (K-BBAR1) 70,140,1710
C
C     SET INDEXES IN AREA VII TO POINT TO THE ACTIVE COLUMNS IN SEQUENCE
C
  140 ASSIGN 260 TO KK
  150 IN1 = I7SP
      K   = 0
  160 IN2 = I3SP + K
      IMHERE = 160
      IF (IX(IN2)) 1710,180,190
  170 IN1 = IN1 + 1
  180 K   = K + 1
      IMHERE = 180
      IF (K-C) 160,250,1710
  190 IF (IN1 .NE. I7SP) GO TO 200
      IX(IN1) = K
      GO TO 170
  200 KKK = 0
  210 IN3 = IN1 - KKK
      IF (IN3 .GT. I7SP) GO TO 220
      IX(IN3) = K
      GO TO 170
  220 IN4 = I3SP + IX(IN3-1)
      IMHERE = 220
      IF (IX(IN2)-IX(IN4)) 240,1710,230
  230 IX(IN3) = K
      GO TO 170
  240 IX(IN3) = IX(IN3-1)
      KKK = KKK + 1
      GO TO 210
  250 GO TO KK, (260,1570)
  260 CONTINUE
C
C     INITIALIZE
C
      SR2FL = FILEU(1)
      SR3FL = SR3FIL
      JPOS  = 1
      PARM(2) = FILEA(1)
      CALL FWDREC (*1690,FILEA(1))
      LCOL  = 0
      CBCNT = 0
      JPOSL = 0
  270 IF (JPOS .GT. NCOL) GO TO 1670
C     ****************************************************************
C     READ NEXT COLUMN OF A INTO AREA II
C     ****************************************************************
      IOFF  = MAX0(1,JPOS-BBBAR1)
      COUNT = CBCNT
      CALL INTPK (*1720,FILEA(1),0,CSP,0)
      K = 1
      IF (JPOS .GT. BBBAR) K = JPOS - B + 1
  280 IF (EOL) 400,290,400
  290 CALL ZNTPKI
      IF (II .LT. K) GO TO 280
      K = JPOS + BBAR
  300 IF (II .GT. K) GO TO 330
C
C     READ ELEMENTS WITHIN THE BAND INTO AREA II
C
      IN1 = I2 + 2*(II-IOFF)
      DX(IN1  ) = DA(1)
      DX(IN1+1) = DA(2)
  310 IF (EOL) 400,320,400
  320 CALL ZNTPKI
      GO TO 300
C
C     TAKE CARE OF ACTIVE ELEMENTS BELOW THE BAND
C
  330 KK  = 0
  340 IN1 = I4SP + KK
      IF (IX(IN1)-II) 350,360,350
  350 KK  = KK + 1
      IMHERE = 350
      IF (KK-CBAR) 340,370,1710
C
C     ADD IN ACTIVE ELEMENT TO EXISTING ROW
C
  360 IN1 = I4 + 2*(KK+1)*BBBAR - 2
      DX(IN1  ) = DA(1)
      DX(IN1+1) = DA(2)
      GO TO 310
C
C     CREATE NEW ACTIVE ROW
C
  370 KK  = 0
  380 IN1 = I4SP + KK
      IF (IX(IN1) .EQ. 0) GO TO 390
      KK = KK + 1
      IMHERE = 380
      IF (KK-CBAR) 380,1710,1710
  390 IX(IN1) = II
      IN1     = IN1 + CBAR
      IX(IN1) = JPOS
      IN1     = I4 + 2*(KK+1)*BBBAR - 2
      DX(IN1  ) = DA(1)
      DX(IN1+1) = DA(2)
      CBCNT   = CBCNT + 1
      GO TO 310
C
C     ARRANGE ACTIVE ROW INDEXES IN SEQUENCE AND STORE THEM IN AREA VI
C
  400 IF (COUNT .EQ. CBCNT) GO TO 500
      IN1 = I6SP
      K   = 0
  410 IN2 = I4SP + K
      IMHERE = 410
      IF (IX(IN2)) 1710,430,440
  420 IN1 = IN1 + 1
  430 K   = K + 1
      IMHERE = 430
      IF (K-CBAR) 410,500,1710
  440 IF (IN1 .NE. I6SP) GO TO 450
      IX(IN1) = K
      GO TO 420
  450 KK  = 0
  460 IN3 = IN1 - KK
      IF (IN3 .GT. I6SP) GO TO 470
      IX(IN3) = K
      GO TO 420
  470 IN4 = I4SP + IX(IN3-1)
      IMHERE = 470
      IF (IX(IN2)-IX(IN4)) 490,1710,480
  480 IX(IN3) = K
      GO TO 420
  490 IX(IN3) = IX(IN3-1)
      KK = KK + 1
      GO TO 460
  500 CONTINUE
C
C     TEST FOR POSSIBLE MERGING BETWEEN AN INACTIVE-ACTIVE COLUMN AND
C     THE CURRENT PIVOTAL COLUMN
C
      IF (CCOUNT .EQ. 0) GO TO 600
      IN1 = IX(I7SP) + I3SP
      IMHERE = 505
      IF (IX(IN1)-JPOS) 1710,510,600
C
C     MERGE ACTIVE COLUMN AND CURRENT PIVOTAL COLUMN AND ZERO THAT
C     ACTIVE COLUMN IN AREA III
C
  510 IX(IN1) = 0
      IN1     = IN1 + C
      IX(IN1) = 0
      IN1     = I3 + 2*IX(I7SP)*BBAR1
      CCOUNT  = CCOUNT - 1
      KK      = 0
  520 IN2     = IN1 + KK + KK
      IN3     = I2  + KK + KK
      DX(IN3  ) = DX(IN3  ) + DX(IN2  )
      DX(IN3+1) = DX(IN3+1) + DX(IN2+1)
      DX(IN2  ) = 0.0
      DX(IN2+1) = 0.0
      KK = KK + 1
      IMHERE = 525
      IF (KK-BBAR1) 520,530,1710
C
C     MERGE INTERACTION ELEMENTS
C
  530 CONTINUE
      IF (CBCNT .EQ. 0) GO TO 580
      IN1 = I5 + 2*IX(I7SP)*CBAR
      K   = 0
  540 IN2 = I4SP + K
      IF (ABS(IX(IN2)) .LT. EPSI) GO TO 560
      IN3 = IN1 + K + K
      IF (ABS(DX(IN3)).LT.EPSI .AND. ABS(DX(IN3+1)).LT.EPSI)
     1    GO TO 560
      IF (IX(IN2) .GT. JPOS+BBAR) GO TO 570
C
C     STORE ELEMENT WITHIN THE LOWER BAND
C
      IN2 = I2 + 2*(IX(IN2)-IOFF)
      DX(IN2  ) = DX(IN2  ) - DX(IN3  )
      DX(IN2+1) = DX(IN2+1) - DX(IN3+1)
  550 DX(IN3  ) = 0.0
      DX(IN3+1) = 0.0
  560 K = K + 1
      IMHERE = 560
      IF (K-CBAR) 540,580,1710
C
C     STORE ELEMENT IN THE ACTIVE ROW
C
  570 IN2 = I4 + 2*(K+1)*BBBAR - 2
      DX(IN2+1) = DX(IN2+1) - DX(IN3+1)
      DX(IN3+1) = 0.0
      DX(IN2) = DX(IN2)-DX(IN3)
      DX(IN3) = 0.0
      GO TO 550
C
C     MOVE THE POINTERS IN AREA VII UP ONE
C
  580 IN1 = I7SP + CCOUNT - 1
      DO 590 I = I7SP,IN1
  590 IX(I) = IX(I+1)
      IX(IN1+1) = 0
  600 IF (LCOL .EQ. 0) GO TO 830
C     ****************************************************************
C     OPERATE ON THE CURRENT COLUMN OF A BY ALL PREVIOUS COLUMNS OF L,
C     MAKING NOTED INTERCHANGES AS YOU GO
C     ****************************************************************
      IF (SCRFLG .EQ. 0) GO TO 630
      IF (LCOL-(R-1)) 630,620,610
  610 PARM(2) = SR2FL
      CALL OPEN (*1680,SR2FL,IX(SR2BUF),RD)
  620 PARM(2) = SR3FL
      CALL OPEN (*1680,SR3FL,IX(SR3BUF),WRTREW)
  630 LL   = 0
      LLL  = 0
      LLLL = 0
C
C     PICK UP INTERCHANGE INDEX FOR COLUMN JPOSL + LL + 1
C
  640 IN1   = I1SP + LL
      INTCHN= IX(IN1)
      IN2   = I2 + LL + LL
      IF (INTCHN .EQ. 0) GO TO 650
C
C     PERFORM ROW INTERCHANGE
C
      IN1 = IN2 + 2*INTCHN
      DA(1)   = DX(IN1)
      DX(IN1) = DX(IN2)
      DX(IN2) = DA(1)
      DA(1)   = DX(IN1+1)
      DX(IN1+1) = DX(IN2+1)
      DX(IN2+1) = DA(1)
  650 CONTINUE
C
C     COMPUTE THE CONTRIBUTION FROM THAT COLUMN
C
      END = MIN0(BBAR1,NCOL-(JPOSL+LL))
      IF (ABS(DX(IN2)).LT.EPSI .AND. ABS(DX(IN2+1)).LT.EPSI)
     1    GO TO 720
      IN1 = I1 + 2*LLL*BBAR
      CALL SLOOP (DX(IN2+2),DX(IN1),DX(IN2),END-1)
      IF (CBCNT .EQ. 0) GO TO 720
C
C     TEST TO SEE IF AN INACTIVE-ACTIVE ROW CONTRIBUTION SHOULD BE
C     ADDED IN
C
      KKK = 0
  690 IN3 = I6SP + KKK
      IN1 = IX(IN3) + I4SP
      IF (IX(IN1) .GT. JPOS+BBAR) GO TO 720
      KK = IN1 + CBAR
      IF (IX(KK)      .GT. JPOSL+LL+1) GO TO 710
      IF (IX(IN1)-JPOSL-BBAR1 .LE. LL) GO TO 710
C
C     ADD IN EFFECT OF THE INACTIVE-ACTIVE ROW
C
      IN4 = I2 + 2*(IX(IN1)-IOFF)
      K   = I4 + 2*(JPOSL+BBBAR - JPOS+LL + IX(IN3)*BBBAR)
      DX1 = DX(K  )
      DX2 = DX(K+1)
      IF (ABS(DX1) .LT. EPSI) DX1 = 0.
      IF (ABS(DX2) .LT. EPSI) DX2 = 0.
      DX(IN4  ) = DX(IN4  ) - DX1*DX(IN2) + DX2*DX(IN2+1)
      DX(IN4+1) = DX(IN4+1) - DX(IN2+1)*DX1 - DX(IN2)*DX2
      IF (ABS(DX(IN4  )) .LT. EPSI) DX(IN4  ) = 0.
      IF (ABS(DX(IN4+1)) .LT. EPSI) DX(IN4+1) = 0.
  710 KKK = KKK + 1
      IF (KKK .LT. CBCNT) GO TO 690
  720 LL  = LL  + 1
      LLL = LLL + 1
      IF (LL .EQ. LCOL) GO TO 780
      IF (LL-R+1) 640,730,760
  730 IF (R .EQ. BBBAR1) GO TO 640
      IN1  = I1  + 2*LL*BBAR
  750 ICRQ = IN1 + BBAR*J4 - 1 - SR3BUF
      IF (ICRQ .GT. 0) GO TO 1715
      IBBAR4 = BBAR*J4
      CALL READ (*1690,*1700,SR2FL,DX(IN1),IBBAR4,0,FLAG)
      GO TO 640
  760 IN1 = I1 + 2*(LLL-1)*BBAR
      IF (LL.EQ.R .AND. LCOL.EQ.BBBAR1) GO TO 770
      CALL WRITE (SR3FL,DX(IN1),J4*BBAR,0)
  770 LLL = LLL - 1
      GO TO 750
  780 CONTINUE
C
C     COMPUTE ELEMENTS FOR THE ACTIVE ROWS
C
      IF (CBCNT .EQ. 0) GO TO 830
          JKK = I4SP + CBAR
          WRITE  (6,788) JPOS,BBAR,(IX(K),K=I4SP,JKK)
  788     FORMAT (' JPOS,BBAR,IX(I4SP...I4)=',12I6)
      K   = 0
  790 IN1 = I4SP + K
          WRITE  (6,791) K,I4SP,IN1,IX(IN1),JPOS,BBAR
  791     FORMAT (' CDCMPS/791  K,I4SP,IN1,IX(INX),JPOS,BBAR=',6I7)
      IF (IX(IN1) .GT. JPOS+BBAR) GO TO 810
  800 K   = K + 1
      IMHERE = 800
      IF (K-CBAR) 790,830,1710
  810 IN1 = IN1 + CBAR
      IF (IX(IN1) .EQ. JPOS) GO TO 800
      KKK = MAX0(0,BBBAR-JPOS+IX(IN1)-1)
          WRITE  (6,811) IN1,IX(IN1),JPOS,KKK
  811     FORMAT ('    CDCMPS/811  IN1,IX(IN1),JPOS,KKK=',4I10)
      IN2 = I4  + 2*K*BBBAR - 2
      IN3 = I2  + 2*(KKK-1-MAX0(0,BBBAR-JPOS))
      IN1 = IN2 + 2*BBBAR
      IN2 = IN2 + 2*KKK
  820 IN2 = IN2 + 2
      KKK = KKK + 1
      IN3 = IN3 + 2
      DX(IN1  ) = DX(IN1  ) - DX(IN2)*DX(IN3) + DX(IN2+1)*DX(IN3+1)
      DX(IN1+1) = DX(IN1+1) - DX(IN2+1)*DX(IN3) - DX(IN2)*DX(IN3+1)
      IMHERE = 825
          WRITE  (6,821) KKK,BBBAR1,IN1
  821     FORMAT ('         CDCMPS/821  KKK,BBBAR1,IN1=',3I7)
      IF (KKK-BBBAR1) 820,800,1710
C
C     SEARCH THE LOWER BAND FOR THE MAXIMUM ELEMENT AND INTERCHANGE
C     ROWS TO BRING IT TO THE DIAGONAL
C
  830 K   = 1
      IN1 = I2 + 2*(JPOS-IOFF)
      DX1 = 0.0
      DX2 = 0.0
      IF (ABS(DX(IN1  )) .GT. EPSI) DX1 = DX(IN1  )**2
      IF (ABS(DX(IN1+1)) .GT. EPSI) DX2 = DX(IN1+1)**2
      MAX(1) = DX1 + DX2
      INTCHN = 0
      END    = MIN0(BBAR1,NCOL-JPOS+1)
      IF (END .EQ. 1) GO TO 870
  840 IN2 = IN1 + K + K
      DX1 = 0.0
      DX2 = 0.0
      IF (ABS(DX(IN2  )) .GT. EPSI) DX1 = DX(IN2  )**2
      IF (ABS(DX(IN2+1)) .GT. EPSI) DX2 = DX(IN2+1)**2
      DX2 = DX2 + DX1
      IF (DX2 .GT. MAX(1)) GO TO 860
  850 K   = K + 1
      IMHERE = 850
      IF (K-END) 840,870,1710
  860 MAX(1) = DX2
      INTCHN = K
      GO TO 850
C
  870 IF(INTCHN.EQ.0)GO TO 880
C
C     INTERCHANGE ROWS IN AREA II
C
      DET(1)  =-DET(1)
      DET(2)  =-DET(2)
C
      MAX(1)  = DX(IN1)
      IN2     = IN1 + 2*INTCHN
      DX(IN1) = DX(IN2)
      DX(IN2) = MAX(1)
      MAX(1)  = DX(IN1+1)
      DX(IN1+1) = DX(IN2+1)
      DX(IN2+1) = MAX(1)
C
C     STORE THE PERMUTATION INDEX
C
      IN2 = I1SP + LCOL
      IX(IN2) = INTCHN
C
C     DIVIDE THE LOWER BAND BY THE DIAGONAL ELEMENT
C
  880 DX1 = 0.0
      DX2 = 0.0
      IF (ABS(DX(IN1  )) .GT. EPSI) DX1 = DX(IN1  )**2
      IF (ABS(DX(IN1+1)) .GT. EPSI) DX2 = DX(IN1+1)**2
      DA(1) = DX1 + DX2
      IF (ABS(DA(1)) .LT. EPSI) GO TO 1720
      MAX(1) = DX(IN1)/DA(1)
      MAX(2) =-DX(IN1+1)/DA(1)
      TEMP   = SQRT(DA(1))
      IF (MINDIA .LT. DBLE(TEMP)) MINDIA = DBLE(TEMP)
      DA(1)  = SNGL(DABS(DET(1)))
      TEMP   = SNGL(DABS(DET(2)))
      IF (TEMP .GT. DA(1)) DA(1) = TEMP
  890 IF (DA(1) .LE. 10.0) GO TO 900
      DET(1) = DET(1)*.1D0
      DET(2) = DET(2)*.1D0
      DA(1)  = DA(1) *.10
      POWER  = POWER + 1
      GO TO 890
  900 IF (DA(1) .GE. .1) GO TO 910
      DET(1) = DET(1)*10.D0
      DET(2) = DET(2)*10.D0
      DA(1)  = DA(1) *10.0
      POWER  = POWER - 1
      GO TO 900
  910 DA(1)  = DET(1)*DX(IN1) - DET(2)*DX(IN1+1)
      DET(2) = DET(2)*DX(IN1) + DET(1)*DX(IN1+1)
      DET(1) = DA(1)
      K   = 1
      END = MIN0(BBAR1,NCOL-JPOS+1)
      IF (END .EQ. 1) GO TO 930
  920 IN2   = IN1 + K + K
      DA(1) = DX(IN2)*MAX(1) - DX(IN2+1)*MAX(2)
      DX(IN2+1) = DX(IN2)*MAX(2) + DX(IN2+1)*MAX(1)
      DX(IN2  ) = DA(1)
      K = K + 1
      IMHERE = 930
      IF (K-END) 920,930,1710
  930 IF (CBCNT .EQ. 0) GO TO 950
C
C     DIVIDE THE ACTIVE ROWS BY THE DIAGONAL
C
      K   = 0
      IN1 = I4 + 2*BBBAR1
  940 DA(1) = DX(IN1)*MAX(1) - DX(IN1+1)*MAX(2)
      DX(IN1+1) = DX(IN1)*MAX(2) + DX(IN1+1)*MAX(1)
      DX(IN1  ) = DA(1)
      IN1 = IN1 + 2*BBBAR
      K   = K + 1
      IMHERE = 945
      IF (K-CBAR) 940,950,1710
  950 CONTINUE
C
C     INTERCHANGE ACTIVE COLUMNS AND ADD IN EFFECT OF THE CURRENT COLUMN
C
      IF (CCOUNT .EQ.   0) GO TO 1000
      IF (JPOS .LT. BBBAR) GO TO 1000
      INTCH = IX(I1SP)
      K   = 0
  960 IN1 = I3SP + K
      IF (INTCH .EQ. 0) GO TO 970
      IN1 = I3  + 2*K*BBAR1
      IN2 = IN1 + 2*INTCH
      DA(1)   = DX(IN1)
      DX(IN1) = DX(IN2)
      DX(IN2) = DA(1)
      DA(1)   = DX(IN1+1)
      DX(IN1+1) = DX(IN2+1)
      DX(IN2+1) = DA(1)
  970 KK  = 1
      IN2 = I1 - 2
      IN1 = I3 + 2*K*BBAR1
      IF (ABS(DX(IN1)).LT.EPSI .AND. ABS(DX(IN1+1)).LT.EPSI) GO TO 990
  980 IN3 = IN1 + 2*KK
      IN4 = IN2 + 2*KK
      DX(IN3  ) = DX(IN3)   - DX(IN1)*DX(IN4) + DX(IN1+1)*DX(IN4+1)
      DX(IN3+1) = DX(IN3+1) - DX(IN1)*DX(IN4+1) - DX(IN1+1)*DX(IN4)
      KK = KK + 1
      IMHERE = 985
      IF (KK-BBAR1) 980,990,1710
  990 K = K + 1
      IMHERE = 990
      IF (K-C) 960,1000,1710
C
C     WRITE OUT THE NEXT COLUMN OF U AND THE ROW OF ACTIVE ELEMENTS
C
 1000 PARM(2) = SR2FIL
      CALL BLDPK (CSP,TYPEL,SR2FIL,0,0)
      IN1 = I2
      JJ  = IOFF
 1010 DZ(1) = DX(IN1  )
      DZ(2) = DX(IN1+1)
      IF (ABS(DZ(1)).LT.EPSI .AND. ABS(DZ(2)).LT.EPSI) GO TO 1030
      CALL ZBLPKI
 1030 IN1 = IN1 + 2
      JJ  = JJ  + 1
      IF (JJ-JPOS) 1010,1010,1040
 1040 IF (ABS(DX(IN1-2)).LT.EPSI .AND. ABS(DX(IN1-1)).LT.EPSI)
     1    GO TO 1720
C
C     PACK ACTIVE COLUMN ELEMENTS ALSO
C
      IF (CCOUNT .EQ.   0) GO TO 1090
      IF (JPOS .LT. BBBAR) GO TO 1090
      K   = 0
 1060 IN1 = I7SP + K
      IN2 = IX(IN1)+I3SP
      GO TO 1080
 1070 K = K + 1
      IMHERE = 1070
      IF (K-CCOUNT) 1060,1090,1710
 1080 IN3   = I3 + 2*(IX(IN1)*BBAR1)
      DZ(1) = DX(IN3  )
      DZ(2) = DX(IN3+1)
      IF (ABS(DZ(1)).LT.EPSI .AND. ABS(DZ(2)).LT.EPSI) GO TO 1070
      JJ = IX(IN2)
      CALL ZBLPKI
      GO TO 1070
 1090 CALL BLDPKN (SR2FIL,0,FILEU)
C
C     COMPUTE ACTIVE ROW-COLUMN INTERACTION
C
      IF (CCOUNT.EQ.0 .OR. CBCNT.EQ.0) GO TO 1140
      IF (JPOS .LT. BBBAR) GO TO 1140
      K = 0
 1100 CONTINUE
      IN1 = I3 + 2*K*BBAR1
      IF (ABS(DX(IN1)).LT.EPSI .AND. ABS(DX(IN1+1)).LT.EPSI)
     1    GO TO 1130
      KK  = 0
 1110 IN2 = I4 + 2*KK*BBBAR
      IF (ABS(DX(IN2)).LT.EPSI .AND. ABS(DX(IN2+1)).LT.EPSI)
     1    GO TO 1120
      IN3 = I5 + 2*(K*CBAR+KK)
      DX(IN3  ) = DX(IN3  ) +DX(IN2)*DX(IN1  ) - DX(IN2+1)*DX(IN1+1)
      DX(IN3+1) = DX(IN3+1) +DX(IN2)*DX(IN1+1) + DX(IN2+1)*DX(IN1  )
 1120 KK = KK + 1
      IMHERE = 1120
      IF (KK-CBAR) 1110,1130,1710
 1130 K = K + 1
      IMHERE = 1130
      IF (K-C) 1100,1140,1710
C
C     MOVE ELEMENTS IN AREA III UP ONE CELL
C
 1140 IF (CCOUNT . EQ.  0) GO TO 1190
      IF (JPOS .LT. BBBAR) GO TO 1190
      K   = 0
 1150 IN1 = I3SP + K
      IF (IX(IN1) .EQ. 0) GO TO 1180
      KK  = 0
      IN1 = I3  + 2*K*BBAR1
 1160 IN2 = IN1 + KK + KK
      DX(IN2  ) = DX(IN2+2)
      DX(IN2+1) = DX(IN2+3)
      KK = KK + 1
      IMHERE = 1170
      IF (KK-BBAR) 1160,1170,1710
 1170 DX(IN2+2) = 0.0
      DX(IN2+3) = 0.0
 1180 K = K + 1
      IMHERE = 1180
      IF (K-C) 1150,1190,1710
C
C
C     DETERMINE IF A COLUMN OF L CAN BE WRITTEN OUT
C
 1190 IF (LCOL-BBBAR1) 1370,1200,1200
C
C     OUTPUT A COLUMN OF L
C
 1200 PARM(2) = FILEL(1)
      JPOSL   = JPOSL + 1
      CALL BLDPK (CSP,TYPEL,FILEL(1),0,0)
C
C     STORE THE PERMUTATION INDEX AS THE DIAGONAL ELEMENT
C
      JJ    = JPOSL
      DZ(1) = IX(I1SP)
      DZ(2) = 0.0
      CALL ZBLPKI
      K     = 0
 1210 JJ    = JPOSL + K   + 1
      IN2   = I1 + 2*K
      DZ(1) = DX(IN2)
      DZ(2) = DX(IN2+1)
      IF (ABS(DZ(1)).LT.EPSI .AND. ABS(DZ(2)).LT.EPSI) GO TO 1230
      CALL ZBLPKI
 1230 K = K + 1
      IMHERE = 1230
      IF (K-BBAR) 1210,1240,1710
C
C     PACK ACTIVE ROW ELEMENTS ALSO
C
 1240 IF (CBCNT .EQ. 0) GO TO 1280
      K   = 0
 1250 IN1 = I6SP + K
      IN2 = I4 + 2*(IX(IN1)*BBBAR)
      IN1 = IX(IN1) + I4SP
      JJ  = IX(IN1)
      DZ(1) = DX(IN2  )
      DZ(2) = DX(IN2+1)
      IF (ABS(DZ(1)).LT.EPSI .AND. ABS(DZ(2)).LT.EPSI) GO TO 1270
      CALL ZBLPKI
 1270 K = K + 1
      IMHERE = 1270
      IF (K-CBCNT) 1250,1280,1710
 1280 CALL BLDPKN (FILEL,0,FILEL)
C
C     MOVE PERMUTATION INDICES OVER ONE ELEMENT
C
      END = I1SP + LCOL
      DO 1290 I = I1SP,END
 1290 IX(I) = IX(I+1)
C
C     MOVE ELEMENTS IN AREA I OVER ONE COLUMN
C
      K = 0
      IF (SCRFLG .EQ. 0) GO TO 1310
      CALL CLOSE (SR2FL,REW)
      CALL OPEN  (*1680,SR2FL,IX(SR2BUF),RD)
      IF (R .GT. 2) GO TO 1310
      ICRQ = I1 + BBAR*J4 - 1 - SR3BUF
      IF (ICRQ .GT. 0) GO TO 1715
      IBBAR4 = BBAR*J4
      CALL READ (*1690,*1700,SR2FL,DX(I1),IBBAR4,0,FLAG)
      GO TO 1360
 1310 IN1 = I1  + 2*K*BBAR
      IN2 = IN1 + BBAR + BBAR
      CALL SXLOOP (DX(IN1),DX(IN2),BBAR)
      K = K + 1
      IMHERE = 1340
      IF (K-R+2) 1310,1340,1360
 1340 IF (R-BBBAR1) 1350,1310,1710
 1350 ICRQ = IN2 + BBAR*J4 - 1 - SR3BUF
      IF (ICRQ .GT. 0) GO TO 1715
      IBBAR4 = BBAR*J4
      CALL READ (*1690,*1700,SR2FL,DX(IN2),IBBAR4,0,FLAG)
 1360 LCOL = LCOL - 1
C
C     STORE CURRENT COLUMN OF L
C
 1370 IF (CBCNT .EQ. 0) GO TO 1420
C
C     MOVE ELEMENTS IN AREA IV UP ONE CELL
C
      K   = 0
 1380 IN1 = I4SP + K
      IF (IX(IN1) .EQ. 0) GO TO 1410
      KK  = 0
      IN1 = I4  + 2*K*BBBAR
 1390 IN2 = IN1 + KK + KK
      DX(IN2  ) = DX(IN2+2)
      DX(IN2+1) = DX(IN2+3)
      KK = KK + 1
      IMHERE = 1395
      IF (KK-BBBAR1) 1390,1400,1710
 1400 DX(IN2+2) = 0.0
      DX(IN2+3) = 0.0
 1410 K = K + 1
      IMHERE = 1410
      IF (K-CBAR) 1380,1420,1710
 1420 IF (SCRFLG .NE. 0) GO TO 1450
C
C     STORE COLUMN IN CORE
C
 1430 IN1 = I1 + 2*LCOL*BBAR
      END = MIN0(BBAR,NCOL-JPOS)
      IF (END .EQ. 0) GO TO 1480
      K   = 0
      IN3 = I2  + 2*(JPOS-IOFF+1)
 1440 IN2 = IN1 + K + K
      IN4 = IN3 + K + K
      DX(IN2  ) = DX(IN4  )
      DX(IN2+1) = DX(IN4+1)
      K   = K + 1
      IMHERE = 1445
      IF (K-END) 1440,1480,1710
C
C     STORE COLUMN ON THE SCRATCH FILE
C
 1450 IF (LCOL-R+1) 1430,1470,1460
 1460 IN1 = I1 + 2*(LLL-1)*BBAR
      CALL WRITE (SR3FL,DX(IN1),BBAR*J4,0)
 1470 IN1 = I2 + 2*(JPOS-IOFF+1)
      CALL WRITE (SR3FL,DX(IN1),BBAR*J4,0)
C
C     CLOSE SCRATCH FILES AND SWITCH THE POINTERS TO THEM
C
      CALL CLOSE (SR3FL,REW)
      CALL CLOSE (SR2FL,REW)
      IN1   = SR2FL
      SR2FL = SR3FL
      SR3FL = IN1
 1480 LCOL  = LCOL + 1
      IF (C    .EQ.     0) GO TO 1570
      IF (JPOS .LT. BBBAR) GO TO 1570
C
C     READ IN THE NEXT ROW OF ACTIVE COLUMN ELEMENTS
C
      COUNT = CCOUNT
      IF (ITRN .LT.         0) GO TO 1570
 1490 IF (ITRN .GT. JPOS-B+2) GO TO 1560
C
C     TEST TO SEE IF COLUMN IS ALREADY ACTIVE
C
      K   = 0
 1500 IN1 = I3SP + K
      IF (IX(IN1) .EQ. JTRN) GO TO 1540
      K = K + 1
      IMHERE = 1500
      IF (K-C) 1500,1510,1710
C
C     CREATE A NEW ACTIVE COLUMN
C
 1510 K   = 0
 1520 IN1 = I3SP + K
      IF (IX(IN1) .EQ. 0) GO TO 1530
      K = K + 1
      IMHERE = 1520
      IF (K-C) 1520,1710,1710
 1530 IX(IN1) = JTRN
      IN1     = IN1 + C
      IX(IN1) = ITRN
      IN1     = I3 + 2*(K+1)*BBAR1 - 2
      DX(IN1  ) = DTRN(1)
      DX(IN1+1) = DTRN(2)
      CCOUNT  = CCOUNT + 1
      GO TO 1550
C
C     STORE ELEMENT IN EXISTING COLUMN
C
 1540 IN1 = I3 + 2*(K+1)*BBAR1 - 2
      DX(IN1  ) = DX(IN1  ) + DTRN(1)
      DX(IN1+1) = DX(IN1+1) + DTRN(2)
 1550 CALL READ (*1690,*1700,SR1FIL,ITRAN(1),N64,0,FLAG)
      IF (ITRN .GT. 0) GO TO 1490
      CALL CLOSE (SR1FIL,REW)
 1560 IF (CCOUNT .EQ. COUNT) GO TO 1570
C
C     RE-ARRANGE INDEXES IN SEQUENTIAL ORDER
C
      ASSIGN 1570 TO KK
      GO TO 150
 1570 CONTINUE
      JPOS = JPOS + 1
C
C     ZERO AREA II
C
      END = I2 + 2*MIN0(JPOS-IOFF+BBAR-1,NCOL-1) + 1
      DO 1590 I = I2,END
 1590 DX(I) = 0.0
C
C      TEST TO SEE IF ROW INTERACTION ELEMENTS WILL MERGE INTO AREA III
C
      IF (CBCNT  .EQ. 0) GO TO 270
      IF (CCOUNT .EQ. 0) GO TO 1640
      IF (JPOS-1 .LT. BBBAR) GO TO 270
      IN1 = I4SP
      K   = 0
 1600 IN2 = IN1 + K
      IF (IX(IN2) .EQ. JPOS-B+1) GO TO 1610
      K   = K + 1
      IF (K .LT. CBAR) GO TO 1600
      GO TO 270
 1610 IN1 = I5 + K + K
      IN2 = I3 + BBAR + BBAR
      K   = 0
 1620 DX(IN2  ) = DX(IN2  ) - DX(IN1)
      DX(IN2+1) = DX(IN2+1) - DX(IN1+1)
      DX(IN1  ) = 0.0
      DX(IN1+1) = 0.0
      IN2 = IN2 + BBAR1 + BBAR1
      IN1 = IN1 + CBAR  + CBAR
      K   = K + 1
      IF (K .LT. C) GO TO 1620
C
C     TEST TO SEE IF A ACTIVE ROW HAS BEEN ELIMINATED
C
 1640 IN1 = IX(I6SP) + I4SP
      IF (IX(IN1)-JPOSL-BBAR1) 270,1650,270
C
C     ELIMINATE THE ACTIVE ROW
C
 1650 IX(IN1) = 0
      IN1     = IN1 + CBAR
      IX(IN1) = 0
      CBCNT   = CBCNT - 1
C
C     MOVE INDEXES IN AREA VI UP ONE
C
      IN1 = I6SP + CBCNT - 1
      DO 1660 I = I6SP,IN1
 1660 IX(I) = IX(I+1)
      IX(IN1+1) = 0
      GO TO 270
C
C     FINISH WRITING OUT THE COMPLETED COLUMNS OF L
C
 1670 CALL CLOSE (SR1FIL,REW)
      CALL CLOSE (FILEL,NOREW)
      CALL CLOSE (SR2FIL,NOREW)
      CALL COMFIN (ITERM,SCRFLG,SR2FL,JPOSL,I1SP,BBAR,I1,CBCNT,IPAK,R,
     1             BBBAR1,BBBAR,I6SP,I4,I4SP,IX,DX,X,LCOL)
      PARM(5) = IEND
      CALL CONMSG (PARM(3),3,0)
      FILEU(7) = BBBAR
      RETURN
C
C     ERROR EXITS
C
 1680 PARM(1) = -1
      GO TO 1730
 1690 PARM(1) = -2
      GO TO 1730
 1700 PARM(1) = -3
      GO TO 1730
 1710 PARM(1) = -25
      WRITE (NOUT,1711) IMHERE,
     1           I1SP,I1,I2,I3SP,I3,I4SP,I4,I5,I6SP,I7SP,END,
     2           BBAR,BBAR1,BBBAR,C,CBAR,NCOL,K,KK,KKK,
     3           IN1,IN2,IN4,IX(IN1),IX(IN2),IX(IN4),IX(I7SP),
     4           R,CCOUNT,CBCNT,JPOS
 1711 FORMAT (//,' IMHERE=',I5,/,
     1       '   I1SP,I1,I2,I3SP,I3,I4SP,I4,I5,I6SP,I7SP,END=',11I7,/,
     2       '   BBAR,BBAR1,BBBAR,C,CBAR,NCOL,K,KK,KKK=',9I8,/,
     3       '   IN1,IN2,IN4,IX(IN1),IX(IN2),IX(IN4),IX(I7SP)=',7I8,/,
     4       '   R,CCOUNT,CBCNT,JPOS=',4I8,/)
      GO TO 1730
 1715 PARM(1) = -8
      PARM(2) = ICRQ
      GO TO 1730
C
C     SINGULAR MATRIX - CLOSE ALL FILES AND RETURN TO USER
C
 1720 CALL CLOSE (FILEA(1),REW)
      CALL CLOSE (FILEL(1),REW)
      CALL CLOSE (FILEU(1),REW)
      CALL CLOSE (SR1FIL,  REW)
      CALL CLOSE (SR2FIL,  REW)
      CALL CLOSE (SR3FIL,  REW)
      FILEU(2) = BBBAR
      RETURN 1
 1730 CALL MESAGE (PARM(1),PARM(2),PARM(3))
      RETURN
      END
