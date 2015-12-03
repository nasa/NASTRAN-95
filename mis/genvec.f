      SUBROUTINE GENVEC (*,IBUF,FILEA,NX,IX,NCOL,B,BBAR,C,CBAR,R,IENTRY)
C
C     GENVEC WILL PICK THE OPTIMUM VALUE OF B AND BBAR FOR A GIVEN
C     MATRIX
C
      INTEGER         FILEA(1)  ,NAME(2)  ,BMAX     ,CMAX     ,
     1                IX(2)     ,RSP      ,EOL      ,SYSBUF   ,
     2                B         ,BBAR     ,C        ,CBAR     ,
     3                R         ,BB       ,CC       ,BBR      ,
     4                CCR       ,RRR      ,BBR1     ,CCR1     ,
     5                BBR2      ,CCR2     ,RR1      ,RR2      ,
     6                P         ,DBNAME(2),FINDC    ,NAMIN(2,2)
      DIMENSION       IBUF(1)   ,XMB(2)
      CHARACTER       UFM*23    ,UWM*25   ,UIM*29
      COMMON /XMSSG / UFM       ,UWM      ,UIM
      COMMON /NAMES / RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                REW       ,NOREW    ,EOFNRW   ,RSP
      COMMON /ZNTPKX/ IA(4)     ,II       ,EOL
C     COMMON /DESCRP/ LENGTH    ,MAJOR(1)
      COMMON /NTIME / LNTIME    ,TCONS(15)
      COMMON /SYSTEM/ ISTV(65)
      COMMON /DCOMPX/ DUM(35)   ,ISYM
      EQUIVALENCE     (ISTV( 1),SYSBUF)   ,(ISTV( 2),NOUT  )  ,
     1                (ISTV(55),P     )   ,(TCONS(8),XMB(1))
      DATA    NAME  / 4HGENV,4HEC  /  ,CMAX / 200 /,
     1        NAMIN / 4H REA,1HL   ,4HCOMP,3HLEX  /
C
C
      CALL FNAME (FILEA,DBNAME)
      CALL SSWTCH (11,L11)
      IF (L11 .NE. 0) WRITE (NOUT,6) FILEA
    6 FORMAT ('O*** DIAG 11 OUTPUT FROM GENVEC (UNSYMMETRIC DECOMP) FOR'
     1,       ' FILE',I6 , /9X,1HB,6X,4HBBAR,9X,1HC,6X,4HCBAR,9X,1HR,3X,
     2        4HTIME )
C
      BMAX = MIN0(IFIX(1.0E+05/SQRT(FLOAT(NCOL)*XMB(P))),NCOL)
      IFILE= FILEA(1)
      CALL OPEN (*280,FILEA(1),IBUF,RDREW)
      I1   = NCOL
      I4   = 4*NCOL + 2*CMAX
      ICRQ = I4 - NX + SYSBUF
      IF (I4 .GT. NX-SYSBUF) GO TO 300
      DO 10 I = 1,I4
   10 IX(I) = 0
      NMAX  = 0
      MMAX  = 0
      CALL FWDREC (*290,FILEA(1))
C
C     GENERATE THE ROW AND COLUMN VECTORS
C
      DO 60 I = 1,NCOL
      CALL INTPK (*320,FILEA(1),0,RSP,0)
      CALL ZNTPKI
      IN1  = I1 + I
      IX(IN1) = II
      NMAX = MAX0(NMAX,I-II+1)
   20 IF (IX(II)) 40,30,40
   30 IX(II) = I
      MMAX = MAX0(MMAX,II-I+1)
   40 IF (EOL) 60,50,60
   50 CALL ZNTPKI
      GO TO 20
   60 CONTINUE
      CALL CLOSE (FILEA(1),REW)
      I2   = I1 + NCOL + 1
      I3   = I2 + 2*NCOL
      NMAX = MIN0(NMAX,BMAX)
      MMAX = MIN0(MMAX,BMAX)
      MMAX = MAX0(MMAX,2)
C
C     SET UP ACTIVE COLUMN BANDWIDTH VECTOR
C
      DO 100 I = 2,NCOL
      J = NCOL - I + 1
      ICOUNT = 0
      DO 90 K = 1,J
      L = I2 - K
      IF (IX(L)-I) 70,80,80
   70 ICOUNT = ICOUNT + 1
   80 L = I2 + (J-K)*2
   90 IX(L) = MAX0(IX(L),ICOUNT)
  100 CONTINUE
C
C     REDUCE LIST TO UNIQUE PAIRS
C
      I = I2
      J = I2 + 2
      K = 2
  110 IF (IX(J) .EQ. 0) GO TO 140
      IF (IX(J) -IX(I)) 120,130,120
  120 I = I + 2
      IX(I  ) = IX(J)
      IX(I+1) = K
  130 J = J + 2
      K = K + 1
      GO TO 110
  140 CONTINUE
      I = I + 2
      IX(I  ) = 0
      IX(I+1) = K
      ILAST   = 0
C
C     BEGIN SEARCH FOR B,BBAR
C
      TIME = 1000000.
      B    = 0
      BBAR = 0
      C    = 0
      CBAR = 0
  150 BB = IX(I+1)
      IF (BB .LE. BMAX) GO TO 155
      I  = I - 2
      GO TO 150
  155 CONTINUE
C
C    MAKE PRELIMINARY SEARCH
C
      TT1 = 1000000.
  156 CONTINUE
      BB  = IX(I+1)
      CC  = IX(I) + 1
      IF (CC .EQ. 1) CC = 0
      BBR = BB
      CCR = CC
      CALL RCORE (BB,BBR,CC,CCR,NCOL,IENTRY,NX,RRR)
      RRR = MIN0(RRR,BB+BBR-1,NCOL-1)
      IF (RRR .LT. 2) GO TO 157
      CALL TIMEEQ (FLOAT(BB),FLOAT(BBR),FLOAT(CC),FLOAT(CCR),FLOAT(RRR),
     1             IENTRY,NCOL,TT)
      IF (ILAST .EQ. 0) ILAST = I
      IF (L11   .EQ. 0) GO TO 1500
      WRITE  (NOUT,151) BB,BBR,CC,CCR,RRR,TT
  151 FORMAT (5I10,F10.2)
 1500 CONTINUE
      IF (TT .GT. TT1) GO TO 157
      TT1  = TT
      BBR1 = BBR
      CCR1 = CCR
      RR1  = RRR
  157 I    = I - 2
      IF (BB .LT.   3) GO TO 158
      IF (I .GE. I2+2) GO TO 156
  158 CONTINUE
      I  = I + 2
      IF (TT1 .EQ. 1000000.)GO TO 300
      BB = BBR1
      CC = CCR1
      TT1= 1000000.
C
C     SEARCH ON INCREASING BBAR
C
  159 BBR = BB
      INCRXX = MAX1(.02*FLOAT(BB),1.)
  160 CCR = FINDC(BB,BBR,NCOL,IX(1),IX(I3))
      CALL RCORE (BB,BBR,CC,CCR,NCOL,IENTRY,NX,RRR)
      RRR = MIN0(RRR,BB+BBR-1)
      RRR = MIN0(RRR,NCOL-1)
      IF (RRR .LT. 2) GO TO 170
      CALL TIMEEQ (FLOAT(BB),FLOAT(BBR),FLOAT(CC),FLOAT(CCR),FLOAT(RRR),
     1             IENTRY,NCOL,TT)
      IF (L11 .EQ. 0) GO TO 1600
      WRITE (NOUT,151) BB,BBR,CC,CCR,RRR,TT
 1600 CONTINUE
      IF (TT1 .EQ. 1000000.) TT1 = TT
      IF (TT  .GT. TT1) GO TO 170
      TT1  = TT
      BBR1 = BBR
      CCR1 = CCR
      RR1  = RRR
  170 CONTINUE
      BBR = BBR + INCRXX
      IF (TT .GT. 1.2*TT1) GO TO 180
      IF (CCR  .EQ.     0) GO TO 180
      IF (BBR  .LT.  BMAX) GO TO 160
C
C     BEGIN SEARCH ON DECREASING BBAR
C
  180 TT2 = 1000000.
      BBR = BB - INCRXX
  190 IF (BBR .LE. 2 ) GO TO 210
      CCR = FINDC(BB,BBR,NCOL,IX(1),IX(I3))
      CALL RCORE (BB,BBR,CC,CCR,NCOL,IENTRY,NX,RRR)
      RRR = MIN0(RRR,BB+BBR-1)
      RRR = MIN0(RRR,NCOL-1)
      IF (RRR .LT. 2) GO TO 200
      CALL TIMEEQ (FLOAT(BB),FLOAT(BBR),FLOAT(CC),FLOAT(CCR),FLOAT(RRR),
     1             IENTRY,NCOL,TT)
      IF (L11 .EQ. 0) GO TO 195
      WRITE (NOUT,151) BB,BBR,CC,CCR,RRR,TT
  195 CONTINUE
      IF (TT2 .EQ. 1000000.) TT2 = TT
      IF (TT  .GT. TT2) GO TO 200
      TT2  = TT
      BBR2 = BBR
      CCR2 = CCR
      RR2  = RRR
  200 CONTINUE
      BBR  = BBR - INCRXX
      IF (TT .GT. 1.20*TT2) GO TO 210
      GO TO 190
  210 CONTINUE
      IF (TT1 .GE. TIME) GO TO 220
      TIME = TT1
      B    = BB
      C    = CC
      BBAR = BBR1
      CBAR = CCR1
      R    = RR1
  220 IF (TT2 .GE. TIME) GO TO 230
      TIME = TT2
      B    = BB
      C    = CC
      BBAR = BBR2
      CBAR = CCR2
      R    = RR2
  230 IF (TT1.EQ.1000000. .AND. TT2.EQ.1000000.) GO TO 275
      IB = B
      IC = C
      IBBAR = BBAR
      ICBAR = CBAR
      IR    = R
      IX(1) = C
      IX(2) = R
      CALL PAGE2 (4)
      WRITE  (NOUT,240) UIM,B,BBAR,C,CBAR,R
  240 FORMAT (A29,' 3028',6X,3HB =,I5,5X,6HBBAR =,I5, /40X,3HC =,I5,5X,
     1       6HCBAR =,I5, /40X,3HR =,I5)
      CALL TFIN (FLOAT(B),FLOAT(BBAR),FLOAT(C),FLOAT(CBAR),FLOAT(R),
     1           IENTRY,FLOAT(NCOL),TIME)
      IX(1) = TIME
      CALL PAGE2 (3)
      WRITE  (NOUT,250) UIM,NAMIN(1,IENTRY),NAMIN(2,IENTRY),DBNAME,NCOL,
     1                  IX(1)
  250 FORMAT (A29,' 3027, UNSYMMETRIC ',2A4,' DECOMPOSITION OF DATA ',
     1       'BLOCK ',2A4,6H (N = ,I5,1H), /5X,'TIME ESTIMATE = ',I8,
     2       8H SECONDS)
      CALL TMTOGO (IXY)
      IF (IXY .LT. IX(1)) CALL MESAGE (-50,IX(1),NAME)
      RETURN
C
C     TRY TO FIND POSSIBLE SOLUTION WITHIN FEASIBLE RANGE BY VARYING  BB
C
  275 I  = I + 2
      IF (I .GT. ILAST) GO TO 300
      BB = IX(I+1)
      CC = IX(I) + 1
      IF (BB .GT. BMAX) GO TO 300
      GO TO 159
  280 NO = -1
      GO TO 310
  290 NO = -2
      GO TO 310
  300 NO = -8
      IFILE = ICRQ
  310 CALL MESAGE (NO,IFILE,NAME)
      RETURN
C
C     NULL COLUMN DISCOVERED
C
  320 WRITE  (NOUT,325) UFM,I,NAMIN(1,IENTRY),NAMIN(2,IENTRY)
  325 FORMAT (A23,' 3097, COLUMN',I7,' IS SINGULAR.  UNSYMMETRIC ',2A4,
     1       'DECOMP ABORTED.')
      RETURN 1
C
      END
