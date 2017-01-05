      SUBROUTINE BUFCHK        
C        
      IMPLICIT INTEGER (A-Z)        
      LOGICAL          OD,OS,OR,STRDAT        
      INTEGER          I(4)        
      REAL             S(4)        
      DOUBLE PRECISION D(2)        
      CHARACTER*8      NAM,IIBL        
      COMMON /ZZZZZZ/  B(1)        
      COMMON /BUFCOM/  OFFSET,OUTBGN,OUTEND,OUTLVL        
      EQUIVALENCE      (I(1),S(1),D(1))        
      DATA             NOUT    ,  NAM      , IIBL      /        
     1                 6       ,  'BUFCHK@','II,B(L)=' /        
C VAX:        
      DATA             RECTRL  ,  RCTRLL   , RCTRLC  , COLHDR, COLTRL / 
     1                 '1'X    ,  '2'X     , '3'X    , '4'X  , '8'X   / 
      DATA             RECHDR  ,  RCHDST   , STRDUM  , EOBSTR   /       
     1                 'F1111'X,  'F2222'X , 'FAAAA'X, 'FBBBB'X /       
      DATA             EOB     ,  EOF      , STRHDR  , STRTRL   /       
     1                 'F5555'X,  'F7777'X , 'F8888'X, 'F9999'X /       
C UNIX:        
C     DATA             RECTRL  ,  RCTRLL   , RCTRLC  , COLHDR, COLTRL / 
C    1                 X'1'    ,  X'2'     , X'3'    , X'4'  , X'8'   / 
C     DATA             RECHDR  ,  RCHDST   , STRDUM  , EOBSTR   /       
C    1                 X'F1111',  X'F2222' , X'FAAAA', X'FBBBB' /       
C     DATA             EOB     ,  EOF      , STRHDR  , STRTRL   /       
C    1                 X'F5555',  X'F7777' , X'F8888', X'F9999' /       
C        
C*****        
      LSHIFT(K,J) = ISHFT(K, J)        
      RSHIFT(K,J) = ISHFT(K,-J)        
C     WHERE         ISHFT(K,+J) IS  LEFT-SHIFT K BY J BITS, ZERO FILL   
C                   ISHFT(K,-J) IS RIGHT-SHIFT K BY J BITS, ZERO FILL   
C     AND           ISHFT IS SYSTEM ROUTINE        
C        
C UNIX:        
C     REMOVE ABOVE 2 ON-LINE FUNCTIONS IF THE SYSTEM ISHFT FUNCTION IS  
C     NOT AVAILABLE.  LSHIFT AND RSHIFT ARE ALREADY ENTRY POINTS IN     
C     SUBROUTINE MAPFNS.        
C*****        
C        
      L      = OFFSET - 1        
      DATBGN = L + 1        
      DATEND = L + 8        
      DATTYP = 5        
      SKP    = 0        
C        
      DO 700 II = 1,10000        
      IF (II .LE. OUTEND) GO TO 110        
      IF (OD) WRITE (NOUT,100) NAM,II        
  100 FORMAT (5X,A7,'100  LIMIT POINTER REACHED.  II=',I6)        
      GO TO 900        
  110 L   = L + 1        
      OD  = II.GE.OUTBGN .AND. OUTLVL.GT.0        
      OS  = II.GE.OUTBGN .AND. OUTLVL.GT.1        
      OR  = II.GE.OUTBGN .AND. OUTLVL.GT.2        
      SKP = SKP - 1        
      IF (SKP .GT. 0) GO TO 700        
      IF (L.GE.DATBGN .AND. L.LE.DATEND) GO TO 500        
      W   = B(L)        
      F1  = RSHIFT(       W    ,28)        
      F2  = RSHIFT(LSHIFT(W, 4),16)        
      F3  = RSHIFT(LSHIFT(W,20),20)        
      F12 = RSHIFT(       W    ,12)        
      F31 = RSHIFT(LSHIFT(W,20),24)        
      F32 = RSHIFT(LSHIFT(W,28),28)        
      IF (F12 .NE. RECHDR) GO TO 160        
      DATBGN = L + 1        
      DATEND = L + F3        
      DATTYP = 5        
      IF (OD) WRITE (NOUT,150) NAM,IIBL,II,B(L)        
  150 FORMAT (5X,A7,'150  REC HDR - NO STRING.  ',A8,I6,Z8)        
      GO TO 700        
  160 IF (F12 .NE. RCHDST) GO TO 180        
      IF (OD) WRITE (NOUT,170) NAM,IIBL,II,B(L)        
  170 FORMAT (5X,A7,'170  REC HDR - STRING.  ',A8,I6,Z8)        
      STRDAT = .TRUE.        
      GO TO 700        
  180 IF (.NOT.(F1.EQ.RECTRL .OR. F1.EQ.RCTRLL)) GO TO 210        
      IF (OD) WRITE (NOUT,200) NAM,IIBL,II,B(L)        
  200 FORMAT (5X,A7,'200  REC TRAILR - END OF RECORD.  ',A8,I6,Z8)      
      GO TO 700        
  210 IF (F1 .NE. RCTRLC) GO TO 240        
      IF (OD) WRITE (NOUT,230) NAM,IIBL,II,B(L)        
  230 FORMAT (5X,A7,'230  REC TRAILER - RECORD CONTINUES.  ',A8,I6,Z8)  
      GO TO 700        
  240 IF (F12 .NE. EOB) GO TO 260        
      IF (OD) WRITE (NOUT,250) NAM,IIBL,II,B(L)        
  250 FORMAT (5X,A7,'250  END OF BLOCK.  ',A8,I6,Z8)        
      GO TO 700        
  260 IF (F12 .NE. EOF) GO TO 280        
      IF (OD) WRITE (NOUT,270) NAM,IIBL,II,B(L)        
  270 FORMAT (5X,A7,'270  END OF FILE.  ',A8,I6,Z8)        
      GO TO 700        
  280 IF (F1 .NE. COLHDR) GO TO 310        
      IF (OD) WRITE (NOUT,300) NAM,IIBL,II,B(L),F2        
  300 FORMAT (5X,A7,'300  COLUMN HEADER.  ',A7,',F2=',I6,Z8,I8)        
      DATTYP = F31        
      GO TO 700        
  310 IF (F1 .NE. COLTRL) GO TO 340        
      IF (OD) WRITE (NOUT,330) NAM,IIBL,II,B(L),F2        
  330 FORMAT (5X,A7,'330  COLUMN TRAILER.  ',A7,',F2=',I6,Z8,I8)        
      GO TO 700        
  340 IF (F12 .NE. STRHDR) GO TO 360        
      IF (OD) WRITE (NOUT,350) NAM,IIBL,II,B(L),B(L+1),F3        
  350 FORMAT (5X,A7,'350  STRING HEADER.  ',A7,',B(L+1),F3=',I6,2Z8,I8) 
      SKP    = 2        
      DATBGN = L + 1        
      DATEND = L + F3        
      GO TO 700        
  360 IF (F12 .NE. STRTRL) GO TO 380        
      IF (OD) WRITE (NOUT,370) NAM,IIBL,II,B(L),B(L+1),F3        
  370 FORMAT (5X,A7,'370  STRING TRAILER.  ',A7,',B(L+1),F3=',I6,2Z8,I8)
      SKP = 2        
      GO TO 700        
  380 IF (F12 .NE. STRDUM) GO TO 410        
      IF (OD) WRITE (NOUT,400) NAM,IIBL,II,B(L)        
  400 FORMAT (5X,A7,'400  STRING DUMMY WORD.  ',A8,I6,Z8)        
      GO TO 700        
  410 WRITE (NOUT,430) NAM,IIBL,II,B(L)        
  430 FORMAT (5X,A7,'430  INVALID CONTROL WORD.  ',A8,I6,Z8)        
      GO TO 800        
C        
  500 CONTINUE        
      DO 510 J = 1,4        
  510 I(J) = B(L+J-1)        
      GO TO (610,620,630,640,650) DATTYP        
      WRITE (NOUT,550) NAM,DATTYP        
  550 FORMAT (5X,A7,'550  BAD DATA TYP,  DATTYP=',I6)        
      GO TO 800        
  610 IF (OS) WRITE (NOUT,615) NAM,II,S(1)        
  615 FORMAT (5X,A7,'615  II,S(1)=',I6,E13.6)        
      GO TO 700        
  620 IF (OS) WRITE (NOUT,625) NAM,II,D(1)        
  625 FORMAT (5X,A7,'625  II,D(1)=',I6,D17.9)        
      SKP = 2        
      GO TO 700        
  630 IF (OS) WRITE (NOUT,635) NAM,II,S(1),S(2)        
  635 FORMAT (5X,A7,'635  II,S(1),S(2)=',I6,2E13.6)        
      SKP = 2        
      GO TO 700        
  640 IF (OS) WRITE (NOUT,645) NAM,II,D(1),D(2)        
  645 FORMAT (5X,A7,'645  II,D(1),D(2)=',I6,2D17.9)        
      SKP = 4        
      GO TO 700        
  650 IF (OR) WRITE (NOUT,660) NAM,II,I(1)        
  660 FORMAT (5X,A7,'615  II,I(1)=',I6,I14)        
  700 CONTINUE        
C        
      WRITE (NOUT,750) NAM        
  750 FORMAT (5X,A7,'750  BLOCK TOO LONG')        
  800 CALL VAXEND        
  900 RETURN        
      END        
