      SUBROUTINE EXFORT (RW,U,F,BUF,NWDS,PREC,DBUF)        
C*****        
C        
C         *** IBM 360/370, VAX/780 VERSION ***        
C        
C     EXFORT PERFORMS FORTRAN FORMATTED IO FOR MODULE EXIO        
C        
C*****        
      INTEGER          RW,U,F,BUF(NWDS),PREC,FP,FMT,FRMT(10)        
      DOUBLE PRECISION DBUF(1)        
      COMMON /BLANK /  X1(26),LBUF        
      COMMON /EXIO2P/  NF,FP(5,1)        
      COMMON /EXIO2F/  FMT(1)        
C     COMMON /EXIO2X/  ==> /ZZEXO2/ UNIVAC ONLY        
C        
      DATA    LEOF  / 4H&EOF/        
C        
      IF (NWDS .LE. 0) RETURN        
      IF (F    .LE. 0) GO TO 8        
      IFMT = FP(1,F)-1        
      DO 5 I = 1,10        
    5 FRMT(I) = FMT(IFMT+I)        
    8 GO TO (10,20,80,150), RW        
   10 GO TO (30,50), PREC        
   20 GO TO (40,60), PREC        
C        
C     READ -- SINGLE PRECISION        
C        
   30 READ (U,FRMT,ERR=35) BUF        
   35 IF (BUF(1) .EQ. LEOF) GOTO 70        
      RETURN        
C        
C     WRITE -- SINGLE PRECISION        
C        
   40 WRITE (U,FRMT,ERR=45) BUF        
   45 RETURN        
C        
C     READ -- DOUBLE PRECISION        
C        
   50 N = NWDS/3        
      READ (U,FRMT) (BUF(4*I-3),DBUF(2*I),I=1,N)        
      RETURN        
C        
C     WRITE -- DOUBLE PRECISION        
C        
   60 N = NWDS/3        
      WRITE (U,FRMT) (BUF(4*I-3),DBUF(2*I),I=1,N)        
      RETURN        
C        
C     END OF FILE        
C        
   70 BUF(3) = -1        
      RETURN        
C        
C     POSITION THE FILE        
C        
   80 GO TO (90,100,100), NWDS        
   90 REWIND U        
      RETURN        
  100 N = LBUF/33+1        
      DO 110 I = 1,N        
  110 BACKSPACE U        
  120 READ (U,160) N        
      IF (N .NE. LEOF) GO TO 120        
      BACKSPACE U        
      RETURN        
C        
C     WRITE LOGICAL EOF        
C        
  150 N = LBUF/33        
      DO 170 I = 1,N        
      WRITE  (U,160) LEOF        
  160 FORMAT (A4,128X)        
  170 CONTINUE        
      RETURN        
      END        
