      SUBROUTINE RCOVSL (NAME,ITEM,IN,AMAT,SCR2,SCR3,OUT,Z,IZ,LCORE,
     1                   FIRST,RFNO)
C
C     RCOVSL CALCULATES THE STATIC LOAD VECTORS FOR THE SUBSTRUCTURING
C     PHASE 2 AND PHASE 3 OPERATIONS FROM THE SUBSTRUCTURE SOLN ITEM
C
      LOGICAL         FIRST
      INTEGER         NAME(2),AMAT,SCR2,SCR3,OUT,PMX,FMX,CMX,SLMX,T,
     1                SIGNPF,SIGNC,PREC,SCR,RD,RDREW,WRT,WRTREW,REW,
     2                OTYPP,SYSBUF,SOLN,SRD,SUBR(2),BUF1,FSS(2),IBUF(3),
     3                IZ(1),RC,RFNO,TYPE
      REAL            Z(1)
      COMMON /MPYADX/ PMX(7),FMX(7),CMX(7),SLMX(7),MCORE,T,SIGNPF,SIGNC,
     1                PREC,SCR
      COMMON /NAMES / RD,RDREW,WRT,WRTREW,REW,NOREW
      COMMON /PACKX / ITYPP,OTYPP,IROWP,NROWP,INCP
      COMMON /SYSTEM/ SYSBUF,NOUT
      DATA    SOLN  / 4HSOLN /, SRD   / 1 /
      DATA    SUBR  / 4HRCOV ,  4HSL  /
C
C     INITIALIZE
C
      BUF1  = LCORE - SYSBUF + 1
      ITYPP = 1
      IROWP = 1
      INCP  = 1
      MCORE = LCORE
      T     = 0
      SIGNPF= 1
      PREC  = 0
C
C     READ LOAD MATRIX FROM SOF ONTO GINO FILE
C
      PMX(1) = IN
      CALL RDTRL (PMX)
      IF (PMX(1) .GT. 0) GO TO 5
      ITM = ITEM
      CALL MTRXI (SCR2,NAME,ITEM,Z(BUF1),RC)
      IF (RC .EQ. 3) GO TO 600
      IF (RC .NE. 1) GO TO 1000
      PMX(1) = SCR2
      CALL RDTRL (PMX)
    5 NROWP = PMX(2)
      TYPE  = PMX(5)
      IF (RFNO .EQ. 8 .AND. TYPE .LE. 2) TYPE = TYPE + 2
      OTYPP = TYPE
      IF (FIRST) GO TO 500
C
C     PROCESS INITIAL SOLN DATA
C
      ITM = SOLN
      CALL SFETCH (NAME,SOLN,SRD,RC)
      IF (RC .NE. 1) GO TO 1000
      CALL SUREAD (FSS,2,N,RC)
      IF (RC .NE. 1) GO TO 1100
      CALL SUREAD (IBUF,3,N,RC)
      IF (RC .NE. 1) GO TO 1100
      IF (RFNO .EQ. 3) GO TO 600
      NB  = IBUF(2)
      NST = IBUF(3)
C
C     INTILIZE SCR1 FILE
C
      CALL MAKMCB (FMX,AMAT,NROWP,2,TYPE)
      CALL GOPEN (AMAT,Z(BUF1),WRTREW)
C
C     PACK FACTOR MATRIX FOR R. F. 1,2
C
      IF (RFNO.EQ.8 .OR. RFNO.EQ.9) GO TO 100
      DO 40 I = 1,NST
      DO 10 J = 1,NROWP
      Z(J) = 0.0
   10 CONTINUE
      N = 1
      CALL SJUMP (N)
      IF (N .LT. 0) GO TO 1200
      CALL SUREAD (NL,1,N,RC)
      IF (RC .NE. 1) GO TO 1100
      IF (NL .LT. 0) GO TO 40
      IF (NL .EQ. 0) GO TO 30
      IF (NROWP+2*NL .GE. BUF1) CALL MESAGE (-8,0,SUBR)
      CALL SUREAD (Z(NROWP+1),2*NL,N,RC)
      IF (RC .NE. 1) GO TO 1100
      NROW = NROWP - 1
      DO 20 J = 1,NL
      NROW = NROW + 2
      NR   = IZ(NROW)
      Z(NR)= Z(NROW+1)
   20 CONTINUE
   30 CALL PACK (Z(1),AMAT,FMX)
   40 CONTINUE
      CALL CLOSE (AMAT,REW)
      CALL WRTTRL(FMX)
      GO TO 500
C
C     PACK FACTOR MATRIX FOR R. F. 8,9
C
  100 CALL SUREAD (IZ(1),3*NB,N,RC)
      IF (RC .NE. 1) GO TO 1100
      CALL SUREAD (NL,1,N,RC)
      IF (RC .NE. 1) GO TO 1100
      IF (NL .LE. 0) GO TO 600
      IF (NL .GE. BUF1) CALL MESAGE (-8,0,SUBR)
      CALL SUREAD (IZ(1),NL,N,RC)
      IF (RC .NE. 1) GO TO 1100
      N  = 1
      CALL SJUMP (N)
      IF (N .LT. 0) GO TO 1200
      IP = 1
      IF (RFNO .EQ. 8) IP = 2
      IF (RFNO .EQ. 8) ITYPP = 3
      IFACT = NL + 1
      NFACT = NL + NL*IP
      ICOL  = NFACT + 1
      NCOL  = NFACT + IP*NROWP
      IF (NCOL .GE. BUF1) CALL MESAGE (-8,0,SUBR)
C
      DO 230 I = 1,NST
      DO 210 J = ICOL,NCOL
      Z(J) = 0.0
  210 CONTINUE
      N = 1
      CALL SJUMP (N)
      IF (N .LT. 0) GO TO 1200
      CALL SUREAD (Z(IFACT),NL*IP,N,RC)
      IF (RC .NE. 1) GO TO 1100
      NROW = IFACT - IP
      NRS  = ICOL  - IP
      DO 220 J = 1,NL
      NROW = NROW + IP
      NR   = NRS  + IZ(J)*IP
      Z(NR)= Z(NROW)
      IF (IP .EQ. 2) Z(NR+1) = Z(NROW+1)
  220 CONTINUE
      CALL PACK (Z(ICOL),AMAT,FMX)
  230 CONTINUE
      CALL CLOSE (AMAT,REW)
      CALL WRTTRL (FMX)
C
C     OUT = LOADS*FACTORS
C
  500 FMX(1) = AMAT
      CALL RDTRL (FMX)
      CMX(1) = 0
      CALL MAKMCB (SLMX,OUT,PMX(3),2,TYPE)
      SCR = SCR3
      CALL MPYAD (Z,Z,Z)
      CALL WRTTRL (SLMX)
      GO TO 700
C
C     NO SCALAR LOADS
C
  600 OUT = 0
      CALL CLOSE (AMAT,REW)
  700 RETURN
C
C     ERRORS
C
 1000 CALL SMSG (RC-2,ITM,NAME)
      GO TO 600
 1100 CALL SMSG (RC+4,ITM,NAME)
      GO TO 600
 1200 CALL SMSG (7,ITM,NAME)
      GO TO 600
      END
