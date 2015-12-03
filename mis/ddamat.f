      SUBROUTINE DDAMAT
C
C     DDAMAT  A,B/C/C,Y,GG=1.  $
C
C     DDAMAT  TAKES THE OUTER PRODUCT OF MATRICES A AND B, AND MULTIPLES
C     BY GG TO GET C, I.E.  CIJ=GG*(AIJ*BIJ).  ALSO, IF B HAS ONLY ONE
C     COLUMN, AND NUMBER OF COLUMNS OF A .GT. 1, THEN USE THAT COLUMN
C     ON EACH COLUMN OF A.
C
      INTEGER          A,B,C,BUF1,BUF2,BUF3
      DOUBLE PRECISION DZ(1), DGG, GGDZ
      DIMENSION        NAM(2),MCB(7)
      COMMON /UNPAKX/  JOUT,III,NNN,JNCR
      COMMON /PACKX /  IIN,IOUT,II,NN,INCR
      COMMON /SYSTEM/  IBUF(80)
      COMMON /BLANK /  GG
      COMMON /ZZZZZZ/  Z(1)
      EQUIVALENCE      (IPREC,IBUF(55)),(Z(1),DZ(1))
      DATA    A,B,C /  101,102,201  /
      DATA    NAM   /  4HDDAM,4HAT  /
C
C     SET PACK AND UNPACK PARAMETER
C
      JOUT = IPREC
      IIN  = IPREC
      IOUT = IPREC
      INCR = 1
      JNCR = 1
      II   = 1
      III  = 1
C
C     SET OPEN CORE
C
      LCORE = KORSZ(Z)
      BUF1  = LCORE - IBUF(1) + 1
      BUF2  = BUF1 - IBUF(1)
      BUF3  = BUF2 - IBUF(1)
      LCORE = BUF3 - 1
      IF (LCORE .LE. 0) GO TO 1008
C
      MCB(1) = A
      CALL RDTRL (MCB)
      NCOLA  = MCB(2)
      NROWA  = MCB(3)
      MCB(1) = B
      CALL RDTRL (MCB)
      NCOLB  = MCB(2)
      NROWB  = MCB(3)
      IF (NROWA .NE. NROWB) GO TO 1007
      IF (LCORE .LT. 2*NROWA*IPREC) GO TO 1008
C
C     NO. OF COLUMNS OF A AND B MUST BE EQUAL OR
C     NO. OF COLUMNS OF B MUST BE 1
C
      IF (NCOLA .EQ. NCOLB) GO TO 5
      IF (NCOLB .EQ.     1) GO TO 5
      GO TO 1007
C
    5 NN  = NROWA
      NNN = NROWA
      MCB(1) = C
      MCB(2) = 0
      MCB(3) = NROWA
      MCB(6) = 0
      MCB(7) = 0
      IF (IPREC .EQ. 2) DGG = GG
C
      CALL GOPEN (A,Z(BUF1),0)
      CALL GOPEN (B,Z(BUF2),0)
      CALL GOPEN (C,Z(BUF3),1)
C
C     UNPACK A COLUMN OF A AND B, COMPUTE PRODUCTS, AND PACK TO C.
C     IF I.GT.1 AND B=1, USE THE ONE COLUMN OF B OVER AGAIN.
C
      DO 70 I = 1,NCOLA
C
      INULL = 0
      GO TO (10,40), IPREC
   10 GGZ = GG
      CALL UNPACK (*11,A,Z(1))
      GO TO 12
   11 INULL = 1
   12 IF (I.GT.1 .AND. NCOLB.EQ.1) GO TO 20
      CALL UNPACK (*15,B,Z(NROWA+1))
      GO TO 20
   15 INULL = 1
      DO 16 J = 1,NROWA
   16 Z(NROWA+J) = 0.
   20 IF (INULL .EQ. 1) GGZ = 0.
      DO 30 J = 1,NROWA
      Z(J) = GGZ*Z(J)*Z(NROWA+J)
   30 CONTINUE
      CALL PACK (Z(1),C,MCB)
      GO TO 70
   40 GGDZ = DGG
      CALL UNPACK (*41,A,DZ(1))
      GO TO 42
   41 INULL = 1
   42 IF (I.GT.1 .AND. NCOLB.EQ.1) GO TO 50
      CALL UNPACK (*45,B,DZ(NROWA+1))
      GO TO 50
   45 INULL = 1
      DO 46 J = 1,NROWA
   46 DZ(NROWA+J) = 0.D0
   50 IF (INULL .EQ. 1) GGDZ = 0.D0
      DO 60 J = 1,NROWA
      ISUB  = NROWA + J
      DZ(J) = GGDZ*DZ(J)*DZ(ISUB)
   60 CONTINUE
      CALL PACK (DZ(1),C,MCB)
C
C     DO ANOTHER COLUMN
C
   70 CONTINUE
C
      CALL WRTTRL (MCB)
      CALL CLOSE (A,1)
      CALL CLOSE (B,1)
      CALL CLOSE (C,1)
      RETURN
C
C     FATAL ERROR MESSAGE
C
 1007 K = -7
      GO TO 1010
 1008 K = -8
 1010 CALL MESAGE (K,0,NAM)
      RETURN
      END
