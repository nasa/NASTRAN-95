      SUBROUTINE FRD2C (A,B,X,SCR1,SCR2,SCR3,SCR4,SCR5,NLOAD,NFREQ)
C
C     SOLVE A X = B
C     USE INCORE DECOMP IF POSSIBLE
C
      INTEGER         A,B,X,SCR1,SCR2,SCR3,SCR4,SCR5,SYSBUF,OUT,TA(7),
     1                TB(7),TX(7)
      DIMENSION       ZZ(1)
      CHARACTER       UFM*23,UWM*25,UIM*29
      COMMON /XMSSG / UFM,UWM,UIM
      COMMON /SYSTEM/ SYSBUF,OUT,DUM(52),IPREC
      COMMON /PACKX / ITI,ITO,II,NN,INCR
      COMMON /UNPAKX/ IOUT,INN,NNN,INCR1
      COMMON /FRD2BC/ IH,IP
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (ZZ(1),Z(1))
C
      ICORE= KORSZ(Z)
      INCR = 1
      II   = 1
      INN  = 1
      INCR1= 1
      IOUT = 3
      IF (IH.EQ.0 .AND. IPREC.EQ.2) IOUT = 4
C
C     IH IN /FRD2BC/ IS INITIALIZED BY ROUTINE FRRD2.
C     (COMPLEX D.P. ARITHMETIC IS USED IF IH=0)
C
      ITO = IOUT
      ITI = ITO
C
C     DECIDE IF INCORE IS POSSIBLE
C
      TA(1) = A
      CALL RDTRL (TA)
      TB(1) = B
      CALL RDTRL (TB)
      NA    = TA(2)
      NB    = TB(3)*NLOAD
      IBUF1 = ICORE - SYSBUF
      NCORE = NA*NA*2 + NB*2 + NB*2 + SYSBUF
C
C     IF IH=0, COMPLEX D.P. COMPUTATION WILL BE USED.  NOTICE THAT THE
C     ROUTINE INCORE IS WRITTEN ONLY FOR COMPLEX S.P. OPERATION.
C
      IF (IH .EQ. 0) GO TO 102
      IF (NCORE .GT. ICORE) GO TO 100
C
C     DO INCORE
C
      IA = 1
      CALL GOPEN (A,Z(IBUF1),0)
      NNN = TA(3)
      INCR1 = NNN
      N = NA + NA
      DO 10 I = 1,N,2
      CALL UNPACK (*11,A,Z(I))
      GO TO 10
   11 DO 12 K = 1,N,2
      L = (K-1)*NNN
      Z(I+L  ) = 0.0
      Z(I+L+1) = 0.0
   12 CONTINUE
   10 CONTINUE
      CALL CLOSE (A,1)
C
C     GET FREQ FROM B
C
      IB   = NNN*NNN*2 + 1
      NNN  = TB(3)
      INCR1= NLOAD
      N1   = NNN + NNN
      J    = TB(2)/NLOAD - 1
      M    = 0
      CALL GOPEN (B,Z(IBUF1),0)
      CALL SKPREC (B,NFREQ-1)
      DO 30 I = 1,NLOAD
      CALL UNPACK (*31,B,Z(IB+M))
      GO TO 33
   31 DO 32 K = 1,N1,2
      L = (K-1)*NLOAD + IB + M
      Z(L  ) = 0.0
      Z(L+1) = 0.0
   32 CONTINUE
   33 IF (I .NE. NLOAD) CALL SKPREC (B,J)
      M = M+2
   30 CONTINUE
      CALL CLOSE (B,1)
      IX = NLOAD*NNN*2 + IB
      CALL INCORE (Z(IA),NA,Z(IB),Z(IX),NLOAD)
      NN = NA
      CALL GOPEN (X,Z(IBUF1),1)
      CALL MAKMCB (TX,X,NN,TB(4),ITO)
      INCR = NLOAD
      J = IX
      DO 50 I = 1,NLOAD
      CALL PACK (Z(J),X,TX)
   50 J = J + 2
      CALL CLOSE (X,1)
      CALL WRTTRL (TX)
      GO TO 1000
C
C     USE FILE SOLVE
C
  100 IF (IP .NE. 0) GO TO 102
      IP = NCORE - ICORE
      WRITE  (OUT,101) UIM,IP
  101 FORMAT (A29,' 2437, ADDITIONAL CORE NEEDED FOR IN-CORE ',
     1       'DECOMPOSITION IN FRRD2 MODULE IS',I8,' WORDS.')
  102 CALL CFACTR (A,SCR1,SCR2,SCR3,SCR4,SCR5,IOPT)
      ICORE = KORSZ(ZZ)
      IBUF1 = ICORE - SYSBUF
      IBUF2 = IBUF1 - SYSBUF
      CALL GOPEN (B,ZZ(IBUF1),0)
      CALL GOPEN (SCR3,ZZ(IBUF2),1)
      IOUT = 3
      IF (IH.EQ.0 .AND. IPREC.EQ.2) IOUT = 4
      INCR1 = 1
      J = TB(2)/NLOAD - 1
      NN = TB(3)
      CALL MAKMCB (TX,SCR3,NN,TB(4),ITO)
      CALL SKPREC (B,NFREQ-1)
      DO 110 I = 1,NLOAD
      CALL CYCT2B (B,SCR3,1,ZZ,TX)
      IF (I .NE. NLOAD) CALL SKPREC (B,J)
  110 CONTINUE
      CALL CLOSE (SCR3,1)
      CALL CLOSE (B,1)
      CALL WRTTRL (TX)
      CALL CFBSOR (SCR1,SCR2,SCR3,X,IOPT)
 1000 RETURN
      END
