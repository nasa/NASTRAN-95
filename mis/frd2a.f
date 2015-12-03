      SUBROUTINE FRD2A (NQHL,QHR,QHI,IH,NFREQ)
C
      INTEGER         QHR,QHI,SYSBUF,MCB(7),THR(7),THI(7)
      DIMENSION       Z(1)
      COMMON /ZZZZZZ/ Z
      COMMON /SYSTEM/ SYSBUF
      COMMON /UNPAKX/ IOUT,INN,NNN,INCR1
      COMMON /PACKX / ITI,ITO,II,NN,INCR
C
C     FIND COLUMN OF NQHL AND COPY REAL TO QHR AND IMAG TO QHI
C
      NZ = KORSZ(Z) - SYSBUF
      MCB(1) = NQHL
      CALL RDTRL (MCB)
      IF (MCB(2) .EQ.0) GO TO 999
      IOUT = MCB(5)
      ITI  = 1
      IF (IOUT .EQ. 4) ITI = 2
      ITO  = ITI
      NNN  = MCB(3)
      INN  = 1
      INCR1= 1
      II   = 1
      NN   = IH
      INCR = 2
      NWC  = 2
      IF (IOUT .EQ. 4) NWC = 4
      IBUF1 = NZ
      IBUF2 = IBUF1 - SYSBUF
      CALL OPEN (*999,NQHL,Z(IBUF1),0)
      CALL READ (*999,*999,NQHL,Z(1),-2,1,FLAG)
      CALL MAKMCB (THR,QHR,IH,MCB(4),ITO)
      CALL MAKMCB (THI,QHI,IH,MCB(4),ITO)
      CALL SKPREC (NQHL,NFREQ-1)
      CALL UNPACK (*25,NQHL,Z(1))
      GO TO 30
   25 CALL ZEROC  (Z,NNN*NWC)
   30 J = 1
      CALL CLOSE (NQHL,1)
      CALL GOPEN (QHR,Z(IBUF2),1)
      CALL GOPEN (QHI,Z(IBUF1),1)
      DO 40 I = 1,IH
      CALL PACK (Z(J),QHR,THR)
      CALL PACK (Z(J+1),QHI,THI)
      J = J + IH*NWC
   40 CONTINUE
      CALL CLOSE  (QHR,1)
      CALL CLOSE  (QHI,1)
      CALL WRTTRL (THR)
      CALL WRTTRL (THI)
      CALL DMPFIL (-QHR,Z,NZ)
      CALL DMPFIL (-QHI,Z,NZ)
      GO TO 1000
  999 CALL MAKMCB (THR,QHR,0,0,0)
      CALL WRTTRL (THR)
      THR(1) = QHI
      CALL WRTTRL (THR)
 1000 RETURN
      END
