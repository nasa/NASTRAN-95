      SUBROUTINE FRD2B (A,ALP,B,BET,C,GAM,D,DEL,E,EPS,OUT)
C
C     ADD UP MATRICIES
C
      INTEGER         A,B,C,D,E,OUT,TYPA,TYPB,TYPC,TYPD,TYPE
      REAL            ALP(2),BET(2),GAM(2),DEL(2),EPS(2),Z(1)
      COMMON /SYSTEM/ KSYSTM(54), IPREC
      COMMON /ZZZZZZ/ Z
      COMMON /SADDX / NOMAT,LCORE,MCBA(7),TYPA,ALPHA(4),MCBB(7),TYPB,
     1                BETA(4),MCBC(7),TYPC,GAMA(4),MCBD(7),TYPD,
     2                DELTA(4),MCBE(7),TYPE,EPSLN(4),MC(7)
      COMMON /FRD2BC/ IH
C
      NC    = KORSZ(Z)
      NOMAT = 5
      LCORE = NC
      TYPA  = 3
      TYPB  = 3
      TYPC  = 3
      TYPD  = 3
      TYPE  = 3
      ALPHA(1) = ALP(1)
      ALPHA(2) = ALP(2)
      BETA(1)  = BET(1)
      BETA(2)  = BET(2)
      GAMA(1)  = GAM(1)
      GAMA(2)  = GAM(2)
      DELTA(1) = DEL(1)
      DELTA(2) = DEL(2)
      EPSLN(1) = EPS(1)
      EPSLN(2) = EPS(2)
      MCBA(1)  = A
      MCBB(1)  = B
      MCBC(1)  = C
      MCBD(1)  = D
      MCBE(1)  = E
      CALL RDTRL (MCBA)
      CALL RDTRL (MCBB)
      CALL RDTRL (MCBC)
      CALL RDTRL (MCBD)
      CALL RDTRL (MCBE)
      IFO = 6
      ITY = 3
      IF (IH.EQ.0 .AND. IPREC.EQ.2) ITY = 4
C
C     IH IN /FRD2BC/ IS INITIALIZED BY ROUTINE FRRD2.
C     (COMPLEX D.P. ARITHMETIC IS USED IF IH = 0)
C
      N = 0
      DO 10 I = 1,49,12
      IF (MCBA(I  ) .LT. 0) MCBA(I) = 0
      IF (MCBA(I+1) .EQ. 0) MCBA(I) = 0
      IF (MCBA(I  ) .EQ. 0) GO TO 10
      IF (N .EQ. 0) N = MCBA(I+1)
      IROW = MCBA(I+2)
      IF (MCBA(I+3) .NE. 6) IFO  = 1
   10 CONTINUE
      CALL MAKMCB (MC,OUT,IROW,IFO,ITY)
      MC(2) = N
      CALL SADD (Z,Z)
      CALL WRTTRL (MC)
      CALL DMPFIL (-OUT,Z,NC)
      RETURN
      END
