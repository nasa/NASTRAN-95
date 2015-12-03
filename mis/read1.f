      SUBROUTINE READ1 (DM,MR,SCR1,SCR2,SCR3,PHIA,USET,NR1,LAMA,SCR4)
C
      INTEGER          DM,MR,IMR(7),SYSBUF,SCR1,SCR2,ISCR1(7),PHIA,
     1                 SCR4,SCR3,NAM(2)
      DOUBLE PRECISION DCORE(1),SI,TERM
      CHARACTER        UFM*23
      COMMON /XMSSG /  UFM
      COMMON /SYSTEM/  SYSBUF,NOUT,KSYSTM(63)
      COMMON /ZZZZZZ/  CORE(1)
      COMMON /UNPAKX/  ITB,II,JJ,INCUR
      COMMON /PACKX /  ITA1,ITB1,II1,JJ1,INCUR1
      COMMON /BITPOS/  UM,UO,UR,USG,USB,UL,UA,UF,US,UN,UG
      EQUIVALENCE      (DCORE(1),CORE(1))
      DATA    NAM   /  4HREAD,4H1    /
C
C     BRING MR INTO CORE
C
      LC = KORSZ(CORE) - SYSBUF
      CALL GOPEN (MR,CORE(LC+1),0)
      IMR(1) = MR
      CALL RDTRL (IMR)
      NR   = IMR(2)
      NR1  = NR
      II   = 1
      JJ   = NR
      INCUR= 1
      ITB  = IMR(5)
      NR2  = ITB*NR
      IVI  = NR*NR
      IPHI = IVI
      IVI2 = ITB*IVI
      IALPH= 2*IVI
      ILOOP= 0
      K    = 0
      DO 20 I = 1,NR
      CALL UNPACK (*12,MR,CORE(K+1))
      GO TO 16
C
C     NULL COLUMN
C
   12 DO 14 J = 1,NR2
      CORE(J+K) = 0.0
   14 CONTINUE
   16 KKK = K + IVI2
      DO 10 J = 1,NR2
      CORE(J+KKK) = 0.0
   10 CONTINUE
      IF (ITB .EQ. 1) GO TO 18
      KKK = KKK/2
      DCORE(KKK+I) = 1.0D0
      GO TO 19
   18 CORE(KKK+I) = 1.0
   19 K = K + NR2
   20 CONTINUE
      CALL CLOSE (MR,1)
C
C     COMPUTE SI
C
      IF (ITB .NE. 2) GO TO 35
   30 SI = 0.0D0
      DO 50 I = 1,NR
      TERM = 0.0D0
      DO 40 J = 1,NR
      K  = (J-1)*NR + I
      KK = IVI + J
   40 TERM = TERM + DCORE(K)*DCORE(KK)
      K  = IVI + I
      SI = SI + TERM*DCORE(K)
   50 CONTINUE
      IF (SI .GT. 0.0D0) GO TO 51
   53 WRITE  (NOUT,52) UFM
   52 FORMAT (A23,' 2200, INCONSISTENT RIGID BODY SYSTEM.')
      CALL MESAGE (-61,0,NAM)
   51 CONTINUE
      SI = 1.0D0/DSQRT(SI)
C
C     CONVERT VI INTO PHI
C
      DO 60 I = 1,NR
      K = IVI + I
   60 DCORE(K) = DCORE(K) *SI
      ILOOP = ILOOP + 1
      IF (ILOOP .EQ. NR) GO TO 120
C
C     CALCULATE ALPHAJ
C
      DO 90 J = 1,ILOOP
      K = IALPH + J
      DCORE(K) = 0.0D0
      DO 80 I = 1,NR
      TERM = 0.0D0
      DO 70 L = 1,NR
      KK  = (L-1)*NR + I
      KKK = IVI + NR + L
   70 TERM = TERM + DCORE(KK)*DCORE(KKK)
      KK = IPHI + (J-1)*NR + I
   80 DCORE(K) = DCORE(K)+TERM*DCORE(KK)
   90 CONTINUE
C
C     COMPUTE NEXT V VECTOR
C
      DO 110 I = 1,NR
      TERM = 0.0D0
      DO 100 J = 1,ILOOP
      KK = IALPH + J
      K  = IPHI + (J-1)*NR + I
  100 TERM = TERM + DCORE(KK)*DCORE(K)
      K = IVI + NR + I
  110 DCORE(K) = DCORE(K) - TERM
      IVI = IVI + NR
      GO TO 30
   35 SSI = 0.0
      DO 55 I = 1,NR
      STERM = 0.0
      DO 45 J = 1,NR
      K  = (J-1)*NR + I
      KK = IVI + J
   45 STERM = STERM + CORE(K)*CORE(KK)
      K   = IVI + I
      SSI = SSI + STERM*CORE(K)
   55 CONTINUE
      IF (SSI .LE. 0.0) GO TO 53
      SSI = 1.0/SQRT(SSI)
C
C     CONVERT VI INTO PHI
C
      DO 65 I = 1,NR
      K = IVI + I
   65 CORE(K) = CORE(K)*SSI
      ILOOP = ILOOP + 1
      IF (ILOOP .EQ. NR) GO TO 120
C
C     CALCULATE ALPHAJ
C
      DO 95 J = 1,ILOOP
      K = IALPH + J
      CORE(K) = 0.0
      DO 85 I = 1,NR
      STERM = 0.0
      DO 75 L = 1,NR
      KK  = (L-1)*NR + I
      KKK = IVI + NR + L
   75 STERM = STERM + CORE(KK)*CORE(KKK)
      KK = IPHI + (J-1)*NR + I
   85 CORE(K) = CORE(K) + STERM*CORE(KK)
   95 CONTINUE
C
C     COMPUTE NEXT V VECTOR
C
      DO 115 I = 1,NR
      STERM = 0.0
      DO 105 J = 1,ILOOP
      KK = IALPH + J
      K  = IPHI + (J-1)*NR + I
  105 STERM = STERM + CORE(KK)*CORE(K)
      K = IVI + NR + I
  115 CORE(K) = CORE(K) - STERM
      IVI = IVI + NR
      GO TO 35
C
C     PACK PHIRO
C
  120 ITA1 = ITB
      ITB1 = ITB
      II1  = 1
      JJ1  = NR
      INCUR1 = 1
      CALL GOPEN (SCR1,CORE(LC+1),1)
      CALL MAKMCB (ISCR1,SCR1,NR,1,ITB)
      DO 130 I = 1,NR
      K = IVI2 + (I-1)*NR2
  130 CALL PACK (CORE(K+1),SCR1,ISCR1)
      CALL CLOSE (SCR1,1)
      CALL WRTTRL (ISCR1(1))
C
C     COMPUTE PHILO = DM*PHIRO
C
      CALL SSG2B (DM,SCR1,0,SCR2,0,ITB,1,SCR4)
C
C     MERGE PHIRP AND PHILO TO FORM PHIA
C
      CALL SDR1B (SCR3,SCR2,SCR1,SCR4,UA,UL,UR,USET,0,0)
      CALL GOPEN (SCR4,CORE(LC+1),0)
      LC = LC - SYSBUF
      CALL GOPEN (PHIA,CORE(LC+1),1)
      IMR(1) = SCR4
      CALL RDTRL (IMR(1))
      NPROB = IMR(3)
      DCORE(1) = 0.D0
      JJ = NPROB
      INCUR = 1
      I3 = 3
      DO 170 J = 1,NR
      II = 0
      CALL UNPACK (*150,SCR4,CORE(I3))
      II1 = II
      JJ1 = JJ
      CALL PACK (CORE(I3),PHIA,ISCR1)
      GO TO 170
C
C     NULL COLUMN
C
  150 II1 = 1
      JJ1 = 1
      CALL PACK (CORE,PHIA,ISCR1)
  170 CONTINUE
      CALL CLOSE (SCR4,1)
      CALL CLOSE (PHIA,1)
      LC = LC + SYSBUF
C
C     PUT NR ZEROS ON LAMA
C
      CALL GOPEN (LAMA,CORE(LC+1),1)
      DCORE(1) = 0.D0
      DO 180 I = 1,NR
  180 CALL WRITE (LAMA,CORE,ITB,1)
      CALL CLOSE (LAMA,2)
      RETURN
      END
