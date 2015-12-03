      SUBROUTINE FA1PKI (FSAVE,QHHL)
C
C     FA1PKI BUILDS AN INTERPOLATION MATRIX IN CORE FOR PK METHOD
C
C     LAST REVISED  2/91, BY J.PETKAS/LOOKHEED
C     TO ALLOW CALCULATION OF INTERPOLATION MATRIX IN D.P.
C
      INTEGER         FSAVE,QHHL,SYSBUF,NAME(2),TRL(7),BUF1,FLOOP
      REAL            NEWM
      DOUBLE PRECISION DX1,DX2,DET,DZ(1)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /BLANK / FLOOP
      COMMON /SYSTEM/ SYSBUF,NOUT
      COMMON /UNPAKX/ IOUT,INN,NNN,INCR1
      COMMON /ZZZZZZ/ Z(1)
      COMMON /FA1PKC/ NCORE,NK,IMVR,IK,IA,IQ,ICP,IFLAG
      EQUIVALENCE     (DZ(1),Z(1))
      DATA    OLDM  / -1.0/
      DATA    NAME  / 4HFA1P,4HKI  /
C
      IFLAG = 0
      IF (OLDM .NE. -1.0) GO TO 20
      NCORE= KORSZ(Z)
      BUF1 = NCORE - SYSBUF
      IMVR = 1
C
C     PUT M V   IN CORE ON SECOND LOOP RETURN IF SAME MACH
C
      IFLE = FSAVE
      CALL GOPEN (FSAVE,Z(BUF1),0)
      CALL READ (*180,*10,FSAVE,Z(IMVR),BUF1,1,NWR)
   10 IK = IMVR + NWR
      CALL CLOSE (FSAVE,1)
   20 I = (FLOOP-1)*3 + IMVR
      NEWM = Z(I)
      IF (OLDM .EQ. NEWM) GO TO 200
      OLDM = NEWM
      IFLAG= 1
C
C     PUT LIST OF M K'S IN CORE FOR THIS MACH
C
      TRL(1) = QHHL
      CALL RDTRL (TRL)
      NROW = TRL(3)
      NI   = (TRL(2)/TRL(3))*2
      IOUT = 3
      INN  = 1
      INCR1= 1
      NNN  = NROW
      N2   = NROW*2
      NN   = NROW*NROW
      IFLE = QHHL
      CALL OPEN (*180,QHHL,Z(BUF1),0)
      CALL READ (*180,*180,QHHL,Z,-3,0,NWR)
      CALL READ (*180,*180,QHHL,N, 1,0,NWR)
      N  = N + N
      NI = MIN0(NI,N)
      CALL READ (*180,*180,QHHL,Z(IK),NI,1,NWR)
C
C     FIND M'S CLOSEST TO NEWM
C
      IA  = IK + NI
      IF (MOD(IA,2) .EQ. 0) IA = IA + 1
      RMI = 1.E20
      RMS = 0.0
      DO 30 I = 1,NI,2
      RMX = ABS(Z(IK+I-1)-NEWM)
      RMI = AMIN1(RMI,RMX)
      IF (RMX .GT. RMI) GO TO 30
      RMS = Z(IK+I-1)
   30 CONTINUE
      RMI = RMS
C
C     COUNT K"S
C
      NK = 0
      DO 50 I = 1,NI,2
      IF (Z(IK+I-1) .EQ. RMI) GO TO 40
      GO TO 50
   40 NK = NK + 1
   50 CONTINUE
C
C     ALLOCATE CORE FOR A-1 AND Q.  THEN BUILD THEM.
C
      I  = 2*(NK+1)**2
      IQ = IA + I
      ICP= IQ + NN*2*NK
      IF (MOD(ICP,2) .EQ. 0) ICP = ICP + 1
      IF (ICP+SYSBUF+N2 .GT. NCORE) CALL MESAGE (-8,0,NAME)
C
C     BUILD A
C
      J = 0
      DO 70 I = 1,NI,2
      IF (Z(IK+I-1) .EQ. RMI) GO TO 60
      GO TO 70
   60 Z(IQ+J) = Z(IK+I)
      J = J + 1
   70 CONTINUE
      NK1 = NK + 1
      N   = 0
      M   = IQ - 1
      IAD = IA/2  + 1
      ICPD= ICP/2 + 1
      DO 90 I = 1,NK1
      DX2 = Z(M+I)
      DO 90 J = 1,NK1
      IF (I.EQ.NK1 .AND. J.EQ.NK1) GO TO 100
      IF (J.EQ.NK1 .OR.  I.EQ.NK1) GO TO 75
      DX1 = Z(M+J)
      DZ(IAD+N) = DABS((DX1-DX2)**3) + (DX1+DX2)**3
      GO TO 80
   75 DZ(IAD+N) = 1.0D+0
   80 N = N + 1
   90 CONTINUE
  100 DZ(IAD+N) = 0.0D+0
C
C     MODIFICATION FOR LEVEL 17.7 UPDATE
C     REPLACE ALL CALLS TO INVAER WITH CALLS TO INVERS.
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERD (NK1,DZ(IAD),NK1,0,0,DET,ISING,DZ(ICPD))
      IF (ISING .EQ. 2) GO TO 150
C
C     BUILD Q
C
      N  = 0
      IN = NN
      L  = 0
      DO 140 I = 1,NI,2
      IF (Z(IK+I-1) .EQ. RMI) GO TO 110
      CALL SKPREC (QHHL,NROW)
      GO TO 140
  110 DO 130 J = 1,NROW
      CALL UNPACK (*115,QHHL,Z(ICP))
      GO TO 120
  115 CALL ZEROC (Z(ICP),NROW*2)
C
C     SPLIT REAL AND IMAGINARY DIVIDE IMAGINARY BY K
C
  120 DO 125 K = 1,N2,2
      Z(IQ+N) = Z(ICP+K-1)
      N = N + 1
      Z(IQ+IN) = Z(ICP+K)/Z(IK+I)
      IN = IN + 1
  125 CONTINUE
  130 CONTINUE
      Z(IK+L) = Z(IK+I)
      L = L  + 1
      N = N  + NN
      IN= IN + NN
  140 CONTINUE
      CALL CLOSE (QHHL,1)
      GO TO 200
C
  150 WRITE  (NOUT,160) UFM,NAME
  160 FORMAT (A23,' 2427, SINGULAR MATRIX FOR INTERPOLATION IN ',2A4)
      CALL MESAGE (-61,0,0)
  180 CALL MESAGE (-2,IFLE,NAME)
  200 RETURN
      END
