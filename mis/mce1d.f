      SUBROUTINE MCE1D
C
C     MCE1D SOLVES FOR GM IN THE MATRIX EQUATION RM*GM = -RN
C     WHERE RM IS A DIAGONAL MATRIX.
C
      INTEGER         SYSBUF,EOL    ,EOR   ,TYPE  ,RDP   ,BCD  ,RM   ,
     1                RN    ,GM
      REAL            Z(1)  ,A(1)   ,B(1)
      DOUBLE PRECISION ZD   ,AD     ,BD
      DIMENSION       BCD(2),MCB1(7),MCB2(7)
      COMMON /BLANK / USET  ,RG     ,GM    ,SCR1  ,SCR2  ,SCR3  ,RM  ,
     1                RN    ,L      ,U     ,MCB(7)
      COMMON /ZNTPKX/ AD (2),I      ,EOL   ,EOR
      COMMON /ZBLPKX/ BD (2),J
      COMMON /ZZZZZZ/ ZD (1)
      COMMON /SYSTEM/ SYSBUF,SKP(53),IPR
      EQUIVALENCE     (MCB(2),NCOL) ,(AD(1),A(1)) ,(MCB(5),TYPE) ,
     1                (ZD(1) ,Z(1)) ,(BD(1),B(1)) ,(MCB1(2),NCOL1)
      DATA    BCD   , RDP   /4HMCE1 ,4HD   ,  2   /
C
C     OPEN RM MATRIX,SKIP HEADER RECORD AND READ MATRIX CONTROL BLOCK
C
      NZ = KORSZ(Z)
      N  = NZ - SYSBUF
      CALL GOPEN (RM,Z(N+1),0)
      MCB(1) = RM
      CALL RDTRL (MCB)
C
C     FORM -RM
C
      NCOL = MCB(2)
      DO 22 K = 1,NCOL
      CALL INTPK (*83,RM,0,RDP,0)
      CALL ZNTPKI
      IF (I .NE. K) GO TO 84
   22 ZD(K) = -AD(1)
      CALL CLOSE (RM,1)
C
C     OPEN RN MATRIX,SKIP HEADER RECORD AND READ MATRIX CONTROL BLOCK
C
      CALL GOPEN (RN,Z(N+1),0)
      MCB1(1) = RN
      CALL RDTRL (MCB1)
C
C     SET UP MATRIX CONTROL BLOCK BLOCK FOR GM
C
      CALL MAKMCB (MCB2,GM,MCB1(3),MCB1(4),IPR)
C
C     OPEN OUTPUT FILE FOR GM AND WRITE HEADER RECORD
C
      N1 = N - SYSBUF
      CALL GOPEN (GM,Z(N1+1),1)
C
C     FORM GM = -RM(-1)*RN
C
      NCOL1 = MCB1(2)
      DO 62 K = 1,NCOL1
      CALL BLDPK (RDP,IPR,GM,0,0)
      CALL INTPK (*62,RN,0,RDP,0)
   61 CALL ZNTPKI
      J = I
      BD(1) = AD(1)/ZD(J)
      CALL ZBLPKI
      IF (EOL) 62,61,62
   62 CALL BLDPKN (GM,0,MCB2)
C
C     CLOSE GM AND RM FILES AND WRITE TRAILER FOR GM
C
      CALL CLOSE (GM,1)
      CALL CLOSE (RN,1)
      CALL WRTTRL (MCB2)
      RETURN
C
C     CALL MESSAGE WRITER IF FATAL ERROR DETECTED
C
   83 L = -5
      GO TO 86
   84 L = -16
   86 CALL MESAGE (L,RM,BCD)
      RETURN
      END
