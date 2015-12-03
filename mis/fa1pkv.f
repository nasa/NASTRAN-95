      SUBROUTINE FA1PKV (AZ,AMK,AMB,N,E1,CZ,BREF,PI,VEL,IBUF)
C
      INTEGER         IBUF(1),IV(6),TRL(7)
      REAL            E1(5),V(6),E(2),AMK(1),AMB(1),CZ(1)
      COMPLEX         AZ(1),CEIG,EIGEN,EIGZ
      COMMON /SYSTEM/ SYSBUF,NOUT,SPACE(6),NLPP,X(2),LINES
      EQUIVALENCE     (V(1),IV(1)),(EIGEN,E(1))
      DATA    ISCR  / 301/, IPASS /0/
C
      EIGZ = (0.0,0.0)
      IF (N .LT. 2) GO TO 1000
      E(1) = E1(1)
      E(2) = E1(2)
      IF (IPASS .NE. 0) GO TO 5
      CALL OPEN (*1000,ISCR,IBUF,1)
      GO TO 9
    5 CALL OPEN (*1000,ISCR,IBUF,3)
    9 IPASS = IPASS + 1
C
C     BUILD A = IP2 + M-1B P + M-1K
C
      CEIG = EIGEN*EIGEN
      K = 0
      DO 10 I = 1,N
      DO 10 J = 1,N
      K = K + 1
      AZ(K) = -AMB(K)*EIGEN - AMK(K)
      IF (I .EQ. J) AZ(K) = AZ(K) + CEIG
   10 CONTINUE
C
C     CORE FOR EGNVCT
C
      N2 = N*2
      NA = 1  + N2*N
      NB = NA + N2
      NC = NB + N2
      ND = NC + N2
      CALL EGNVCT (AZ,CZ(NA),EIGZ,CZ(NB),CZ(NC),CZ(ND),N)
C
C     BUILD ON SCR1 DATA FOR VECTOR OUTPUT
C
      IV(1) = IPASS
      IV(2) = IPASS
      V (3) = E1(1)
      V (4) = E1(2)
      IF (E1(2) .EQ. 0.0) GO TO 20
      V(5)  = E1(3)
      V(6)  = E1(5)
      GO TO 22
   20 V(5)  = 0.0
      V(6)  = (BREF/(.34657*VEL))*E1(1)
   22 CALL WRITE (ISCR,IV,6,1)
      CALL WRITE (ISCR,CZ(NB),N2,1)
C
C     VECTOR IS IN CZ(NB)
C
      LINES = NLPP
      K = 0
      DO 30 I = 1,N
      IF (LINES .LT. NLPP) GO TO 25
      CALL PAGE1
      WRITE  (NOUT,21) EIGEN
   21 FORMAT (1H0,47X,30HEIGENVECTOR FROM THE PK METHOD, /3X,
     1        13HEIGENVALUE = ,1P,E15.5,1P,E15.5, //3X,11HEIGENVECTOR)
      LINES = LINES + 5
   25 LINES = LINES + 1
      WRITE  (NOUT,26) CZ(NB+K),CZ(NB+K+1)
   26 FORMAT (16X,1P,E15.5,1P,E15.5)
      K = K + 2
   30 CONTINUE
      TRL(1) = ISCR
      TRL(2) = 1
      CALL WRTTRL (TRL)
 1000 CALL CLOSE (ISCR,3)
      RETURN
      END
