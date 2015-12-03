      SUBROUTINE EQMCKA (IP,BGPDT,CSTM,EQEXIN,D,ISCALR)
C
C     ROUTINE FORMS D MATRIX (ACCTUALLY D TRANSPOSE)
C
      INTEGER         BGPDT,FILE,CSTM,EQEXIN,D,SYSBUF,IZ(1),MCB(7),
     1                NAME(2)
      REAL            TR(3,3),TI(3,3),DD(6,6),R(3)
      DIMENSION       TT(3,3)
C
      COMMON /SYSTEM/ SYSBUF
      COMMON /PACKX / IT1,IT2,II,JJ,INCR
      COMMON /ZZZZZZ/ Z(1)
C
      EQUIVALENCE     (IZ(1),Z(1))
C
      DATA    IZ2   , IZ3,IZ4,IZ5  / 2,3,4,5 /
      DATA    NAME  / 4HEQMC,4HKA  /
C
C     CONVERT  EXTERNAL IP TO INTERNAL IP
C
      IBUF = KORSZ(Z)-SYSBUF+1
      FILE = EQEXIN
      CALL GOPEN (EQEXIN,Z(IBUF),0)
      CALL READ  (*220,*10,EQEXIN,IZ(1),IBUF-1,0,IFLAG)
      GO TO 240
   10 CALL CLOSE (EQEXIN,1)
      DO 20 I = 1,IFLAG,2
      IF (IZ(I) .EQ.IP) GO TO 40
   20 CONTINUE
      CALL MESAGE (41,IP,NAME)
      IP = 0
      GO TO 50
   30 CALL MESAGE (41,IP,NAME)
C
C     SCALAR POINT
C
      GO TO 60
   40 IP = IZ(I+1)
C
C     FIND RZERO FOR  IP
C
   50 FILE = BGPDT
      R(1) = 0.0
      R(2) = 0.0
      R(3) = 0.0
      CALL GOPEN (BGPDT,Z(IBUF),0)
      IF (IP .EQ. 0) GO TO 70
      I= (IP-1)*4
      CALL FREAD (BGPDT,Z,-I,0)
      CALL FREAD (BGPDT,I, 1,0)
      IF (I .EQ. -1) GO TO 30
      CALL FREAD (BGPDT,R,3,0)
   60 CALL REWIND (BGPDT)
      CALL SKPREC (BGPDT,1)
C
C     SET UP TO WRITE D
C
   70 IBUF1 = IBUF-SYSBUF
      NZ = IBUF1-5
C
C     BRING IN CSTM
C
      FILE = CSTM
      CALL OPEN   (*90,CSTM,Z(IBUF1),0)
      CALL FWDREC (*220,CSTM)
      CALL READ   (*220,*80,CSTM,Z(IZ5),NZ,0,NCSTM)
      GO TO 240
   80 CALL CLOSE  (CSTM,1)
      CALL PRETRS (Z(IZ5),NCSTM)
   90 CALL GOPEN  (D,Z(IBUF1),1)
      CALL MAKMCB (MCB,D,6,2,1)
      ISCALR = 0
      II     = 1
      JJ     = 6
      IT1    = 1
      IT2    = 1
      INCR   = 1
C
C     EXAMINE BGPDT
C
  100 CALL READ (*220,*190,BGPDT,Z(1),4,0,IFLAG)
      IF (IZ(1) .EQ. -1) GO TO 170
C
C     COMPUTE  TR
C
      ISCALR  = 1
      TR(1,1) = 0.0
      TR(2,2) = 0.0
      TR(3,3) = 0.0
      TR(2,1) = Z(IZ4) -R(3)
      TR(1,2) =-TR(2,1)
      TR(3,1) = R(2)- Z(IZ3)
      TR(1,3) =-TR(3,1)
      TR(3,2) = Z(IZ2)-R(1)
      TR(2,3) =-TR(3,2)
      DO 110 I = 1,3
      DO 110 J = 1,3
      TI(I,J) = 0.0
      IF (I .EQ. J) TI(I,J) = 1.0
  110 CONTINUE
      IF (IZ(1) .EQ. 0) GO TO 130
      CALL TRANSS (IZ(1),TI)
      CALL GMMATS (TI,3,3,1,TR,3,3,0,TT)
      DO 120 I = 1,3
      DO 120 J = 1,3
  120 TR(I,J) = TT(I,J)
C
C     MOVE STUFF INTO  DD
C
  130 DO 150 I = 1,6
      DO 150 J = 1,3
      IF (I .GT. 3) GO TO 140
      DD(I  ,J  ) = TI(J,I)
      DD(I+3,J+3) = DD(I,J)
      GO TO 150
  140 DD(I,J) = TR(I-3,J)
      DD(J,I) = 0.0
  150 CONTINUE
      DO 160 I = 1,6
      CALL PACK (DD(1,I),D,MCB)
  160 CONTINUE
      GO TO 100
C
C     SCALAR POINT
C
  170 DO 180 I = 1,6
  180 DD(I,1) = 0.0
      CALL PACK (DD,D,MCB)
      GO TO 100
C
C     END BGPDT
C
  190 CALL CLOSE  (BGPDT,1)
      CALL CLOSE  (D,1)
      CALL WRTTRL (MCB)
      RETURN
C
C     ERROR MESAGES
C
  210 CALL MESAGE (IP1,FILE,NAME)
  220 IP1 = -2
      GO TO 210
  240 IP1 = -8
      GO TO 210
      END
