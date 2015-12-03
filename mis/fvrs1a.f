      SUBROUTINE FVRS1A (BASE,BASE1,Z,W,BUF,INDEX,MODFRL,BASEXG,NROW,
     1                   NF,NFX,FKMAX,OMEGA)
C
      LOGICAL MODFRL
C
      INTEGER BASEXG,FKMAX
C
      COMPLEX BASE(3,NFX),Z(NROW),BASE1(3,NFX)
C
      DIMENSION MCB(7),BUF(1),W(NF),INDEX(1)
C
      COMMON /PACKX/ IN,IOUT,NS,NL,INCR
C
C-----------------------------------------------------------------------
C     COMPUTE NUMBER OF GRID POINTS (SCALAR POINTS ARE NOT ALLOWED).
C-----------------------------------------------------------------------
      NPTS=NROW/6
C-----------------------------------------------------------------------
C     GENERATE BASE TABLE
C----------------------------------------------------------------------
      IF (MODFRL) GO TO 100
      CALL FVRS1B(BASE,W,NF)
      GO TO 135
 100  CALL FVRS1C(BASE,W,OMEGA,NF)
 135  CONTINUE
C---------------------------------------------------------------------
C     SORT BASE BY INDEX TO MAKE IT COMPATIBLE TO FRLX
      IF (.NOT. MODFRL) GO TO 137
      CALL FVRS1D(BASE,BASE1,INDEX,NFX)
 137  CONTINUE
C---------------------------------------------------------------------
C     PREPARE TO OUTPUT BASEXG
C----------------------------------------------------------------------
      CALL GOPEN(BASEXG,BUF,1)
C-------------------------------
C     DEFINE MCB
      MCB(1)=BASEXG
      MCB(2)=0
      MCB(3)=NROW
      MCB(4)=2
      MCB(5)=3
      MCB(6)=0
      MCB(7)=0
C-------------------------------
C     DEFINE PACKING CONSTANTS
      IN=3
      IOUT=3
      NS=1
      NL=NROW
      INCR=1
C-----------------------------------------------------------------------
C     GENERATE AND PACK 1ST NF COLUMNS OF BASEXG
C     BASEXG-1
C     ZERO OUT COLUMN
      DO 140 I=1,NROW
 140  Z(I)=(0.0,0.0)
      DO 160 I=1,NFX
      L=1
      DO 143 K=1,NPTS
      Z(L)=BASE(1,I)
      L=L+6
 143  CONTINUE
      CALL PACK(Z,BASEXG,MCB)
 160  CONTINUE
      IF(FKMAX.LT.2)GO TO 500
C----------------------------------------------------------------------
C     GENERATE AND PACK 2ND NF COLUMNS OF BASEXG
C     BASEXG-2
C     ZERO COLUMN
      DO 240 I=1,NROW
      Z(I)=(0.0,0.0)
 240  CONTINUE
      DO 260 I=1,NFX
      L=1
      DO 243 K=1,NPTS
      Z(L+1)=BASE(2,I)
      Z(L+2)=BASE(3,I)
      L=L+6
 243  CONTINUE
      CALL PACK(Z,BASEXG,MCB)
 260  CONTINUE
      IF(FKMAX.LT.3) GO TO 500
C----------------------------------------------------------------------
C     GENERATE AND PACK 3RD NF COLUMNS OF BASEXG
C     BASEXG-3
      DO 360 I=1,NFX
      L=1
      DO 343 K=1,NPTS
      Z(L+1)=BASE(3,I)
      Z(L+2)=-BASE(2,I)
      L=L+6
 343  CONTINUE
      CALL PACK(Z,BASEXG,MCB)
 360  CONTINUE
C-----------------------------------------------------------------------
C     GENERATE 4TH THRU FKMAX NF COLUMN GROUPS-(NULL)INTO BASEXG
      IF(FKMAX.LT.4)GO TO 500
      NS=1
      NL=1
      Z(1)=(0.0,0.0)
      DO 400 I=4,FKMAX
      DO 390 K=1,NFX
      CALL PACK(Z,BASEXG,MCB)
 390  CONTINUE
 400  CONTINUE
C----------------------------------------------------------------------
C     CLOSE OUTPUT DATA BLOCK
 500  CALL CLOSE(BASEXG,1)
      CALL WRTTRL(MCB)
      RETURN
      END
