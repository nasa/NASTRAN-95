      SUBROUTINE HESS2(NROW,IDEN,IPV)
C
C     HESS2  WILL GENERATE  AN IDENTITY MATRIX AND A PARTIIONING VECTOR
C
      INTEGER MCB(7) , IZ(1)
      INTEGER SYSBUF
C
      COMMON /PACKX/IT1,IT2,II,JJ,INCR
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /ZZZZZZ/Z(1)
C
      EQUIVALENCE ( KSYSTM( 1) , SYSBUF )
      EQUIVALENCE ( Z(1),IZ(1) )
C
C ----------------------------------------------------------------------
C
      CALL MAKMCB( MCB, IDEN, NROW, 8, 1 )
      NZ = KORSZ(Z)
      IBUF1 = NZ- SYSBUF
      CALL GOPEN(IDEN,IZ(IBUF1),1)
      IT1=1
      IT2=1
      INCR=1
      Z(1)=-1.0
      DO 10 I=1,NROW
      II = I
      JJ=I
      CALL PACK(Z,IDEN,MCB)
   10 CONTINUE
      CALL CLOSE(IDEN,1)
      CALL WRTTRL(MCB)
C
C     BUILD PARTITIONING VECTOR
C
      CALL MAKMCB( MCB, IPV, 2*NROW, 2, 1 )
      CALL GOPEN(IPV,IZ(IBUF1),1)
      DO 20 I=1,NROW
      Z(I)=1.0
   20 CONTINUE
      II = NROW+1
      JJ= 2*NROW
      CALL PACK(Z,IPV,MCB)
      CALL WRTTRL(MCB)
      CALL CLOSE(IPV,1)
      RETURN
      END
