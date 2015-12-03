      SUBROUTINE EMPCOR(MT1X,MT2X,PT,PC,FRSROW,MIDROW,LASROW,NX,A,Z)
C
C     EMPTY CORE OF A TRIANGULAR MATRIX
C
      INTEGER PT,PC,FRSROW,ROW,MCB(7)
      REAL A(1),Z(1)
C
C
C     MT1      FIRST PART OF THE MATRIX (UP TO ROW -MIDROW-).
C     MT2      REST OF THE MATRIX.
C     PT       PRECISION OF THE MATRIX ON TAPE.
C     PC       ......... .. ... ...... IN CORE.
C     FRSROW   FIRST ROW IN CORE.
C     LAST     LAST  ... .. CORE.
C     N        SIZE OF THE COMPLETE MATRIX.
C     A        LOCATION OF THE COMPLETE MATRIX.
C
      COMMON /PACKX/IT1,IT2,II,JJ,INCR
      DATA  MCB /7*0/
      MT1 = MT1X
      MT2 = MT2X
      N   = NX
      MT  = MT1
      IF(FRSROW .GT. MIDROW .AND. MT2 .NE. 0) MT = MT2
      NA  =1
      INCR = 1
      IT1 = PC
      IT2 = PT
      JJ  = N
      DO 105 ROW =  FRSROW,LASROW
      II = ROW
      CALL PACK(A(NA),MT,MCB)
      IF( ROW .EQ. N) GO TO 110
      NA = NA + PC* (N-ROW+1)
      IF( ROW .NE. MIDROW .OR. MT2 .EQ. 0) GO TO 105
      CALL CLOSE(MT,1)
      MT = MT2
      CALL GOPEN(MT,Z,1)
  105 CONTINUE
      GO TO 115
C
C     END OF CORE DUMP
C
  110 CALL CLOSE(MT,1)
  115 RETURN
      END
