      SUBROUTINE GFSTRN(A,AT,I,SCR1)
C
C     MATRIX TRANSPOSE ROUTINE
C
C     MORE EFFICIENT THEN TRANSPOSE FOR SPARSE MATRICES
C
C
C     TRANSPOSE IS SOLVED BY THE FOLLOWING EQUATION
C
C                                    T
C                  --    --   --   --  --   --
C                  I      I   I     I  I     I
C                  I  AT  I = I  A  I  I  I  I
C                  I      I   I     I  I     I
C                  --    --   --   --  --   --
C
C     WHERE I IS AN IDENITY MATRIX
C
C
      INTEGER       A        ,AT       ,I        ,SCR1     ,MCB(7)
     1             ,Z        ,SYSBUF   ,NAME(2)
C
C     SYSTEM PARAMETERS
C
      COMMON /SYSTEM /        SYSBUF
C
C     PACK COMMON
C
      COMMON / ZBLPKX /       VAL(4)   ,IROW
C
C     OPEN CORE
C
      COMMON / ZZZZZZ /        Z(1)
C
      DATA NAME / 4HGFST , 4HRN   /
C
C***********************************************************************
C
      NZ = KORSZ(Z)
      IBUF = NZ - SYSBUF
      IF(IBUF .LT. 0) CALL MESAGE(-8,0,NAME)
C
C     GET MATRIX TRAILER
C
      MCB(1) = A
      CALL RDTRL(MCB)
      IF(MCB(1) .LT. 0) RETURN
      IR = MCB(3)
C
C     GENERATE A SQUARE IDENITY MATRIX   IR BY IR
C
      VAL(1) = 1.0
      CALL MAKMCB(MCB,I,IR,2,2)
      CALL GOPEN(I,Z(IBUF),1)
C
      DO 10 IROW=1,IR
      CALL BLDPK(1,2,I,0,0)
      CALL ZBLPKI
      CALL BLDPKN(I,0,MCB)
   10 CONTINUE
      CALL CLOSE(I,1)
      CALL WRTTRL(MCB)
C
C     PERFORM MULTIPLY
C
      CALL SSG2B(A,I,0,AT,1,2,1,SCR1)
C
      RETURN
      END
