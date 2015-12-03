      SUBROUTINE MCE1C
C
C     MCE1C PERFORMS A FORWARD-BACKWARD SUBSTITUTION WITH THE
C     TRIANGULAR FACTORS OF RM TO SOLVE FOR GM IN THE EQUATION
C     RM*GM = -RN.
C
C
      INTEGER USET  , RG    ,GM    ,SCR1  ,SCR2  ,SCR3  ,RM    ,RN     ,
     1        U     , UX    ,RNX   ,GMX   ,PREC  ,SIGN
      COMMON /BLANK / USET  ,RG    ,GM    ,SCR1  ,SCR2  ,SCR3  ,RM    ,
     1                RN    ,L     ,U     ,MCB(7)
      COMMON /GFBSX / LX (7),UX (7), RNX(7),GMX(7),NZ   ,PREC  ,SIGN
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SYSTEM/ KSYSTM(65)
      EQUIVALENCE     (KSYSTM(55),IPREC)
C
C     INITIALIZE MATRIX CONTROL BLOCKS
C
      NZ = KORSZ(Z)
      LX(1)  = L
      CALL RDTRL (LX)
      UX(1)  = U
      CALL RDTRL (UX)
      RNX(1) = RN
      CALL RDTRL (RNX)
      GMX(1) = GM
      GMX(3) = RNX(3)
      GMX(4) = RNX(4)
      GMX(5) = IPREC
      PREC   = IPREC
      SIGN   =-1
C
C     PERFORM SOLUTION
C
      CALL GFBS (Z,Z)
C
C     WRITE TRAILER
C
      CALL WRTTRL (GMX)
      RETURN
      END
