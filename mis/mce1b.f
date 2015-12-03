      SUBROUTINE MCE1B
C
C     MCE1B DECOMPOSES RM INTO LOWER AND UPPER TRIANGULAR FACTORS
C
      INTEGER USET  , RG    ,GM    ,SCR1  ,SCR2  ,SCR3  ,RM    ,RN   ,
     1        A     , UX    ,SCRX1 ,SCRX2 ,SCRX3 ,NAM(2),U
      DOUBLE PRECISION DET  ,MINDIA
      COMMON /BLANK / USET  ,RG    ,GM    ,SCR1  ,SCR2  ,SCR3  ,RM    ,
     1                RN    ,L     ,U     ,MCB(7)
      COMMON /DCOMPX/ A(7)  ,LX(7) ,UX(7) ,SCRX1 ,SCRX2 ,SCRX3 ,DET   ,
     1                POWER ,NZ    ,MINDIA
      COMMON /ZZZZZZ/ Z(1)
      DATA    NAM   / 4HMCE1,4HB   /
C
C     INITIALIZE MATRIX CONTROL BLOCKS
C
      NZ    = KORSZ(Z)
      A(1)  = RM
      CALL RDTRL (A)
      LX(1) = L
      LX(3) = A(3)
      LX(4) = 4
      LX(5) = A(5)
      UX(1) = U
      UX(3) = A(3)
      UX(4) = 5
      UX(5) = A(5)
      SCRX1 = SCR1
      SCRX2 = SCR2
      SCRX3 = SCR3
C
C     PERFORM DECOMPOSITION
C
      CALL DECOMP (*40,Z,Z,Z)
C
C     WRITE TRAILERS
C
      CALL WRTTRL (LX)
      CALL WRTTRL (UX)
      RETURN
C
C     FATAL ERROR MESSAGE FOR SINGULAR MATRIX
C
   40 CALL MESAGE (-5,RM,NAM)
      RETURN
      END
