      SUBROUTINE MCE1A
C
C     MCE1A PARTITIONS RG INTO RM AND RN
C
      INTEGER USET   ,RG    ,GM    ,SCR1  ,SCR2  ,SCR3  ,RM    ,RN    ,
     1        UM     ,UN    ,UG    ,A     ,A11   ,A21   ,A12   ,A22   ,
     2        RULE   ,USETXX,Z     ,RECT  ,SQUARE
      COMMON /BLANK / USET  ,RG    ,GM    ,SCR1  ,SCR2  ,SCR3  ,RM    ,
     1                RN    ,L     ,U     ,MCB(7)
      COMMON /BITPOS/ UM    ,UO    ,UR    ,USG   ,USB   ,UL    ,UA    ,
     1                UF    ,US    ,UN    ,UG
      COMMON /PARMEG/ A(7)  ,A11(7),A21(7),A12(7),A22(7),N     ,RULE
      COMMON /PATX  / NZ    ,NSUB1 ,NSUB2 ,NSUB3 ,USETXX
      COMMON /ZZZZZZ/ Z(1)
      DATA    RECT  / 2 /   ,SQUARE / 1 / ,I / 1 /
C
C     GENERATE ROW PARTITIONING VECTOR
C
      NZ = KORSZ(Z)
      USETXX = USET
      CALL CALCV (SCR1,UG,UN,UM,Z)
C
C     GENERATE NULL COLUMN PARTITIONING VECTOR
C
      Z(I  ) = 0
      Z(I+2) = NSUB2
      Z(I+7) = 1
      Z(I+8) = 2
      Z(I+9) =-16777215
C
C     INITIALIZE MATRIX CONTROL BLOCKS
C
      N    = NZ
      RULE = 0
      A(1) = RG
      CALL RDTRL (A)
      A11(1) = RN
      A11(2) = NSUB1
      A11(3) = NSUB2
      A11(4) = RECT
      A11(5) = A(5)
      A12(1) = RM
      A12(2) = NSUB2
      A12(3) = NSUB2
      A12(4) = SQUARE
      A12(5) = A(5)
      MCB(1) = SCR1
      CALL RDTRL (MCB)
      A21(1) = 0
      A22(1) = 0
C
C     PARTITION RG INTO RM AND RN
C
      CALL PARTN (MCB,Z,Z)
C
C     WRITE TRAILERS FOR RM AND RN
C
      CALL WRTTRL (A12)
      CALL WRTTRL (A11)
      RETURN
      END
