      SUBROUTINE FACTRU(*,A,LLL,ULL,SCR1,SCR2,SCR3)
C
      INTEGER A,SCR1,SCR2,SCR3,ULL
      DOUBLE PRECISION DETT,MINDIA
C
      COMMON /DCOMPX/IA(7),IL(7),IU(7),ISCR1,ISCR2,ISCR3,DETT,IPOW,
     1  NZ,MINDIA,IB,IBB
      COMMON /SYSTEM/SYS(54),IPREC
      COMMON /ZZZZZZ/ XX(1)
C
C ----------------------------------------------------------------------
C
      IB = 0
      IBB = 0
      IA(1) = A
      CALL RDTRL(IA)
      IL(1)=LLL
      IU(1)=ULL
      ISCR1=SCR1
      ISCR2=SCR2
      ISCR3=SCR3
      NZ = KORSZ(XX)
      IL(3) = IA(3)
      IU(3) = IA(3)
      IL(4) =4
      IU(4) =5
      IU(5) = IPREC
      IL(5) = IPREC
      CALL DECOMP(*10,XX,XX,XX)
      CALL WRTTRL(IL)
      CALL WRTTRL(IU)
      RETURN
   10 RETURN 1
C
      END
