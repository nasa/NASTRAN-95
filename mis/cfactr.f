      SUBROUTINE CFACTR (A,LL,UL,SCR1,SCR2,SCR3,IOPT)
C
      INTEGER           FA        ,FL       ,FU       ,SR1      ,
     1                  SR2       ,SR3      ,UL       ,SCR1     ,
     2                  SCR2      ,SCR3     ,A        ,
     3                  MCB(7)    ,NAME(2)
      DOUBLE PRECISION  DET       ,MIND
      COMMON   /CDCMPX/ FA(7)     ,FL(7)    ,FU(7)    ,SR1      ,
     1                  SR2       ,SR3      ,DET(2)   ,POWR     ,
     2                  NX        ,MIND     ,IB       ,IBBAR
      COMMON   /SFACT / MFA(7)    ,MFL(7)   ,MFC(7)   ,M1FIL    ,
     1                  M2FIL     ,MXX      ,D(5)     ,M3FIL    ,
     2                  D1(2)     ,ICHOL
      COMMON   /SDCCSP/ JFA(7)    ,JFL(7)   ,JFC(7)   ,J1FIL    ,
     1                  J2FIL     ,JX
      COMMON   /ZZZZZZ/ IZ(1)
      DATA      NAME  / 4HCFAC,4HTR   /
C
C
      NZ = KORSZ(IZ)
      MCB(1) = A
      CALL RDTRL (MCB)
      IF (MCB(4) .NE. 6) GO TO 200
C
C     SYMMETRIC  COMPLEX
C
      DO 10 I = 1,7
      MFA(I) = MCB(I)
      MFL(I) = MCB(I)
      MFC(I) = MCB(I)
   10 CONTINUE
      MFL(1) = LL
      MFC(1) = UL
      MFL(4) = 4
      MFC(4) = 5
      M1FIL  = SCR1
      M2FIL  = SCR2
      MXX    = NZ
      M3FIL  = SCR3
      ICHOL  = 0
      CALL SDCOMP (*900,IZ,IZ,IZ)
      CALL WRTTRL (MFL)
      IOPT  = 2
      GO TO  60
C
C     UNSYMMETRIC  COMPLEX
C
  200 DO 210 I = 1,7
      FA(I) = MCB(I)
      FL(I) = MCB(I)
      FU(I) = MCB(I)
  210 CONTINUE
      FL(1) = LL
      FU(1) = UL
      FL(4) = 4
      FU(4) = 5
      SR1   = SCR1
      SR2   = SCR2
      SR3   = SCR3
      NX    = NZ
C     IB    = 0
C
C     IF IB IS SET TO ZERO HERE, T08021 PRINTS 27 MORE MESSAGES 3027
C     AND 3028 FROM GENVEC WHICH IS CALLED BY CFACTR, WHCIH IS CALLED BY
C     FRD2C, IN FRRD2 MODULE
C
CIBMI 6/93
      IBBAR = 0
      CALL CDCOMP (*900,IZ,IZ,IZ)
      CALL WRTTRL (FU)
      CALL WRTTRL (FL)
      IOPT  = 1
   60 RETURN
C
C     ERRORS
C
  900 CALL MESAGE (-5,A,NAME)
C
      RETURN
      END
