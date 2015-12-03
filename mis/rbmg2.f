      SUBROUTINE RBMG2
C
      INTEGER          SCR1,    SCR2,    SCR3,    SCR4
      DOUBLE PRECISION DET(2)
      COMMON /BLANK /  JPOWR,   DETRM
      COMMON /SFACT /  QQ(29)
      EQUIVALENCE      (QQ(25), DET(1)), (QQ(29), IPWR)
      DATA    KLL   ,  LLL,     SCR1,    SCR2,    SCR3,    SCR4  /
     1        101   ,  201,     301,     302,     303,     304   /
C
C     DECOMPOSE KLL INTO LLL
C
      CALL FACTOR (KLL,LLL,SCR1,SCR2,SCR3,SCR4)
      JPOWR = IPWR
      DETRM = DET(1)
      RETURN
      END
