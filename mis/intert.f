      SUBROUTINE INTERT (NL,NL1,NL2,NM,AJJ,TA)
C
      DIMENSION AJJ(1),TA(1)
C
      T1 = TA(NL1)
      T2 = TA(NL2)
      T  = TA(NL)
      N1 = NM *(NL1-1)
      N2 = NM *(NL2-1)
      N  = NM*(NL -1)
      FRACT = (T-T1) / (T2 -T1)
      DO 100 I=1,NM
  100 AJJ(I+N) = AJJ(I+N1) + FRACT *(AJJ(I+N2) - AJJ(I+N1))
      RETURN
      END
