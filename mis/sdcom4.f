      SUBROUTINE SDCOM4( P, AC, WA, WB )
C******
C
C SDCOM4 COMPUTES THE CONTRIBUTIONS OF THE PIVOT ROW FOR SDCOMP IN CDP
C
C******
      INTEGER AC(1), ROW, C, START, SC
C
      DOUBLE PRECISION P(2), WA(1), WB(1), PIR, PII
      DOUBLE PRECISION  P1,  P2
C
      COMMON/ SDCOMX / ROW, C, SPFLG, START, FRSTPC, LASTPL, LASTI, SC
C
      J = 1
      L = 1
C
C FOR THE OUTER LOOP I RUNS FROM START TO LASTI.
C BEGIN BY FORMING -P(I)/P(1). THEN DECIDE WHICH INNER LOOP TO EXECUTE
C
      P2 = P(1)**2 + P(2)**2
      P1 = P(1) / P2
      P2 = P(2) / P2
      DO 48 I=START,LASTI
      PIR = -P(2*I-1) * P1 - P(2*I) * P2
      PII =  P(2*I-1) * P2 - P(2*I) * P1
      IF( I .LE. LASTPL ) GO TO 30
      IF( AC(I) .LT. 0 ) GO TO 20
      K1 = I
C
C LOOP 1 -- L IS INCREMENTED WHENEVER AC(K) .GT. 0
C
   10 DO 18 K=K1,C
      IF( AC(K) .GT. 0 ) GO TO 12
      WB(J  ) = PIR*P(2*K-1) - PII*P(2*K  )
      WB(J+1) = PIR*P(2*K  ) + PII*P(2*K-1)
      GO TO 14
   12 WB(J  ) = PIR*P(2*K-1) - PII*P(2*K  ) + WA(L  )
      WB(J+1) = PIR*P(2*K  ) + PII*P(2*K-1) + WA(L+1)
      L = L + 2
   14 J = J + 2
   18 CONTINUE
      GO TO 40
C
C LOOP 2 -- L IS NEVER INCREMENTED
C
   20 DO 28 K=I,C
      WB(J  ) = PIR*P(2*K-1) - PII*P(2*K  )
      WB(J+1) = PIR*P(2*K  ) + PII*P(2*K-1)
      J = J + 2
   28 CONTINUE
      GO TO 40
C
C LOOP 3 -- K RUNS FROM I TO LASTPL AND L IS INCREMENTED EVERY TIME
C           THEN, IF LASTPL .LT. C, LOOP 1 IS EXECUTED TO FINISH IT UP
C
   30 DO 38 K=I,LASTPL
      WB(J  ) = PIR*P(2*K-1) - PII*P(2*K  ) + WA(L  )
      WB(J+1) = PIR*P(2*K  ) + PII*P(2*K-1) + WA(L+1)
      L = L + 2
      J = J + 2
   38 CONTINUE
      IF( LASTPL .EQ. C ) GO TO 40
      K1 = LASTPL + 1
      GO TO 10
C
C END OUTER LOOP BY STORING -P(I)/P(1) AT P(1).
C
   40 P(2*I-1 ) = PIR
      P(2*I   ) = PII
   48 CONTINUE
      RETURN
      END
