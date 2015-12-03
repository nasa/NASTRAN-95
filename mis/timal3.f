      SUBROUTINE TIMAL3
C
C     ASSEMBLY LANGUAGE ROUTINE FOR TIMTS3
C
C     NOTES FROM G.CHAN/UNISYS, 10/1989
C     THIS ROUTINE IS NOT USED IN NASTRAN. IF IT IS ACTIVATED, MAKE SURE
C     THE EQUIVALENCE OF A=B=C=D=AC=BC=CC=DC=AD=BD=CD=DD IS REMOVED
C
      REAL             B(1),  C(1),  D(1)
      DOUBLE PRECISION AD(2), BD(1), CD(1), DD(1)
      COMPLEX          AC(1), BC(1), CC(1), DC(1)
C
      COMMON /BLANK /  N,M
      COMMON /ZZZZZZ/  A(1)
C
      EQUIVALENCE     (A(1),AC(1),AD(1), B(1),BC(1),BD(1),
     1                 C(1),CC(1),CD(1), D(1),DC(1),DD(1))
C
C
      ENTRY TMTRSP
C     ============
C     REAL SINGLE PRECISION - TIGHT LOOP
C
      DO 100 I = 1,N
      DO 110 J = 1,M
      D(J) = A(J)*B(J) + C(J)
  110 CONTINUE
  100 CONTINUE
      GO TO 999
C
C
      ENTRY TMMRSP
C     ============
C     REAL SINGLE PRECISION - MEDIUM LOOP
C
      DO 200 I = 1,N
      DO 210 J = 1,M
      D(J) = A(I)*B(J) + C(J)
  210 CONTINUE
  200 CONTINUE
      GO TO 999
C
C
      ENTRY TMLRSP
C     ============
C     REAL SINGLE PRECISION - LOOSE LOOP
C
      DO 300 I = 1,N
      DO 310 J = 1,M
      L = I+J-1
      D(J) = A(I)*B(L) + C(J)
  310 CONTINUE
  300 CONTINUE
      GO TO 999
C
C
      ENTRY TMTRDP
C     ============
C     REAL DOUBLE PRECISION - TIGHT LOOP
C
      DO 120 I = 1,N
      DO 130 J = 1,M
      DD(J) = AD(J)*BD(J) + CD(J)
  130 CONTINUE
  120 CONTINUE
      GO TO 999
C
C
      ENTRY TMMRDP
C     ============
C     REAL DOUBLE PRECISION - MEDIUM LOOP
C
      DO 220 I = 1,N
      DO 230 J = 1,M
      DD(J) = AD(I)*BD(J) + CD(J)
  230 CONTINUE
  220 CONTINUE
      GO TO 999
C
C
      ENTRY TMLRDP
C     ============
C     REAL DOUBLE PRECISION - LOOSE LOOP
C
      DO 320 I = 1,N
      DO 330 J = 1,M
      L = I + J - 1
      DD(J) = AD(I)*BD(L) + CD(J)
  330 CONTINUE
  320 CONTINUE
      GO TO 999
C
C
      ENTRY TMTCSP
C     ============
C     COMPLEX SINGLE PRECISION - TIGHT LOOP
C
      DO 410 I = 1,N
      DO 420 J = 1,M
      DC(J) = AC(J)*BC(J) + CC(J)
  420 CONTINUE
  410 CONTINUE
      GO TO 999
C
C
      ENTRY TMMCSP
C     ============
C     COMPLEX SINGLE PRECISION - MEDIUM LOOP
C
      DO 430 I = 1,N
      DO 440 J = 1,M
      DC(J) = AC(I)*BC(J) + CC(J)
  440 CONTINUE
  430 CONTINUE
      GO TO 999
C
C
      ENTRY TMLCSP
C     ============
C     COMPLEX SINGLE PRECISION - LOOSE LOOP
C
      DO 450 I = 1,N
      DO 460 J = 1,M
      L = I + J - 1
      DC(J) = AC(I)*BC(L) + CC(J)
  460 CONTINUE
  450 CONTINUE
      GO TO 999
C
C
      ENTRY TMTCDP
C     ============
C     COMPLEX DOUBLE PRECISION - TIGHT LOOP
C
      DO 160 I = 1,N
      DO 170 J = 1,M
C
C     D(J) AND D(J+1) CALCULATIONS WERE REVERSED
C     IN ORDER TO COUNTERACT THE ITERATIVE BUILD UP
C
      DD(J+1) = AD(J) * BD(J  ) - AD(J+1) * BD(J+1) + CD(J  )
      DD(J  ) = AD(J) * BD(J+1) + AD(J+1) * BD(J  ) + CD(J+1)
  170 CONTINUE
  160 CONTINUE
      GO TO 999
C
C
      ENTRY TMMCDP
C     ============
C     COMPLEX DOUBLE PRECISION - MEDIUM LOOP
C
      DO 260 I = 1,N
      DO 270 J = 1,M
      DD(J  ) = AD(I)*BD(J  ) - AD(I+1)*BD(J+1) + CD(J  )
      DD(J+1) = AD(I)*BD(J+1) + AD(I+1)*BD(J  ) + CD(J+1)
  270 CONTINUE
  260 CONTINUE
      GO TO 999
C
C
      ENTRY TMLCDP
C     ============
C     COMPLEX DOUBLE PRECISION - LOOSE LOOP
C
      DO 360 I = 1,N
      DO 370 J = 1,M
      L = I + J - 1
      DD(J  ) = AD(I)*BD(L  ) - AD(I+1)*BD(L+1) + CD(J  )
      DD(J+1) = AD(I)*BD(L+1) + AD(I+1)*BD(L  ) + CD(J+1)
  370 CONTINUE
  360 CONTINUE
C
  999 RETURN
      END
