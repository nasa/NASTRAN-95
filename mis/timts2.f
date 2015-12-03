      SUBROUTINE TIMTS2
C
C     TIMTS2 TIME TESTS CPU TIMES FOR VARIOUS TYPES OF LOOPS
C
C
      INTEGER         SYSBUF, OUTPUT, BUF1, BUF2, END, END2, END4,
     1                P, OPT1, OPT2, TYPE, NAME(4), TIG(16), MED(16),
     2                LOS(16), ISUBR(2)
      REAL            B(1), C(1), D(1)
      DOUBLE PRECISION ADND, AD(1), BD(1), CD(1), DD(1)
      COMPLEX         AC(1), BC(1), CC(1), DC(1), ADNC
      COMMON /BLANK / N, M, TYPE, OPT1, OPT2
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /ZZZZZZ/ A(1)
      EQUIVALENCE     (KSYSTM(1),SYSBUF), (KSYSTM(2),OUTPUT),
     1                (A(1),AC(1),AD(1), B(1),BC(1),BD(1),
     2                 C(1),CC(1),CD(1), D(1),DC(1),DD(1))
      DATA    TIG   / 1H ,4HTIGH, 4HT( R,4HSP ) ,
     1                1H ,4HTIGH, 4HT( R,4HDP ) ,
     2                1H ,4HTIGH, 4HT( C,4HSP ) ,
     3                1H ,4HTIGH, 4HT( C,4HDP ) /
      DATA    MED   / 1H ,4HMEDI, 4HUM(R,4HSP ) ,
     1                1H ,4HMEDI, 4HUM(R,4HDP ) ,
     2                1H ,4HMEDI, 4HUM(C,4HSP ) ,
     3                1H ,4HMEDI, 4HUM(C,4HDP ) /
      DATA    LOS   / 1H ,4HLOOS, 4HE (R,4HSP ) ,
     1                1H ,4HLOOS, 4HE (R,4HDP ) ,
     2                1H ,4HLOOS, 4HE (C,4HSP ) ,
     3                1H ,4HLOOS, 4HE (C,4HDP ) /
      DATA    ISUBR / 4HTIMT, 4HS2  /, M8/-8/
C
C     INITIALIZE
C
      CALL PAGE1
      WRITE  (OUTPUT,11) N,M,TYPE,OPT1
   11 FORMAT (1H  , 20X, 25HNASTRAN TIME TEST D   N =, I4, 5H, M =, I4 ,
     1        8H, TYPE =,I4, 8H, OPT1 =,I4)
      BUF1 = KORSZ(A) - SYSBUF
      BUF2 = BUF1 - SYSBUF
      END = N*M
      IF (END .GE. BUF1-1) CALL MESAGE (M8,0,ISUBR)
C
C     CPU TIME TESTS
C
      P = 4*(TYPE-1) + 1
      ASQ  = M + N
      ADNO = 1/(ASQ*ASQ)
      ADND = ADNO
      ADNC = CMPLX(ADNO,ADNO)
      END2 = END/2
      END4 = END/4
      GO TO (105,106,107,108), TYPE
C
C     REAL CPU TIME TESTS
C
  105 CONTINUE
C
      IF (M.GT.END .OR. N.GT.END) CALL MESAGE (M8,0,ISUBR)
      DO 111 I = 1,END
  111 A(I) = ADNO
      CALL CPUTIM (T1,T1,1)
      DO 100 I = 1,N
      DO 110 J = 1,M
      D(J) = A(J)*B(J) + C(J)
  110 CONTINUE
  100 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 1
      NAME(1) = TIG(P  )
      NAME(2) = TIG(P+1)
      NAME(3) = TIG(P+2)
      NAME(4) = TIG(P+3)
      GO TO 500
  501 CONTINUE
C
      DO 211 I = 1,END
  211 A(I) = ADNO
      CALL CPUTIM (T1,T1,1)
      DO 200 I = 1,N
      DO 210 J = 1,M
      D(J) = A(I)*B(J) + C(J)
  210 CONTINUE
  200 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 2
      NAME(1) = MED(P  )
      NAME(2) = MED(P+1)
      NAME(3) = MED(P+2)
      NAME(4) = MED(P+3)
      GO TO 500
  502 CONTINUE
C
      DO 311 I = 1,END
  311 A(I) = ADNO
      CALL CPUTIM (T1,T1,1)
      DO 300 I = 1,N
      DO 310 J = 1,M
      L = I + J - 1
      D(J) = A(I)*B(L) + C(J)
  310 CONTINUE
  300 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 3
      NAME(1) = LOS(P  )
      NAME(2) = LOS(P+1)
      NAME(3) = LOS(P+2)
      NAME(4) = LOS(P+3)
      GO TO 500
C
C     DOUBLE PRECISION TESTS
C
  106 CONTINUE
C
      IF (M.GT.END2 .OR. N.GT.END2) CALL MESAGE (M8,0,ISUBR)
      DO 131 I = 1,END2
  131 AD(I) = ADND
      CALL CPUTIM (T1,T1,1)
      DO 120 I = 1,N
      DO 130 J = 1,M
      DD(J) = AD(J)*BD(J) + CD(J)
  130 CONTINUE
  120 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 4
      NAME(1) = TIG(P  )
      NAME(2) = TIG(P+1)
      NAME(3) = TIG(P+2)
      NAME(4) = TIG(P+3)
      GO TO 500
  504 CONTINUE
C
      DO 231 I = 1,END2
  231 AD(I) = ADND
      CALL CPUTIM (T1,T1,1)
      DO 220 I = 1,N
      DO 230 J = 1,M
      DD(J) = AD(I)*BD(J) + CD(J)
  230 CONTINUE
  220 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 5
      NAME(1) = MED(P  )
      NAME(2) = MED(P+1)
      NAME(3) = MED(P+2)
      NAME(4) = MED(P+3)
      GO TO 500
  505 CONTINUE
C
      DO 331 I = 1,END2
  331 AD(I) = ADND
      CALL CPUTIM (T1,T1,1)
      DO 320 I = 1,N
      DO 330 J = 1,M
      L = I + J - 1
      DD(J) = AD(I)*BD(L) + CD(J)
  330 CONTINUE
  320 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 6
      NAME(1) = LOS(P  )
      NAME(2) = LOS(P+1)
      NAME(3) = LOS(P+2)
      NAME(4) = LOS(P+3)
      GO TO 500
C
C     COMPLEX SINGLE PRECISION TESTS
C
  107 CONTINUE
C
      IF (M.GT.END2 .OR. N.GT.END2) CALL MESAGE (M8,0,ISUBR)
      DO 421 I = 1,END2
  421 AC(I) = ADNC
      CALL CPUTIM (T1,T1,1)
      DO 410 I = 1,N
      DO 420 J = 1,M
      DC(J) = AC(J)*BC(J) + CC(J)
  420 CONTINUE
  410 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 7
      NAME(1) = TIG(P  )
      NAME(2) = TIG(P+1)
      NAME(3) = TIG(P+2)
      NAME(4) = TIG(P+3)
      GO TO 500
  507 CONTINUE
C
      DO 441 I = 1,END2
  441 AC(I) = ADNC
      CALL CPUTIM (T1,T1,1)
      DO 430 I = 1,N
      DO 440 J = 1,M
      DC(J) = AC(I)*BC(J) + CC(J)
  440 CONTINUE
  430 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 8
      NAME(1) = MED(P  )
      NAME(2) = MED(P+1)
      NAME(3) = MED(P+2)
      NAME(4) = MED(P+3)
      GO TO 500
  508 CONTINUE
C
      DO 461 I = 1,END2
  461 AC(I) = ADNC
      CALL CPUTIM (T1,T1,1)
      DO 450 I = 1,N
      DO 460 J = 1,M
      L = I + J - 1
      DC(J) = AC(I)*BC(L) + CC(J)
  460 CONTINUE
  450 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 9
      NAME(1) = LOS(P  )
      NAME(2) = LOS(P+1)
      NAME(3) = LOS(P+2)
      NAME(4) = LOS(P+3)
      GO TO 500
C
C     DOUBLE PRECISION COMPLEX TESTS
C
  108 CONTINUE
C
      IF (M.GT.END4 .OR. N.GT.END4) CALL MESAGE (M8,0,ISUBR)
      DO 171 I = 1,END2
  171 AD(I) = ADND
      CALL CPUTIM (T1,T1,1)
      DO 160 I = 1,N
      DO 170 J = 1,M
C
C     D(J) AND D(J+1) CALCULATIONS WERE REVERSED
C     IN ORDER TO COUNTERACT THE ITERATIVE BUILD UP
C
      DD(J+1) = AD(J)*BD(J  ) - AD(J+1)*BD(J+1) + CD(J  )
      DD(J  ) = AD(J)*BD(J+1) + AD(J+1)*BD(J  ) + CD(J+1)
  170 CONTINUE
  160 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 10
      NAME(1) = TIG(P  )
      NAME(2) = TIG(P+1)
      NAME(3) = TIG(P+2)
      NAME(4) = TIG(P+3)
      GO TO 500
  510 CONTINUE
C
      DO 271 I = 1,END2
  271 AD(I) = ADND
      CALL CPUTIM (T1,T1,1)
      DO 260 I = 1,N
      DO 270 J = 1,M
      DD(J  ) = AD(I)*BD(J  ) - AD(I+1)*BD(J+1) + CD(J  )
      DD(J+1) = AD(I)*BD(J+1) + AD(I+1)*BD(J  ) + CD(J+1)
  270 CONTINUE
  260 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 11
      NAME(1) = MED(P  )
      NAME(2) = MED(P+1)
      NAME(3) = MED(P+2)
      NAME(4) = MED(P+3)
      GO TO 500
  511 CONTINUE
C
      DO 371 I = 1,END2
  371 AD(I) = ADND
      CALL CPUTIM (T1,T1,1)
      DO 360 I = 1,N
      DO 370 J = 1,M
      L = I + J - 1
      DD(J  ) = AD(I)*BD(L  ) - AD(I+1)*BD(L+1) + CD(J  )
      DD(J+1) = AD(I)*BD(L+1) + AD(I+1)*BD(L  ) + CD(J+1)
  370 CONTINUE
  360 CONTINUE
      CALL CPUTIM (T2,T2,1)
      IRET = 12
      NAME(1) = LOS(P  )
      NAME(2) = LOS(P+1)
      NAME(3) = LOS(P+2)
      NAME(4) = LOS(P+3)
      GO TO 500
  600 CONTINUE
      RETURN
C
C
C     INTERNAL ROUTINE TO WRITE OUTPUT ONTO THE OUTPUT FILE
C
  500 TIME = T2 - T1
      ITOT = M*N
      TPEROP = 1.0E6*TIME/ITOT
      IF (IRET.EQ.2 .OR. IRET.EQ.5 .OR. IRET.EQ.8 .OR. IRET.EQ.11)
     1    WRITE (OUTPUT,998) NAME,ITOT,TIME,TPEROP
C
      IF (IRET.NE.2 .AND. IRET.NE.5 .AND. IRET.NE.8 .AND. IRET.NE.11)
     1    WRITE (OUTPUT,999) NAME,ITOT,TIME,TPEROP
C
  998 FORMAT (1H0, 4A4, ' CPU TIME FOR ', I9,
     1        ' OPERATIONS = ', E12.5, ' SECONDS'/
     2        1X , 16X, ' CPU TIME FOR ', '      ONE',
     3        ' OPERATION  = ', E12.5, ' MICROSECONDS')
C
  999 FORMAT (1H0, 4A4, ' CPU TIME FOR ', I9,
     1        ' OPERATIONS = ', E12.5, ' SECONDS'/
     2        1X , 16X, ' CPU TIME FOR ', '      ONE',
     3        ' OPERATION  = ', E12.5, ' MICROSECONDS',
     4        '  ---  DATA FOR USE IN COMMON /NTIME/')
C
      GO TO (501,502,600,504,505,600,507,508,600,510,511,600), IRET
      END
