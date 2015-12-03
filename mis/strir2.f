      SUBROUTINE STRIR2 (TI)
C
C*****
C THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE TRIANGULAR
C
C CROSS SECTION RING
C*****
C
C
C
      DIMENSION          TI(3)
      DIMENSION          DUM3(225)
      DIMENSION          STRES(100),    FORCE(25)
      DIMENSION          ISTRES(100),   IFORCE(25)
C
C
C SDR2 VARIABLE CORE
C
      COMMON   /ZZZZZZ/  ZZ(1)
C
C
C SDR2 BLOCK FOR POINTERS AND LOADING TEMPERATURES
C
      COMMON   /SDR2X4/
     1                   DUM1(33)
     2,                  ICSTM,    NCSTM,    IVEC,     IVECN
     3,                  TEMPLD,   ELDEFM
C
C
C SDR2 INPUT AND OUTPUT BLOCK
C
      COMMON   /SDR2X7/
     1                   IDEL,     IGP(3),   TZ
     2,                  SEL(36),  TS(4),    AK(81),   DUM2(99)
C
C
C SCRATCH BLOCK
C
      COMMON   /SDR2X8/
     1                   DISP(9),  EFORC(9), ESTRES(4)
C
C
      EQUIVALENCE (DUM3(1) , IDEL)
      EQUIVALENCE  (DUM3(101) , STRES(1) , ISTRES(1))
      EQUIVALENCE  (DUM3(201) , FORCE(1) , IFORCE(1))
      EQUIVALENCE (LDTEMP, TEMPLD)
C
C
C INITIALIZE COUNTERS
C
      NDOF  = 3
      NUMPT = 3
      N = NDOF * NUMPT
      NSP   = 1
      NCOMP =  4
      NS = NSP * NCOMP
C
C
C LOCATE THE DISPLACEMENTS
C
      K = 0
      DO 100 I = 1,NUMPT
      ILOC = IVEC + IGP(I) - 2
      DO 100 J = 1,NDOF
      ILOC = ILOC + 1
      K = K + 1
      DISP(K) = ZZ(ILOC)
  100 CONTINUE
C
C
C COMPUTE THE GRID POINT FORCES
C
      CALL GMMATS ( AK(1) , N, N, 0, DISP(1) , N, 1, 0, EFORC(1) )
C
C
C COMPUTE THE STRESSES
C
      CALL GMMATS ( SEL(1), NS, N, 0, DISP(1) , N, 1, 0, ESTRES(1) )
C
C
C COMPUTE THERMAL STRESS IF THERMAL LOAD EXISTS
C AND SUBTRACT FROM APPARENT STRESS
C
      IF (LDTEMP .EQ. (-1)) GO TO 300
C
      DT = (TI(1) + TI(2) +TI(3)) / 3.0E0  -  TZ
      DO 200 I = 1,NS
      ESTRES(I) = ESTRES(I) - DT * TS(I)
  200 CONTINUE
C
  300 CONTINUE
C
C
C STORE RESULTS FOR OUTPUT PRINT
C
      J = 1
      ISTRES( 1)  = IDEL
      DO 400 I = 1,NCOMP
      J = J + 1
      STRES(J) = ESTRES(I)
  400 CONTINUE
C
C
      K = 0
      J = 1
      IFORCE(1)   = IDEL
      DO 500 I = 1,NUMPT
      DO 500 KK= 1,NDOF
      J = J + 1
      K = K + 1
      FORCE(J) = EFORC(K)
  500 CONTINUE
C
      RETURN
      END
