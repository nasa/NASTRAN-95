      SUBROUTINE WPLT10 (A,OPT)
C
C     TO WRITE PLOTTER COMMANDS FOR NASTRAN GENERAL PURPOSE PLOTTER
C     REF - NASTRAN PROGRAMMER'S MANUAL P.3.4-111
C
C     REVISED  9/1990 BY G.CHAN/UNISYS
C     SEE SGINO FOR IMPLEMENTATION OF PLT1 FILE
C
C     INPUT -
C        OPT = 0 IF ARRAY A IS A PLOT COMMAND.
C        OPT = 1 IF CURRENT SERIES OF PLOT COMMANDS IS TO BE TERMINATED
C
C     OUTPUT -
C       A(1) = PLOT MODE DIGIT
C       A(2) = CONTROL DIGIT
C       A(3) = X1 = X-COORDINATE
C       A(4) = Y1 = Y-COORDINATE
C       A(5) = X2 = X-COORDINATE
C       A(6) = Y2 = Y-COORDINATE
C
C     A PLT2 FILE PLOTTER COMMAND IS OF THE FOLLOWING FORMAT
C
C         MC1111122222333334444400000000
C            WHERE M = MODE            1 BYTE
C                  C = CONTROL         1 BYTE
C                  1 = DIGIT OF X1     5 BYTES
C                  2 = ..... .. Y1     5 BYTES
C                  3 = ..... .. X2     5 BYTES
C                  4 = ..... .. Y2     5 BYTES
C                  0 = ZERO            8 BYTES
C                              ---------------
C                              TOTAL  30 BYTES
C
C     SEE SGINO FOR PLT1 FILE PLOTTER COMMAND FORMAT
C
C     /PLTDAT/
C     EDGE = SIZE OF THE BORDERS (X,Y) IN PLOTTER UNITS,    REAL - INPUT
C     PLOT = GINO FILE NAME OF THE PLOT TAPE TO BE WRITTEN,  BCD - INPUT
C     MAXCHR = PLOT TAPE BUFFER SIZE (NUMBER OF CHARACTERS), INT - INPUT
C              (AN INTEGER MULTIPLE OF THE NUMBER OF CHARACTERS
C              PER WORD ON THE COMPUTER ON WHICH THE PLOT TAPE IS
C              BEING READ)
C
      IMPLICIT INTEGER (A-Z)
      REAL             EDGE
      INTEGER          A(6),C(30),TEN(5),ZERO(30)
      COMMON /PLTDAT/  SKPPLT(8),EDGE(12),SKPA(10),PLOT,MAXCHR
      EQUIVALENCE      (C1,C(1))
      DATA    NCHR  ,  TEN,PZERO / 0, 10000, 1000, 100, 10, 1, +0 /,
     1        PLT2  ,  NC,ZERO,C / 4HPLT2,   30,   30*0,    30*0  /
C
      IF (PLOT .EQ. PLT2) GO TO 100
C
C     PLT1 FILE - NON BYTE PACKING LOGIC
C     A FORMAT OF (5(2I3,4I5)) IS COMPOSED IN SGINO
C     =============================================
C
      NC = 6
      IF (OPT .NE. 0) GO TO 40
C
C     SET UP THE MODE AND CONTROL CHARACTERS IN THE COMMAND.
C
      C1   = A(1)
      C(2) = A(2)
C
      I3   = IFIX(EDGE(1) + .1)
      I4   = IFIX(EDGE(2) + .1)
      C(3) = A(3) + I3
      C(4) = A(4) + I4
      C(5) = A(5)
      C(6) = A(6)
      IF (C1.EQ.4 .OR. C1.EQ.14) GO TO 20
      C(5) = A(5) + I3
      C(6) = A(6) + I4
   20 CALL SWRITE (PLOT,C,NC,0)
      GO TO 200
C
C     TERMINATE A SET OF PLOT COMMANDS
C     SEND A RECORD OF ALL ZERO-S TO SWRITE
C
   40 CALL SWRITE (PLOT,ZERO,NC,0)
      CALL SWRITE (PLOT,0,0,1)
      GO TO 200
C
C     PLT2 FILE - WITH BYTE PACKING LOGIC
C     A FORMAT OF (10(180A4)) IS COMPOSED IN SGINO
C     ============================================
C
  100 IF (OPT .NE. 0) GO TO 140
C
C     SET UP THE MODE + CONTROL CHARACTERS IN THE COMMAND.
C
      C1   = A(1)
      C(2) = A(2)
C
C     SEPARATE THE DECIMAL DIGITS OF THE X + Y COORDINATES.
C
      DO 110 J = 1,4
      I = 1
      IF (J.EQ.2 .OR. J.EQ.4) I = 2
      N = A(J+2)
      IF (J.LT.3 .OR. (C1.NE.4 .AND. C1.NE.14)) N = N + IFIX(EDGE(I)+.1)
      K = 5*(J-1)
      DO 110 I = 1,5
      M = N/TEN(I)
C
C   . M MAY BE A -0 (UNIVAC), SET IT TO +0 FOR SURE
C
      IF (M .EQ. 0) M = PZERO
      C(K+3) = M
      K = K + 1
      N = N - M*TEN(I)
  110 CONTINUE
C
      CALL SWRITE (PLOT,C,NC,0)
      NCHR = NCHR + NC
      IF (NCHR .EQ. MAXCHR) NCHR = 0
      GO TO 200
C
C     TERMINATE A SET OF PLOT COMMANDS (FILL THE RECORD WITH ZERO-S).
C
  140 IF (NCHR .EQ. 0) GO TO 160
  150 CALL SWRITE (PLOT,ZERO,NC,0)
      NCHR = NCHR + NC
      IF (NCHR .NE. MAXCHR) GO TO 150
      NCHR = 0
  160 CALL SWRITE (PLOT,0,0,1)
C
  200 RETURN
      END
