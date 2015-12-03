      SUBROUTINE VEC
C
C     THE CALL TO THIS MODULE IS
C                   VEC USET  / V / C,N,X / C,N,X0 / C,N,X1 $
C          OR       VEC USETD / V / C,N,X / C,N,X0 / C,N,X1 $
C
C     ALTERNATE FORM OF THE CALL TO THIS MODULE IS
C                   VEC USET  / V / C,N,X / C,N,X0 / C,N,COMP $
C          OR       VEC USETD / V / C,N,X / C,N,X0 / C,N,COMP $
C
C     ALTERNATE FORM OF THE CALL TO THIS MODULE IS
C                   VEC USET  / V / C,N,X / C,N,COMP / C,N,X1 $
C          OR       VEC USETD / V / C,N,X / C,N,COMP / C,N,X1 $
C
C     ALTERNATE FORM OF THE CALL TO THIS MODULE IS
C                   VEC USET  / V / C,N,BITID / C,N,* / C,N,* / C,N,I $
C          OR       VEC USET  / V / C,N,BITID / C,N,X1 $
C          OR       VEC USETD / V / C,N,BITID / C,N,* / C,N,* / C,N,I $
C          OR       VEC USETD / V / C,N,BITID / C,N,X1 $
C
C     ALTERNATE FORM OF THE CALL TO THIS MODULE IS
C                   VEC USET  / V / C,N,COLUMNS / C,N,LEFT  / C,N,* /
C                                                             C,N,I $
C          OR       VEC USETD / V / C,N,COLUMNS / C,N,LEFT  / C,N,* /
C                                                             C,N,I $
C                   ( V WILL HAVE -I- COLUMNS GENERATED FROM BIT
C                     POSITIONS 1,2,3,...,I OF USET (OR USETD) WHERE
C                     THE 32 RIGHT-MOST BITS ARE CONSIDERED, COUNTING
C                     FROM LEFT TO RIGHT. )
C
C     ALTERNATE FORM OF THE CALL TO THIS MODULE IS
C                   VEC USET  / V / C,N,COLUMNS / C,N,RIGHT / C,N,* /
C                                                             C,N,I $
C          OR       VEC USETD / V / C,N,COLUMNS / C,N,RIGHT / C,N,* /
C                                                             C,N,I $
C                   ( V WILL HAVE -I- COLUMNS GENERATED FROM BIT
C                     POSITIONS 32,31,...,33-I OF USET (OR USETD) WHERE
C                     THE 32 RIGHT-MOST BITS ARE CONSIDERED, COUNTING
C                     FROM LEFT TO RIGHT. )
C
C
C     CORE REQUIREMENTS.. ONE BUFFER PLUS USET (OR USETD).
C     FOR COLUMNS OPTION, ONE GINO BUFFER PLUS 2*USET (OR USETD) REQD.
C
C
      EXTERNAL        ANDF
      LOGICAL         LZ,L0,L1,COLS,FLAG1,FLAG2
      INTEGER         ANDF,MODNAM(2),FI,FO,F,NAM(2),T(7),TWO,
     1                P(2),P1,P2,P3,P4,BN,BLANK,TYIN,TYOU,B(2),C(2),
     2                OFFSET,D(2),LR(2,2)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /BLANK / P1(2),P2(2),P3(2),P4
     1       /ZZZZZZ/ X(1)
     2       /SYSTEM/ LB,NOUT,NERR
     3       /BITPOS/ BN(32,2)
     4       /PACKX / TYIN,TYOU,II,NN,INCR
     5       /TWO   / TWO(32)
      EQUIVALENCE     (NR,T(3))
      DATA    NERMAX, BLANK / 10,1H /
      DATA    B,C,D / 4HBITI,4HD    ,4HCOMP,4H     ,4HCOLU,4HMNS  /
      DATA    LR    / 4HRIGH,4HT    ,4HLEFT,4H     /
      DATA    MODNAM/ 4HVEC ,4H     /
      DATA    FI,FO , NBN   / 101,201, 32          /
C
C
      FLAG1  = .FALSE.
      FLAG2  = .FALSE.
      OFFSET = 0
      NERR   = 0
      LZ     = .FALSE.
      L0     = .FALSE.
      L1     = .FALSE.
      LC     = KORSZ(X) - LB
      IF (LC .LE. 0) CALL MESAGE (-8,LC,MODNAM)
      IB = LC + 1
C
C     CHECK PARAMETER VALUES AND COMPUTE MASKS.
C
      IF (P1(1).NE.D(1) .OR. P1(2).NE.D(2)) GO TO 5
      COLS = .TRUE.
      DO 3 J = 1,2
      IF (P2(1).EQ.LR(1,J) .AND. P2(2).EQ.LR(2,J)) GO TO 4
    3 CONTINUE
      J = 2
    4 J = 2*J - 3
      GO TO 13
    5 CONTINUE
      COLS = .FALSE.
      IF (P1(1).EQ.B(1) .AND. P1(2).EQ.B(2)) GO TO 13
      IF (P1(2) .NE. BLANK) GO TO 11
      DO 10 I = 1,NBN
      IF (P1(1) .EQ. BN(I,2)) GO TO 19
   10 CONTINUE
   11 P(1) = P1(1)
      P(2) = P1(2)
      GO TO 9904
   13 LZ   = .TRUE.
      L0   = .TRUE.
      IF (P4.LT.0 .OR. P4.GT.32) GO TO 9908
      IF (COLS) GO TO 50
      IF (P4 .GT. 0) GO TO 18
      IF (P2(2) .NE. BLANK) GO TO 21
      DO 15 I = 1,NBN
      IF (P2(1) .EQ. BN(I,2)) GO TO 35
   15 CONTINUE
      GO TO 21
   18 MASKX1 = TWO(P4)
      GO TO 50
   19 I = BN(I,1)
      MASKX  = TWO(I)
C
      IF (P2(1).EQ.C(1) .AND. P2(2).EQ.C(2)) GO TO 23
      IF (P2(2) .NE. BLANK) GO TO 21
      DO 20 I = 1,NBN
      IF (P2(1) .EQ. BN(I,2)) GO TO 25
   20 CONTINUE
   21 P(1) = P2(1)
      P(2) = P2(2)
      GO TO 9904
   23 L0   = .TRUE.
      GO TO 26
   25 I    = BN(I,1)
      MASKX0 = TWO(I)
C
   26 CONTINUE
      IF (P3(1).EQ.C(1) .AND. P3(2).EQ.C(2)) GO TO 33
      IF (P3(2) .NE. BLANK) GO TO 31
      DO 30 I = 1,NBN
      IF (P3(1) .EQ. BN(I,2)) GO TO 35
   30 CONTINUE
   31 P(1) = P3(1)
      P(2) = P3(2)
      GO TO 9904
   33 L1   = .TRUE.
      IF (L0) GO TO 9907
      GO TO 50
   35 I    = BN(I,1)
      MASKX1 = TWO(I)
C
C     BLAST READ USET (OR USETD) INTO CORE.
C
   50 CONTINUE
      F = FI
      CALL FNAME (F,NAM)
      CALL GOPEN (F,X(IB),0)
      CALL READ (*9902,*100,F,X,LC,0,NW)
C
C     INSUFFICIENT CORE - IF DESIRED, THIS ROUTINE CAN BE WRITTEN TO
C     RUN IN SMALLER CORE.
C
      LCEX = 0
   70 CALL READ (*9902,*80,F,X,LC,0,NW)
      LCEX = LCEX + LC
      GO TO 70
   80 LCEX = LCEX + NW
      IF (COLS) LCEX = 2*LCEX
      GO TO 9903
  100 CONTINUE
      CALL CLOSE (F,1)
      IF (.NOT.COLS) GO TO 150
      IF (P4 .LE. 0) GO TO 9908
      OFFSET = NW
      K = 1
      L = 1
      IF (J .LT. 0) K = 32
      MASKX1 = TWO(K)
      IF (2*NW .LE. LC) GO TO 150
      LCEX = 2*NW - LC
      GO TO 9903
  150 CONTINUE
C
C     PREPARE OUTPUT FILE.
C
      F = FO
      CALL GOPEN  (F,X(IB),1)
      CALL MAKMCB (T,F,0,2,1)
      TYIN = 1
      TYOU = 1
      II   = 1
      INCR = 1
C
C     CREATE VECTOR IN CORE OCCUPIED BY USET (OR USETD).
C
  170 NR = 0
      NZ = 0
C
      DO 500 I = 1,NW
      IF (LZ) GO TO 220
      IF (ANDF(X(I),MASKX) .EQ. 0) GO TO 400
  220 CONTINUE
      IF (.NOT.L0) GO TO 230
      IF (ANDF(X(I),MASKX1) .EQ. 0) GO TO 370
      GO TO 300
  230 IF (.NOT.L1) GO TO 240
      IF (ANDF(X(I),MASKX0) .EQ. 0) GO TO 300
      GO TO 370
  240 CONTINUE
      IF (ANDF(X(I),MASKX1) .EQ. 0) GO TO 350
      IF (ANDF(X(I),MASKX0) .EQ. 0) GO TO 300
      NERR = NERR + 1
      IF (NERR .GT. NERMAX) GO TO 500
      WRITE  (NOUT,250) UFM,I
  250 FORMAT (A23,' 2120, MODULE VEC - BOTH SUBSET BITS ARE NON-ZERO.',
     1       3X,'I =',I10)
      GO TO 500
  300 NR = NR + 1
      NZ = NZ + 1
      X(NR+OFFSET) = 1.0
      GO TO 500
  350 CONTINUE
      IF (ANDF(X(I),MASKX0) .NE. 0) GO TO 370
      NERR = NERR + 1
      IF (NERR .GT. NERMAX) GO TO 500
      WRITE  (NOUT,360) UFM,I
  360 FORMAT (A23,' 2121, MODULE VEC - BOTH SUBSET BITS ARE ZERO.',3X,
     1       'I =',I10)
      GO TO 500
  370 NR = NR + 1
      X(NR+OFFSET) = 0.0
      GO TO 500
  400 IF (L0) GO TO 450
      IF (ANDF(X(I),MASKX0) .EQ. 0) GO TO 450
      NERR = NERR + 1
      IF (NERR .GT. NERMAX) GO TO 450
      WRITE  (NOUT,410) UFM,I
  410 FORMAT (A23,' 2122, MODULE VEC - SET X BIT IS ZERO BUT SUBSET X0',
     1       ' BIT IS NOT.  I =',I10)
  450 IF (L1) GO TO 500
      IF (ANDF(X(I),MASKX1) .EQ. 0) GO TO 500
      NERR = NERR + 1
      IF (NERR .GT. NERMAX) GO TO 500
      WRITE  (NOUT,460) UFM,I
  460 FORMAT (A23,' 2123, MODULE VEC - SET X BIT IS ZERO BUT SUBSET X1',
     1       ' BIT IS NOT.  I =',I10)
  500 CONTINUE
C
      IF (NERR .LE. 0) GO TO 540
      IF (NERR-NERMAX) 9995,9995,9906
  540 CONTINUE
C
      IF (FLAG1) GO TO 600
      FLAG1 = .TRUE.
      IF (NR .GT. 0) GO TO 600
      WRITE  (NOUT,550) UWM
  550 FORMAT (A25,' 2124, MODULE VEC - NR=0, OUTPUT WILL BE PURGED.')
      GO TO 900
  600 IF (NZ .GT. 0) GO TO 700
      IF (FLAG2) GO TO 700
      FLAG2 = .TRUE.
      WRITE  (NOUT,650) UWM
  650 FORMAT (A25,' 2125, MODULE VEC - NZ=0, ONE OR MORE COLUMNS OF ',
     1       'OUTPUT MATRIX WILL BE NULL.')
      GO TO 750
  700 CONTINUE
C
C     PACK OUT COLUMN OF OUTPUT VECTOR.
C
  750 NN = NR
      CALL PACK (X(OFFSET+1),F,T)
      IF (.NOT.COLS .OR. L.GE.P4) GO TO 800
      L = L + 1
      K = K + J
      MASKX1 = TWO(K)
      GO TO 170
  800 CALL WRTTRL (T)
  900 CALL CLOSE  (F,1)
C
      RETURN
C
C     ERROR PROCESSING.
C
 9902 WRITE  (NOUT,9952) UFM,F,NAM
 9952 FORMAT (A23,' 2141, MODULE VEC - EOF ENCOUNTERED WHILE READING ',
     1       'GINO FILE ',I3,', DATA BLOCK ',2A4)
      GO TO 9995
 9903 WRITE  (NOUT,9953) UFM,LC,LCEX
 9953 FORMAT (A23,' 2142, INSUFFICIENT CORE FOR MODULE VEC.  AVAILABLE',
     1       ' CORE =',I11,' WORDS.', /5X,
     2       'ADDITIONAL CORE NEEDED =',I11,' WORDS.')
      GO TO 9995
 9904 WRITE  (NOUT,9954) UFM,P
 9954 FORMAT (A23,' 2143, MODULE VEC UNABLE TO IDENTIFY SET OR SUBSET ',
     1       'DESCRIPTOR ',2A4)
      GO TO 9995
 9906 WRITE  (NOUT,9956) UFM,NERR,NERMAX
 9956 FORMAT (A23,' 2145,',I8,' FATAL MESSAGES HAVE BEEN GENERATED IN',
     1       ' SUBROUTINE VEC.', /5X,
     2       'ONLY THE FIRST',I4,' HAVE BEEN PRINTED.')
      GO TO 9995
 9907 WRITE  (NOUT,9957) UFM
 9957 FORMAT (A23,' 2146, BOTH OF THE SECOND AND THIRD VEC PARAMETERS ',
     1       'REQUEST COMPLEMENT.')
      GO TO 9995
 9908 WRITE  (NOUT,9958) UFM,P4
 9958 FORMAT (A23,' 2150, ILLEGAL VALUE FOR FOURTH PARAMETER =',I11)
      GO TO 9995
 9995 CALL MESAGE (-61,0,0)
      RETURN
C
      END
