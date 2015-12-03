      SUBROUTINE RAND2 (FILE,ILIST,LOAD,IF,LEN,LLIST)
C
C     READS FILE UNTIL IT FINDS DATA RECORD IN LIST - RETURNS LOAD,
C     IF, AND LEN
C
      INTEGER         FILE,IDR(10),NAME(2),ILIST(2),MID(2,10),ITEMP(5),
     1                IDATA(50),IFMT(2,84),IFMTT(11),DATA1(100),DATA(1),
     2                ITB(180),ITB1(137),ITB2(145),FILEX
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /SYSTEM/ IBUF,NOUT
      EQUIVALENCE     (ITB1(1),ITB(1)), (ITB2(1),ITB(138))
      DATA    IFMTT / 1,11,41,55,61,99,121,155,181,199,237/
      DATA    IOLDLD/ 0 /
      DATA    IFMT  /
     1        1, 1,    -1,-1,     1, 1,     1, 1,     1, 1,     6, 2,
     7        6, 2,     6, 2,     0, 3,     1, 1,     4, 4,     4, 4,
     3        4, 4,     4, 0,     6, 2,     0, 3,     6, 2,     6, 2,
     9        6, 2,    -1,-1,    -1,-1,    -1,-1,    -1,-1,    -1,-1,
     5       -1,-1,    -1,-1,    -1,-1,    -1,-1,    -1,-1,    -1,-1,
     1       -1,-1,    -1,-1,    -1,-1,     7, 5,    -1,-1,    -1,-1,
     7       -1,-1,    -1,-1,     0, 8,     0, 8,     0, 8,     0, 8,
     3       -1,-1,    -1,-1,    -1,-1,    -1,-1,     0, 9,     0,10,
     9        0,11,     0, 6,     0, 8,    -1,-1,    -1,-1,    -1,-1,
     5       -1,-1,    -1,-1,    -1,-1,    -1,-1,    -1,-1,    -1,-1,
     1       -1,-1,     0, 3,     0, 3,     7, 2,    -1,-1,    -1,-1,
     7       -1,-1,    -1,-1,    -1,-1,    -1,-1,    -1,-1,    -1,-1,
     3       -1,-1,    -1,-1,    -1,-1,    -1,-1,    -1,-1,    -1,-1,
     9       -1,-1,    -1,-1,    -1,-1,    -1,-1,     7, 2,    -1,-1/
C
C     IFMT TABLE (ELEM ID IN GPTABD ORDER) HAS 2 WORDS PER ENTRY
C        WORD1  FORCE  FORMAT POINTER INTO IFMTT TABLE
C        WORD2  STRESS FORMAT POINTER INTO IFMTT TABLE
C
C     IFMTT TABLE  HAS ONE ENTRY PER FORMAT TYPE
C        THE ENTRY IS THE BEGINNING OF THE FORMAT IN THE ITB TABLE
C
      DATA ITB1/
     1   6,   3,   5,   4,   6,
     1   0,   1,   1,   2,   2,
     2  16,   3,   4,  12,   5,  13,   6,  14,   7,   8,  16,   9,  17,
     *  10,  18,
     2   0,   1,   2,   2,   3,   3,   4,   4,   5,   6,   6,   7,   7,
     *   8,   8,
     3   8,   3,   6,   4,   7,   5,   8,
     3   0,   1,   1,   2,   2,   3,   3,
     4   4,   3,   4,
     4   0,   1,   1,
     5  20,   3,   4,   5,   6,   7,  12,  13,  14,  15,  16,   8,   9,
     *  10,  11,  17,  18,  19,  20,
     5   0,   1,   2,   3,   4,   5,   1,   2,   3,   4,   5,   6,   7,
     *   8,   9,   6,   7,   8,   9,
     6  12,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,
     6   0,   1,   2,   3,   4,   5,   1,   2,   3,   4,   5,
     7  18,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,
     *  15,  16,  17,  18/
      DATA ITB2/
     7   0,   1,   2,   3,   4,   5,   6,   7,   8,   1,   2,   3,   4,
     *   5,   6,   7,   8,
     8  14,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,
     8   0,   1,   2,   3,   4,   5,   6,   1,   2,   3,   4,   5,   6,
     9  10,   3,   4,   5,   6,   7,   8,   9,  10,
     9   0,   1,   2,   3,   4,   1,   2,   3,   4,
     O  20,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,
     *  15,  16,  17,  18,  19,  20,
     O   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   1,   2,   3,
     *   4,   5,   6,   7,   8,   9,
     A  24,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,
     *  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,
     A   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,   1,
     *   2,   3,   4,   5,   6,   7,   8,   9,  10,  11/
      DATA NAME/ 4HRAND,4H2   /
      DATA MID / 3001  ,4HDISP,
     1           3010  ,4HVELO,
     2           3011  ,4HACCE,
     3           3002  ,4HLOAD,
     4           3003  ,4HSPCF,
     5           3004  ,4HELFO,
     6           3005  ,4HSTRE,
     7           3015  ,4HDISP,
     8           3016  ,4HVELO,
     9           3017  ,4HACCE/
C
      FILEX = FILE
C
C     POSITION TO + READ ID RECORD
C
    5 CALL FWDREC (*950,FILE)
      CALL READ (*950,*920,FILE,IDR,10,1,I)
      IDR(5) = IDR(5)/10
C
C     IDR(5) = 10*ELEM.ID + DEV.CODE
C     IDR(2) = GINO FILE 3004, 3005 ETC.
C     CONVERT MAJOR ID TO MNEMONIC
C
      DO 10 I = 1,10
      IF (IDR(2) .EQ. MID(1,I)) GO TO 20
   10 CONTINUE
C
C     ILLEGAL FORMAT
C
      GO TO 970
C
C     CHECK FOR  MID
C
   20 IF (ILIST(1) .NE. MID(2,I)) GO TO 5
      IELEM = I
C
C     LOOK FOR ID IN LIST
C
      DO 30 I = 1,LLIST,5
      IF (IDR(5)-ILIST(I+1)) 5,50,30
   30 CONTINUE
      GO TO 5
C
C     ID IS IN  LIST
C
   50 I = I - 1
      IF (I .EQ. 0) GO TO 100
C
C     FLIP LIST ORDER
C
      M  = 0
      LL = I
C
C     SAVE CURRENT STUFF AT END OF LIST
C
   54 DO 51 J = 1,5
      L = I + J
      ITEMP(J) = ILIST(L)
   51 CONTINUE
C
C     PUSH DOWN LIST
C
      DO 52 J = 1,LL
      K = I - J + 1
      ILIST(K+5) = ILIST(K)
   52 CONTINUE
C
C     RESTORE CURRENT STUFF IN FRONT OF LIST, IN FLIPPED ORDER
C
      DO 53 J = 1,5
      K = M + J
      ILIST(K) = ITEMP(J)
   53 CONTINUE
C
C     AGAIN
C
      IF (ILIST(I+7) .NE. ITEMP(2)) GO TO 100
      M = M + 5
      I = I + 5
      GO TO 54
C
C     FOUND IT
C
C     IDR( 3) = ELEMENT TYPE
C     IDR( 4) = SUBCASE NO.
C     IDR( 9) = FORMAT CODE, 1=REAL, 2=REAL/IMAG, 3=MAG/PHASE
C     IDR(10) = NO. OF WORDS PER ENTRY
C     IELEM   = 6 OR 7 FOR ELFORCE (OEFC2) OR STRESS (OESC2)
C
  100 LOAD = IDR(4)
      IF   = 0
      IF (IDR(9) .EQ. 3) IF = 1
      LEN1 = IDR(10)
      LEN  = LEN1
      IELTP= IDR(3)
      IF (IELEM.LT.6 .OR. IELEM.GT.7) GO TO 150
C
C     EXECUTE THIS PORTION FOR STRESSES AND FORCES
C
C     FIND FORMAT TYPE
C
      IF (IFMT(1,IELTP) .EQ. -1) GO TO 930
C
C     PICK UP FORMAT POINTER
C
      IFMTP = IFMT(IELEM-5,IELTP)
      IF (IFMTP  .EQ. 0) GO TO 970
      J = IFMTT(IFMTP)
C
C     SAVE EXTERNAL DATA LENGTH
C
      LEN  = ITB(J)
C
C     SAVE MAP OF ITB
C
      DO 130 I = 1,LEN1
      K = J + I - 1
      IDATA(I) = ITB(K)
  130 CONTINUE
C
C     CONVERT POINTERS TO NEW DATA VALUES
C
      IF (IOLDLD .EQ.    0) GO TO 131
      IF (IOLDLD .NE. LOAD) GO TO 150
  131 IOLDLD = LOAD
      DO 140 I = 1,LLIST,5
      IF (ILIST(I).NE.MID(2,IELEM) .OR. ILIST(I+1).NE.ILIST(2))
     1    GO TO 150
      K = ILIST(I+2)
      IF (K .LE. LEN1) GO TO 141
C
C     POINTER OUT OF RANGE
C
      CALL MESAGE (52,ILIST(I),ILIST(I+1))
      K = LEN1
  141 K = J + K - 1 + LEN1
      ILIST(I+2) = ITB(K)
  140 CONTINUE
  150 ICHK  = 1234321
      LENX  = LEN
C
C     FILE AND LEN WERE SAVED LOCALLY IN FILEX AND LENX, SO THAT THEY
C     CAN BE USED IN RAND2A
C
      RETURN
C
C
      ENTRY RAND2A (DATA)
C     ===================
C
C     WILL OBTAIN DATA AND REFORMAT IF NECESSARY
C
C     READ DATA
C
      IF (ICHK .NE. 1234321) CALL MESAGE (-37,0,NAME)
      CALL READ (*910,*920,FILEX,DATA(1),LEN1,0,IFLAG)
      IF (IELEM .LT. 6) RETURN
C
C     APPLY DATA MAP  I.E. REARRANGE DATA ACCORDING TO DATA MAP
C
      DO 170 I = 1,LENX
      DATA1(I) = 0
  170 CONTINUE
      DATA1(1) = DATA(1)
      DO 180 I = 2,LEN1
      J = IDATA(I)
      DATA1(J) = DATA(I)
  180 CONTINUE
CWKBR 9/93 DO 190 I = 1,LEN
      DO 190 I = 1,LENX
      DATA(I)  = DATA1(I)
  190 CONTINUE
      RETURN
C
C     FILE ERRORS
C
  910 IP1 = -2
  911 CALL MESAGE (IP1,FILEX,NAME)
  920 IP1 = -3
      GO TO 911
  930 WRITE  (NOUT,940) UWM,IELTP
  940 FORMAT (A25,' 2185, CURRENTLY RAND2 ROUTINE DOES NOT PROCESS ',
     1        'ELEMENT TYPE',I5)
      GO TO 5
C 950 LOAD = 0
  950 CALL REWIND (FILEX)
CWKBI 9/93
      WRITE(NOUT,9901)
9901  FORMAT(' THE FOLLOWING I/O ERROR OCCURRED MOST LIKELY BECAUSE'
     &,/,' THERE WAS A PLOT REQUEST FOR A POINT THAT DOES NOT EXIST.')
      CALL MESAGE (-2,FILE,NAME)
      GO TO 150
  970 IP1 = -7
      GO TO 911
      END
