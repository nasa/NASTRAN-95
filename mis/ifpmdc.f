      SUBROUTINE IFPMDC
C
C     IFPMDC MODIFIES BULK DATA CARDS GIVEN THE INFORMATION ON IFIL
C
      EXTERNAL        LSHIFT,RSHIFT,ANDF,ORF
      LOGICAL         ABORT,CF,DIAG
      INTEGER         RET,ANDF,ORF,RSHIFT,T1,CNT,DUM,X,EXI,TEST,APPRCH,
     1                ICK(6),IVC(2),INC(2),XI(2),CON(38)
      DIMENSION       RM(1),RM1(1),CD(6)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /IFPX1 / NCDS,T1(2,1)
      COMMON /MACHIN/ MACH
      COMMON /SYSTEM/ IBUF,NOUT,ABORT,DUM(17),APPRCH,DUM1(17),NBITS,X,
     1                NCPW,DUM2(41),JRUN
      COMMON /IFPDTA/ ID(2),KN,D1(52),M(50),MF(50),M1(35),M1F(35),
     1                D2(3),NOPEN,D3(6),KNT,D4(18)
      COMMON /TWO   / ITWO(32)
      COMMON /IFPX0 / LBD,LCC,IBITS(18),IPARPT
      COMMON /ZZZZZZ/ KOR(1)
      COMMON /XSRTCM/ IM1(6),IM2(5),IM3(4),IM4(4),IM5(2),IM6,IM7(8),IM8,
     1                ISFT(4),IM9(7),ISFIM,IM10(3),MIS
      EQUIVALENCE (RM(1),M(1)),(RM1(1),M1(1)),(ICK(1),CD(1)),(K,ICK(1))
      DATA CON/4H    ,4H   0,4H   1,4H   2,4H   3,4H   4,4H   5,4H   6,
     1  4H   7,4H   8,4H   9,4H   A,4H   B,4H   C,4H   D,4H   E,4H   F,
     2  4H   G,4H   H,4H   I,4H   J,4H   K,4H   L,4H   M,4H   N,4H   O,
     3  4H   P,4H   Q,4H   R,4H   S,4H   T,4H   U,4H   V,4H   W,4H   X,
     4  4H   Y,4H   Z,4H    /
      DATA    IFIL, IEOF, ICYCL,   IEFM,   IEND,   DIAG  /
     1        213,    0,     0, -32767, 4HZZZZ, .FALSE. /
C
      IF (IEOF .EQ. -1) GO TO 190
      CNT = 0
      IF (IEOF .EQ.  1) GO TO 10
C
C     FIRST CALL INITIALIZE OPEN FILE ADJUST CORE
C
      IBUF1 = NOPEN + 2*IBUF
      NOPEN = NOPEN - IBUF
      DO 1 I = 1,38
    1 CON(I) = ANDF(CON(I),IM3(4))
      IF (NOPEN .LE. 0) GO TO 1001
      CF   = .FALSE.
      IOD  = 0
      ISC  = 0
      NF   = 0
      IONF = 0
      ILST = 0
      IEOF = 1
      CALL OPEN (*1002,IFIL,KOR(IBUF1+1),0)
    5 CALL READ (*180,*180,IFIL,ICK,6,0,NW)
C
C     CHECK INCOMING  CALL FOR VARY MATCH SORT, UNSORT AND/OR CONT
C
   10 IF (K .EQ. KN) GO TO 20
C
C     NOT CARD WE ARE WORKING ON CHECK ALPH POSITION
C
      IF (CF .OR. IOD.EQ.KN) GO TO 190
      IOD = KN
      ISC = 0
      ASSIGN 15 TO EXI
      XI(1) = T1(1,K)
      XI(2) = T1(2,K)
      GO TO 100
   15 IVC(1) = XI(1)
      IVC(2) = XI(2)
      ASSIGN 16 TO EXI
      XI(1) = T1(1,KN)
      XI(I) = T1(2,KN)
      GO TO 100
   16 INC(1) = XI(1)
      INC(2) = XI(2)
      IF (MACH .EQ. 2) GO TO 18
      INC(1) = RSHIFT(INC(1),1)
      IVC(1) = RSHIFT(IVC(1),1)
   18 IF (INC(1) .LT. IVC(1)) GO TO 190
      IF (INC(1) .GT. IVC(1)) GO TO 1004
C
C     SHIFT IN CASE OF STAR
C
      INC(2) = RSHIFT(INC(2),NBITS)
      IVC(2) = RSHIFT(IVC(2),NBITS)
      IF (INC(2)-IVC(2)) 190,1004,1004
C
C     CARD TYPE FOUND TRY ID
C
   20 IF (ICK(2) .LT. 0) GO TO 70
      IF (CF .AND. NF.NE.0 .AND. ILST.EQ.ICK(2) .AND. CNT.EQ.1) GO TO 31
      IF (CF .AND. NF.NE.0 .AND. ILST.EQ.ICK(2)) GO TO 25
      IF (CF) GO TO 190
      NF   = 0
      IONF = 0
      ASSIGN 5 TO RET
      IF (M(1) .LT. ICK(2)) GO TO 190
      IF (M(1) .GT. ICK(2)) GO TO 1004
      ILST = ICK(2)
C
C     FIND FIELD FORMAT DOES NOT COUNT FOR FIELD  1 OR 10 K1=COUNT
C
   25 DO 27 I = 1,50
      IF (MF(I) .EQ. IEFM) GO TO 30
   27 CONTINUE
      GO TO 1002
   30 NF  = NF + I - 1
      CNT = 1
   31 K1  = ICK(3)
C
C     FIND NUMBER OF FIELDS TO PITCH
C
      I  = K1/10
      J  = (K1-1)/10
      K1 = K1 - I - J - 1
C
C     CHECK TO SEE IF WE HAVE IT NOW
C
      IF (K1 .GT. NF) GO TO 60
C
C     CHECK FORMAT FIELD FOR TYPE
C
      K1 = K1 - IONF
      IF (MF(K1).NE.2 .AND. MF(K1).NE.0) GO TO 1003
      J = 0
      DO 36 I = 1,K1
      J = J + 1
      IF (MF(I) .GT. 2) J = J +1
   36 CONTINUE
C
C     PERFORM VARY
C
      IF (CD(6) .EQ. 0.0) GO TO 38
      RM(J) = RM(J)*(1.0 + CD(4)*CD(5))**CD(6)
      IF (DIAG) WRITE (NOUT,1000) UIM,T1(1,K),T1(2,K),KNT,ICK(2),ICK(3),
     1          RM(J)
      GO TO 40
   38 RM(J ) = RM(J) + CD(4)*CD(5)
      MF(K1) = 2
      IF (DIAG) WRITE (NOUT,1000) UIM,T1(1,K),T1(2,K),KNT,ICK(2),ICK(3),
     1          RM(J)
      GO TO 40
C
C     SET RESTART BITS
C
   40 IF (APPRCH .GE. 0) GO TO 50
C
C     CHECK FOR PARAM CARDS (82)
C
      IF (KN .NE. 82) GO TO 45
      DO 41 I = IPARPT,NCDS
      IF (M(1).EQ.T1(1,I) .AND. M(2).EQ.T1(2,I)) GO TO 42
   41 CONTINUE
      GO TO 50
   42 J = I - 1
      GO TO 46
   45 J = KN - 1
   46 KARL = 1
      IF (ICYCL .EQ. 0) IBITS(KARL) = ORF(IBITS(KARL),RSHIFT(1,(X-1)))
      ICYCL = (J/31) + KARL
      IPOS  = MOD(J,31) + 2
      IBITS(ICYCL) = ORF(IBITS(ICYCL),ITWO(IPOS))
   50 GO TO RET, (5,90)
   60 IF (M1(1).NE.0 .AND. M1(2).NE.0) GO TO 1004
      GO TO 190
C
C     SORTED TYPE OF IDS NEED TO COUNT PARENTS IN THE GROUP
C
   70 CONTINUE
      IF (CF .AND. NF.NE.0 .AND. ISC.EQ.ICK(2) .AND. CNT.EQ.1) GO TO 31
      IF (CF .AND. NF.NE.0 .AND. ISC.EQ.ICK(2)) GO TO 25
      IF (CF) GO TO 190
      IF (CNT .EQ. 1) GO TO 80
      CNT = 1
      NF  = 0
      IONF= 0
      ASSIGN 90 TO RET
      ISC = ISC - 1
   80 CONTINUE
      IF (ISC .GT. ICK(2)) GO TO 190
      IF (ISC-ICK(2)) 1004,25,25
C
C     FOUND ID FIND FIELD
C
   90 CALL READ (*180,*180,IFIL,ICK,6,0,NW)
      IF (K.EQ.KN .AND. NF.NE.0 .AND. ISC.EQ.ICK(2)) GO TO 31
      GO TO 10
C
C     CHANGE EXTERNAL BCD TO INTERNAL BCD FOR SORT TEST
C
  100 DO 150 I = 1,2
      ITM  = XI(I)
      DO 130 J = 1,4
      JI   = 5 - J
      ISTS = ISFT(JI)
      TEST = RSHIFT(ANDF(ITM,IM3(J)),ISTS)
      DO 110 L = 1,37
      IF (TEST .EQ. CON(L)) GO TO 120
  110 CONTINUE
      L = 1
      GO TO 140
  120 ITM = ORF(ANDF(ITM,IM4(J)),LSHIFT(L,ISTS +ISFIM))
      IF (L .EQ. 1) GO TO 140
  130 CONTINUE
  140 XI(I) = ITM
      IF (L .EQ. 1) GO TO 160
  150 CONTINUE
  160 GO TO EXI, (15,16)
C
C     IFP IS DONE BUT VARY IS NOT   MESSAGES FOR ANY LEFT
C
  170 WRITE (NOUT,1014) UFM,T1(1,K),T1(2,K),ICK(2),ICK(3)
      CALL READ (*180,*180,IFIL,ICK,6,0,NW)
      GO TO 170
C
C     END OF IFIL
C
  180 CALL CLOSE (IFIL,1)
      IEOF  = -1
      NCORE = NCORE + IBUF
C
  190 CF = .FALSE.
      IONF = NF
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) CF = .TRUE.
      IF (M1(1) .NE. IEND) GO TO 200
C
C     LAST TIME ENTERED MAKE SURE FILE IS USED UP
C
      IF (IEOF .GE. 0) GO TO 170
  200 RETURN
C
C     ERROR MESSAGES
C
 1000 FORMAT (A29,' 3310, CARD TYPE ',2A4,' SORTED',I9,' ID',I9,
     1       ' FIELD',I9,' CHANGED TO ',E16.8)
 1001 WRITE  (NOUT,1011) UFM
 1011 FORMAT (A23,' 303, NO OPEN CORE IFP')
      GO TO  1111
 1002 WRITE  (NOUT,1012) SFM
 1012 FORMAT (A25,' 3037, ERROR IN IFPMDC')
      GO TO  1111
 1003 WRITE  (NOUT,1013) UFM,T1(1,K),T1(2,K),KNT,ICK(2),ICK(3)
 1013 FORMAT (A23,' 0301, FIELD TO VARY IS NOT A REAL NUMBER. CARD ',
     1        2A4,'SORTED',I9,' ID',I9,' FIELD',I9)
      ABORT = .TRUE.
      GO TO RET, (5,90)
 1004 WRITE  (NOUT,1014) UFM,T1(1,K),T1(2,K),ICK(2),ICK(3)
 1014 FORMAT (A23,' 520, CARD TO VARY NOT FOUND. CARD ',2A4,' ID',I9,
     1       ' FIELD',I9)
      GO TO RET, (5,90)
 1111 ABORT = .TRUE.
      NOPEN = NOPEN + IBUF
      IEOF  = -1
      GO TO 190
      END
