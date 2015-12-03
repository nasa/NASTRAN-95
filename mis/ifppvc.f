      SUBROUTINE IFPPVC (*,IPVS,JR)
C
C     IFPPVC TAKES 1PARM AND 1VARY CARDS AND MAKES A SCRATCH FILE
C     TO USE IN MODIFYING OTHER BULK DATA CARDS
C
      LOGICAL         ABORT
      INTEGER         DUM,T1,BLANK,JR(1),NAME(2)
      DIMENSION       Z(1)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ IBUF,NOUT,ABORT,DUM(79),JRUN
      COMMON /IFPDTA/ ID(2),KN,D1(52),M(50),MF(50),M1(35),M1F(35),
     1                D2(3),NOPEN,D3(6),KNT,D4(18)
      COMMON /ZZZZZZ/ KOR(1)
      COMMON /IFPX1 / NCDS,T1(2,1)
      EQUIVALENCE     (KOR(1), Z(1))
      DATA    NCDSMX, IFIL,  IPLUS, ISTAR, NPTP,  ITHR,  BLANK   /
     1        343,    213,   1H+,   1H*,   4HNPTP,4HTHRU,4H      /
      DATA    IVAR,   IPAR,  IVAR1, IPAR1, NAME                  /
     1        4HAVAR, 4HAPAR,4H1VAR,4H1PAR,4HIFPP,4HVC           /
C
      ISTOP = 0
      ICS   = 0
      LTJ   = 0
      ISORT = 0
      IPP   = 2*IBUF + 2
      LN    = IPP
      II    = IPP - 1
      LST   = 0
      NV    = 0
      IDON  = 0
      IF0   = 0
      IOLDN = 0
      ISV   = 0
      IPLUS = KHRFN1(BLANK,4,IPLUS,1)
      ISTAR = KHRFN1(BLANK,4,ISTAR,1)
      CALL SSWTCH (42,L42)
      GO TO 20
C
C     READ NEW CARD
C
   10 CALL READ (*410,*410,NPTP,JR,20,1,KDUM)
      KNT = KNT + 1
   20 IT  = KHRFN1(BLANK,4,JR(1),1)
      IF (IT.EQ.IPLUS .OR. IT.EQ.ISTAR) GO TO 420
      IF (JR(1) .EQ. IVAR1) GO TO 90
      IF (JR(1) .NE. IPAR1) GO TO 300
C
C     1PARM CARDS
C
      JR(1) = IPAR
      IF (L42 .EQ. 0) CALL RCARD2 (M1,M1F,NW,JR)
      IF (L42 .NE. 0) CALL RCARD  (M1,M1F,NW,JR)
      IF (NW .NE. 10) GO TO 430
C
C     CHECK FORMAT
C
      IF (M1F(2).NE.1 .OR. M1(3).LT.0) GO TO 430
      IF (M1(3) .LT. JRUN) GO TO 10
      IF (M1F(3).NE.0 .AND. M1F(3).NE.1) GO TO 430
      IF (M1F(5).NE.0 .AND. M1F(5).NE.1) GO TO 430
      IF (M1F(7).NE.0 .AND. M1F(7).NE.1) GO TO 430
      IF (M1( 4).LT.0  .OR. M1( 6).LT.0) GO TO 430
      IF (M1( 8).LT.0  .OR. M1F(9).NE.0) GO TO 430
      IF (M1F(4).NE.2 .AND. M1F(4).NE.0) GO TO 430
      IF (M1F(6).NE.2 .AND. M1F(6).NE.0) GO TO 430
      IF (M1F(8).NE.2 .AND. M1F(8).NE.0) GO TO 430
      IF (JRUN .EQ. 0) GO TO 10
      IF (M1(3).NE.JRUN .AND. ISORT.EQ.0) GO TO 80
      IF (M1(3) .NE. JRUN) GO TO 40
C
C     FORM LIST OF K  SK PAIRS FOR THIS J
C
      ISORT = 1
   25 IF (IPP .GE. NOPEN) GO TO 440
      DO 30 I = 3,7,2
      IF (M1F(I).EQ.0 .AND. M1F(I+1).NE.0) GO TO 430
      IF (M1F(I) .EQ. 0) GO TO 30
      KOR(IPP  ) = M1(I+1)
      KOR(IPP+1) = M1(I+2)
      IPP = IPP + 2
   30 CONTINUE
      GO TO 10
C
C     SORT LIST ERROR IF DUPLICATE
C
   40 I  = LN
      IT = 0
      N  = IPP - I
      IF (N .LT. 3) GO TO 50
      CALL SORT (0,0,2,-1,KOR(I),N)
      IT = KOR(I)
      J  = N - 1
      DO 45 K = 2,J,2
      IF (KOR(I+K) .NE. IT) GO TO 42
      ABORT = .TRUE.
      WRITE (NOUT,450) UFM,IT
   42 IT = KOR(I+K)
   45 CONTINUE
   50 ISORT = 0
      IF (ICS .NE. 0) GO TO 55
      LST= N
      LN = IPP
   55 IF (JR(1) .EQ. IVAR1) GO TO 100
      IF (IDON .EQ. 1) GO TO 310
C
C     CHECK FOR DUPLICATE K ON 1PARM ON JRUN = 1
C
   80 IF (JRUN .NE. 1) GO TO 10
      IF (ICS  .NE. 0) GO TO 82
      ICS = 1
      LTJ = M1(3)
   82 IF (LTJ .EQ. M1(3)) GO TO 25
      LTJ = M1(3)
      GO TO 40
C
C     1VARY CARDS START BUILDING SCRATCH FILE
C
   90 IF (ISORT.EQ.1 .OR. LTJ.NE.0) GO TO 40
C
C     IF LST = 0 USE ALL DEFAULT VALUES FOR SK
C
  100 LTJ = 0
      NV  = NV + 1
      JR(1) = IVAR
      IF (L42 .EQ. 0) CALL RCARD2 (M1,M1F,NW,JR)
      IF (L42 .NE. 0) CALL RCARD  (M1,M1F,NW,JR)
      IF (NW.LT.10 .OR. NW.GT.12) GO TO 430
C
C     CHECK FORMAT
C
      IF (M1F(2) .NE. 3) GO TO 430
      IF (M1F(3).NE.1  .OR. M1F(4).NE.1) GO TO 430
      IF (M1( 5).LE.0  .OR. M1( 6).LE.0) GO TO 430
      IF (M1F(5).NE.0 .AND. M1F(5).NE.2) GO TO 430
      IF (M1F(6).NE.0 .AND. M1F(6).NE.2) GO TO 430
      IF (M1F(7).NE.0 .AND. M1F(7).NE.1) GO TO 430
      IF (M1F(8).NE.0 .AND. M1F(8).NE.1 .AND. M1F(8).NE.3) GO TO 430
      IF (M1F(9).NE.0 .AND. M1F(9).NE.1) GO TO 430
      IF (M1F(7).EQ.0 .AND. M1F(8).EQ.0 .AND. M1F(9).EQ.0) GO TO 430
      IF (M1F(7).EQ.1 .AND. M1( 9).EQ.0) GO TO 430
      IF (M1F(8).EQ.1 .AND. M1(10).EQ.0) GO TO 430
      I = 0
      IF (M1F(8) .EQ. 3) I = 1
      IF (M1F(9).EQ.1 .AND. M1(I+11).EQ. 0) GO TO 430
      IF (M1F(8).EQ.3 .AND. M1(10).NE.ITHR) GO TO 430
      IF (M1F(8).EQ.3 .AND. M1(9).GT.0 .AND. M1(12).LT.0) GO TO 430
      IF (M1F(8).EQ.3 .AND. M1(9).LT.0 .AND. M1(12).GT.0) GO TO 430
      IF (JRUN .EQ. 0) GO TO 10
      DO 105 KN = 1,NCDSMX
      IF (M1(3).EQ.T1(1,KN) .AND. M1(4).EQ.T1(2,KN)) GO TO 110
  105 CONTINUE
      GO TO 460
  110 IF (KN.NE.IOLDN .AND. IOLDN.NE.0) GO TO 140
  112 IOLDN = KN
C
C     START A LIST WITH THIS NUMONIC
C
      IFIELD = M1(5)
      K      = M1(6)
      IA     = M1(7)
      IB     = M1(8)
      IF (M1F(8) .EQ. 3) GO TO 120
      IF (LST+ISV+18 .GT. NOPEN) GO TO 440
      DO 115 I = 7,9
      IF (M1F(I) .EQ. 0) GO TO 115
      KOR(LN+ISV  ) = KN
      KOR(LN+ISV+1) = M1(I+2)
      KOR(LN+ISV+2) = IFIELD
      KOR(LN+ISV+3) = K
      KOR(LN+ISV+4) = IA
      KOR(LN+ISV+5) = IB
      ISV = ISV + 6
  115 CONTINUE
      GO TO 10
C
C     THRU OPTION
C
  120 N1 = M1(9)
      N2 = M1(12)
      IF (N2 .GE. N1) GO TO 125
      IT = N1
      N1 = N2
      N2 = IT
  125 IF (LST+ISV+(IABS(N2-N1)*6) .GT. NOPEN) GO TO 440
  130 KOR(LN+ISV  ) = KN
      KOR(LN+ISV+1) = N1
      KOR(LN+ISV+2) = IFIELD
      KOR(LN+ISV+3) = K
      KOR(LN+ISV+4) = IA
      KOR(LN+ISV+5) = IB
      ISV = ISV + 6
      N1  = N1  + 1
      IF (N1 .LE. N2) GO TO 130
      GO TO 10
C
C     THIS TYPE OF CARD IS DONE SORT LIST AND MAKE FILE
C     SORT ON ID THEN FIELD THEN K
C
  140 IF (ISV .EQ. 6) GO TO 150
      CALL SORT (0,0,6,-2,KOR(LN),ISV)
      CALL SORT (0,0,6,-3,KOR(LN),ISV)
      CALL SORT (0,0,6,-4,KOR(LN),ISV)
C
C     FIX UP CORE FOR THIS BUFFER AND OPEN FILE
C
  150 IF (IF0 .NE. 0) GO TO 160
      IBUF1 = NOPEN + 2*IBUF
      NOPEN = NOPEN - IBUF
      IF0   = 1
      IF (LST+ISV .GT. NOPEN) GO TO 440
      CALL OPEN (*470,IFIL,KOR(IBUF1+1),1)
C
C     TEST FOR DUPLICATE K FOR SAME FIELD AND ID PLUS SORT AND REG
C
  160 IF (ISV .EQ. 6) GO TO 220
      IT  = KOR(LN+1)
      ICS = KOR(LN+2)
      IK  = KOR(LN+3)
      DO 210 I = 7,ISV,6
      IF (IT.EQ.KOR(LN+I).AND.ICS.EQ.KOR(LN+I+1).AND.IK.EQ.KOR(LN+I+2))
     1   GO TO 170
      GO TO 180
  170 ABORT = .TRUE.
      WRITE (NOUT,480) UFM,IT,ICS,IK
  180 IF (IT.LT.0 .AND. KOR(LN+I).GT.0) GO TO 220
      IF (IT.GT.0 .AND. KOR(LN+I).LT.0) GO TO 200
  190 IT  = KOR(LN+I  )
      ICS = KOR(LN+I+1)
      IK  = KOR(LN+I+2)
      GO TO 210
  200 J = KOR(LN)
      WRITE (NOUT,485) UFM,T1(1,J),T1(2,J)
      GO TO 190
  210 CONTINUE
C
C     PUT OUT CARDS SORT TYPE OF IDS (NEG) DO IN REVERSE
C     FIND VALUES OF SK FOR EACH K
C
  220 N = 6
      I = LN
      IF (KOR(LN+1) .GT. 0) GO TO 230
      N = -6
      I = LN + ISV - 6
  230 A = 0.0
      IF (KOR(I+3).EQ.JRUN .AND. LST.EQ.0) A = 1.0
      IF (LST .EQ. 0) GO TO 250
      DO 240 K = 1,LST,2
      IF (KOR(I+3) .NE. KOR(II+K)) GO TO 240
      A = Z(II+K+1)
      GO TO 250
  240 CONTINUE
  250 Z(I+3) = A
      IT = KOR(I+1)
      ICS= KOR(I+2)
      IF (IT.GT.0 .AND .ICS.EQ.2) GO TO 260
      J = ICS/10
      J = J*10
      IF (J .NE. ICS) GO TO 270
  260 ABORT = .TRUE.
      J = KOR(LN)
      WRITE (NOUT,500) UFM,T1(1,J),T1(2,J),IT,ICS
      GO TO 280
  270 J = (ICS-1)/ 10
      J = J*10
      IF (J .EQ. ICS-1) GO TO 260
  280 CONTINUE
      IF (ABORT .OR. A.EQ.0.0) GO TO 290
      CALL WRITE (IFIL,KOR(I),6,0)
  290 I = I + N
      ISV = ISV - IABS(N)
      IF (ISV .GT. 0) GO TO 230
      ISV = 0
      IF (IDON .EQ. 1) GO TO 310
      GO TO 112
C
C     CARDS ARE DONE
C
  300 IDON = 1
      IF (JRUN .EQ. 0) GO TO 320
      IF (NV.EQ.0 .AND. JRUN.GT.0) GO TO 490
      IF (NV .EQ. 0) GO TO 310
      GO TO 140
  310 IF (JRUN .EQ. 0) GO TO 320
      CALL WRITE (IFIL,0,0,1)
      CALL CLOSE (IFIL,1)
      IPVS = 1
  320 IF (ISTOP .EQ. 0) RETURN
      IF (ISTOP .EQ. 1) RETURN 1
C
C     ERROR MESSAGES
C
  400 ABORT = .TRUE.
      IF (IDON-1) 10,310,10
  410 WRITE  (NOUT,415) UFM
  415 FORMAT (A23,', NO BULK DATA CARDS TO MODIFY.  ERROR IN IFPPVC')
      ISTOP = 1
      GO TO  310
  420 WRITE  (NOUT,425) UFM,JR
  425 FORMAT (A23,' 312, NO CONTINUATION CARD ALLOWED ON 1PARM OR ',
     1       '1VARY CARDS', /5X,'CARD- ',20A4)
      GO TO  400
  430 I = KNT +1
      WRITE  (NOUT,435) UFM,M1(1),M1(2),I,JR
  435 FORMAT (A23,' 317, ILLEGAL DATA OR FORMAT ON CARD ',2A4,' SORTED',
     1        I8 ,/5X,'CARD- ' ,20A4)
      GO TO  400
  440 WRITE  (NOUT,445) UFM
  445 FORMAT (A23,' 3008, NOT ENOUGH CORE FOR 1PARM AND 1VARY CARDS')
      GO TO  400
  450 FORMAT (A23,' 314, DUPLICATE OR NO K  ON 1PARM CARDS FOR SOME J ',
     1       'K =',I9)
  460 WRITE  (NOUT,465) UFM,M1(3),M1(4)
  465 FORMAT (A23,'316, CARD TYPE ',2A4,' NOT LEGAL ON 1VARY')
      GO TO  400
  470 CALL MESAGE (-1,IFIL,NAME)
  480 FORMAT (A23,'314, DUPLICATE K FOR  ID',I9,' FIELD',I9,' K',I9)
  485 FORMAT (A23,'316, ILLEGAL TO USE SORTED COUNT AND REGULAR ID ON ',
     1       'SAME TYPE OF CARD ',2A4)
  490 WRITE  (NOUT,495) UFM
  495 FORMAT (A23,', NO 1VARY CARDS TO GO WITH 1PARM CARDS.  ERROR IN ',
     1       'IFPPVC')
      IF (ISORT.EQ.1 .OR. LTJ.NE.0) GO TO 40
      GO TO  400
  500 FORMAT (A23,'31, CARD TYPE ',2A4,' ID =',I9,' HAS ILLEGAL FIELD',
     1        I9)
      END
