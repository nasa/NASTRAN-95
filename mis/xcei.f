      SUBROUTINE XCEI
C
      IMPLICIT INTEGER (A-Z)
      EXTERNAL        LSHIFT,RSHIFT,ANDF,ORF
      DIMENSION       DCPARM(2),NXCEI(2),IDIC(1),NXPTDC(2),CONTRL(4)
      COMMON /XVPS  / VPS(1)
CWKBR COMMON /XCEITB/ CEITBL(2)
      COMMON /XCEITB/ CEITBL(42)
      COMMON /OSCENT/ BUF(7)
      COMMON /ZZZZZZ/ DATABF(1)
      COMMON /SYSTEM/ BFSZ,ISYSOU,DUM21(21),ICFIAT,DUM57(57),ICPFLG
      COMMON /XFIAT / IFIAT(3)
      COMMON /XDPL  / IDPL(3)
      EQUIVALENCE     (DATABF(1),IDIC(1))
      DATA    NXPTDC/ 4HXPTD,4HIC  /
      DATA    NXCEI / 4HXCEI,4H    /
      DATA    NOSCAR/ 4HXOSC/
      DATA    POOL  / 4HPOOL/
      DATA    CONTRL/ 4HJUMP,4HREPT,4HCOND,4HEXIT/
      DATA    NBLANK/ 4H    /
      DATA    MASK1 / 65535 /, NOFLGS / 536870911/
C
C     MASK1  = 000000177777 =     65536 = 2**16-1
C     NOFLGS = 003777777777 = 536870911 = 2**29-1
C     MASK   = 017777600000
C     LPFLG  = 010000000000
C
      MASK  = LSHIFT(MASK1,16)
      LPFLG = LSHIFT(1,30)
      CALL OPEN (*310,POOL,DATABF,2)
C
C     DETERMINE WHICH TYPE OF CONTROL REQUEST
C
      DO 10 J = 1,4
      IF (BUF(4) .EQ. CONTRL(J)) GO TO (150,110,250,270), J
   10 CONTINUE
      CALL MESAGE (-61,0,0)
C
C     PROCESS  JUMP CONTROL REQUEST
C
   30 IF (NEWSQ .GT. BUF(2)) GO TO 60
C
C     MUST BACKSPACE WITHIN OSCAR FILE
C     DUE TO GINO TECHNIQUES IT IS USUALLY FASTER TO REWIND AND FORWARD
C     REC RATHER THAN BACKREC
C
      CALL REWIND (POOL)
C
C     POSITION POOL TAPE AT BEGINNING OF OSCAR FILE
C
      JJ = IDPL(3)*3 + 1
      DO 40 J = 4,JJ,3
      IF (IDPL(J) .EQ. NOSCAR) GO TO 50
   40 CONTINUE
      CALL MESAGE (-61,0,0)
   50 CALL SKPFIL (POOL,ANDF(IDPL(J+2),MASK1)-1)
      NEWSQ = NEWSQ - 1
      GO TO 70
C
C     MUST FORWARD REC WITHIN OSCAR FILE
C
   60 NEWSQ = NEWSQ - BUF(2) - 1
      IF (NEWSQ .EQ. 0) GO TO 260
   70 DO 80 I = 1,NEWSQ
   80 CALL FWDREC (*290,POOL)
C
C     CHECK FOR REPEAT INSTRUCTION
C
      IF (BUF(4) .EQ. CONTRL(2)) GO TO 260
C
C     JUMP REQUEST - CHECK FOR JUMP OUT OF LOOPS
C
      NEWSQ = RSHIFT(ANDF(BUF(7),MASK),16)
      KK = 3
      CEITBX = 0
  100 CEITBX = 4 + CEITBX
      IF (CEITBX .GT. CEITBL(2)) GO TO 260
      IF (ANDF(CEITBL(CEITBX-1),LPFLG).EQ.0 .OR. CEITBL(CEITBX+1).EQ.0)
     1    GO TO 100
      NBEGN = RSHIFT(ANDF(CEITBL(CEITBX-1),NOFLGS),16)
      NEND  = ANDF(MASK1,CEITBL(CEITBX-1))
      IF (NEWSQ.LT.NBEGN .OR. NEWSQ.GT.NEND) GO TO 130
      GO TO 100
C
C     PROCESS  REPEAT CONTROL REQUEST
C
  110 KK = 1
  120 CEITBX = ANDF(BUF(7),MASK1)
      IF (CEITBL(CEITBX)) 121,122,122
C
C      NEGATIVE ENTRY IMPLIES VARIABLE REPT INSTRUCTION
C      FIND VALUE IN VPS AND UPDATE CEITBL
C
  121 IVPSPT = RSHIFT(ANDF(CEITBL(CEITBX),NOFLGS),16)
      LOOP   = ANDF(CEITBL(CEITBX),MASK1)
      IVPSPT = VPS(IVPSPT+3)
      CEITBL(CEITBX) = ORF(LSHIFT(IVPSPT,16),LOOP)
  122 CONTINUE
C
C     CHECK FOR END OF LOOP
C
      MXLOOP = RSHIFT(ANDF(CEITBL(CEITBX),NOFLGS),16)
      LOOP   = ANDF(CEITBL(CEITBX),MASK1)
      IF (MXLOOP .GT. LOOP) GO TO 140
C
C     REPEATS FINISHED - ZERO LOOP COUNT AND TURN OFF LOOP FLAG
C
  130 CEITBL(CEITBX  ) = ANDF(CEITBL(CEITBX  ),MASK  )
      CEITBL(CEITBX-1) = ANDF(CEITBL(CEITBX-1),NOFLGS)
      GO TO (260,280,100), KK
C
C     ANOTHER  TIME THRU - INCREMENT COUNTER BY 1
C
  140 CEITBL(CEITBX) = CEITBL(CEITBX) + 1
C
C     SET LOOP FLAG IN WORD 1 OF CEITBL ENTRY
C
      CEITBL(CEITBX-1) = ORF(CEITBL(CEITBX-1),LPFLG)
      GO TO (150,260), KK
  150 NEWSQ = RSHIFT(ANDF(BUF(7),MASK),16)
C
C     MAKE SURE WE ARE LOOPING
C
      IF (NEWSQ .GE. BUF(2)) GO TO 30
C
C     IF CHECKPOINTING - BACKUP PROBLEM TAPE DICTIONARY TO BEGINNING OF
C     LOOP
C
      IF (ICPFLG .EQ. 0) GO TO 210
C
C     READ IN CHECKPOINT DICTIONARY
C
      ITOP = 2*BFSZ + 1
      LDIC = KORSZ(IDIC(ITOP))
      CALL OPEN (*310,NXPTDC,DATABF(BFSZ+1),0)
      CALL READ (*300,*160,NXPTDC,DCPARM,2,1,NRECSZ)
  160 IF (NXPTDC(1) .NE. DCPARM(1)) CALL MESAGE (-61,0,0)
      CALL READ (*300,*170,NXPTDC,DCPARM,2,1,NRECSZ)
  170 CALL READ (*300,*180,NXPTDC,IDIC(ITOP),LDIC,1,NRECSZ)
      GO TO 310
  180 IBOT = NRECSZ + ITOP - 3
      CALL CLOSE (NXPTDC,1)
      J = IBOT
      DO 190 I = ITOP,IBOT,3
      IF (IDIC(I) .NE. NBLANK) GO TO 190
      IF (ANDF(IDIC(I+2),MASK1) .LT. NEWSQ) GO TO 190
      J = I - 3
      GO TO 200
  190 CONTINUE
  200 IBOT = J
C
C     WRITE IDIC ON NEW PROBLEM TAPE
C
      CALL OPEN  (*310,NXPTDC,DATABF(BFSZ+1),1)
      CALL WRITE (NXPTDC,NXPTDC,2,1)
      CALL WRITE (NXPTDC,DCPARM,2,1)
      CALL WRITE (NXPTDC,IDIC(ITOP),IBOT+3-ITOP,1)
      CALL CLOSE (NXPTDC,1)
C
C     SCAN FIAT FOR FILES REGENERATED NEXT TIME THRU LOOP.
C
  210 J  = IFIAT(3)*ICFIAT - 2
      JJ = IDPL(3) *3 + 1
      DO 240 I = 4,J,ICFIAT
      IF (RSHIFT(ANDF(IFIAT(I),NOFLGS),16) .GE. BUF(2)) GO TO 240
      IF (RSHIFT(ANDF(IFIAT(I),NOFLGS),16) .EQ.      0) GO TO 240
      IF (ANDF(RSHIFT(IFIAT(I),30),1)      .NE.      0) GO TO 240
C
C     LTU IS LESS THAN LOOP END - CLEAR FIAT TRAILER
C
      IFIAT(I+ 3) = 0
      IFIAT(I+ 4) = 0
      IFIAT(I+ 5) = 0
      IF (ICFIAT .EQ. 8) GO TO 212
      IFIAT(I+ 8) = 0
      IFIAT(I+ 9) = 0
      IFIAT(I+10) = 0
C
C     IF EQUIV, REMOVE ENTIRE ENTRY FROM FIAT
C     REMOVE ENTIRE ENTRY FROM FIAT TO FORCE REALLOCATION
C
  212 IHOLD = ANDF(MASK1,IFIAT(I))
      IFIAT(I  ) = 0
      IFIAT(I+1) = 0
      IFIAT(I+2) = 0
      IF (I .LT. IFIAT(1)*ICFIAT) IFIAT(I) = IHOLD
C
C     ZERO FILE NAME IF IN DPL
C
      DO 220 II = 4,JJ,3
      IF (IDPL(II).EQ.IFIAT(I+1) .AND. IDPL(II+1).EQ.IFIAT(I+2))
     1    GO TO 230
  220 CONTINUE
      GO TO 240
  230 IDPL(II  ) = 0
      IDPL(II+1) = 0
  240 CONTINUE
      GO TO 30
C
C     PROCESS  CONDITIONAL CONTROL REQUEST
C
  250 CEITBX = ANDF(BUF(7),MASK1)
      IF (VPS(CEITBX) .LT. 0) GO TO 150
  260 CALL CLOSE (POOL,2)
      RETURN
C
C     PROCESS EXIT  CONTROL REQUESTS
C
  270 KK = 2
      IF (BUF(7) .NE. CONTRL(4)) GO TO 120
  280 CALL PEXIT
  290 CALL MESAGE (-2,POOL  ,NXCEI)
  300 CALL MESAGE (-2,NXPTDC,NXCEI)
  310 CALL MESAGE (-61,0,0)
      RETURN
      END
