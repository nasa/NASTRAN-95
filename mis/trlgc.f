      SUBROUTINE TRLGC (TMLDTB,TRL,DIT,ITRL,FCT,FCO,TOL,IFLAG)
C
C     THE PURPOSE OF THIS SUBROUTINE IS TO PRODUCE A MATRIX OF FUNCTIONS
C     OF TIME.  EACH COLUMN IS A TIME STEP (AS DEFINE BY TRL) AND EACH
C     TERM IN A COLUMN CORRESPONDS TO A UNIQUE FUNCTION OF TIME (EITHER
C     BY TABLE FROM TLOAD, TIME DELAY, OR QVECT)
C
C     INPUTS (3)
C         TMLDTB - TABLE SHOWING TIME DEPENDANT DATA
C         TRL    - TIME STEP LIST
C         DIT    - DIRECT INPUT TABLES
C         ITRL   - SELECTED TRL SET NUMBER FROM CASECC
C
C     OUTPUTS(3)
C         FCT    - TIME FUNCTIONS AT ALL TIMES
C         FCO    - TIME FUNCTIONS AT OUTPUT TIMES
C         IFLAG  - -1 IMPLIES ALL TIMES OUTPUT (I.E. FCO = FCT)
C         TOL    - TABLE OF OUTPUT TIMES
C
C     THE FORMAT OF THE  TMLDTB TABLE IS AS FOLLOWS
C         REC NO.  WORD  DESCRIPTION
C         0        1-2   TABLE NAME
C         1        1     TERM NUMBER
C                  2     TLOAD ID
C                  3     TLOAD TYPE(1,2)
C                  4     TAU ( FROM DELAY CARDS--REAL)
C                  5     TID (TABLES FROM TLOAD1 CARD)
C                  5     T1   CONSTANTS FROM TLOAD 2 CARDS
C                  6     T2
C                  7     F
C                  8     P
C                  9     C
C                  10    B
C                  11    QVECT POINTER INTO SECOND RECORD
C
C         WORDS  1 THRU 11 ARE REPEATED FOR EACH UNIQUE TIME FUNCTION
C
C         2        1    I1   QVECT TABLE ID'S
C                  2    I2
C                  3    I3
C                  4    V1   QVECT ORIENTATION VECTORS
C                  5    V2
C                  6    V3
C                  7    V4
C                  8    V5
C                  9    V6
C
C     CORE LAYOUT IS AS FOLLOWS $                                POINT
C     ========================================  ===============  =====
C     TERM DESCRIPTORS (11 WORDS PER TERM)      11*NTERM WORDS   ITERM
C     QVECT STUFF      (9  WORDS PER QVECT)     9*NQVECT WORDS   IQVECT
C     TRL   STUFF      (3 WORDS PER GROUP)      3*NGROUP WORDS+1 TGROUP
C     TABLE LIST       (1 WORD  PER UNIQUETAB)  NTAB WORDS+1     ITAB
C     TABLE DATA        PRETAB STORED           LTAB WORDS       ILTAB
C     TERM VALUES                               NTERM WORDS      IVS
C
C     3    BUFFERS      FCT                                      IBUF1
C                       FCO                                      IBUF2
C                       TOL                                      IBUF3
C
      LOGICAL         DEC
      INTEGER         TMLDTB,TRL,FCT,FCO,TOL,MCB(7),MCB1(7),NAME(2),
     1                SYSBUF,DIT,FILE,ITLIST(13),IZ(1)
      COMMON /BLANK / DUMMY,NCONT
      COMMON /MACHIN/ MACH
      COMMON /ZZZZZZ/ Z(1)
      COMMON /ZBLPKX/ ZA(4),II1
      COMMON /SYSTEM/ SYSBUF
      COMMON /PACKX / IT1,IT2,II,JJ,INCR
      COMMON /CONDAS/ CONSTS(5)
      EQUIVALENCE     (Z(1),IZ(1)),(CONSTS(2),TWOPI),(CONSTS(4),DEGRA)
      DATA    NAME  / 4HTRLG,4HC   /
      DATA    ITLIST/ 4,1105,11,1,1205,12,2,1305,13,3,1405,14,4 /
C
      DEC    = MACH.EQ.5 .OR. MACH.EQ.6 .OR. MACH.EQ.21
      NOLOAD = 0
      MCB(1) = TMLDTB
      CALL RDTRL (MCB)
      IF (MCB(2) .LE. 0) NOLOAD = -1
      MCB(2) = 100
      IGROUP = 1
      IFLAG  =-1
      NZ     = KORSZ(Z)
      IBUF1  = NZ    - SYSBUF
      IBUF2  = IBUF1 - SYSBUF
      IBUF3  = IBUF2 - SYSBUF
      NZ     = IBUF3 - 1
      IF (NZ .LE. 0) CALL MESAGE (-8,0,NAME)
C
C     BRING IN  TIME DATA
C
      IF (NOLOAD .NE. 0) GO TO 30
      ITERM = 1
      LREC  = 11
      FILE  = TMLDTB
      CALL GOPEN (TMLDTB,IZ(IBUF1),0)
      CALL READ (*290,*10,TMLDTB,IZ(ITERM),NZ,0,ILEN)
      CALL MESAGE (-8,0,NAME)
   10 NTERM = ILEN/LREC
      IQVEC = ITERM + ILEN
      NZ    = NZ - ILEN
C
C     BRING IN  QVECT DATA
C
      CALL READ (*290,*20,TMLDTB,IZ(IQVEC),NZ,0,ILEN)
      CALL MESAGE (-8,0,NAME)
   20 NQVECT = ILEN/9
      IGROUP = IQVEC + ILEN
      NZ     = NZ - ILEN
      CALL CLOSE (TMLDTB,1)
C
C     FIND TRL STUFF FOR CORE
C
   30 FILE = TRL
      CALL OPEN (*310,TRL,IZ(IBUF1),0)
      CALL FREAD (TRL,IZ(IGROUP),3,1)
      CALL SKPREC (TRL,IZ(IGROUP+2))
   40 CALL READ (*291,*50,TRL,IZ(IGROUP),NZ,0,ILEN)
      CALL MESAGE (-8,0,NAME)
   50 IF (IZ(IGROUP) .NE. ITRL) GO TO 40
      NGROUP = (ILEN-1)/3
      ITAB   = IGROUP + ILEN
      NZ     = NZ - ILEN
      CALL CLOSE (TRL,1)
      IF (NOLOAD .NE. 0) GO TO 122
C
C     BUILD LIST OF UNIQUE TABLES
C
      NTABL = 1
      K     = ITAB + NTABL
      IZ(K) = 0
      DO 120 I = 1,NTERM
      K = ITERM + LREC*(I-1) + 4
      IF (IZ(K-2) .NE. 3) GO TO 60
      ITID = IZ(K)
      ASSIGN  60  TO IRET
      GO TO 90
   60 K = ITERM + LREC*(I-1) + 10
      IF (IZ(K) .EQ. 0) GO TO 120
C
C     LOOK AT QVECT  TABLE  ID S
C
      IQ   = (IZ(K)-1)*9 + IQVEC
      ITID = IZ(IQ)
      ASSIGN 70  TO IRET
      GO TO 90
   70 ITID = IZ(IQ+1)
      ASSIGN 80 TO IRET
      GO TO 90
   80 ITID = IZ(IQ+2)
      ASSIGN 120 TO IRET
C
C     SEARCH TABLE LIST
C
   90 L = NUMTYP(ITID)
      IF (DEC .AND. ITID.GT.16000 .AND. ITID.LE.99999999) L = 1
      IF (ITID.LE.0 .OR. L.NE.1) GO TO 110
      DO 100 L = 1,NTABL
      K =  ITAB + L
      IF (IZ(K) .EQ. ITID) GO TO 110
  100 CONTINUE
C
C     NEW TABLE
C
      NTABL = NTABL + 1
      K     = ITAB  + NTABL
      IZ(K) = ITID
  110 GO TO IRET, (60,70,80,120)
  120 CONTINUE
      IZ(ITAB) = NTABL
      ILTAB = ITAB + NTABL + 1
      NZ    = NZ - NTABL - 1
C
C     BRING IN TABLE STUFF
C
      LTAB = 0
      IF (NTABL .EQ. 1) GO TO 121
      CALL PRETAB (DIT,IZ(ILTAB),IZ(ILTAB),IZ(IBUF1),NZ,LTAB,IZ(ITAB),
     1             ITLIST)
  121 CONTINUE
      NZ  = NZ - LTAB
      IVS = ILTAB + LTAB
      IF (NZ .LT. NTERM) CALL MESAGE (-8,0,NAME)
C
C     SET UP FOR PACK
C
      IT1 = 1
      IT2 = 1
      II  = 1
      JJ  = NTERM
      INCR= 1
      CALL MAKMCB (MCB, FCT,NTERM,2,IT2)
      CALL MAKMCB (MCB1,FCO,NTERM,2,IT2)
C
C     OPEN OUTPUT FILES
C
      CALL GOPEN (FCT,IZ(IBUF1),1)
  122 CONTINUE
      FILE = TOL
      TO   = 0.0
      IF (NCONT .LE. 2) GO TO 123
C
C     BRING BACK LAST TIME FOR CONTINUE MODE
C
      CALL OPEN  (*310,TOL,IZ(IBUF2),0)
      CALL FREAD (TOL,TO,-NCONT-1,0)
      CALL FREAD (TOL,TO,1,1)
      CALL CLOSE (TOL,1)
  123 CONTINUE
      CALL OPEN  (*310,TOL,IZ(IBUF2),1)
      CALL FNAME (TOL,ZA)
      CALL WRITE (TOL,ZA,2,0)
      IF (NOLOAD .NE. 0) GO TO 150
C
C     DETERMINE IF ALL TIME STEPS OUTPUT
C
      DO 130 I = 1,NGROUP
      K =  IGROUP + (I-1)*3 + 3
      IF (IZ(K) .NE. 1) GO TO 140
  130 CONTINUE
      IFLAG = -1
      GO TO 150
  140 IFLAG = 1
      CALL GOPEN (FCO,IZ(IBUF3),1)
  150 CONTINUE
      T   = TO
      IST = -1
      DO 280 I = 1,NGROUP
C
C     PICK UP  TIME CONSTANTS
C
      K = IGROUP + (I-1)*3 + 1
      NSTEP  = IZ(K)
      IF (I .EQ. NGROUP) NSTEP = NSTEP + 1
      NOUT   = IZ(K+2)
      DELTAT =  Z(K+1)
      IF (I .EQ. 1) NSTEP = NSTEP + 1
      DO 270 J = 1,NSTEP
      IF (NOLOAD .NE. 0) GO TO 231
      DO  230 L = 1,NTERM
      IP = ITERM + (L-1)*LREC
      M  = IZ(IP+2) - 2
      GO TO (160,170), M
C
C     TLOAD1  CARD
C
  160 TT = T - Z(IP+3)
      CALL TAB (IZ(IP+4),TT,FT)
      GO TO 200
C
C     TLOAD2  CARD2
C
  170 TT   = T - Z(IP+3) - Z(IP+4)
      ZRAD = Z(IP+7)*DEGRA
      IF (TT .EQ. 0.0) GO TO 180
      IF (TT.LT. 0.0 .OR. TT.GT.Z(IP+5)-Z(IP+4)) GO TO 190
      FT = TT**Z(IP+9)*EXP(Z(IP+8)*TT)*COS(TWOPI*Z(IP+6)*TT + ZRAD)
      GO TO 200
C
C     TT = 0.0  TRY  LIMITS OF EXPRESSION
C
  180 IF (Z(IP+ 9) .NE. 0.0) GO TO 190
      FT = COS(ZRAD)
      GO TO 200
C
C     FT = 0.0
C
  190 FT = 0.0
C
C     NOW TRY FOR  QVECT  STUFF
C
  200 IF (IZ(IP+10) .EQ. 0) GO TO 220
C
C     EVALUATE  QVECT FUNCTION
C
      IQ = (IZ(IP+10)-1)*9 + IQVEC
      TT = T - Z(IP+3)
C
C     CHECK FOR CONSTANT FLUX VALUE (FLOATING POINT).
C     IF TIME DEPENDENT, CALL TABLE LOOKUP.
C
      IQ1 = IZ(IQ)
      Q1  = Z(IQ)
      LX  = NUMTYP(IQ1)
      IF (DEC .AND. IQ1.GT.16000 .AND. IQ1.LE.99999999) LX = 1
      IF (IQ1.LE.0 .OR. LX.NE.1) GO TO 202
      CALL TAB (IQ1,TT,Q1)
  202 IQ2 = IZ(IQ+1)
      Q2  = Z(IQ+1)
      LX  = NUMTYP(IQ2)
      IF (DEC .AND. IQ2.GT.16000 .AND. IQ2.LE.99999999) LX = 1
      IF (IQ2.LE.0 .OR. LX.NE.1) GO TO 204
      CALL TAB (IQ2,TT,Q2)
  204 IQ3 = IZ(IQ+2)
      Q3  = Z(IQ+2)
      LX  = NUMTYP(IQ3)
      IF (DEC .AND. IQ3.GT.16000 .AND. IQ3.LE.99999999) LX = 1
      IF (IQ3.LE.0 .OR. LX.NE.1) GO TO 206
      CALL TAB (IQ3,TT,Q3)
  206 IF (Z(IQ+6).NE.0.0 .OR. Z(IQ+6).NE.0.0 .OR. Z(IQ+7).NE.0.0 .OR.
     1    Z(IQ+8).NE.0.0) GO TO 210
C
C     V2 = 0
C
      RT = Q1*Z(IQ+3) + Q2*Z(IQ+4) + Q3*Z(IQ+5)
      IF (RT .GT. 0.0) RT = 0.0
      FT = -RT*FT
      GO TO 220
C
C     V2   0
C
  210 FT = SQRT((Q1*Z(IQ+3) + Q2*Z(IQ+4) + Q3*Z(IQ+5))**2 +
     1          (Q1*Z(IQ+6) + Q2*Z(IQ+7) + Q3*Z(IQ+8))**2)*FT
      GO TO 220
C
C     PUT IN FT
C
  220 M    = IVS + L - 1
      Z(M) = FT
  230 CONTINUE
C
C     COLUMN BUILT
C
      CALL PACK (Z(IVS),FCT,MCB)
  231 CONTINUE
      IF (I.EQ.NGROUP .AND. J.EQ.NSTEP-1) GO TO 240
      IF (J.EQ.1 .OR. J.EQ.NSTEP) GO TO 240
      IF (MOD(J+IST,NOUT) .NE. 0) GO TO 260
C
C     OUTPUT TIME
C
  240 CALL WRITE (TOL,T,1,0)
      IF (IFLAG .EQ. -1) GO TO 250
      CALL PACK (Z(IVS),FCO,MCB1)
  250 IF (J .EQ. NSTEP) DELTAT = Z(K+4)
  260 T = T + DELTAT
  270 CONTINUE
      IST = 0
  280 CONTINUE
C
C     ALL OUTPUT
C
      CALL WRITE (TOL,0,0,1)
      CALL CLOSE (TOL,1)
      IF (NOLOAD .NE. 0) GO TO 281
      CALL CLOSE (FCT,1)
      CALL WRTTRL (MCB)
      IF (IFLAG .EQ. -1) GO TO 281
      CALL CLOSE (FCO,1)
      CALL WRTTRL (MCB1)
  281 CONTINUE
      MCB(1) = TOL
      CALL WRTTRL (MCB)
      RETURN
C
C     ERROR MESSAGES
C
  290 IP1 = -2
  300 CALL MESAGE (IP1,FILE,NAME)
      RETURN
  310 IP1 = -1
      GO TO 300
C
C     NO PROPER TSTEP CARD FOUND
C
  291 CALL MESAGE (-31,ITRL,NAME)
      RETURN
      END
