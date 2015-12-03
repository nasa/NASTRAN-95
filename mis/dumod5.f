      SUBROUTINE DUMOD5
C
C     MSFC ROUTINE, TO CONVERT NASTRAN TABULAR DATA BLOCKS INTO 2-
C     DIMENSIONAL DATA BLOCKS (S.P. REAL ONLY) FOR CONVENIENCE IN
C     MANIPULATION AND OUTPUT, SPECIALLY TO BE USED WITH OUTPUT5 AND
C     INPUT5.
C
C     THIS VERSION WAS MODIFIED BY R. MOORE/MSFC IN JAN. 1989
C     TO ALLOW SELECTION OF EITHER 8 OR 16 VALUES PER ELEMENT BY
C     USING A 7TH PARAMETER ON DMAP
C
C     DUMMOD5  T1,T2,T3,T4,T5/O1,O2,O3,O4,O5/C,N,P1/C,N,P2/C,N,P3
C              C,N,P4/C,N,P5/C,N,Q/C,N,R $
C
C     TI  = INPUT GINO FILE, OEF1, OQG1 OR SIMILAR TYPE OF TABULAR
C           DATA BLOCKS
C     OI  = OUTPUT GINO DATA BLOCK, PACKED, BUT NOT QUITE A REGULAR
C           NASTRAN MATRIX BLOCK, SEE PICTURE BELOW
C           IF OI IS PURGED (NOT PRESENT), MATRIX BLOCK IS WRITTEN OUT
C           TO FORTRAN UNIT 15 (INP1) DIRECTLY, IN BINARY RECORDS,
C           BANDED MATRIX FORM (FROM FIRST NON-ZERO TO LAST NON-ZERO
C           ELEMENTS), D.R. OR S.P.
C     PI  = TI TABLE IS MAPPED INTO A PI X 8 2-DIMENSIONAL BLOCKS.
C           EACH BLOCK IS PACKED AS A COLUMN OF A MATRIX
C     Q   = ELEMENT/GRID POINT ID PRINT-PUNCH CONTROL
C         = -1, NO PRINT AND NO PUNCH
C         =  0, PRINT ONLY, NO PUNCH
C         = +1, BOTH PRINT AND PUNCH
C         = /2/ CONTENTS OF OUTPUT TAPE, INP1, WILL BE PRINTED OUT
C     R   = SWITCH TO CHANGE FROM 8 TO 16 VALUES IN TABLE MAPPING
C           DEFAULT = 0 WHICH SETS TO 8.    R = 1 SETS IT TO 16
C
C     CDC USER ONLY - FORTRAN UNIT 11 (UT1) IS USED INSTEAD OF 15 (INP1)
C
C
C           |<------ 8 OR 16 ------->|
C           ==========================
C         / I                        I \
C        /  I------- TABULAR --------I  \
C       P1  I         DATA           I  BLOCK 1 (MATRIX COLUMN 1)
C        \  I-------- BLOCKS --------I  /
C         \ I                        I /
C           ==========================
C         / I                        I \
C        /  I------------------------I  \
C       P1  I                        I  BLOCK 2 (MATRIX COLUMN 2)
C
C     WRITTEN BY SOMEBODY FOR MARSHALL SPACE FLIGHT CENTER (MSFC).
C     MODIFIED BY G.CHAN/UNISYS TO EMPLOY OPEN-CORE SPACE INSTEAD OF
C     THE FIXED DIMENSION ARRAYS, AND TO EXPAND FROM ONE INPUT DATA
C     BLOCK TO FIVE. IF A CORRESPONDING OUTPUT FILE IS MISSING OR
C     PURGED, THE DATA BLOCKS ARE WRITTEN DIRECTLY TO FORTRAN TAPE
C     (UNIT 15, INP1) USING OUTPUT5 BINARY FORMAT.
C
C     CONTENTS OF INP1 TAPE IF IT IS WRITTEN -
C
C         RECORD   WORD     CONTENT                           TYPE
C         ------  ------   ----------------------------------------
C            0              TAPE HEADER RECORD
C                   1-2     'XXXXXXXX', TAPE ID              2*BCD
C                   3-4     MACHINE TYPE                     2*BCD
C                   5-7     DATE                             3*INT
C                    8      SYSTEM BUFFSIZE                    INT
C                    9      0 (BINARY TAPE)                    INT
C            1              FIRST MATRIX HEADER
C                    1      0                                  INT
C                   2,3     1,1                              2*INT
C                    4      A DOUBLE PRECISION ZERO           D.P.
C                   5-10    6 WORDS FROM MATRIX TRAILER      6*INT
C                           (COL,ROW,FORM,TYPE,MAX,DENSITY-
C                            TYPE=1 OR 3, DENSITY=1)
C                  11-12    MATRIX DMAP NAME                 2*BCD
C            2       1      1 (FIRST COLUMN ID)                INT
C                    2      LOCATION OF FIST NON-ZERO ELEMENT  INT
C                    3      LOCATION OF LAST NON-ZERO ELEMENT  INT
C                   4-N     S.P. DATA                         REAL
C            3       1      2 (SECOND COLUMN ID)               INT
C                   2-N     SAME AS RECORD 1
C            :      1-N     REPEAT FOR MORE COLUMNS
C
C            X       1      X (X-TH COLUMN ID, A NUL COLUMN)   INT
C                   2-3     1,1                                INT
C                   4-5     0.0,0.0                           REAL)
C
C            M      1-N     LAST COLUMN, SAME AS RECORD 1
C           M+1      1      -1 (ELEM) OR -2 (GRID)             INT
C                    2      1                                  INT
C                    3      LENGTH OF ELEM./GRID ID LIST, L    INT
C                  4-L+4    LIST OF ELEMENT OR GRID IDS        INT
C
C           M+2             SECOND MATRIX HEADER
C            :       :      REPEAT 1 THRU (M+1) FOR THE SECOND MATRIX
C
C            :       :      REPEAT, UP TO 5 OUTPUT DATA BLOCKS PER TAPE
C
C     COMMENTS FROM G.C. -
C     (1) THIS MODULE IS VERY LIMITED IN SCOPE. IT HANDLES ONLY SOME
C         SPECIAL TYPES OF TABULAR INPUT DATA BLOCKS. THE (PI X 8) MATRI
C         SPACE IS FOR PRINT/PUNCH PURPOSE. THE ORIGINAL PROGRAM SEEMS
C         TO BE WRITTEN TO MEET A PARTICULAR JOB REQUIREMENT.
C
C     (2) CURRENT MODULE HANDLES ONLY SINGLE PRECISION DATA
C
C     (3) THE PROCEDURE TO READ AND/OR WRITE THE TAPE IS COMMONLY USED
C         AMONG INPUTT5, OUTPUT5, AND DUMMOD5. ANY PROCEDURE CHANGE
C         SHOULD BE MADE TO ALL THREE MODULES.
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL         NONE,     DEBUG
      INTEGER         NAME(2),  MCB(7),   TRL(7),  IZ(8),  TEMP(10),
     1                EG(2),    IR(5001), ID(5001),UNVC(2),MT(2),
     2                INFILE(2),OUTFIL(2),DATE(3), SAVE(2,5)
      REAL            Z,        EPSI
      DOUBLE PRECISION          DZERO,    DTEMP
      CHARACTER       UFM*23,   UWM*25,   UIM*29
CWKBNB
      CHARACTER*80    DSNAMES
      COMMON /DSNAME/ DSNAMES(80)
CWKBNE
      COMMON /XMSSG / UFM,      UWM,      UIM
      COMMON /ZZZZZZ/ Z(1)
      COMMON /MACHIN/ MACH,     IJHALF(3),MCHNAM
      COMMON /SYSTEM/ IBUF,     NOUT,     DUMM(88),LPCH
      COMMON /PACKX / TYPIN,    TYPOUT,   II,      JJ,     INCR
      COMMON /BLANK / P(5),     Q,        R
      EQUIVALENCE     (Z(1),IZ(1)),       (DATE(1),DUMM(13))
CWKBI
      DATA    IFIRST/0/
      DATA    TAPE,   IRDLMT,   ID,       IM, IE,  XX,     EPSI    /
     1        15,     5000,     5001*0,   1H-,1H=, 4HXXXX, 1.0E-30 /
      DATA    ZERO,   ONE,      EG,                NAME            /
     1        0,      1,        4HELEM,   4HGRID,  4HDUMO, 4HD5    /
      DATA    UNVC,   MT  /     4HUNIV,   4HAC  ,  2*4H            /
      DATA    DEBUG,  DZERO,    SAVE  /   .FALSE., 0.D0,   10*1H   /
C
      IF (MACH .EQ. 12) TAPE = 11
      CALL PAGE
      WRITE  (NOUT,5) P,Q,R
    5 FORMAT ('0*** MODULE DUMMOD5 CALLED BY USER DMAP ALTER.', /5X,
     1        'PARAMETERS ARE    P=',5(I5,1H,),5X,'Q=',I5,5X,'R=',I4,/)
      I6 OR 8 = 8
      IF (R .EQ. 1) I6 OR 8 = 16
      INCR  = 1
      TYPIN = 1
      TYPOUT= 1
      II    = 1
      TAPX  =-1
      TAPP  =-1
      CORE  = KORSZ(Z)
      BUF1  = CORE - IBUF + 1
      BUF2  = BUF1 - IBUF
      CORE  = BUF2 - 1
      HALF  = CORE/2
      HALF1 = HALF + 1
CWKBNB
      IF ( IFIRST .NE. 0 ) GO TO 1
      CLOSE ( UNIT=TAPE)
      OPEN ( UNIT=TAPE, FILE=DSNAMES(TAPE), FORM='UNFORMATTED',
     1       STATUS='UNKNOWN' )
      IFIRST = 1
1     CONTINUE
CWKBNE
C
      DO 450 LOOP = 1,5
      INPUT = 100 + LOOP
      OUTPT = 200 + LOOP
      TRL(1)= INPUT
      CALL RDTRL (TRL(1))
      IF (TRL(1) .LE. 0) GO TO 450
      CALL FNAME (INPUT,INFILE)
C
C     INPUT DATA PRECISION TYPE IS S.P. ONLY
C
      TYPE = 1
C
      IF (P(LOOP) .LE. 0) P(LOOP) = PV
      PV = P(LOOP)
      JJ = P(LOOP)*I6 OR 8
      DO 10 J = 1,JJ
   10 Z(J+HALF) = 0.0
      CALL GOPEN (INPUT,Z(BUF1),0)
      MCB(1) = OUTPT
      CALL RDTRL (MCB)
      NONE = .FALSE.
      IF (MCB(1) .LE. 0) NONE = .TRUE.
      IF (NONE) GO TO 15
      CALL GOPEN  (OUTPT,Z(BUF2),1)
      CALL FNAME  (OUTPT,OUTFIL)
      CALL MAKMCB (MCB,OUTPT,0,2,1)
      GO TO 20
   15 TAPX = TAPX + 1
      IF (TAPX .LE. 0) GO TO 20
      SAVE(1,TAPX) = INFILE(1)
      SAVE(2,TAPX) = INFILE(2)
   20 I    = 1
      NXZH = 0
      NXIR = 0
      CALL READ (*290,*30,INPUT,TEMP,10,1,M)
      NWDS = TEMP(10)
      NELTP= TEMP( 3)
C     IF (NELTP.GE.11 .AND. NELTP.LE.14) GO TO 320
C               CELAS1            CELAS4
      GO TO 60
   30 CALL MESAGE (-37,0,NAME)
   40 CALL READ (*290,*50,INPUT,TEMP,10,1,M)
      NWDS = TEMP(10)
      IF (TEMP(3) .NE. NELTP) GO TO 60
      GO TO 130
   50 CALL MESAGE (-61,INPUT,NAME)
C  60 IF (TEMP(3).GE.11 .AND. TEMP(3).LE.14) GO TO 320
C                 CELAS1              CELAS4
   60 CONTINUE
      NEWLT = TEMP(3)
      NWDS1 = NWDS - 1
      NWDS2 = NWDS - 2
      DO 70 L = 1,JJ
   70 Z(L) = 0.0
      DO 80 L = 1,IRDLMT
   80 IR(L) = 0
      CALL READ (*330,*350,INPUT,IR(1),1,0,M)
      KOUNT = 0
      DO 90 JSQ = 1,IRDLMT
      KOUNT = KOUNT + 1
      LOC = NWDS1*JSQ - NWDS2
      CALL READ (*330,*350,INPUT,Z(LOC),NWDS1,0,M)
C     LAST = LOC + NWDS1 - 1
      LAST = KOUNT*I6 OR 8
      CALL READ (*330,*100,INPUT,IR(JSQ+1),1,0,M)
   90 CONTINUE
  100 M   = NWDS*KOUNT
      IJK = 0
      DO 120 J = 1,M,NWDS
      IJK = IJK + 1
      NROP  = (IR(IJK)-1)/10
      LOCID = NXIR + IJK
      ID(LOCID) = NROP*100 + NEWLT
      LOCA = (IJK*I6 OR 8) - (I6 OR 8 -1) + NXZH
      LJ  = NWDS1*IJK - NWDS1
      KK  = LOCA + NWDS + HALF
      IF (KK .GT. CORE) CALL MESAGE (-8,0,NAME)
      DO 110 JM = 1,NWDS1
  110 Z(LOCA+JM-1+HALF) = Z(LJ+JM)
  120 CONTINUE
      NXIR = NXIR + JSQ
      NXZH = NXZH + LAST
      GO TO 40
  130 IF (Q .LT. 1) GO TO 150
      IS = IM
      KK = HALF + NXZH
      WRITE  (NOUT,140) IS,I,(Z(J),J=HALF1,KK)
  140 FORMAT ('  COLUMN',A1,I5, /,(2X,8E16.6))
  150 I = I + 1
      IF (NONE) GO TO 180
      CALL PACK (Z(HALF1),OUTPT,MCB)
      GO TO 270
  160 IF (TAPX .GT. 0) GO TO 170
C
C     WRITE TAPE HEADER AND MATRIX HEADER
C     (CURRENTLY, OUTPUT TAPE IS WRITTEN OUT IN SINGLE PRECISION ONLY)
C     CHANGE IN 89 VERSION -
C     MUST SET MATRIX DENSITY IN MATRIX TRAILER TO NON-ZERO IF INPUT5
C     IS TO BE USED
C
      TAPX = 1
      SAVE(1,TAPX) = INFILE(1)
      SAVE(2,TAPX) = INFILE(2)
      MT(1) = MCHNAM
      IF (MACH .NE. 3) GO TO 162
      MT(1) = UNVC(1)
      MT(2) = UNVC(2)
  162 WRITE (TAPE) XX,XX,MT,DATE,IBUF,ZERO
      IF (DEBUG) WRITE (NOUT,165) XX,XX,MT,DATE,IBUF,ZERO
  165 FORMAT ('0+++TAPE HEADER/DUMMOD5-',/3X,2A4,1X,2A4,3I4,2I6)
  170 IF (TAPX .EQ. TAPP) GO TO 190
      TAPP   = TAPX
      TRL(5) = TYPOUT
      TRL(7) = 1
      WRITE (TAPE) ZERO,ONE,ONE,DZERO,(TRL(K),K=2,7),INFILE
      IF (DEBUG) WRITE (NOUT,175) ZERO,ONE,ONE,DZERO,(TRL(K),K=2,7),
     1           INFILE
  175 FORMAT (' +++MATRIX HEADER/DUMMOD5- ',3I5,D8.0,6I5,1X,2A4)
      GO TO 190
C
  180 ASSIGN 270 TO RETN
  190 DO 200 JB = II,JJ
CWKBNB 8/94 ALPHA-VMS
      ITYPE = NUMTYP( Z(JB+HALF) )
      IF ( ITYPE .LE. 1 ) GO TO 200
CWKBNE 8/94 ALPHA-VMS
      IF (ABS(Z(JB+HALF)) .GT. EPSI) GO TO 210
  200 CONTINUE
      WRITE (TAPE) I,ONE,ONE,(ZERO,J=1,TYPE)
      IF (DEBUG) WRITE (NOUT,205) I,ONE,ONE,(ZERO,J=1,TYPE)
  205 FORMAT (' +++ZEROS/DUMMOD5- ',7I5)
      GO TO 265
  210 JE = JJ
      DO 220 J = II,JJ
CWKBNB 8/94 ALPHA-VMS
      ITYPE = NUMTYP( Z(JE+HALF) )
      IF ( ITYPE .LE. 1 ) GO TO 220
CWKBNE 8/94 ALPHA-VMS
      IF (ABS(Z(JE+HALF)) .GT. EPSI) GO TO 230
  220 JE = JE - 1
  230 GO TO (260,240,240,250), TYPE
  240 IF (MOD(JB,2) .EQ. 0) JB = JB - 1
      IF (MOD(JE,2) .EQ. 1) JE = JE + 1
      GO TO 260
  250 J = MOD(JB,4)
      IF (J .EQ. 0) J = 4
      JB = JB - J + 1
      J  = MOD(JE,4)
      IF (J .EQ. 0) J = 4
      JE = JE - J + 4
  260 WRITE (TAPE) I,JB,JE,(Z(J+HALF),J=JB,JE)
      IF (DEBUG) WRITE (NOUT,262) I,JB,JE
  262 FORMAT (' +++DATA RECORD/DUMMOD5- ',3I5)
  265 GO TO RETN, (270,370)
C
  270 DO 280 L = 1,JJ
  280 Z(L+HALF) = 0.0
      NXZH = 0
      NXIR = 0
      GO TO 60
  290 IF (Q .LT. 0) GO TO 300
      IS = IE
      KK = HALF + NXZH
      WRITE (NOUT,140) IS,I,(Z(J),J=HALF1,KK)
  300 ASSIGN 370 TO RETN
      IF (NONE) GO TO 160
      CALL PACK (Z(HALF1),OUTPT,MCB)
      MCB(3) = JJ
      CALL WRTTRL (MCB)
      IF (Q .EQ. 2) WRITE (NOUT,310) (MCB(J),J=1,5)
  310 FORMAT (/2X,'MCB=',6I8)
      GO TO 370
C 320 CALL READ (*330,*40 ,INPUT,IR(1),1,0,M)
C     CALL READ (*330,*350,INPUT, Z(1),1,0,M)
C     Z(1) = 0.0
C     GO TO 320
  330 WRITE  (NOUT,340) INFILE
  340 FORMAT (/5X,'*** EOF ENCOUNTERED ON INPUT ',2A4,' DATA BLOCK')
      GO TO 440
  350 WRITE  (NOUT,360) INFILE
  360 FORMAT (/5X,'*** INPUT ',2A4,'DATA BLOCK IS EMPTY')
      GO TO 440
  370 IF (.NOT.NONE) WRITE (NOUT,380) UIM,INFILE,OUTFIL
  380 FORMAT (A29,', MODULE DUMMOD5 SUCCESSFULLY PROCESSED TABULAR ',
     1        'DATA FROM ',2A4,' TO DATA BLOCK ',2A4, /5X,
     2        'IN GINO PACKED FORM')
      IF (NONE) WRITE (NOUT,390) UIM,INFILE,TAPE
  390 FORMAT (A29,', MODULE DUMMOD5 SUCCESSFULLY COPIED TABULAR DATA ',
     1        'FROM ',2A4,' TO OUTPUT TAPE', /5X,
     3        '(FORTRAN UNIT',I4,') IN BANDED MATRIX FORM')
      IF (Q .GT. 0) WRITE (LPCH,400) (ID(J),J=1,NXIR)
  400 FORMAT (8I10)
      L = EG(1)
      IF (NEWLT .GT. 0) GO TO 420
      L = EG(2)
      DO 410 J = 1,NXIR
  410 ID(J) = ID(J)/100
  420 WRITE  (NOUT,430) L,INFILE,(ID(J),J=1,NXIR)
  430 FORMAT (//5X,A4,'-ID ARRAY FOLLOWS/FROM ',2A4, (/5X,15I8))
      IF (.NOT.NONE) GO TO 440
      I = -1
      IF (NEWLT .EQ. 0) I = -2
      WRITE (TAPE) I,ONE,NXIR,(ID(J),J=1,NXIR)
      IF (DEBUG) WRITE (NOUT,435) I,ONE,NXIR
  435 FORMAT (' +++ELEM/GRID ID RECORD/DUMMOD5- ',3I5)
  440 CONTINUE
      CALL CLOSE (INPUT,1)
      IF (.NOT.NONE) CALL CLOSE (OUTPT,1)
  450 CONTINUE
C
      IF (TAPX .LE. 0) GO TO 590
      WRITE  (NOUT,455) UIM,TAPE,(SAVE(1,J),SAVE(2,J),J=1,TAPX)
  455 FORMAT (A29,', FOLLOWING DATA BLOCKS WERE COPIED TO FORTRAN UNIT',
     1        I3,' BY MODULE DUMMOD5', /5X,
     2        'USING UNFORMATTED (BINARY) WRITE', /6X,5(2A4,3X))
      ENDFILE TAPE
      REWIND TAPE
C
C     TO READ THE OUTPUT TAPE, Q=/2/
C
      IF (IABS(Q) .LT. 2) GO TO 590
      CALL PAGE1
      K = 1
      READ (TAPE,END=575) MCB,J,I
      WRITE  (NOUT,460) MCB,J
  460 FORMAT (//,'  TAPEID=',2A4,'   FROM ',A4,A2,' MACHINE,  DATE',I5,
     1        1H/,I2,1H/,I2,'  BINARY TAPE.   BUFFSIZE=',I7//)
  470 READ (TAPE,END=580) I,JB,JE,(Z(J),J=JB,JE)
      IF (I) 560,480,500
  480 BACKSPACE TAPE
      READ (TAPE,END=580) I,JB,JE,DTEMP,(IZ(J),J=1,8)
      WRITE  (NOUT,490) K,IZ(7),IZ(8),(IZ(J),J=1,6)
  490 FORMAT (//,'  DATA BLOCK',I3,3X,2A4,'  TRAILER=',6I5)
      K = K + 1
      GO TO 470
  500 WRITE  (NOUT,510) I,JB,JE,(Z(J),J=JB,JE)
  510 FORMAT (//,'  COLUMN RECORD =',I3,'   JB,JE =',2I5,/,(1X,10E13.6))
      GO TO 470
C
  560 L = EG(-I)
      WRITE  (NOUT,570) L,(IZ(J),J=JB,JE)
  570 FORMAT (//2X,A4,'-ID LIST -',/,(1X,10I10))
      GO TO 470
  575 WRITE  (NOUT,577)
  577 FORMAT (//,'  EMPTY TAPE')
  580 REWIND TAPE
  590 CONTINUE
      RETURN
      END
