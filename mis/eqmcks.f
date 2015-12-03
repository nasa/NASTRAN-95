      SUBROUTINE EQMCKS
C
C     THIS SUBROUTINE CALCULATES AND OUTPUTS OVERALL EQUILIBRIUM FORCES
C
C     THE INPUT FILES ARE
C         KSCC - CASE CONTROL     - NOT PREPOSITIONED.
C         KPGG - LOAD VECTORS     - FILE 110 OR SCRATCH4
C         KQG  - SPC CONSTRAINTS  - FILE 111 OR SCRATCH5
C         QMG  - MPC CONSTRAINTS  - SCRATCH3
C         DT   - RIGID BODY TRANS - SCRATCH2
C
      LOGICAL         LSTEIG
      INTEGER         EJECT    ,NAME(2)  ,PARM     ,
     1                RDNRW    ,RDRW     ,WRTNRW   ,WRTRW
      REAL            HEAD(2,4),COR1(8,1),COR3(8,3)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /NAMES / RDNRW,RDRW,WRTNRW,WRTRW,KRW,KNRW,KNERW
CWKBR 3/94 SPR93007      COMMON /SYSTEM/ ISBZ,NOUT        
      COMMON /SYSTEM/ ISBZ,NOUT,DUM(52),IPREC  
      COMMON /BLANK / IOPT,IGPT,NSKIP,SKPB(15),CORE(8,4)
      COMMON /UNPAKX/ IUNPR,IUNRW,NUNRW,IUNINC
      COMMON /MPYADX/ MA(7),MB(7),MC(7),MD(7),MZ,MT,MSAB,MSC,MPR,MSCR
      COMMON /EQMK1 / KSCC,KEQIN(8),KPGG,KQG,KCSTM,KLAMA,KOQM,KSCR(7)
     1,               KMPC,KLOAD,KSPC,PARM(4)
CZZ   COMMON /ZZEQMS/ ZZ(1)
      COMMON /ZZZZZZ/ ZZ(20000)
      EQUIVALENCE     (MB(6),FREQ), (CORE(1,1),COR1(1,1),COR3(1,1))
      DATA    NAME  / 4HEQMC,4HKS    /
      DATA    HEAD  / 4HAPPL,4HIED , 4HSPCF,4HORCE,  4HMPCF,4HORCE
     1,               4H---T,4HOTAL  /
C
      PARM(3) = NAME(1)
      PARM(4) = NAME(2)
      NZZ   = KORSZ (ZZ)
      NZZ3  = NZZ  - 3*ISBZ + 1
      NZZ2  = NZZ3 + ISBZ
      NZZ1  = NZZ2 + ISBZ
C
      NVEC  = 0
      MA(1) = KSCR(2)
      MC(1) = 0
      CALL RDTRL (MA)
      MZ    = NZZ
      MT    = 0
      MSAB  = 1
      MSC   = 1
CWKBR 11/93 SPR93007      MPR   = 1
      MPR = IPREC
      MSCR  = KSCR(1)
C
C     CALCULATE  DT*PG  ON SCRATCH7
C
      IF (KLOAD .LE. 0) GO TO 40
      MB(1) = KPGG
      MD(1) = KSCR(7)
      CALL RDTRL (MB)
      MD(3) = MA(3)
      MD(4) = MB(4)
CWKBR 11/93 SPR93007      MD(5) = 1
      MD(5) = IPREC
      CALL MPYAD (ZZ,ZZ,ZZ)
      IF (MD(3) .EQ. MD(2)) MD(4) = 1
      CALL WRTTRL (MD)
      NVEC  = MD(2)
C
C     CALCULATE DT*QG  ON SCRATCH6
C
   40 IF (KSPC .LE. 0) GO TO 50
      MB(1) = KQG
      MD(1) = KSCR(6)
      CALL RDTRL (MB)
      MD(3) = MA(3)
      MD(4) = MB(4)
CWKBR 11/93 SPR93007      MD(5) = 1
      MD(5) = IPREC
      CALL MPYAD (ZZ,ZZ,ZZ)
      IF (MD(3) .EQ. MD(2)) MD(4) = 1
      CALL WRTTRL (MD)
      NVEC = MAX0(NVEC,MD(2))
C
C     CALCULATE  DT*MPC  ON SCRATCH5
C
   50 IF (KMPC .LE. 0) GO TO 60
      MD(1) = KSCR(5)
      MB(1) = KSCR(3)
      CALL RDTRL (MB)
      PARM(2) = MB(1)
      IF (MB(1) .LE. 0) GO TO 520
      MD(3) = MA(3)
      MD(4) = MB(4)
CWKBR 11/93 SPR93007      MD(5) = 1
      MD(5) = IPREC
      CALL MPYAD (ZZ,ZZ,ZZ)
      IF (MD(3) .EQ. MD(2)) MD(4) = 1
      CALL WRTTRL (MD)
      NVEC  = MAX0(MD(2),NVEC)
   60 IF (NVEC .LE. 0) GO TO 400
C
C     POSITION CASE CONTROL
C
      CALL GOPEN (KSCC,ZZ(NZZ1),RDRW)
      IF (NSKIP .GT. 0) GO TO 70
C
C     RESERVE THIRD BUFFER FOR LAMA
C
      IBFL = NZZ3
      PARM(2) = KLAMA
      CALL GOPEN (KLAMA,ZZ(NZZ3),RDRW)
      CALL FWDREC (*510,KLAMA)
      GO TO 90
   70 IBFL = NZZ2
      IF (NSKIP .LE. 1) GO TO 90
C
C     ASSUME USER MAY MALADJUST NSKIP
C
      J = NSKIP - 1
      PARM(2) = KSCC
      DO 80 I = 1,J
   80 CALL FWDREC (*510,KSCC)
C
C     READ INTO CORE AS MANY (MAXVEC) VECTORS THAT FIT
C
   90 NENTRY = 0
      IF (KLOAD .GT. 0) NENTRY = 6
      IF (KMPC  .GT. 0) NENTRY = NENTRY + 6
      IF (KSPC  .GT. 0) NENTRY = NENTRY + 6
C
      MAXVEC = (IBFL-1)/NENTRY
      IF (MAXVEC .GE. NVEC) GO TO 110
C
C     INSUFFICIENT CORE TO DO ALL VECTORS
C
      CALL PAGE2 (2)
      WRITE  (NOUT,100) UWM,MAXVEC,NAME
  100 FORMAT (A25,' 2374, INSUFFICIENT CORE TO PROCESS MORE THAN',I7,
     1       ' VECTORS IN ',2A4)
C
      IF (MAXVEC .LE. 0) GO TO 400
C
  110 MAXVEC = MIN0 (NVEC,MAXVEC)
      L = 1
      MA(1) = 0
      IF (KLOAD .LE. 0) GO TO 160
      PARM(2) = KSCR(7)
      MA(1) = 1
      ASSIGN 160 TO IRET
C
C     INTERNAL FUNCTION TO LOAD MAXVEC COLUMNS INTO CORE
C
  120 CONTINUE
      CALL GOPEN (PARM(2),ZZ(NZZ2),RDRW)
      IUNPR  = 1
      IUNINC = 1
      IUNRW  = 1
      NUNRW  = 6
C
      DO 150 MT = 1,MAXVEC
      CALL UNPACK (*130,PARM(2),ZZ(L))
      GO TO 150
  130 MPR = L - 1
      DO 140 I = 1,6
      MPR = MPR + 1
  140 ZZ(MPR) = 0.0
  150 L = L + 6
C
      CALL CLOSE (PARM(2),KRW)
      GO TO IRET, (160,170,180)
C
  160 MA(2) = 0
      IF (KSPC .LE. 0) GO TO 170
      PARM(2) = KSCR(6)
      MA(2) = L
      ASSIGN 170 TO IRET
      GO TO 120
C
  170 MA(3) = 0
      IF (KMPC .LE. 0) GO TO 180
      PARM(2) = KSCR(5)
      MA(3) = L
      ASSIGN 180 TO IRET
      GO TO 120
C
  180 IVEC = 0
      LSTEIG = .FALSE.
      CALL PAGE1
C
C     LOOP ON OUTPUT
C
  200 CONTINUE
      IVEC = IVEC + 1
      IF (LSTEIG) GO TO 260
      PARM(2) = KSCC
      CALL READ (*250,*500,KSCC,MB(1),7,1,I)
      I = MB(1)
      IF (IVEC.EQ.1 .OR. EJECT(11).NE.0) WRITE (NOUT,210) IGPT
  210 FORMAT (1H0,20X,'E Q U I L I B R I U M   C H E C K   L O A D S',
     1        /,1H0,16X,'RESULTANT LOADS AT POINT',I7,
     2        ' IN BASIC COORDINATE SYSTEM')
      IF (NSKIP .LE. 0) GO TO 260
C
C     STATICS SUBCASES
C
      IF (MB(4) .EQ. 0) MB(4) = MB(7)
      IF (MB(4) .EQ. 0) MB(4) = MB(6)
      WRITE  (NOUT,220) MB(1),MB(4)
  220 FORMAT (1H0,24X,7HSUBCASE,I8,8H,   LOAD,I8)
      WRITE  (NOUT,230)
  230 FORMAT (1H0,5X,46H-TYPE-        T1             T2             T3,
     1   13X,32HR1             R2             R3)
C
  240 FORMAT (5X,2A4,1P,6E15.6)
C
      GO TO 300
C
C     EOF FOUND
C
  250 CONTINUE
      IF (IVEC .GT. MAXVEC) GO TO 400
      IF (NSKIP .GT. 0) GO TO 510
      LSTEIG = .TRUE.
C
C     EIGENVALUE PROBLEM
C
  260 PARM(2) = KLAMA
      CALL READ (*510,*500,KLAMA,MB(2),7,0,I)
      WRITE  (NOUT,270) MB(1),MB(2),FREQ
  270 FORMAT (1H0,24X,7HSUBCASE,I8,8H,   MODE,I5,13H,   FREQUENCY,
     1        1P,E15.6)
      WRITE  (NOUT,230)
C
C     LOOP ON OUTPUT CATAGORY
C
  300 K = NENTRY/6 + 1
      IHDCNT = 1
      DO 310 I = 3,8
  310 CORE(I,K) = 0.0E0
C
      DO 330 I = 1,3
      IF (MA(I) .EQ. 0) GO TO 330
      CORE(1,IHDCNT) = HEAD(1,I)
      CORE(2,IHDCNT) = HEAD(2,I)
      J = MA(I) + IVEC*6 - 6
C
      DO 320 L = 3,8
      CORE(L,IHDCNT) = ZZ(J)
      CORE(L,K) = CORE(L,K) + ZZ(J)
      J = J + 1
  320 CONTINUE
      IHDCNT = IHDCNT + 1
  330 CONTINUE
C
      CORE(1,K) = HEAD(1,4)
      CORE(2,K) = HEAD(2,4)
      IF (K .EQ. 2) WRITE (NOUT,240) COR1
      IF (K .EQ. 3) WRITE (NOUT,240) COR3
      IF (K .EQ. 4) WRITE (NOUT,240) CORE
      IF (IVEC .LT. MAXVEC) GO TO 200
  400 CALL CLOSE (KSCC,KRW)
      IF (NSKIP .LE. 0) CALL CLOSE (KLAMA,KRW)
      RETURN
C
C     ERROR MESSAGES
C
C     EOR
C
  500 PARM(1) = 3
      GO TO 600
C
C     EOF
C
  510 PARM(1) = 2
      GO TO 600
C
C     ILLEGAL INPUT
C
  520 PARM(1) = 1
      GO TO 600
C
  600 CALL MESAGE (PARM(1),PARM(2),PARM(3))
      GO TO 400
      END
