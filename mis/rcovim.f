      SUBROUTINE RCOVIM (HIGHER)
C
C     THIS SUBROUTINE CALCULATES THE ENERGIES ON THE MODAL COORDINATES
C     IN A SUBSTRUCTURE THAT WAS MODAL REDUCED.  IT WILL ALSO
C     CALCULATE THE TOTAL ENERGY FOR EACH COLUMN.
C
      INTEGER         FSS        ,RSS        ,UA         ,RFNO      ,
     1                Z          ,RC         ,SOF1       ,SOF2      ,
     2                SOF3       ,BUF1       ,BUF2       ,BUF3      ,
     3                TFLAG      ,SIGNAB     ,SIGNC      ,SCRM      ,
     4                SCR5       ,SCR6       ,SCR7       ,SCR8      ,
     5                SCR9       ,NAME(2)    ,BUF4       ,FILE      ,
     6                RSP        ,HIGHER(2)  ,UVEC
      REAL            RZ(1)
      COMMON /BLANK / DRY        ,LOOP       ,STEP       ,FSS(2)     ,
     1                RFNO       ,NEIGV      ,LUI        ,UINMS(2,5) ,
     2                NOSORT     ,UTHRES     ,PTHRES     ,QTHRES
      COMMON /RCOVCR/ ICORE      ,LCORE      ,BUF1       ,BUF2       ,
     1                BUF3       ,BUF4       ,SOF1       ,SOF2       ,
     2                SOF3
      COMMON /RCOVCM/ MRECVR     ,UA         ,PA         ,QA         ,
     1                IOPT       ,RSS(2)     ,ENERGY     ,UIMPRO     ,
     2                RANGE(2)   ,IREQ       ,LREQ       ,LBASIC
      COMMON /ZZZZZZ/ Z(1)
      COMMON /MPYADX/ MCBA(7)    ,MCBB(7)    ,MCBC(7)    ,MCBD(7)    ,
     1                MPYZ       ,TFLAG      ,SIGNAB     ,SIGNC      ,
     2                MPREC      ,SCRM
      COMMON /UNPAKX/ ITINU      ,IRU        ,NRU        ,INCRU
      COMMON /PACKX / ITINP      ,ITOUTP     ,IRP        ,NRP        ,
     1                INCRP
      COMMON /NAMES / RD         ,RDREW      ,WRT        ,WRTREW     ,
     1                REW        ,NOREW      ,EOFNRW     ,RSP        ,
     2                RDP        ,CSP        ,CDP        ,SQUARE     ,
     3                RECT       ,DIAG       ,UPPER      ,LOWER      ,
     4                SYM
      EQUIVALENCE     (Z(1),RZ(1))
      DATA    UVEC  , KMTX,MMTX  / 4HUVEC,4HKMTX,4HMMTX /
      DATA    SCR5  , SCR6,SCR7,SCR8,SCR9 / 305,306,307,308,309 /
      DATA    NAME  / 4HRCOV,4HIM         /
C
C     INITIALIZE
C
      LCOREZ = KORSZ(Z)
      MPYZ   = LCOREZ
      TFLAG  = 0
      SIGNAB = 1
      SIGNC  = 1
      MPREC  = 0
C
C     GET THE DISPLACEMENT VECTOR FOR THE HIGHER LEVEL REDUCED
C     SUBSTRUCTURE.
C
      ITEM = UVEC
      CALL MTRXI (SCR5,HIGHER,UVEC,0,RC)
      IF (RC .NE. 1) GO TO 6000
C
C     CALCULATE VELOCITIES IF NOT ALREADY DONE FOR THE OUTPUT PHASE.
C
      INTYP = 1
      IF (RFNO.EQ.3 .OR. RFNO.EQ.8) INTYP = 0
      CALL RCOVVA (SCR5,INTYP,0,SCR8,SCR9,0,HIGHER,Z(1),Z(1),Z(1))
      IF (UA .LE. 0) GO TO 9200
C
C     CALCULATE THE KENETIC ENERTY MULTIPLIER - M * V
C
      ITEM = MMTX
      CALL MTRXI (SCR5,HIGHER,MMTX,0,RC)
      IF (RC .NE. 1) GO TO 6000
      MCBA(1) = SCR5
      CALL RDTRL (MCBA)
      MCBB(1) = SCR9
      CALL RDTRL (MCBB)
      NCOL    = MCBB(2)
      MCBC(1) = 0
      CALL MAKMCB (MCBD,SCR7,MCBB(3),RECT,MCBB(5))
      SCRM = SCR6
      CALL SOFCLS
      CALL MPYAD (Z(1),Z(1),Z(1))
      CALL WRTTRL (MCBD)
C
C     CALCULATE THE KENETIC ENERGIES BY PERFORMING THE SCALAR
C     MULTIPLY IN SINGLE PERCISION.  USE ONLY THE REAL PART IF COMPLEX
C     VECTORS.  APPEND THE TOTAL KINETIC ENERGY TO THE END OF EACH
C     COLUMN.
C
      ITINU = RSP
      IRU   = 1
      NRU   = MCBD(3)
      INCRU = 1
      ITINP = RSP
      ITOUTP= RSP
      IRP   = 1
      NRP   = NRU + 1
      INCRP = 1
      IVEC1 = 1
      IVEC2 = IVEC1 + NRU + 1
      IF (IVEC2+NRU+1 .GT. SOF3) GO TO 9008
C
      FILE = SCR9
      CALL GOPEN (SCR7,Z(SOF1),RDREW)
      CALL GOPEN (SCR9,Z(SOF2),RDREW)
      CALL GOPEN (SCR6,Z(SOF3),WRTREW)
      CALL MAKMCB (MCBA,SCR6,NRP,RECT,RSP)
C
      DO 160 I = 1,NCOL
      ISK = 1
      CALL UNPACK (*130,SCR7,RZ(IVEC1))
      ISK = 0
      CALL UNPACK (*130,SCR9,RZ(IVEC2))
C
      TOTAL = 0.0
      DO 120 J = 1,NRU
      K = J - 1
      RZ(IVEC1+K) = RZ(IVEC1+K)*RZ(IVEC2+K)
      TOTAL = TOTAL + RZ(IVEC1+K)
  120 CONTINUE
      RZ(IVEC1+NRU) = TOTAL
      GO TO 150
C
  130 DO 140 J = 1,NRP
  140 RZ(IVEC1+J-1) = 0.0
      IF (ISK .NE. 0)CALL FWDREC (*9002,SCR9)
C
  150 CALL PACK (RZ(IVEC1),SCR6,MCBA)
C
  160 CONTINUE
C
      CALL CLOSE (SCR7,REW)
      CALL CLOSE (SCR9,REW)
      CALL CLOSE (SCR6,REW)
      CALL WRTTRL (MCBA)
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     CALCULATE THE POTENTIAL ENERTY MULTPLYIER - K*U
C
      ITEM = KMTX
      CALL MTRXI (SCR5,HIGHER,KMTX,0,RC)
      IF (RC .NE. 1) GO TO 6000
      MCBA(1) = SCR5
      CALL RDTRL (MCBA)
      MCBB(1) = SCR8
      CALL RDTRL (MCBB)
      CALL MAKMCB (MCBD,SCR9,MCBB(3),RECT,MCBB(5))
      SCRM = SCR7
      CALL SOFCLS
      CALL MPYAD (Z(1),Z(1),Z(1))
      CALL WRTTRL (MCBD)
C
C     CALCULATE THE POTENTIAL ENERGIES BY PERFORMING THE SCALAR
C     MULTIPLY IN SINGLE PERCISION.  USE ONLY THE REAL PART IF COMPLEX
C     VECTORS.  APPEND THE TOTAL POTENTIAL ENERGY TO THE END OF EACH
C     COLUMN.
C
      ITINU = RSP
      IRU   = 1
      NRU   = MCBD(3)
      INCRU = 1
      ITINP = RSP
      ITOUTP= RSP
      IRP   = 1
      NRP   = NRU + 1
      INCRP = 1
C
      FILE = SCR8
      CALL GOPEN (SCR9,Z(SOF1),RDREW)
      CALL GOPEN (SCR8,Z(SOF2),RDREW)
      CALL GOPEN (SCR7,Z(SOF3),WRTREW)
      CALL MAKMCB (MCBA,SCR7,NRP,RECT,RSP)
C
      DO 260 I = 1,NCOL
      ISK = 1
      CALL UNPACK (*230,SCR9,RZ(IVEC1))
      ISK = 0
      CALL UNPACK (*230,SCR8,RZ(IVEC2))
      TOTAL = 0.0
      DO 220 J = 1,NRU
      K = J - 1
      RZ(IVEC1+K) = RZ(IVEC1+K)*RZ(IVEC2+K)
      TOTAL = TOTAL + RZ(IVEC1+K)
  220 CONTINUE
      RZ(IVEC1+NRU) = TOTAL
      GO TO 250
C
  230 DO 240 J = 1,NRP
  240 RZ(IVEC1+J-1) = 0.0
      IF (ISK .NE. 0)CALL FWDREC (*9002,SCR8)
C
  250 CALL PACK (RZ(IVEC1),SCR7,MCBA)
C
  260 CONTINUE
C
      CALL CLOSE (SCR9,REW)
      CALL CLOSE (SCR8,REW)
      CALL CLOSE (SCR7,REW)
      CALL WRTTRL (MCBA)
C
C     NORMAL RETURN
C
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
      RETURN
C
C     ERRORS
C
 6000 CALL SMSG (RC-2,ITEM,HIGHER)
      GO TO 9200
 9002 N = 2
      GO TO 9100
 9008 N = 8
 9100 CALL MESAGE (N,FILE,NAME)
 9200 IOPT = -1
      RETURN
      END
