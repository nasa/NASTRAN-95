      SUBROUTINE DPD
C
C     DPD IS MAIN CONTROL PROGRAM FOR THE DYNAMICS POOL DISTRIBUTOR.
C
      INTEGER         GPL   ,SIL   ,USET  ,USETD ,GPLD  ,SILD  ,DPOOL ,
     1                DLT   ,FRL   ,TFL   ,TRL   ,PSDL  ,EED   ,SCR1  ,
     2                SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2  ,BUF3  ,
     3                BUF4  ,SYSBUF,NGRID ,EPOINT,SEQEP ,Z     ,LOADS ,
     5                EQDYN ,DLOAD ,FREQ1 ,FREQ  ,TIC   ,TSTEP ,TF    ,
     6                PSD   ,EIGR  ,EIGB  ,EIGC  ,SDT
      DIMENSION       BUF(24)   ,EPOINT(2)    ,SEQEP(2)     ,MCB(7)   ,
     1                NAM(2)    ,LOADS(32)    ,DLOAD(2)     ,FREQ1(2) ,
     2                FREQ(2)   ,ZZ(1)        ,BUFR(20)     ,NOLIN(21),
     3                TIC(2)    ,TSTEP(2)     ,TF(2)        ,PSD(2)   ,
     4                MSG(3)    ,EIGR(2)      ,EIGB(2)      ,EIGC(2)
      COMMON /BLANK / LUSET ,LUSETD,NOTFL ,NODLT ,NOPSDL,NOFRL ,NONLFT,
     1                NOTRL ,NOEED ,NOSDT ,NOUE
      COMMON /DPDCOM/ DPOOL ,GPL   ,SIL   ,USET  ,GPLD  ,SILD  ,USETD ,
     1                DLT   ,FRL   ,NLFT  ,TFL   ,TRL   ,PSDL  ,EED   ,
     2                SCR1  ,SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2  ,
     3                BUF3  ,BUF4  ,EPOINT,SEQEP ,L     ,KN    ,NEQDYN,
     4                LOADS ,DLOAD ,FREQ1 ,FREQ  ,NOLIN ,NOGO  ,
     5                MSG   ,TIC   ,TSTEP ,TF    ,PSD   ,EIGR  ,EIGB  ,
     6                EIGC  ,MCB   ,NAM   ,EQDYN ,SDT   ,INEQC
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SYSTEM/ SYSBUF
      EQUIVALENCE     (Z(1),ZZ(1)),(BUF(1),BUFR(1)),(MSG(2),NGRID)
C
C     INITIALIZE CONTROL PARAMETERS.
C
      NOTFL  = -1
      NODLT  = -1
      NOPSDL = -1
      NOFRL  = -1
      NONLFT = -1
      NOTRL  = -1
      NOEED  = -1
      NOSDT  = -1
      NOUE   = -1
      NOGO   =  0
      INEQ   =  0
      DO 10 I = 1,7
   10 MCB(I) = 0
C
C     PERFORM BUFFER ALLOCATION
C
      BUF1 = KORSZ(Z) - SYSBUF - 2
      BUF2 = BUF1 - SYSBUF
      BUF3 = BUF2 - SYSBUF
      BUF4 = BUF3 - SYSBUF
C
C     IF DYNAMICS POOL IS PURGED, EXIT. OTHERWISE, EXECUTE THE PHASES
C     OF DPD
C
      BUF(1) = DPOOL
      CALL RDTRL (BUF)
      IF (BUF(1) .NE. DPOOL) RETURN
      CALL DPD1
      CALL DPD2
      CALL DPD3
      CALL DPD4
      CALL DPD5
      IF (NOGO .NE. 0) CALL MESAGE (-61,0,0)
      RETURN
      END
