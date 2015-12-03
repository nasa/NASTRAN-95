      SUBROUTINE RCOVA
C
C     RCOVA CREATES THE SOLN ITEM FOR A FINAL SOLUTION STRUCTURE (FSS)
C     IN PHASE 2 OF SUBSTRUCTURING
C
      LOGICAL         MRECVR
      INTEGER         IZ(1)      ,NAME(2)    ,SOLN       ,DRY        ,
     1                STEP       ,FSS        ,RFNO       ,BUF1       ,
     2                BUF2       ,BUF3       ,SYSBUF     ,RC         ,
     3                SOF1       ,SOF2       ,SOF3       ,KM(5)      ,
     4                KMU(5)     ,SCHK       ,UVEC       ,PHIS       ,
     5                SCR1
      COMMON /BLANK / DRY        ,LOOP       ,STEP       ,FSS(2)     ,
     1                RFNO       ,NEIGV      ,LUI        ,UINMS(2,5) ,
     2                NOSORT     ,UTHRES     ,PTHRES     ,QTHRES
      COMMON /RCOVCR/ ICORE      ,LCORE      ,BUF1       ,BUF2       ,
     1                BUF3       ,BUF4       ,SOF1       ,SOF2       ,
     2                SOF3
      COMMON /RCOVCM/ MRECVR     ,UA         ,PA         ,QA         ,
     1                IOPT       ,RSS(2)     ,ENERGY     ,UIMPRO     ,
     2                RANGE(2)   ,IREQ       ,LREQ       ,LBASIC
      COMMON /SYSTEM/ SYSBUF     ,NOUT
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (Z(1),IZ(1))
      DATA    SOLN  , UVEC,  PHIS / 4HSOLN,4HUVEC,4HPHIS /
      DATA    KM    / 4HKMTX,4HMMTX,4HUVEC,4HBMTX,4HK4MX /
      DATA    KMU   / 103,104,106,109,110 /
      DATA    SCHK  / 3   /
      DATA    SCR1  / 301 /
      DATA    NAME  / 4HRCOV,4HA    /
C
C     INITIALIZE
C
      SOF1 = KORSZ(Z) - LREQ - SYSBUF + 1
      SOF2 = SOF1 - SYSBUF - 1
      SOF3 = SOF2 - SYSBUF
      BUF1 = SOF3 - SYSBUF
      BUF2 = BUF1 - SYSBUF
      BUF3 = BUF2 - SYSBUF
      ICORE= 1
      LCORE= BUF3 - 1
      IF (LCORE .LE. 0) CALL MESAGE (-8,0,NAME)
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     COPY KGG, MGG, UVEC, BGG AND K4GG TO THE SOF IF THEY ARNT THERE
C
      DO 20 I = 1,5
      IF (KM(I).EQ.UVEC .AND. MRECVR) GO TO 20
      IF (DRY .LT. 0) GO TO 5
    3 CALL MTRXO (KMU(I),FSS,KM(I),Z(BUF1),RC)
      GO TO (20,15,20,10,10,20), RC
    5 RC = 2
      CALL MTRXO (-1,FSS,KM(I),0,RC)
      GO TO 20
   10 CALL SMSG (RC-2,KM(I),FSS)
      GO TO 20
   15 CALL DELETE (FSS,KM(I),RC)
      GO TO 3
   20 CONTINUE
      IF (DRY) 440,70,70
C
C     IF MODAL RECOVER, COPY PHIS ITEM TO UVEC
C
   70 IF (.NOT.MRECVR) GO TO 90
      RFNO = 3
      CALL MTRXI (SCR1,FSS,PHIS,0,RC)
      IF (RC .EQ. 1) GO TO 80
      CALL SMSG (RC-2,PHIS,FSS)
      GO TO 9100
   80 CALL MTRXO (SCR1,FSS,UVEC,0,RC)
C
C     ATTEMPT TO FETCH SOLN ITEM FOR FSS.  IF IT ALREADY EXISTS, RETURN
C
   90 CALL SFETCH (FSS,SOLN,SCHK,RC)
      IF (RC .EQ. 1) GO TO 440
      IF (RC .EQ. 3) GO TO 100
      CALL SMSG (RC-2,SOLN,FSS)
      GO TO 440
C
C     CREATE SOLN ITEM FOR PROPER RIGID FORMAT
C
  100 IF (RFNO.LT.0 .OR. RFNO.GT.9) GO TO 9007
      GO TO (110,110,130,9007,9007,9007,9007,180,180) , RFNO
C
C     STATIC SOLUTION - R.F. 1 AND 2
C
  110 CALL RCOVSS
      GO TO 440
C
C     MODAL SOLUTION - R.F. 3
C
  130 CALL RCOVMS
      GO TO 440
C
C     DYNAMIC SOLUTION - R.F. 8 AND 9
C
  180 CALL RCOVDS
      GO TO 440
C
C     FINISHED
C
  440 CALL SOFCLS
      RETURN
C
C     DIAGNOSTICS
C
 9007 CALL MESAGE (7,0,NAME)
 9100 IOPT = -1
      CALL SOFCLS
      RETURN
      END
