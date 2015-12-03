      SUBROUTINE FNDPNT (IARY,ID)
C
      INTEGER         NAME(2),OLD,BGPDT,SIL,EDT
      DIMENSION       IARY(4),ISAVE(4),ARRY(3),IRY(3),IEDT(2),ICORE(1),
     1                IFED(2)
      COMMON /SYSTEM/ IBUF,NOUT
      COMMON /FPT   / DUM(3),NROW1,LCORE
      COMMON /ZZZZZZ/ CORE(1)
      COMMON /LOADX / I1(2),BGPDT,OLD,CSTM,SIL,ISIL,I2,MPT,GPTT,EDT,
     1                IMPT,IGPTT,IED
      EQUIVALENCE     (IRY(1),ARRY(1)), (CORE(1),ICORE(1))
      DATA    NAME  / 4HFNDP,4HNT  /
      DATA    IEDT  / 4HEDT ,4HFEDT/, IFED/4HFEDT,4HST  /
C
C     FIND POINT ON BGPDT
C
      IF (ID .LT. 0) GO TO 90
      IF (ID.LT.268435455 .AND. OLD.GE.0) GO TO 10
C               268435455 = 2**28 - 1
      WRITE  (NOUT,5) ID,OLD
    5 FORMAT (//,' BAD DATA PASSED TO FNDPNT, ID,OLD =',2I14)
      CALL MESAGE (-37,0,NAME)
   10 NS = 4*(ID-OLD)
      IF (NS-4) 70,30,20
   20 CALL READ (*90,*90,BGPDT,ISAVE(1),-NS+4,0,FLAG)
   30 CALL READ (*90,*90,BGPDT,ISAVE(1),    4,0,FLAG)
      OLD = ID
   40 DO 50 I = 1,4
   50 IARY(I) = ISAVE(I)
   60 RETURN
C
   70 IF (NS) 80,40,80
   80 CALL BCKREC (BGPDT)
      OLD = 0
      GO TO 10
C
   90 IPM = BGPDT
  100 CALL MESAGE (-2,IPM,NAME)
  110 IPM = SIL
      GO TO 100
  120 IPM = EDT
      GO TO 100
C
C
      ENTRY FNDSIL (IP)
C     =================
C
C     FIND SIL VALUE
C
  130 NS = IP - ISIL
      IF (NS-1) 140,170,160
  140 IF (NS) 150,180,150
  150 CALL BCKREC (SIL)
      ISIL = 0
      GO TO 130
  160 CALL READ (*110,*110,SIL,I,-NS+1,0,FLAG)
  170 CALL READ (*110,*110,SIL,IF,   1,0,FLAG)
      ISIL = IP
  180 IP   = IF
      GO TO 60
C
C
      ENTRY FEDTST (IDEF)
C     ===================
C
C     FIND ENFORCED DISPLACEMENT
C
C     PUT DEFORM EID S AND VALUES INTO CORE FOR THIS SET
C
      ICP = NROW1 + 1
      K   = 0
      CALL READ (*120,*120,EDT,ARRY(1),-3,0,FLAG)
  200 CALL READ (*120,*210,EDT,ARRY(1), 3,0,FLAG)
      IF (IDEF.NE.IRY(1) .AND. K.EQ.0) GO TO 200
      IF (IDEF .NE. IRY(1)) GO TO 210
      K = K + 2
      CORE(ICP+K   ) = ARRY(3)
      ICORE(ICP+K-1) = IRY(2)
      IF (LCORE-NROW1+K .LE. 0) CALL MESAGE (-8,IPM,IFED)
      GO TO 200
  210 IF (K .EQ. 0) CALL MESAGE (-32,IDEF,IEDT)
      CALL BCKREC (EDT)
      GO TO 60
C
C
      ENTRY FEDT (IED1,DELTA,IDEF)
C     ============================
C
C     FIND VALUE FOR EID IF IT EXISTS
C
      DO 220 I = 1,K,2
      IF (IED1 .NE. ICORE(ICP+I)) GO TO 220
      ICORE(ICP+I) = -ICORE(ICP+I)
      DELTA = CORE(ICP+I+1)
      GO TO 60
  220 CONTINUE
      DELTA = 0.0
      GO TO 60
C
C
      ENTRY FEDTED (IDEF)
C     ===================
C
C     CHECK TO SEE IF ALL ELEMENTS IN THE SET WERE USED
C
      IFOUND = 0
      DO 230 I = 1,K,2
      IF (ICORE(ICP+I) .LT. 0) GO TO 230
      IEDT(1) = ICORE(ICP+I)
      IEDT(2) = IDEF
      CALL MESAGE (30,139,IEDT)
      IFOUND = 1
  230 CONTINUE
      IF (IFOUND .EQ. 1) CALL MESAGE (-61,0,0)
      GO TO 60
      END
