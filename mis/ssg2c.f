      SUBROUTINE SSG2C (A,B,C,OP,BLOCK)
C
      INTEGER          A,B,C    ,OP       ,BLOCK(11),NA(2)    ,NB(2)
      DIMENSION        IA(5)    ,IB(5)    ,IC(5)    ,IT(1)    ,IT1(1)
      DOUBLE PRECISION DIT1
      INTEGER          DT1(2)
      CHARACTER        UFM*23   ,UWM*25
      COMMON /XMSSG /  UFM      ,UWM
      COMMON /ZZZZZZ/  CORE(1)
      COMMON /SADDX /  NOMAT    ,LCORE    ,MCBS(67)
      COMMON /SYSTEM/  KSYSTM(65)
      EQUIVALENCE      (DT1,DIT1)
      EQUIVALENCE      (KSYSTM(55),IPR1)  ,(MCBS(1),IA(1))  ,
     1                 (MCBS(8),IT(1),DIT),(MCBS(13),IB(1)) ,
     2                 (MCBS(20),IT1(1)),(MCBS(61),IC(1)) ,
     3                 (KSYSTM(2),NOUT)   ,(IA5,IA(5)) ,(IB5,IB(5))
C
C     BLOCK(6) WAS NOT USED IN ORIGINAL NASTRAN. IT IS NOW USED TO FLAG
C     THE CHECKING OF THE INPUT MATRICES COMPATABILITY IF THE CALLER
C     PRESETS BLOCK(6) TO -1
C
      IA(1) = A
      CALL RDTRL (IA)
      IF (IA(1) .LT. 0) IA(1) = 0
      IB(1) = B
      CALL RDTRL (IB)
      IF (IB(1) .GT. 0) GO TO 10
      IB(1) = 0
      IF (IA(1)) 150,150,30
   10 DO 20 I = 2,4
   20 IC(I) = IB(I)
      GO TO 50
   30 DO 40 I = 2,4
   40 IC(I) = IA(I)
C
   50 NOMIX = 0
      IF (BLOCK(6) .NE. -1) GO TO 70
      IF (IA5.EQ.0 .OR. IB5.EQ.0) GO TO 70
      IF ((IA5.LE.2 .AND. IB5.LE.2) .OR. (IA5.GE.3 .AND. IB5.GE.3))
     1    GO TO 70
      IF (MAX0(IA5,BLOCK(1)) .EQ. MAX0(IB5,BLOCK(7))) GO TO 70
      NOMIX = 1
      CALL FNAME (A,NA)
      CALL FNAME (B,NB)
      WRITE  (NOUT,60) UWM,NA,IA(2),IA(3),IA5,IA(4),NB,IB(2),IB(3),
     1                 IB5,IB(4)
   60 FORMAT (A25,', SSG2C RECEIVES TWO MIXED FILE TYPES FOR ADDING.',
     1        /,2(5X,'FILE ',2A4,'(',I6,' X',I6,') TYPE =',I3,
     2        ', FORM =',I3))
C
C     UNSY + SYM = UNSY
C
   70 IF (IC(4) .NE. 6) GO TO 80
      IF (IA(1).NE.0 .AND. IA(4).NE.6) IC(4) = 1
      IF (IB(1).NE.0 .AND. IB(4).NE.6) IC(4) = 1
   80 IF (OP .LT. 0) IA(2) = -IC(2)
      DO 90 I = 1,5
      IT(I)  = BLOCK(I  )
   90 IT1(I) = BLOCK(I+6)
      DT1(1) = MCBS(20)
      DT1(2) = MCBS(21)
      IF (NOMIX .NE. 0) WRITE (NOUT,92,ERR=95) IT(1),DIT,IT1(1),DIT1
   92 FORMAT ('  MULTIPLIERS =',I3,D12.3,I8,D12.3)
   95 IC(1)  = C
      LCORE  = KORSZ(CORE)
C
C     DETERMINE TYPE OF OUTPUT
C
      IRC = 0
      IF (IA(1) .EQ. 0) GO TO 100
      IF (IA5.GT.2 .OR.  IT(1).GT.2) IRC = 2
  100 IF (IB(1) .EQ. 0) GO TO 110
      IF (IB5.GT.2 .OR. IT1(1).GT.2) IRC = 2
  110 CONTINUE
      IPREC = IPR1
      IC(5) = IRC + IPREC
      NOMAT = 2
      IF (NOMIX .EQ. 0) GO TO 130
      CALL FNAME (IC(1),NA)
      WRITE  (NOUT,120) NA,IC(2),IC(3),IC(5),IC(4)
  120 FORMAT (5X,'FILE ',2A4,'(',I6,' X',I6,') TYPE =',I3,', FORM =',I3,
     1        5X,'(RESULTANT)')
  130 CALL SADD (CORE,CORE)
      CALL WRTTRL (IC)
  150 RETURN
      END
