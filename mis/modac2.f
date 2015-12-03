      SUBROUTINE MODAC2(     NV,INP1,IOUT)
C
C     MODAC2  REDUCES THE SIZE OF INP1 (BY REMOVING SELECTED COLUMNS)
C
C     CORE IS LAIDED OUT AS FOLLOWS
C
C         CONTENTS            LENGTH  TYPE   POINTER
C         --------            ------  ----   -------
C
C         NEW TIMES           NFN      R     IFN
C         KEEP/REMOVE         NFO      I     IKR
C         COPIED COLUMN       MCB(3)   R     ICOL
C
C         2  BUFFERS          SYSBUF   I     IBUF1
C                             SYSBUF   I     IBUF2
C
C     VARIABLES
C
C     NV       NUMBER OF COLUMS TO PROCESS TOGETHER (MINUS SAYS ADD HEAD
C     INP1     COPY FROM THIS FILE
C     IOUT     COPY TO  THIS  FILE
C
C
C
      INTEGER   IZ,SYSBUF,NAME(2),IHD(2),MCB(7),FILE
      REAL  Z(1)
      COMMON /UNPAKX/ITC,II,JJ,INCR
      COMMON /SYSTEM/ SYSBUF
      COMMON /MODAC3/ NFO,NFN,NZ
      COMMON /ZZZZZZ/ IZ(1)
      EQUIVALENCE (Z(1),IZ(1))
      DATA  NAME /4HMODA,4HC2  /
C
C     ALLOCATE CORE
C
      MCB(1) =IOUT
      CALL RDTRL(MCB)
      IF ( MCB(1) .LE. 0) RETURN
      MCB(1) =  INP1
      CALL RDTRL(MCB)
      IF (MCB(1)  .LE. 0) RETURN
      NLOAD = MCB(2)/(NFO*IABS(NV))
      IFN =1
      IKR = IFN + NFN
      ICOL = IKR + NFO
      IBUF1 = NZ -SYSBUF+1
      IBUF2 = IBUF1- SYSBUF
      IF ( ICOL + MCB(3) + 2*SYSBUF .GT. NZ) CALL MESAGE(-8,0,NAME)
C
C     OPEN  FILES
C
      FILE = INP1
      CALL GOPEN(INP1,IZ(IBUF1),0)
      FILE = IOUT
      CALL OPEN(*900,IOUT,IZ(IBUF2),1)
      CALL FNAME(IOUT,IHD)
      CALL WRITE(IOUT,IHD,2,0)
      IF ( NV  .GT. 0) GO  TO  10
      CALL  WRITE(IOUT,Z,NFN,0)
   10 CALL  WRITE(IOUT,0,0,1)
C
C     SET UP MATRIX TRAILER
C
      FILE = INP1
      MCB(2) =0
      MCB(6) =0
      MCB(7) =0
      MCB(1) = IOUT
      ITC = MCB(5)
      INCR = 1
      INV = IABS(NV)
      DO 200 M = 1,NLOAD
      K = IKR -1
      DO  100  I =1,NFO
      K  =K+1
      IF( IZ(K) .EQ. 0)  GO TO 20
C
C     KEEP COLUMN
C
      CALL CYCT2B(INP1,IOUT,INV,IZ(ICOL),MCB)
      GO TO 100
C
C     SKIP COLUMN
C
   20 DO 30 J = 1,INV
      CALL FWDREC(*910,INP1)
   30 CONTINUE
  100 CONTINUE
  200 CONTINUE
C
C     CLOSE  UP
C
      CALL CLOSE(INP1,1)
      CALL CLOSE(IOUT,1)
      CALL WRTTRL(MCB)
      RETURN
C
C     ERROR MESSAGES
C
  900 IP1= -1
  901 CALL MESAGE(IP1,FILE,NAME)
  910 IP1 = -2
      GO TO 901
      END
