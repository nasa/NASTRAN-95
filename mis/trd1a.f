      SUBROUTINE TRD1A (CASEXX,TRL,IC,NLFTP,NGROUP,MODA1)
C
C     THIS ROUTINE BUILDS THE INITIAL CONDITIONS TABLE, PUTS TSTEP STUFF
C     IN CORE AND EXTRACTS THE NLFTP POINTER
C
C     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
C
      INTEGER            SYSBUF    ,CASEXX   ,TRL      ,INTRL(2) ,
     1                   IZ(160)   ,MCB(7)   ,FILE     ,NAME(2)
C
      COMMON   /SYSTEM/  SYSBUF
      COMMON   /PACKX /  IT1       ,IT2      ,II       ,JJ       ,
     1                   INCR
      COMMON   /ZZZZZZ / Z(1)
C
      EQUIVALENCE        (Z(1)     ,IZ(1))
C
      DATA               NAME                ,INTRL              /
     1                   4HTRD1    ,4HA      ,4HTRL    ,4HTRD    /
C
C     IDENTIFICATION VARIABLES
C
C     NGROUP        NUMBER OF CHANGES OF TIME STEP
C
C     ITSTEP        SELECTED TSTEP ID
C
C     NLFTP         SELECTED NON-LINEAR LOAD ID
C
C     ICP           SELECTED INITIAL CONDITION ID
C
C     LUD           LENGTH OF INITIAL CONDITION--D SET
C
C     IGROUP        POINTER TO TSTEP STUFF
C
C
C
C     INITIALIZE
C
      IT1 = 1
      IT2 = 1
      II  = 1
      INCR= 1
      NZ  = KORSZ (Z)
      NX  = NZ
C
C     PICK UP AND STORE CASECC POINTERS
C
      IBUF1 = NZ -SYSBUF +1
      CALL GOPEN (CASEXX,IZ(IBUF1),0)
      CALL FREAD (CASEXX,IZ,166,1)
      CALL CLOSE (CASEXX,1)
      ITSTEP = IZ(38)
      ICP    = IZ(9)
      NLFTP  = IZ(160)
      IF (ICP.NE.0 .AND. MODA1.EQ.1) GO TO 920
C
C     BUILD INITIAL CONDITION FILE
C
      CALL GOPEN (IC,IZ(IBUF1),1)
      IBUF2 = IBUF1-SYSBUF
      NZ    = NZ - 2*SYSBUF
      ICRQ  =-NZ
      IF (ICRQ .GT. 0) GO TO 980
      FILE = TRL
      CALL OPEN (*900,TRL,IZ(IBUF2),0)
      CALL READ (*910,*10,TRL,IZ(1),NZ,0,IFLAG)
      ICRQ = NZ
      GO TO 980
   10 LUD  = IZ(IFLAG)
      JJ   = LUD
      ICRQ = 2*LUD - NZ
      IF (ICRQ .GT. 0) GO TO 980
      L    = IZ(3)
      ITRL = L
C
C     ZERO I. C.
C
      IVEL  = IBUF2- LUD-1
      IDISP = IVEL -LUD
      DO 20 I = 1,LUD
      K = IVEL +I
      Z(K) = 0.0
      K = IDISP +I
      Z(K) = 0.0
   20 CONTINUE
      CALL MAKMCB (MCB,IC,LUD,2,1)
      IF (ICP   .EQ. 0) GO TO 80
      IF (IZ(3) .EQ. 0) GO TO 40
      IFLAG = IFLAG-1
      DO 30 I = 4,IFLAG
      IF (IZ(I) .EQ. ICP) GO TO 50
   30 CONTINUE
   40 ITSTEP = ICP
      GO TO 940
   50 K = I-4
      L = IFLAG -I
      CALL SKPREC (TRL,K)
   70 CALL READ (*910,*80,TRL,IZ(1),3,0,IFLAG)
      K    = IZ(1) +IDISP
      I2   = 2
      Z(K) = Z(K) + Z(I2)
      K    = IZ(1) + IVEL
      Z(K) = Z(K) + Z(I2+1)
      GO TO 70
   80 CALL PACK (Z(IDISP+1),IC,MCB)
      CALL PACK (Z(IVEL +1),IC,MCB)
      CALL CLOSE (IC,1)
      CALL WRTTRL (MCB)
      CALL SKPREC (TRL,L)
C
C     BRING TSTEP STUFF INTO CORE
C
  100 ITRL = ITRL +1
      CALL READ (*940,*110,TRL,IZ(1),NZ,0,IFLAG)
      ICRQ = NZ
      GO TO 980
  110 IF (IZ(1) .NE. ITSTEP) GO TO 100
C
C     TSTEP CARD FOUND
C
      CALL CLOSE (TRL,1)
      NGROUP = (IFLAG-1)/3
C
C     MOVE TSTEP STUFF TO BOTTOM OF CORE
C
      NZ = NX - IFLAG +1
      IGROUP = NZ+1
      DO 120 I = 2,IFLAG
      K = IGROUP +I-2
      IZ(K) = IZ(I)
  120 CONTINUE
      RETURN
C
C     ERROR MESSAGES
C
  900 IP1 = -1
  901 CALL MESAGE (IP1,FILE,NAME)
      RETURN
  910 IP1 = -2
      GO TO 901
  920 IP1  = -51
      FILE = ICP
      GO TO 901
  940 CALL MESAGE (-31,ITSTEP,INTRL)
      RETURN
  980 IP1 = -8
      FILE = ICRQ
      GO TO 901
      END
