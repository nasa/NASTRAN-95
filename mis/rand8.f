      SUBROUTINE RAND8(NFREQ,NPSDL,NTAU,XYCB,LTAB,IFILE,PSDF,AUTO,NFILE)
C
C     THIS ROUTINE COMPUTES RANDOM RESPONSE FOR COUPLED POWER SPECTRAL
C       DENSITY COEFICIENTS
C
      INTEGER  IZ(1),SYSBUF,FILE,XYCB,PSDF,AUTO,IFILE(1),NAME(2),
     1    MCB1(7),MCB2(7),OLDLD
      REAL Q(2)
      REAL DATA(100)
C
      COMMON /CONDAS/    PI       ,TWOPI    ,RADEG    ,DEGRAD   ,
     1                   S4PISQ
      COMMON /SYSTEM/ SYSBUF
      COMMON /ZZZZZZ/  Z(1)
C
      EQUIVALENCE (Z(1),IZ(1))
C
      DATA  NAME,MCB1,MCB2/4HRAND,4H8   ,14*0/
      DATA IPSDF,IAUTO /4001,4002/
C *****
C     DEFINITION OF VARIABLES
C *****
C     NFREQ    NUMBER OF FREQUENCIES
C     NPSDL    NUMBER OF PSDL  CARDS
C     NTAU     NUMBER OF TIMES
C     XYCB     DATA BLOCK CONTAINING XY USER REQUESTS
C     LTAB     LENGTH OF CORE USED FOR TABLES BY PRETAB
C     IFILE    ARRAY CONTAINING FILE NAMES FOR SORT 2 INPUT FILES
C     PSDF     OUTPUT FILE FOR POWER SPECTRAL DENSITY FUNCTIONS
C     AUTO     OUTPUT FILE FOR AUTOCORRELATION FUNCTIONS
C     NFILE    LENGTH OF IFILE ARRAY
C     MCB1     TRAILER FOR PSDF
C     MCB2     TRAILER FOR AUTO
C     IPSDF    OFP ID FOR PSDF
C     IAUTO    OFP ID FOR AUTO
C     LCORE    AVAILABLE CORE FOR  LISTS
C     IBUF1    BUFFER POINTERS
C     IBUF2
C     IBUF3
C     ITAU     POINTER TO FIRST TAU-1
C     ISAA     POINTER TO SAB TABLE -1
C     TAU      TIMES FOR AUTOCORRELATION
C     SAB      POWER SPECTRAL DENSITY FACTORS
C     ICORE    POINTER TO FIRST REQUEST-1
C     SYSBUF   LENGTH OF ONE BUFFER
C     NPOINT   TOTAL NUMBER OF REQUESTS
C     NZ       CORE AVAIABLE FOR STORING H VALUES
C     IP       POINTER TO FIRST POINT OF CURRENT CORE LOAD
C     NDONE    NUMBER OF REQUESTS PROCESSED
C     OLDLD    LOAD ID OF OLD LOAD SET
C     NDO      NUMBER POSSIBLE TO DO IN CORE
C     ICS      POINTER TO FIRST H ARRAY
C     NLOAD    NUMBER OF LOADS      PROCESSED ON CURRENT CORE LOAD
C     ICDONE   NUMBER CURRENTLY DONE -- SEVERAL COMP FROM EACH VALUE
C     LOAD     SUBCASE ID FROM INPUT RECORD
C     IF       FORMAT FLAG IF=0  DATA IS REAL/IMAG  IF .NE. 0 MAG/PHASE
C     LEN      LENGTH OF DATA RECORD
C     Q        MEAN  RESPONSE
C     R        AUTOCORRALATION FUNCTION AT TIME TAU
C     IP1      LOCAL POINT POINTER
C     NUNQ     NUMBER OF UNIQUE LOAD ID-S
C     ILOAD    POINTER TO LOAD LIST-1
C     ISJ      POINTER TO SJ ADD AREA-1
C     ICS      H STORAGE -1
C
C
C
C *****
C     CORE LAYOUTDURING EXECUTION
C *****
C     FREQUENCIES   NFREQ OF THEM
C     RANDPS DATA   NPSDL OF THEM  5 WORDS PER CARD
C                   LOAD ID  LOAD ID   X   Y   TABLE
C     TAUS          NTAU OF THEM
C     TABLE DATA    LTAB OF IT
C     S(AB)         NFREQ OF THEM-- THESE ARE REEVALUATED WHEN LOAD CHAN
C     UNIQUE ID-S   NUNQ  OF THEM
C     REQUESTS      NPOINT OF THEM 5 WORDS PER REQUEST
C                   DB   ID   COMP O.P. P/P
C     H-S           LENGTH = 2*NFREQ --REAL+IMAGINARY
C                   NUNQ  H-S PER SET-- NDO SETS
C     SJ COMPUTE    NFREQ OF IT
C
C
C     BUFFERS       3 NEEDED
C
C
C
C
C     INITIALIZE GENERAL VARIABLES--ASSIGN BUFFERS,ETC
C
      MCB1(1)=PSDF
      MCB2(1)=AUTO
      LCORE=KORSZ(Z)
      IBUF1=LCORE-SYSBUF+1
      IBUF2=IBUF1-SYSBUF
      IBUF3=IBUF2-SYSBUF
      ITAU=NFREQ+5*NPSDL
      ISAA=NTAU+LTAB+ITAU
      LCORE=LCORE-(ISAA+NFREQ+3*SYSBUF)
      ICRQ = -LCORE
      IF(LCORE .LE. 0) GO TO 980
C
C     BUILD LIST OF UNIQUE LOAD ID-S
C         REPLACE LOAD ID OF PSDL WITH POINTER TO LIST
C
      NUNQ=0
      ILOAD=ISAA+NFREQ
      M=ILOAD+1
      K=M-1
      I=NFREQ+1
      JJ=ITAU+1
      J=1
      GO TO 4
C
C         SEARCH LIST OF UNIQUE ID-S
C
    5 DO 3  L=M,K
      IF(IZ(I) .EQ. IZ(L)) GO TO 9
    3 CONTINUE
      GO TO 4
C
C         NEXT PSDL CARD
C
    2 IF(J .EQ. 0) GO TO 7
      I=I+1
      J=0
      GO TO 5
C
C         SAVE LOAD ID
C
    4 K=K+1
      NUNQ=NUNQ+1
      IZ(K)=IZ(I)
      L=K
C
C         REPLACE ID WITH POINTER INTO LIST
C
    9 IZ(I)=L-M+1
      GO TO 2
C
C         NEXT PSDL CARD
C
    7 I=I+4
      J=1
      IF(I .NE. JJ) GO TO 5
C
C         COMPUTE MINIMUM CORE
C
      MINCR=NUNQ*NFREQ*2+NFREQ
      ICORE=ILOAD+NUNQ
      LCORE=LCORE-NUNQ
      ICRQ = MINCR - LCORE
      IF(LCORE-MINCR .LE. 0) GO TO 980
C
C         OPEN OUTPUT FILES
C
      CALL GOPEN(PSDF,Z(IBUF2),1)
      CALL GOPEN(AUTO,Z(IBUF3),1)
C
C         BEGIN LOOP ON EACH FILE
C
      DO 1000  I=1,NFILE
C
C         BUILD POINT LIST FOR FILE(I)
C
      CALL RAND6 (XYCB,Z(IBUF1),NPOINT,IZ(ICORE+1),IFILE(I),LCORE)
      IF(NPOINT .EQ. 0) GO TO 1000
      NZ=LCORE-5*NPOINT
      ICRQ = -NZ
      IF(NZ .LE. 0) GO TO 980
C
C         OPEN INPUT FILE
C
      FILE=IFILE(I)
      CALL OPEN(*1000,FILE,Z(IBUF1),0)
      IP=ICORE+1
      NDONE=0
      OLDLD=0
      ICS=ICORE+5*NPOINT
      LLIST=5*NPOINT
C
C         COMPUTE NUMBER OF POINTS TO DO AT SAME TIME
C
   13 NDO = MIN0(NPOINT-NDONE,NZ/MINCR)
      ICRQ = MAX0(NPOINT-NDONE,MINCR)
      IF(NDO .EQ. 0) GO TO 980
      LLISTS = LLIST
      ICDONE=0
      IPSAVE=IP
      NLOAD =0
C         GET READY TO OBTAIN FIRST VALUE
C
   15 CALL RAND2 (IFILE(I),IZ(IP),LOAD,IF,LEN,LLIST)
      IF(LOAD .EQ. 0) GO TO 159
C
C         CHECK FOR NEW LOAD
C
      IF(LOAD .EQ. OLDLD) GO TO 50
C
C         NEW LOAD -- SEE IF WANTED
C
      DO 10  KK=1,NUNQ
      L=ILOAD+KK
      IF(LOAD .EQ. IZ(L)) GO TO 20
   10 CONTINUE
C
C         REJECT LOAD -- NOT NEEDED
C
      GO TO 15
C
C         GOOD LOAD -- SAVE DATA
C
   20 OLDLD=LOAD
C
C         BRING DATA INTO KK-TH H SAVE AREA
C
      KK = ICS +(KK-1)*NFREQ*2
   50 IF(LEN .GT. 100) GO TO 970
      DO 60  J=1,NFREQ
C
C     ACCESS DATA FROM FILE  INTO DATA  ARRAY
C
      CALL RAND2A( DATA(1))
      IP1=IP
      II=ICDONE
C
C         COMPUTE REAL/IMAG OF CURRENT COMPONENT
C
   52 IF( (LEN-2)/2 .GE. IZ(IP1+2)) GO TO 53
C
C     REQUEST OUT OF RANGE
C
      CALL MESAGE(52,IZ(IP1),IZ(IP1+1))
      IZ(IP1+2) = (LEN-2)/2
   53 JJ = IZ(IP1+2) +2
      K=JJ+LEN/2-1
      IF ( IF .LE. 0) GO TO 51
      X=DATA(JJ)*COS(DEGRAD*DATA(K))
      DATA(K)=DATA(JJ)*SIN(DEGRAD*DATA(K))
      DATA(JJ)=X
   51 L=KK+J*2-1+II*MINCR
      Z(L)=DATA(JJ)
      Z(L+1)=DATA(K)
C
C         TEST FOR CORE OVERFLOW
C
      IF(II .EQ. NDO-1) GO TO 60
C
C         IS NEXT REQUEST FROM SAME POINT
C
      IF(IZ(IP1) .NE. IZ(IP1+5) .OR. IZ(IP1+1) .NE. IZ(IP1+6)) GO TO 60
      II=II+1
      IP1=IP1+5
      GO TO 52
   60 CONTINUE
      ICDONE=II+1
      IP=IP1+5
      LLIST=LLIST-5*ICDONE
C
C         HAVE I DONE ALL REQUESTS (IN CURRENT CORE)
C
      IF(ICDONE .NE. NDO) GO TO 15
C
C         HAVE I ADDED IN ALL LOADS
      NLOAD = NLOAD +1
      IP=IPSAVE
      IF(NLOAD .EQ. NUNQ ) GO TO 100
C
C         START AGAIN ON NEXT LOAD
      LLIST = NDO*5
      ICDONE=0
      GO TO 15
C
C         ALL LOADS FOR CURRENT BUNCH DONE
C              COMPUTE SJ-S
C
C              ZERO ALL SJ-S
C
  100 DO 101  J=1,NDO
      K=ICS+J*MINCR-NFREQ
      DO 102  L=1,NFREQ
      JJ=K+L
      Z(JJ)=0.0
  102 CONTINUE
  101 CONTINUE
C
C         FOR EACH PSDL CARD  1. EVALUATE SAB
C              FOR EACH POINT
C                   IN CORE   2. COMPUTE 2*RE(HI*SIJ*HJBAR)
C                             3. ADD TO SJ AT EACH FREQ.
C
      DO 120  J=1,NPSDL
C
C         EVALUATE SAB
C
      TWO=2.0
      L=NFREQ+(J-1)*5
      IF(IZ(L+1) .EQ. IZ(L+2)) TWO=1.0
      Q(1) = Z(L+3)
      R=Z(L+4)
      DO 103  K=1,NFREQ
      JJ=ISAA+K
C
C
C                TAB     X    F(X)
      CALL TAB (IZ(L+5),Z(K),Z(JJ))
      IF(IZ(L+5) .EQ.0) Z(JJ) =1.0
  103 CONTINUE
C
C         FOR EACH POINT IN CORE
C
      DO 115  K=1,NDO
      L2=ICS+K*MINCR-NFREQ
      L1=ICS+(K-1)*MINCR-1-NFREQ*2
      DO 110  M=1,NFREQ
      IH1=IZ(L+1)*NFREQ*2 +L1  +2*M
      IH2=IZ(L+2)*NFREQ*2 +L1  +2*M
      JJ=ISAA+M
      ISJ=L2+M
      Z(ISJ)=Z(ISJ)+Z(JJ)*TWO*((Z(IH1)*Q(1)-Z(IH1+1)*R)*Z(IH2)
     1  +(Z(IH1+1)*Q(1)+Z(IH1)*R)*Z(IH2+1))
  110 CONTINUE
  115 CONTINUE
  120 CONTINUE
C
C         OUTPUT STUFF IN CORE
C
      JJ=IP
      J=NDO*5+JJ-1
      L=ICS-NFREQ
      DO 150  K=JJ,J,5
      L=L+MINCR
C
C         CONVERT SJ TO ABSOLUTE VALUE
C
      DO 151  LL=1,NFREQ
      KK=L+LL
      Z(KK)=ABS(Z(KK))
  151 CONTINUE
C
C         COMPUTE MEAN RESPONSE
C
      CALL RAND3 (Z(1),Z(L+1),Q,NFREQ)
      IF(IZ(K+3) .EQ. 2) GO TO 155
C
C         PSDF REQUESTED -- PUT OUT ID
C
      MCB1(7)=MCB1(7)+1
      CALL RAND1(PSDF,IPSDF,IZ(K),IZ(K+1),IZ(K+4),Q)
C
C         PUT OUT DATA RECORDED
C
      DO 152  LL=1,NFREQ
      KK=L+LL
      CALL WRITE (PSDF,Z(LL),1,0)
      CALL WRITE (PSDF,Z(KK),1,0)
  152 CONTINUE
      CALL WRITE (PSDF,0,0,1)
  155 IF(IZ(K+3) .EQ. 1) GO TO 150
C
C         AUTO CORRELATION REQUESTED
C
      IF(NTAU .EQ. 0) GO TO 150
      CALL RAND1(AUTO,IAUTO,IZ(K),IZ(K+1),IZ(K+4),Q)
      MCB2(7)=MCB2(7)+1
C
C         PUT OUT DATA RECORD
C
      DO 156  LL=1,NTAU
      KK=ITAU+LL
      CALL WRITE (AUTO,Z(KK),1,0)
C
C         COMPUTE AUTO
C
      CALL RAND4 (Z(1),Z(L+1),Z(KK),R,NFREQ)
      CALL WRITE (AUTO,R,1,0)
  156 CONTINUE
      CALL WRITE (AUTO,0,0,1)
  150 CONTINUE
C
C         END CORE LOAD
C
      CALL REWIND (IFILE(I))
      NDONE=NDONE+NDO
      IF(NDONE .NE. NPOINT) GO TO 200
C
C         FINISHED WITH FILE
C
  159 CALL CLOSE(IFILE(I),1)
      GO TO 1000
C
C         SPILL ON POINT LISTS -- GO AGAIN
C
  200 OLDLD=0
      LLIST=LLISTS-5*NDO
      IP=IPSAVE+5*NDO
      GO TO 13
 1000 CONTINUE
C
C         ALL STUFF DONE -- GET OUT
C
      CALL CLOSE (PSDF,1)
      CALL CLOSE (AUTO,1)
      CALL WRTTRL(MCB1)
      CALL WRTTRL(MCB2)
      RETURN
C
C         FILE + MISC ERRORS
C
  901 CALL MESAGE (IP1,FILE,NAME)
      RETURN
  970 IP1=-7
      GO TO 901
  980 IP1=-8
      FILE = ICRQ
      GO TO 901
      END
