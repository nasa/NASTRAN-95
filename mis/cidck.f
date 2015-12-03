      SUBROUTINE CIDCK (Z,BUF,NOPEN)
C
C     BULK DATA CARD COORDINATE CHECK
C     THIS ROUTINE IS CALLED ONLY BY IFP, IN LINK1
C
C     WRITTEN BY G.CHAN/UNISYS   9/1989
C
C     LIST OF NASTRAN BULK DATA CARDS THAT REFERENCE COORDINATE CID -
C
C     BULK DATA      CID      NO. OF     GINO         LOCATE
C     CARD           FIELD    WORDS      FILE         INDEX
C     ----------   -------   -------   ---------   ------------
C     AXIF            1          1       AXIC         8815,88
C     BFIELD          1          2       GEOM1        3101,31
C     CEMLOOP*        13        13       GEOM3        3109,31
C     CONM2           3         13       GEOM2        1501,15
C     CORD1C          1          6       GEOM1        1701,17
C     CORD1R          1          6       GEOM1        1801,18
C     CORD1S          1          6       GEOM1        1901,19
C     CORD2C          1,4       13       GEOM1        2001,20
C     CORD2R          1,4       13       GEOM1        2101,21
C     CORD2S          1,4       13       GEOM1        2201,22
C     FORCE           3          7       GEOM3        4201,42
C     GEMLOOP*        3          -       GEOM3        3309,33
C     GRAV            2          6       GEOM3        4401,44
C     GRID/GRDSET     2,6        8       GEOM1        4501,45
C     GRIDB           3          5       GEOM1        8115,81
C     MDIPOLE*        2         10       GEOM3        3509,35
C     MOMENT          3          7       GEOM3        4801,48
C     PIHEX           3          7       EPT          7002,70
C     PLOAD4          9         12       GEOM3        6709,67
C     REMFLUX*        2          -       GEOM3        3409,34
CWKBR 2/95 SPR94015 RFORCE    3     8       GEOM3        5509,55    
C     RFORCE          3          7       GEOM3        5509,55    
C     SPCFLD*         2          -       GEOM3        3209,32
C
C     * THE CID'S ON THESE CARDS CURRENTLY MUST BE ZERO OR BLANK, AND
C       WERE CHECKED ALREADY IN IFS4P. THEREFORE THEY ARE NOT CHECKED
C       HERE.
C
      IMPLICIT INTEGER (A-Z)
      EXTERNAL ANDF
      LOGICAL  ABORT
      INTEGER  AXIF(2),   BFIELD(2), CONM2(2),  CORD(2),   FORCE(2),
     1         GRAV(2),   GRID(2),   GRIDB(2),  MOMENT(2), PIHEX(2),
     2         PLOAD4(2), RFORCE(2),
     3         Z(1),      BUF(1),    TRL(7),    NAME(2)
      CHARACTER*7  CC,    PCC,       CAXIF,     CBFIEL,    CCONM2,
     1         CCORD2,    CFORCE,    CGRAV,     CGRID,     CGRIDB,
     2         CMMENT,    CPIHEX,    CPLOD4,    CRFORC
      CHARACTER*23        UFM
      COMMON   /XMSSG /   UFM
      COMMON   /SYSTEM/   IBUF,      NOUT,      ABORT
      COMMON   /TWO   /   TWO(1)
      DATA     GEOM1,     GEOM2,     GEOM3,     EPT        /
     1         201,       208,       209,       202        /,
     2         AXIC,      NAME,      PCD,       PCC        /
     3         215,       4HCIDC,    2HK , 0,   'XXXX   '  /
      DATA     AXIF,      CAXIF  /   8815,88,   'AXIF   '  /
     1         BFIELD,    CBFIEL /   3101,31,   'BFIELD '  /,
     2         CONM2,     CCONM2 /   1501,15,   'CONM2  '  /
     3         CORD,      CCORD2 /   1601,16,   'CORD2  '  /,
     4         FORCE,     CFORCE /   4201,42,   'FORCE  '  /
     5         GRAV,      CGRAV  /   4401,44,   'GRAV   '  /,
     6         GRID,      CGRID  /   4501,45,   'GRID   '  /
     7         GRIDB,     CGRIDB /   8115,81,   'GRIDB  '  /,
     8         MOMENT,    CMMENT /   4801,48,   'MOMENT '  /
     9         PIHEX,     CPIHEX /   7002,70,   'PIHEX  '  /,
     O         PLOAD4,    CPLOD4 /   6709,67,   'PLOAD4 '  /
     1         RFORCE,    CRFORC /   5509,55,   'RFORCE '  /
C
C
C     OPEN GEOM1 AND SAVE ALL COORDINATE IDS IN Z(1) THRU Z(NCORD)
C     AND REFERENCED COORD ID IN Z(NRID) THRU Z(NOPEN). NOPEN IS
C     LENGTH OF THE AVAILABLE OPEN CORE.
C     SORT AND CHECK ID UNIQUENESS
C
      NCORD= 1
      NRID = NOPEN
      FILE = GEOM1
      CALL PRELOC (*960,BUF,GEOM1)
      K = 6
      DO 20 I = 1,6
      CORD(1) = CORD(1)+100
      CORD(2) = CORD(2)+1
      IF (I .EQ. 4) K = 13
      CALL LOCATE (*20,BUF,CORD(1),M)
 10   CALL READ (*910,*20,GEOM1,Z(NCORD),K,0,M)
      NCORD = NCORD+1
      IF (I.LT.4 .OR. Z(NCORD+2).EQ.0) GO TO 10
      Z(NRID) = Z(NCORD+2)
      NRID  = NRID-1
      GO TO 10
 20   CONTINUE
      NCORD = NCORD-1
      NRID  = NRID +1
      IF (NCORD .LE. 1) GO TO 60
      CALL SORT (0,0,1,1,Z(1),NCORD)
      J = 1
      DO 50 I = 2,NCORD
      IF (Z(I) .NE. Z(I-1)) GO TO 40
      CALL PAGE2 (-2)
      WRITE  (NOUT,30) UFM,Z(I)
 30   FORMAT (A23,' 328, DUPLICATE COORDINATE ID',I9)
      GO TO 50
 40   J = J+1
      Z(J) = Z(I)
 50   CONTINUE
      NCORD = J
C
C     IF CORD2C/R/S CARDS ARE PRESENT, CHECK REFERENCE COORDINATE ID
C
 60   IF (NRID .GT. NOPEN) GO TO 100
      CC = CCORD2
      DO 90 I = NRID,NOPEN
      CID = Z(I)
      IF (NCORD .LE. 0) GO TO 80
      DO 70 J = 1,NCORD
      IF (CID .EQ. Z(J)) GO TO 90
 70   CONTINUE
 80   CALL PAGE2 (-2)
      WRITE (NOUT,830) UFM,CID,CC
      ABORT = .TRUE.
 90   CONTINUE
C
C     DOUBLE THE COORDINATE ID ARRAY FOR 'CIRCULAR' SEARCH, AND MOVE
C     THE ARRAY TO HIGH END OF OPEN CORE, Z(II) THRU Z(NOPEN-1)
C
 100  II = NOPEN-2*NCORD-1
      IF (NCORD .EQ. 0) GO TO 120
      DO 110 I = 1,NCORD
      Z(II+I      ) = Z(I)
 110  Z(II+I+NCORD) = Z(I)
 120  NZ = II
      IM = II+NCORD
      II = II+1
      Z(NOPEN) = -999
C
C     CHECK CID ON GRID CARDS
C
      CC = CGRID
      CALL LOCATE (*190,BUF,GRID(1),M)
      NZX = (NZ/8)*8
 130  CALL READ (*910,*140,GEOM1,Z(1),NZX,0,M)
      M = NZX
      IF (M .LE. 0) GO TO 190
 140  PVCID = 0
      ASSIGN 150 TO IRTN
      I = -6
 150  I = I+8
      IF (I .GT. M) GO TO 160
      CID = Z(I)
      IF (CID.NE.0 .AND. CID.NE.PVCID) GO TO 790
      GO TO 150
 160  PVCID = 0
      ASSIGN 170 TO IRTN
      I = -2
 170  I = I+8
      IF (I .GT. M) GO TO 180
      CID = Z(I)
      IF (CID.NE.0 .AND. CID.NE.PVCID) GO TO 790
      GO TO 170
 180  IF (M .EQ. NZX) GO TO 130
C
C     CHECK GRIDB CARDS
C
 190  CC = CGRIDB
      CALL LOCATE (*240,BUF,GRIDB(1),M)
      NZX = (NZ/5)*5
 200  CALL READ (*910,*210,GEOM1,Z(1),NZX,0,M)
      M = NZX
      IF (M .LE. 0) GO TO 240
 210  PVCID = 0
      ASSIGN 220 TO IRTN
      I = -2
 220  I = I+5
      IF (I .GT. M) GO TO 230
      CID = Z(I)
      IF (CID.NE.0 .AND. CID.NE.PVCID) GO TO 790
      GO TO 220
 230  IF (M .EQ. NZX) GO TO 200
C
C     CHECK BFIELD CARDS
C
 240  CC = CBFIEL
      CALL LOCATE (*270,BUF,BFIELD(1),M)
      CALL READ (*910,*250,GEOM1,Z(1),NZ,1,M)
      GO TO 930
 250  PVCID = 0
      ASSIGN 260 TO IRTN
      I = -1
 260  I = I+2
      IF (I .GT. M) GO TO 270
      CID = Z(I)
      IF (CID.NE.0 .AND. CID.NE.PVCID) GO TO 790
      GO TO 260
C
C     END OF GEOM1 PROCESSING
C
 270  CALL CLOSE (GEOM1,1)
C
C
C     CHECK THE PRESENCE OF CONM2 CARDS IN GEOM2
C
      FILE = GEOM2
      K = CONM2(2)
      ASSIGN 300 TO JRTN
      GO TO 860
 300  IF (K .EQ. 0) GO TO 400
C
C     OPEN GEOM2, AND CHECK CONM2 CARDS
C
      CC = CCONM2
      CALL PRELOC (*400,BUF,GEOM2)
      CALL LOCATE (*350,BUF,CONM2(1),M)
      NZX = (NZ/13)*13
 310  CALL READ (*910,*320,GEOM2,Z(1),NZX,0,M)
      M = NZX
      IF (M .LE. 0) GO TO 350
 320  PVCID = 0
      ASSIGN 330 TO IRTN
      I = -10
 330  I = I+13
      IF (I .GT. M) GO TO 340
      CID = Z(I)
      IF (CID.NE.0 .AND. CID.NE.PVCID) GO TO 790
      GO TO 330
 340  IF (M .EQ. NZX) GO TO 310
C
 350  CONTINUE
      CALL CLOSE (GEOM2,1)
C
C
C     CHECK THE PRESENCE OF BULK DATA CARDS IN GEOM3
C     (FORCE, MOMENT, RFORCE, GRAV AND PLOAD4)
C
 400  FILE = GEOM3
      K = FORCE(2)
      ASSIGN 410 TO JRTN
      GO TO 860
 410  IF (K .NE. 0) GO TO 500
      K = MOMENT(2)
      ASSIGN 420 TO JRTN
      GO TO 870
 420  IF (K .NE. 0) GO TO 500
      K = RFORCE(2)
      ASSIGN 430 TO JRTN
      GO TO 870
 430  IF (K .NE. 0) GO TO 500
      K = GRAV(2)
      ASSIGN 440 TO JRTN
      GO TO 870
 440  IF (K .NE. 0) GO TO 500
      K = PLOAD4(2)
      ASSIGN 450 TO JRTN
      GO TO 870
 450  IF (K .NE. 0) GO TO 500
      GO TO 650
C
C     OPEN GEOM3, AND CHECK CID ON BULK DATA CARDS
C
 500  CALL PRELOC (*650,BUF,GEOM3)
      CALL LOCATE (*510,BUF,FORCE(1),M)
      CC = CFORCE
      IB = 3
      IC = 7
      ASSIGN 510 TO KRTN
      GO TO 600
 510  CALL LOCATE (*520,BUF,MOMENT(1),M)
      CC = CMMENT
      IB = 3
      IC = 7
      ASSIGN 520 TO KRTN
      GO TO 600
 520  CALL LOCATE (*530,BUF,RFORCE(1),M)
      CC = CRFORC
      IB = 3
CWKBR 2/95 SPR94015      IC = 8
      IC = 7
      ASSIGN 530 TO KRTN
      GO TO 600
 530  CALL LOCATE (*540,BUF,GRAV(1),M)
      CC = CGRAV
      IB = 2
      IC = 6
      ASSIGN 540 TO KRTN
      GO TO 600
 540  CALL LOCATE (*550,BUF,PLOAD4(1),M)
      CC = CPLOD4
      IB = 9
      IC = 12
      ASSIGN 550 TO KRTN
      GO TO 600
 550  CONTINUE
      GO TO 630
C
 600  CALL READ (*910,*610,GEOM3,Z(1),NZ,1,M)
      GO TO 930
 610  ASSIGN 620 TO IRTN
      I = IB-IC
 620  I = I +IC
      IF (I .GT. M) GO TO KRTN, (510,520,530,540,550)
      CID = Z(I)
      IF (CID .NE. 0) GO TO 800
      GO TO 620
C
 630  CALL CLOSE (GEOM3,1)
C
C
C     CHECK THE PRESENCE OF PIHEX CARD IN EPT. IF PRESENT, OPEN EPT,
C     AND CHECK CID ON PIHEX CARDS
C
 650  FILE = EPT
      K = PIHEX(2)
      ASSIGN 660 TO JRTN
      GO TO 860
 660  IF (K .EQ. 0) GO TO 700
      CALL PRELOC (*700,BUF,EPT)
      CALL LOCATE (*690,BUF,PIHEX(1),M)
      CALL READ (*910,*670,EPT,Z(1),NZ,1,M)
      GO TO 930
 670  CC = CPIHEX
      ASSIGN 680 TO IRTN
      I = -4
 680  I = I+7
      IF (I .GT. M) GO TO 690
      CID = Z(I)
      IF (CID .NE. 0) GO TO 800
      GO TO 680
 690  CALL CLOSE (EPT,1)
C
C
C     CHECK THE PRESENCE OF AXIF CARD IN AXIC. IF PRESENT, OPEN AXIC,
C     AND CHECK CID ON AXIF CARD. ONLY ONE AXIF CARD EXISTS
C
 700  FILE = AXIC
      K = AXIF(2)
      ASSIGN 710 TO JRTN
      GO TO 860
 710  IF (K .EQ. 0) GO TO 750
      CALL PRELOC (*750,BUF,AXIC)
      CALL LOCATE (*730,BUF,AXIF(1),M)
      CALL READ (*910,*720,AXIC,CID,1,1,M)
 720  CC = CAXIF
      ASSIGN 730 TO IRTN
      IF (CID .NE. 0) GO TO 800
 730  CALL CLOSE (AXIC,1)
C
 750  RETURN
C
C
C     INTERNAL ROUTINE TO LOOK FOR CID MATCH
C     CID ARRAY (DOUBLE) IS AT HIGH END OF CORE, Z(II) THRU Z(NOPEN)
C
 790  PVCID = CID
 800  IF (CID .EQ. Z(II)) GO TO 850
      IF (NCORD  .LE.  1) GO TO 820
      IE = II+NCORD-1
      DO 810 J = II,IE
      IF (CID .EQ. Z(J)) GO TO 840
 810  CONTINUE
 820  IF (CC.EQ.PCC .AND. CID.EQ.PCD) GO TO 850
      CALL PAGE2 (-2)
      WRITE  (NOUT,830) UFM,CID,CC
 830  FORMAT (A23,' 328, UNDEFINED COORDINATE',I9,' IS REFERENCED BY A '
     1,       A7,' CARD')
      PCC = CC
      PCD = CID
      ABORT = .TRUE.
      GO TO 850
 840  II = J
      IF (II .GT. IM) II = II-NCORD
 850  GO TO IRTN, (150,170,220,260,330,620,680,730)
C
C
C     INTERNAL ROUTINE TO CHECK THE PRESENCE OF A PARTICULAR BULK DATA
C     CARD
C
 860  TRL(1) = FILE
      CALL RDTRL (TRL(1))
 870  IF (TRL(1) .LT. 0) GO TO 880
      J = (K-1)/16
      L = K-16*J
      IF (ANDF(TRL(J+2),TWO(L+16)) .NE. 0) GO TO 890
 880  K = 0
 890  GO TO JRTN, (300,410,420,430,440,450,660,710)
C
C     ERRORS
C
 910  J = -2
      GO TO 950
 930  J = -8
 950  CALL MESAGE (J,FILE,NAME)
 960  RETURN
      END
