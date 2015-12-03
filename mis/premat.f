      SUBROUTINE PREMAT (IZ,RZ,BFR,NIMAT,N2MAT,MPTF,DITF)
C
C     REVISED 7/92, BY G.CHAN, NEW REFERENCE TO OPEN CORE ARRAY, SUCH
C     THAT THE SOURCE CODE IS UP TO ANSI FORTRAN 77 STANADARD
C
      LOGICAL         PART1 ,PLA   ,TDEP
      INTEGER         IZ(1) ,BFR(1),DITF  ,QMAT1 ,QMAT2 ,QMATX ,FLAG  ,
     1                MAT1(2)      ,MATT1(2)     ,DIT   ,BUF(3),TEMPID,
     2                NAM(2),BACK  ,RET   ,MAT2(2)      ,MATT2(2)     ,
     3                RET1  ,PASS  ,TABLID,TABLEI(16)   ,MATS1(2)     ,
     4                MAT3(2)      ,MATT3(2)     ,QMAT3 ,QMAT8 ,MAT8(2)
      INTEGER         QMATF ,ELEMID,QMTPZ1,QMTPZ2,QMAT6 ,SYSBUF,MATF(2)
      INTEGER         Z     ,OFFSET
      REAL            NU    ,RZ(1) ,X(27) ,Y(25) ,NUX   ,NUXX  ,J11   ,
     1                J12   ,J22   ,NUXY3 ,NUYZ3 ,NUZX3 ,MATSET,ZZ(1)
      DIMENSION       MATPZ1(2)    ,MATPZ2(2)    ,MTTPZ1(2)    ,
     1                MTTPZ2(2)    ,BUFPZ(51)    ,MAT6(2)      ,
     2                MATT6(2)     ,BUFTM6(39)   ,XY(108)      ,
     3                IB(46)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SYSTEM/ SYSBUF,NOUT  ,SKP(7),TEMPID
      COMMON /NAMES / RD    ,RDREW ,WRT   ,WRTREW,CLSREW
      COMMON /MATIN / MATID ,INFLAG,TEMP  ,PLAARG,SINTH ,COSTH
      COMMON /MATOUT/ E     ,G     ,NU    ,RHO   ,ALPHA ,TO    ,GE    ,
     1                SIGMAT,SIGMAC,SIGMAS,SPACE(15)    ,TDEP  ,
     2                DUM26(26)
C
C     COMMON FOR PIEZOELECTRIC MATERIAL
      COMMON /MATPZ / PZOUT(51)
C
C     ISOPARAMETRIC MATERIALS
C     COMMON /MATISO/ G11   ,G12   ,G13   ,G14   ,G15   ,G16   ,G12   ,
C                     G22,..,G56   ,G66   ,RHO   ,
C                     AXX   ,AYY   ,AZZ   ,AXY   ,AYZ   ,AZX   ,TREF  ,
C                     GE    ,IER
C
      COMMON /MATISO/ BUFM6(46)
      EQUIVALENCE     (E    ,BUF(1),Y(1) ),
     1                (Y( 1),G11   ,EX3  ,PLAANS ,INDSTR),
     2                (Y( 2),G12   ,EY3  ,ICELL2),
     3                (Y( 3),G13   ,EZ3  ),
     4                (Y( 4),G22   ,NUXY3),
     5                (Y( 5),G23   ,NUYZ3),
     6                (Y( 6),G33   ,NUZX3),
     7                (Y( 7),RHOY  ,RHO3 ),
     8                (Y( 8),ALPH1 ,GXY3 ),
     9                (Y( 9),ALPH2 ,GYZ3 ),
     O                (Y(10),ALPH12,GZX3 ),
     1                (Y(11),TOY   ,AX3  ),
     2                (Y(12),GEY   ,AY3  ),
     3                (Y(13),SIGTY ,AZ3  ),
     4                (Y(14),SIGCY ,TREF3),
     5                (Y(15),SIGSY ,GE3  )
      EQUIVALENCE     (Y(16),J11   ,G113 ),
     1                (Y(17),J12   ,G123 ),
     2                (Y(18),J22   ,G133 ),
     3                (Y(19),       G223 ),
     4                (Y(20),       G233 ),
     5                (Y(21),       G333 ),
     6                (Y(22),     SIGTY3 ),
     7                (Y(23),     SIGCY3 ),
     8                (Y(24),     SIGSY3 ),
     9                (Y(25),     MATSET )
      EQUIVALENCE     (X( 1)    , EX     ),
     1                (X( 2)    , GX     ),
     2                (X( 3)    , NUX    ),
     3                (X( 4)    , RHOX   ),
     4                (X( 5)    , ALPHX  ),
     5                (X( 6)    , TOX    ),
     6                (X( 7)    , GEX    ),
     7                (X( 8)    , SIGTX  ),
     8                (X( 9)    , SIGCX  ),
     9                (X(10)    , SIGSX  )
      EQUIVALENCE     (BUFM6(1) , IB(1)  )
      EQUIVALENCE     (ZZ(1)    , Z(1)   )
C
C     DATA DEFINING CARDS TO BE READ
C
      DATA    MAT1   ,  KMAT1  ,  LMAT1  /  103, 1,   11,     31 /  ,
     2        MAT2   ,  KMAT2  ,  LMAT2  /  203, 2,   16,     46 /  ,
     3        MAT3   ,  KMAT3  ,  LMAT3  / 1403,14,   16,     46 /  ,
     6        MAT6   ,  KMAT6  ,  LMAT6  / 2503,25,   40,    118 /  ,
     8        MAT8   ,  KMAT8  ,  LMAT8  /  603, 6,   18,     52 /  ,
     1        MATT1                      /  703, 7               /  ,
     2        MATT2                      /  803, 8               /  ,
     3        MATT3                      / 1503,15               /  ,
     6        MATT6                      / 2603,26               /  ,
     1        MATS1                      /  503, 5               /  ,
     1        MATPZ1 ,  KMTPZ1 ,  LMTPZ1 / 1603,16,   15,     43 /  ,
     2        MATPZ2 ,  KMTPZ2 ,  LMTPZ2 / 1703,17,   52,    154 /  ,
     1        MTTPZ1                     / 1803,18               /  ,
     2        MTTPZ2                     / 1903,19               /  ,
     F        MATF   ,  KMATF  ,  LMATF  / 5110,51,    3,      3 /
      DATA    NTYPES,   TABLEI /  8,
     1        105 , 1,  205 , 2,  305 , 3,  405 , 4,  3105,31,  3205,32,
     2        3305,33,  3405,34/
      DATA    NAM    /  4HMAT ,   4H     /
C
C     MAT1 AND MAT2 HAVE ONE EXTRA WORD AT END
C
      DATA NWMAT1 /  12 /  ,   NWMAT2 / 17  /
C
C      - DATA IN /MATOUT/ IN VARIOUS MAT FORMATS, AND INFLAGS -
C
C     FORMAT  MAT1    MAT2                     MAT3      MAT6  MAT8 MATP
C     INFLAG=   1   2,3,12    4      5    6,8     7   11   10    12
C     -WORD- -----  ------  --- ------ ------ ----- ---- ---- ----- ----
C       1       E     G11   RHO INDSTR PLAANS    EX         :    E1    E
C       2       G     G12             ICDLL/8    EY         :  NU12    E
C       3      NU     G13                        EZ              E2
C       4     PHO     G22                      NUXY  RHO        G12
C       5    ALPH     G23                      NUYZ             G2Z
C       6      T0     G33                      NUZX             G1Z
C       7      GE     RHO                       RHO             RHO
C       8    SIGT   ALPH1    (X/N INDICATES     GXY           ALPH1
C       9    SIGC   ALPH2     ITEM X IS FOR     GYZ           ALPH2
C      10    SIGS  ALPH12     INFLAG=N ONLY)    GZX              TO
C      11              TO                        AX              TL
C      12              GE                        AY              CL
C      13            SIGT                        AZ              TT
C      14            SIGC                        TO              CT
C      15            SIGS                        GE              IS
C      16         E/12 J11/3                     G11             GE
C      17         E/12 J12/3                     G12            F12
C      18              J22/3                     G13
C      19                                        G22
C      20                                        G23
C      21                                        G33
C      22                                       SIGT
C      23                                       SIGC
C      24                                       SIGS
C      25  MATSET  MATSET                     MATSET          MATSET
C      26    TDEP
C       :
C
C     PERFORM GENERAL INITIALIZATION
C
      QMAT1  = 0
      QMAT2  = 0
      QMAT3  = 0
      QMAT6  = 0
      QMAT8  = 0
      QMATF  = 0
      QMATX  = 0
      QMTPZ1 = 0
      QMTPZ2 = 0
      PLA    = .FALSE.
      IF (DITF .LT. 0) PLA = .TRUE.
      PART1  = .TRUE.
      I      =-1
      MPT    = MPTF
      DIT    = IABS(DITF)
      OFFSET = LOCFX(IZ(1)) - LOCFX(Z(1))
      IF (OFFSET .LT. 0) CALL ERRTRC ('PREMAT  ',10)
      N1MAT  = NIMAT + OFFSET
C
C     READ MAT1,MAT2 AND MAT3 CARDS.  SPREAD FORMAT SO THAT MATTI AND
C     MATSI TEMPERATURE AND STRESS-STRAIN TABLE NUMBERS CAN BE MERGED
C
      CALL PRELOC (*350,BFR,MPT)
      IMAT1 = 1 + OFFSET
      I     = 1 + OFFSET
      CALL LOCATE (*60,BFR,MAT1,FLAG)
      QMAT1 = 1
      IMHERE= 30
   30 CALL READ (*1350,*50,MPT,BUF,NWMAT1,0,FLAG)
      Z(I) = BUF(1)
      I    = I + 1
      DO 40 J = 2,KMAT1
      Z(I  ) = BUF(J)
      Z(I+1) = 0
      Z(I+2) = 0
      IF (I .GT. N1MAT) GO TO 1398
   40 I = I + 3
      GO TO 30
   50 NMAT1 = I - LMAT1
   60 IMAT2 = I
      CALL LOCATE (*100,BFR,MAT2,FLAG)
      QMAT2 = 1
      IMHERE= 70
   70 CALL READ (*1350,*90,MPT,BUF,NWMAT2,0,FLAG)
      Z(I) = BUF(1)
      I    = I + 1
      DO 80 J = 2,KMAT2
      Z(I  ) = BUF(J)
      Z(I+1) = 0
      Z(I+2) = 0
      IF (I .GT. N1MAT) GO TO 1398
   80 I = I + 3
      GO TO 70
   90 NMAT2 = I - LMAT2
  100 IMAT3 = I
      CALL LOCATE (*131,BFR,MAT3,FLAG)
      QMAT3 = 1
      IMHERE= 110
  110 CALL READ (*1350,*130,MPT,BUF,KMAT3,0,FLAG)
      Z(I) = BUF(1)
      I    = I + 1
      DO 120 J = 2,KMAT3
      Z(I  ) = BUF(J)
      Z(I+1) = 0
      Z(I+2) = 0
      IF (I .GT. N1MAT) GO TO 1398
  120 I = I + 3
      GO TO 110
  130 NMAT3  = I - LMAT3
  131 IMTPZ1 = I
      CALL LOCATE (*135,BFR,MATPZ1,FLAG)
      QMTPZ1 = 1
      IMHERE = 132
  132 CALL READ (*1350,*134,MPT,BUF,KMTPZ1,0,FLAG)
      Z(I) = BUF(1)
      I    = I + 1
      DO 133 J = 2,KMTPZ1
      Z(I  ) = BUF(J)
      Z(I+1) = 0
      Z(I+2) = 0
      IF (I .GT. N1MAT) GO TO 1398
  133 I = I + 3
      GO TO 132
  134 NMTPZ1 = I - LMTPZ1
  135 IMTPZ2 = I
      CALL LOCATE (*140,BFR,MATPZ2,FLAG)
      QMTPZ2 = 1
      IMHERE = 136
  136 CALL READ (*1350,*138,MPT,BUF,KMTPZ2,0,FLAG)
      Z(I) = BUF(1)
      I    = I + 1
      DO 137 J = 2,KMTPZ2
      Z(I  ) = BUF(J)
      Z(I+1) = 0
      Z(I+2) = 0
      IF (I .GT. N1MAT) GO TO 1398
  137 I = I + 3
      GO TO 136
  138 NMTPZ2 = I - LMTPZ2
  140 IMAT6  = I
      CALL LOCATE (*144,BFR,MAT6,FLAG)
      QMAT6  = 1
      IMHERE = 141
  141 FLAG   = 0
      CALL READ (*1350,*143,MPT,BUF,KMAT6,0,FLAG)
      Z(I) = BUF(1)
      I    = I + 1
      DO 142 J = 2,KMAT6
      Z(I  ) = BUF(J)
      Z(I+1) = 0
      Z(I+2) = 0
      IF (I .GT. N1MAT) GO TO 1398
  142 I = I + 3
      GO TO 141
  143 NMAT6 = I - LMAT6
      IF (FLAG .NE. 0) GO TO 1570
  144 IMAT8 = I
      CALL LOCATE (*1444,BFR,MAT8,FLAG)
      QMAT8 = 1
      IMHERE= 1441
 1441 FLAG  = 0
      CALL READ (*1350,*1443,MPT,BUF,KMAT8,0,FLAG)
      Z(I) = BUF(1)
      I    = I + 1
      DO 1442 J = 2,KMAT8
      Z(I  ) = BUF(J)
      Z(I+1) = 0
      Z(I+2) = 0
      IF (I .GT. N1MAT) GO TO 1398
 1442 I = I + 3
      GO TO 1441
 1443 NMAT8 = I - LMAT8
      IF (FLAG .NE. 0) GO TO 1570
 1444 IMATF = I
      CALL LOCATE (*149,BFR,MATF,FLAG)
      QMATF  = 1
      IMHERE = 145
  145 CALL READ (*1350,*148,MPT,BUF,KMATF,0,FLAG)
      Z(I  ) = BUF(1)
      Z(I+1) = BUF(2)
      Z(I+2) = BUF(3)
      I = I + 3
      GO TO 145
  148 NMATF = I - LMATF
  149 ILIST = I
      IF (I .GT. N1MAT) GO TO 1398
      CALL CLOSE (MPT,CLSREW)
C
C     IF TEMPERATURE OR PLA PROBLEM, READ THE MATTI OR MATSI CARDS.
C     MERGE MATSI AND MATTI DATA WITH MATI DATA.
C     SAVE A LIST OF TABLES REFERENCED.
C
      IF (     PLA .AND. TEMPID.NE.0) GO TO 1540
      IF (.NOT.PLA .AND. TEMPID.EQ.0) GO TO 350
      CALL PRELOC (*350,BFR,MPT)
      IF (TEMPID .NE. 0) GO TO 160
      NX = 3
      CALL LOCATE (*150,BFR,MATS1,FLAG)
      QMATX = 1
      ASSIGN  150 TO BACK
      ASSIGN 1420 TO RET1
      ASSIGN  820 TO PASS
      N = KMAT1
      GO TO 910
  150 CONTINUE
  160 NX = 2
      CALL LOCATE (*170,BFR,MATT1,FLAG)
      QMATX = 1
      ASSIGN  170 TO BACK
      ASSIGN 1450 TO RET1
      ASSIGN  820 TO PASS
      N = KMAT1
      GO TO 910
  170 CALL LOCATE (*180,BFR,MATT2,FLAG)
      QMATX = 1
      ASSIGN  180 TO BACK
      ASSIGN 1460 TO RET1
      ASSIGN  850 TO PASS
      N = KMAT2
      GO TO 910
  180 CALL LOCATE (*181,BFR,MATT3,FLAG)
      QMATX = 1
      ASSIGN  181 TO BACK
      ASSIGN 1520 TO RET1
      ASSIGN  880 TO PASS
      N = KMAT3
      GO TO 910
  181 CALL LOCATE (*182,BFR,MTTPZ1,FLAG)
      QMATX = 1
      ASSIGN  182 TO BACK
      ASSIGN 1551 TO RET1
      ASSIGN  901 TO PASS
      N = KMTPZ1
      GO TO 910
  182 CALL LOCATE (*183,BFR,MTTPZ2,FLAG)
      QMATX = 1
      ASSIGN  183 TO BACK
      ASSIGN 1552 TO RET1
      ASSIGN  904 TO PASS
      N = KMTPZ2
      GO TO 910
  183 CALL LOCATE (*190,BFR,MATT6,FLAG)
      QMATX = 1
      ASSIGN  190 TO BACK
      ASSIGN 1560 TO RET1
      ASSIGN  907 TO PASS
      N = 31
      GO TO 910
  190 ITABL = I
      IMHERE= 190
      IF (I .GT. N1MAT) GO TO 1398
      NLIST = ITABL - 11
      CALL CLOSE (MPT,CLSREW)
C
C     IF ANY MATTI OR MATSI CARDS WERE READ, FORM A SORTED LIST OF TABLE
C     NUMBERS REFERENCED ON THESE CARDS. THEN, DISCARD ANY DUPLICATES IN
C     THE LIST SO THAT THE LIST CONTAINS UNIQUE TABLE NOS. TO BE READ.
C
      IF (QMATX .EQ. 0) GO TO 350
      DO 220 II = ILIST,NLIST,11
      MIN = 999999999
      DO 210 JJ = II,NLIST,11
      IF (Z(JJ) .GE. MIN) GO TO 210
      MIN = Z(JJ)
      JX  = JJ
  210 CONTINUE
      Z(JX) = Z(II)
  220 Z(II) = MIN
      Z(ITABL) = 0
      JJ = ILIST
      DO 230 II = ILIST,NLIST,11
      IF (Z(II+11) .EQ. Z(II)) GO TO 230
      Z(JJ) = Z(II)
      JJ = JJ + 11
  230 CONTINUE
      ITABL = JJ
      NLIST = JJ - 11
C
C     READ THE DIT BY TABLE TYPE. FOR EACH TABLE IN THE DIT, LOOK UP IN
C     TABLE NO. LIST TO DETERMINE IF THE TABLE IS REQUIRED FOR PROBLEM
C     SOLUTION. IF NOT, SKIP THE TABLE. IF SO, READ THE TABLE INTO CORE
C     AND STORE POINTERS TO THE FIRST AND LAST ENTRIES IN THE TABLE AND
C     THE TYPE OF TABLE. THIS INFORMATION IS STORED IN THE TABLE NO.
C     LIST
C
      CALL PRELOC (*1370,BFR,DIT)
      I = ITABL
      J = 1
      ASSIGN 260 TO RET
      ASSIGN 280 TO RET1
  240 JJ = J + J - 1
      CALL LOCATE (*290,BFR,TABLEI(JJ),FLAG)
  250 CALL READ (*1380,*290,DIT,BUF,8,0,FLAG)
      NWDS = 2
      IF (J.EQ.4 .OR. J.EQ.8) NWDS = 1
      TABLID = BUF(1)
      GO TO 960
  260 Z(L+1) = J
      IF (J .GT. 4) Z(L+1) = J - 4
      Z(L+2) = I
      IMHERE = 270
  270 CALL READ (*1380,*1390,DIT,Z(I),NWDS,0,FLAG)
      IF (Z(I) .EQ. -1) GO TO 300
      I = I + NWDS
      IF (I .GT. N1MAT) GO TO 1398
      GO TO 270
  280 CALL READ (*1380,*1390,DIT,BUF,NWDS,0,FLAG)
      IF (BUF(1) .EQ. -1) GO TO 250
      GO TO 280
  290 J = J + 1
      IF (J .LE. NTYPES) GO TO 240
      CALL CLOSE (DIT,CLSREW)
      GO TO 330
  300 Z(L+3) = I - NWDS
C
C     STORE THE PARAMETERS ON THE TABLEI CARD IN LOCATIONS
C     Z(L+4),Z(L+5),...,Z(L+10)
C
      DO 310 K = 2,8
      LX = L + K
  310 Z(LX+2) = BUF(K)
C
C     IF THIS TABLE IS A POLYNOMIAL (TABLE4), EVALUATE THE END POINTS
C     AND STORE AT ZZ(L+8) AND ZZ(L+9)
C
      IF (J .NE. 4) GO TO 250
      XX = (ZZ(L+6) - ZZ(L+4))/ZZ(L+5)
      ASSIGN 1330 TO IGOTO
      GO TO 1280
  320 ASSIGN 1340 TO IGOTO
      XX = (ZZ(L+7) - ZZ(L+4))/ZZ(L+5)
      GO TO 1280
C
C     TEST TO FOR ALL REFERENCED TABLES IN CORE
C
  330 FLAG = 0
      DO 340 L = ILIST,NLIST,11
      IF (Z(L+1) .NE. 0) GO TO 340
      FLAG = 1
      BUF(1) = Z(L)
      BUF(2) = 0
      CALL MESAGE (30,41,BUF)
  340 CONTINUE
      IF (FLAG .NE. 0) CALL MESAGE (-37,0,NAM)
C
C     WRAP UP PREMAT
C
  350 N2MAT  = I + 1 - OFFSET
      MATIDO = 0
      SINTHO = 2.0
      COSTHO = 2.0
      INFLGO = 0
      PART1  =.FALSE.
      MAPCK  =+999
      RETURN
C
C     THE FOLLOWING POINTERS AND FLAGS ARE SET IN PREMAT FOR USE BY MAT-
C     QMAT1 = 0, NO MAT1 TABLE, = 1, MAT1 TABLE PRESENT
C     IMAT1 = POINTER TO FIRST ENTRY IN MAT1 TABLE
C     LMAT1 = LENGTH  OF EACH  ENTRY IN MAT1 TABLE
C     NAMT1 = POINTER TO LAST  ENTRY IN MAT1 TABLE
C     QMAT2,  IMAT2, LMAT2 AND NMAT2 ARE DEFINED AS ABOVE FOR MAT2 TABLE
C     QMATX = 0, NO TEMP OR STRESS TABLES PRESENT, = 1, OTHERWISW
C     ILIST = POINTER TO FIRST ENTRY IN TABLE LIST
C     NLIST = POINTER TO  LAST ENTRY IN TABLE LIST
C
C     THE TABLE LIST HAS 11 WORDS PER ENTRY AS FOLLOWS--
C      1. TABLE NUMBER (I.E. ID NO.)
C      2. TABLE TYPE (I.E. 1 = TABLE1, 2 = TABLE2 ETC.)
C      3. POINTER TO FIRST ENTRY IN TABLE
C      4. POINTER TO  LAST ENTRY IN TABLE
C      5. THRU 11. PARAMETERS ON TABLEI CARD
C     MATIDO = OLD MATERIAL ID (INITIALIZED TO  0 BY PREMAT)
C     SINTHO = OLD SIN THETA   (INITIALIZED TO 2. BY PREMAT)
C     INFLGO = OLD INFLAG      (INITIALIZED TO  0 BY PREMAT)
C
C
C
      ENTRY MAT (ELEMID)
C     ==================
C
C     IF MAPCK .NE. +999 PREMAT HAS BEEN CORRUPTED.  (OVERLAY ERROR)
C
      IF (MAPCK .EQ. +999) GO TO 355
      WRITE  (NOUT,353) MAPCK
  353 FORMAT (//,' *** PREMAT OVERLEY ERROR',I12)
      CALL ERRTRC ('PREMAT  ',353)
C
C
C     INFLAG DESCRIBES PROCESSING REQUESTED BY CALLER
C
  355 GO TO (360 ,400 ,480 ,560 ,620 ,640 ,680 ,2000,2200,2400,2600,
     1      2700), INFLAG
C
C     INFLAG = 1 MEANS CALLER WANTS ONLY MAT1 PROPERTIES IN MAT1 FORMAT
C     IF NO TEMPERATURE DEPENDENT PROPERTIES AND MATID = OLD MATID,
C     RETURN SINCE DATA IS ALREADY IN MATOUT
C
  360 IF (TEMPID.EQ.0 .AND. MATID.EQ.MATIDO .AND. INFLAG.EQ.INFLGO .AND.
     1   .NOT. PLA) RETURN
      MATIDO = MATID
      INFLGO = INFLAG
      TDEP   =.FALSE.
C
C     LOOK UP MATID IN MAT1 TABLE
C
      ASSIGN  380 TO RET
      ASSIGN 1480 TO RET1
      GO TO 820
C
C     PICK UP MATERIAL PROPERTIES FROM MAT1 ENTRY.
C
  380 I = K + 1
      J = 1
      ASSIGN 390 TO BACK
      GO TO 980
  390 Y(J) = PROP
      I = I + 3
      J = J + 1
      IF (J .LT. KMAT1) GO TO 980
      RETURN
C
C     INFLAG = 2 MEANS CALLER WANTS MAT2 FORMAT WHETHER PROPERTIES ARE
C     DEFINED IN MAT1 OR MAT2 TABLE.
C     IF NO TEMPERATURE DEPENDENT PROPERTIES AND MATID = OLD MATID AND
C     SIN THETA = OLD SIN THETA, RETURN
C
  400 IF (TEMPID.EQ.0 .AND.  MATID.EQ.MATIDO .AND. SINTH.EQ.SINTHO .AND.
     1    .NOT.PLA    .AND. INFLAG.EQ.INFLGO .AND. COSTH.EQ.COSTHO)
     2    RETURN
      INFLGO = INFLAG
      MATIDO = MATID
      SINTHO = SINTH
      COSTHO = COSTH
C
C     LOOK UP MATID IN MAT1 TABLE
C
  410 ASSIGN 420 TO RET1
      ASSIGN 430 TO RET
      GO TO 820
C
C     MATID NOT IN MAT1 TABLE, LOOK UP IN MAT2 TABLE
C     - IF NOT PRESENT, FATAL ERROR IF INFLAG = 2
C     - IF NOT PRESENT, SEARCH MAT8 TABLE IF INFLAG = 12
C
  420 ASSIGN 450 TO RET
      ASSIGN 425 TO RET1
      GO TO 850
  425 IF (INFLGO .EQ. 12) GO TO 2710
      GO TO 1480
C
C     MATID FOUND IN MAT1 TABLE.
C     COMPUTE G MATRIX FROM MAT1 PROPERTIES.
C     COMPLETE REMAINDER OF OUTPUT BUFFER IN MAT2 FORMAT.
C
  430 I = K + 1
      J = 1
      ASSIGN 440 TO BACK
      MMAT = 1
      GO TO 980
  440 X(J) = PROP
      I = I + 3
      J = J + 1
      IF (J .LT. KMAT1) GO TO 980
      NUXX  = 1.0 - NUX**2
      G11   = EX/NUXX
      G12   = NUX*G11
      G13   = 0.
      G22   = G11
      G23   = 0.
      G33   = GX
      RHOY  = RHOX
      ALPH1 = ALPHX
      ALPH2 = ALPHX
      ALPH12= 0.
      TOY   = TOX
      GEY   = GEX
      SIGTY = SIGTX
      SIGCY = SIGCX
      SIGSY = SIGSX
      IF (INFLGO .EQ. 12) GO TO 2701
      RETURN
C
C     MATID FOUND IN MAT2 TABLE.
C     PLACE PROPERTIES IN OUTPUT AREA IN MAT2 FORMAT
C     THEN TEST FOR TRANSFORMATION. IF IDENTITY, RETURN. 3THERWISE,
C     PERFORM  U(T)*G*U .
C
  450 I = K + 1
      J = 1
      MMAT = 2
      ASSIGN 460 TO BACK
      GO TO 980
  460 Y(J) = PROP
      I = I + 3
      J = J + 1
      IF (J    .LT. KMAT2) GO TO 980
      IF (INFLGO .EQ.  12) GO TO 2705
      IF (SINTH  .EQ. 0.0) GO TO 470
      IF (ABS(SINTH**2 + COSTH**2 - 1.0) .GT. .0001) GO TO 1485
C
C     TRANSFORM G , THE MATERIAL STIFFNESS PROPERTY MATRIX.
C                M                   T
C                            G   =  U * G * U
C                             E          M
C
      X( 1) = COSTH**2
      X( 2) = SINTH**2
      X( 3) = COSTH*SINTH
      X( 4) = X(2)
      X( 5) = X(1)
      X( 6) =-X(3)
      X( 7) = 2.0*X(6)
      X( 8) =-X(7)
      X( 9) = X(1) - X(2)
      X(10) = G11
      X(11) = G12
      X(12) = G13
      X(13) = G12
      X(14) = G22
      X(15) = G23
      X(16) = G13
      X(17) = G23
      X(18) = G33
      CALL GMMATS (X(10),3,3,0,X( 1),3,3,0,X(19))
      CALL GMMATS (X( 1),3,3,1,X(19),3,3,0,X(10))
      G11   = X(10)
      G12   = X(11)
      G13   = X(12)
      G22   = X(14)
      G23   = X(15)
      G33   = X(18)
C
C     COMPUTE THE TRANSFORMED TEMPERATURE EXPANSION VECTOR
C               (ALPHA) = (U)*(ALPHA)
C                                    M
C
      X(3)  = -X(3)
      X(6)  = -X(6)
      X(7)  = -X(7)
      X(8)  = -X(8)
      CALL GMMATS (X(1),3,3,0, Y(8),3,1,0, X(10))
      ALPH1 = X(10)
      ALPH2 = X(11)
      ALPH12= X(12)
  470 IF (INFLAG .EQ. 7) GO TO 813
      RETURN
C
C     INFLAG = 3 IMPLIES THE CALLER WANTS
C             (1) ONLY J11, J12 AND J22, AND
C             (2) THE FIRST 15 LOCATIONS OF /MATOUT/ TO BE UNDISTURBED.
C
  480 IF (MATID.EQ.MATIDO .AND. INFLAG.EQ.INFLGO .AND. .NOT.PLA) RETURN
      IF (MATID  .NE. MATIDO) GO TO 490
      IF (INFLGO .NE.      2) GO TO 490
      IF (MMAT-2) 530,540,540
C
C     SEARCH MAT1 TABLE FOR MATID
C
  490 ASSIGN 500 TO RET1
      ASSIGN 510 TO RET
      GO TO 820
C
C     MATID NOT IN MAT1 TABLE. LOOK IN MAT2 TABLE - ERROR IF NOT PRESENT
C
  500 ASSIGN 1480 TO RET1
      ASSIGN  540 TO RET
      GO TO 850
  510 I = K + 4
      ASSIGN 520 TO BACK
      GO TO 980
  520 J11 = PROP
      J12 = 0.0
      J22 = PROP
      GO TO 550
  530 J11 = GX
      J12 = 0.0
      J22 = GX
      GO TO 550
  540 J11 = 0.0
      J12 = 0.0
      J22 = 0.0
  550 INFLGO = INFLAG
      MATIDO = MATID
      RETURN
C
C     INFLAG = 4 MEANS CALLER DESIRES ONLY THE DENSITY PROPERTY (RHO)
C     LOOK UP MATID IN MAT1 TABLE.
C
  560 IF (TEMPID.EQ.0 .AND. MATID.EQ.MATIDO .AND. INFLAG.EQ.INFLGO .AND.
     1   .NOT.PLA) RETURN
      ASSIGN 580 TO RET
      ASSIGN 570 TO RET1
      GO TO 820
C
C     MATID NOT IN MAT1 TABLE, LOOK UP IN MAT2 TABLE - ERROR IF NOT
C     PRESENT
C
  570 ASSIGN  610 TO RET
      ASSIGN 1480 TO RET1
      GO TO 850
C
C     MATID FOUND IN MAT1 TABLE. PICK UP RHO
C
  580 I = K + 10
  590 ASSIGN 600 TO BACK
      GO TO 980
  600 Y(1)   = PROP
      MATIDO = MATID
      INFLGO = INFLAG
      RETURN
C
C     MATID FOUND IN MAT2 TABLE. PICK UP RHO.
C
  610 I = K + 19
      GO TO 590
C
C     INFLAG = 5, USED ONLY IN MODULE PLA1, DETERMINES IF THE MAT CARD
C     REFERENCED IS A MAT1 WITH E, YOUNGS MODULUS, DEFINED AS STRESS
C     DEPENDENT.  IF IT IS STRESS DEPENDENT, INDSTR, THE FIRST WORD OF
C     THE /MATOUT/ BLOCK IS SET = 1.  IF NOT STRESS DEPENDENT, INDSTR
C     IS SET = 0 ONLY MAT1 CARDS ARE ADMISSIBLE FOR THIS TEST.
C
  620 IF (PLA .AND. MATID.EQ.MATIDO .AND. INFLAG.EQ.INFLGO) RETURN
      MATIDO = MATID
      INFLGO = INFLAG
      ASSIGN 630 TO RET
      ASSIGN 635 TO RET1
      INDSTR = 0
      GO TO 820
C
C     TEST TO SEE IF THE MATERIAL PROPERTY E IS DEPENDENT ON A TABLE OF
C     STRAIN VS. STRESS (EPSILON VS. SIGMA)
C
  630 TABLID = Z(K+3)
      IF (TABLID .NE. 0) INDSTR = 1
  635 RETURN
C
C     INFLAG = 6, USED ONLY IN SUBROUTINES PLA3 AND PLA4, ACCEPTS
C     EPSILON - STRAIN - IN THE /MATIN/ BLOCK (PLAARG) AND LOOKS-UP
C     SIGMA   - STRESS - AND STORES THIS VALUE IN PLAANS IN /MATOUT/.
C     ONLY MAT1 AND MATS1 CARDS ARE ADMISSIBLE FOR THIS INFLAG.
C
  640 ASSIGN  650 TO RET
      ASSIGN 1500 TO RET1
      MATIDO = MATID
      INFLGO = INFLAG
      GO TO 820
  650 TABLID = Z(K+3)
      IF (TABLID .LE. 0) GO TO 1510
      XX = PLAARG
      ASSIGN  660 TO RET
      ASSIGN 1490 TO RET1
      GO TO 960
  660 ITYPE = Z(L+1)
      IF (ITYPE .NE. 1) GO TO 1530
      ASSIGN 670 TO IRET
      GO TO 1080
  670 PLAANS = PROP
      RETURN
C
C     INFLAG = 7, USED CURRENTLY ONLY BY BELL AEROSYSTEMS ELEMENTS,
C     IMPLIES THE USER WANTS HIS DATA IN MAT3 FORMAT.  IF THE MATID IS
C     FOUND IN THE MAT1 SET, THE DATA IS STORED IN MAT3 FORMAT.  IF NOT
C     FOUND IN THE MAT1 SET, THE MAT3 SET IS SEARCHED. IF NOT FOUND IN
C     THE MAT3 SET THE MAT2 SET IS SEARCHED. IF NOT FOUND HERE, A FATAL
C     ERROR EXISTS.
C
  680 IF (TEMPID.EQ.0 .AND. INFLAG.EQ.INFLGO .AND. MATID.EQ.MATIDO)
     1    RETURN
      INFLGO = INFLAG
      MATIDO = MATID
      ASSIGN 690 TO RET
  685 CONTINUE
      ASSIGN 790 TO RET1
      GO TO 820
  690 I = K + 1
      J = 1
      ASSIGN 700 TO BACK
      GO TO 980
  700 GO TO (710,720,730,740,750,760,770,771,772,773), J
  710 EX3   = PROP
      EY3   = PROP
      EZ3   = PROP
      GO TO 780
  720 GXY3  = PROP
      GYZ3  = PROP
      GZX3  = PROP
      GO TO 780
  730 NUXY3 = PROP
      NUYZ3 = PROP
      NUZX3 = PROP
      GO TO 780
  740 RHO3  = PROP
      GO TO 780
  750 AX3   = PROP
      AY3   = PROP
      AZ3   = PROP
      GO TO 780
  760 TREF3 = PROP
      GO TO 780
  770 GE3   = PROP
      GO TO 780
  771 SIGTY3 = PROP
      GO TO 780
  772 SIGCY3 = PROP
      GO TO 780
  773 SIGSY3 = PROP
      MATSET = 1.0
      RETURN
C
  780 J = J + 1
      I = I + 3
      GO TO 980
C
C     SEARCH FOR MATID IN THE MAT3 SET
C
  790 ASSIGN 800 TO RET
      ASSIGN 811 TO RET1
      GO TO 880
C
C     PICK UP MATERIAL PROPERTIES FROM MAT3 ENTRY
C
  800 I = K + 1
      J = 1
      ASSIGN 810 TO BACK
      GO TO 980
  810 Y(J) = PROP
      I = I + 3
      J = J + 1
      IF (J .LT. KMAT3) GO TO 980
      MATSET = 3.0
      RETURN
C
C     SEARCH FOR MATID IN THE MAT2 SET
C
  811 ASSIGN  812 TO RET
      ASSIGN 1480 TO RET1
      GO TO 850
C
C     GO TO INFLAG = 2 CODE TO PICK UP MAT2 PROPERTIES
C
  812 GO TO 450
  813 SIGTY3 = SIGTY
      SIGCY3 = SIGCY
      SIGSY3 = SIGSY
      TREF3  = TOY
      GE3    = GEY
      AX3    = ALPH1
      AY3    = ALPH2
      AZ3    = ALPH12
      G113   = G11
      G123   = G12
      G133   = G13
      G223   = G22
      G233   = G23
      G333   = G33
      MATSET = 2.0
      RETURN
C
C     INFLAG = 8 IS USED ONLY BY TWO-DIMENSIONAL ELEMENTS IN PIECEWISE
C     LINEAR ANALYSIS.  HERE WE PERFORM AN INVERSE INTERPOLATION TO
C     OBTAIN STRAIN (EPS) GIVEN STRESS (TAU)
C
 2000 ASSIGN 2010 TO RET
      ASSIGN 1500 TO RET1
      MATIDO = MATID
      INFLGO = INFLAG
      YY     = PLAARG
      GO TO 820
 2010 TABLID = Z(K+3)
      IF (TABLID .LE. 0) GO TO 1510
      ASSIGN 2020 TO RET
      ASSIGN 1490 TO RET1
      GO TO 960
 2020 ITYPE = Z(L+1)
      IF (ITYPE .NE. 1) GO TO 1530
C
C     ROUTINE TO PERFORM INVERSE LINEAR INTERPOLATION OR EXTRAPOLATION.
C
      ITABL = Z(L+2)
      NTABL = Z(L+3)
      UP    = 1.0
      IF (ZZ(ITABL) .GT. ZZ(ITABL+2)) UP = -1.0
      KXX1 = ITABL
      IF ((YY - ZZ(ITABL+1))*UP .LT. 0.0) GO TO 2180
      KXX1 = NTABL - 2
      IF ((YY - ZZ(NTABL+1))*UP .LE. 0.0) GO TO 2030
      IF (ZZ(NTABL+1) .EQ. ZZ(NTABL-1)) GO TO 2180
 2030 KLO = 1
      KHI = (NTABL - ITABL)/2 + 1
 2090 KX  = (KLO + KHI + 1)/2
      KXX = (KX - 1)*2 + ITABL
      IF ((YY - ZZ(KXX+1))*UP) 2100,2150,2110
 2100 KHI = KX
      GO TO 2120
 2110 KLO = KX
 2120 IF (KHI-KLO .NE. 1) GO TO 2090
      KXX1 = 2*(KLO-1) + ITABL
      IF (KXX .EQ.       KXX1) GO TO 2130
      IF (YY  .EQ. ZZ(KXX1+3)) GO TO 2140
 2130 PLAANS = (YY - ZZ(KXX1+1))*(ZZ(KXX1+2) - ZZ(KXX1))/(ZZ(KXX1+3)
     1       - ZZ(KXX1+1)) + ZZ(KXX1)
 2135 ICELL2 = 0
      RETURN
C
 2140 KXX = KXX1 + 2
 2150 IF (YY .EQ. ZZ(KXX-1)) GO TO 2160
      IF (YY .EQ. ZZ(KXX+3)) GO TO 2170
      PLAANS = ZZ(KXX)
      GO TO 2135
 2160 PLAANS = (ZZ(KXX) + ZZ(KXX-2))/2.0
      GO TO 2135
 2170 PLAANS = (ZZ(KXX) + ZZ(KXX+2))/2.0
      GO TO 2135
C
C     YY IS OUT OF THE RANGE OF THE FUNCTION, SET THE SECOND CELL OF
C     /MATOUT/ EQUAL TO ONE.
C
 2180 PLAANS = 0.0
      ICELL2 = 1
      RETURN
C
C     INFLAG = 9 IS USED ONLY BY TRAPAX AND TRIAAX WHEN PIEZOELECTRIC
C     MATERIALS ARE SELECTED.  WANT MATERIALS RETURNED INTO MATPZ2
C     FORMAT.
C
C     MATPZ1 CODE TRANSFORMS 1,2,3 MATERIAL DIRECTIONS INTO Z, THETA,
C     R = 0 DIRECTIONS, RESPECTIVLELY, AND INTERCHANGES 4TH AND 6TH ROWS
C     AND COLUMNS TO ACCOUNT FOR DIFFERENT SHEAR ORDERING.
C     ELEMENT ROUTINE WILL TRANSFORM FOR R-POLARIZATION
C     MATPZ2 CODE ASSUMES USER HAS PERFO-MED ALL TRANSFORMATIONS AS
C     EXPLAINED FOR MATPZ1
C
 2200 IF (TEMPID.EQ.0 .AND. INFLAG.EQ.INFLGO .AND. MATID.EQ.MATIDO)
     1    RETURN
      INFLGO = INFLAG
      MATIDO = MATID
C
C     LOOK UP MATID IN MATPZ1 TABLE
C
      ASSIGN 2210 TO RET1
      ASSIGN 2220 TO RET
      GO TO 901
C
C     NOT IN MATPZ1, LOOK AT MATPZ2
C
 2210 ASSIGN 2300 TO RET
      ASSIGN 685 TO RET1
      GO TO 904
C
C     FOUND IN MATPZ1 - PUT OUT LIKE MATPZ2
C
 2220 I = K + 1
      J = 1
      ASSIGN 2230 TO BACK
      GO TO 980
 2230 BUFPZ(J) = PROP
      I = I + 3
      J = J + 1
      IF (J .LT. KMTPZ1)  GO TO 980
      EPSO = 8.854E-12
      DO 2240 IJK = 1,8
 2240 BUFPZ(IJK) = BUFPZ(IJK)*1.E-12
      SE1   = (BUFPZ(4)-BUFPZ(1))*2.*BUFPZ(5)**2 - BUFPZ(2)*
     1        (BUFPZ(4)**2-BUFPZ(1)**2)
      SE2   = 2.*BUFPZ(5)**2 - BUFPZ(2)*(BUFPZ(4) + BUFPZ(1))
      IF (SE1.EQ.0. .OR. SE2.EQ.0.)GO TO 1556
      CE11  =-(BUFPZ(5)**2-BUFPZ(1)*BUFPZ(2))/SE1
      CE12  = (BUFPZ(5)**2-BUFPZ(2)*BUFPZ(4))/SE1
      CE13  =  BUFPZ(5)/SE2
      CE33  =-(BUFPZ(4)+BUFPZ(1))/SE2
      CE44  = 1./BUFPZ(3)
      CE66  = 0.5/(BUFPZ(1) - BUFPZ(4))
      E15   = BUFPZ(8)*CE44
      E31   = BUFPZ(6)*(CE11+CE12) + BUFPZ(7)*CE13
      E33   = BUFPZ(6)*CE13*2. + BUFPZ(7)*CE33
      EPS11 = BUFPZ(9)*EPSO
      EPS33 = BUFPZ(10)*EPSO
      DO 2250 IJK = 4,44
 2250 PZOUT(IJK)= 0.
      PZOUT( 1) = CE33
      PZOUT( 2) = CE13
      PZOUT( 3) = CE13
      PZOUT( 7) = CE11
      PZOUT( 8) = CE12
      PZOUT(12) = CE11
      PZOUT(16) = CE44
      PZOUT(19) = CE44
      PZOUT(21) = CE66
      PZOUT(22) = E33
      PZOUT(23) = E31
      PZOUT(24) = E31
      PZOUT(31) = E15
      PZOUT(38) = E15
      PZOUT(40) = EPS33
      PZOUT(43) = EPS11
      PZOUT(45) = EPS11
      PZOUT(46) = BUFPZ(11)
      PZOUT(47) = BUFPZ(12)
      PZOUT(48) = BUFPZ(12)
      PZOUT(49) = BUFPZ(12)
      PZOUT(50) = BUFPZ(13)
      PZOUT(51) = BUFPZ(14)
      MATSET = 4.0
      RETURN
C
C     FOUND IN MATPZ2 FORMAT
C
 2300 I = K + 1
      J = 1
      ASSIGN 2310 TO BACK
      GO TO 980
 2310 PZOUT(J) = PROP
      I = I + 3
      J = J + 1
      IF (J .LT. KMTPZ2) GO TO 980
      MATSET = 5.0
      RETURN
C
C     INFLAG = 10, USED CURRENTLY ONLY BY ISOPARAMETRIC SOLIDS IHEX1,2,3
C     IMPLIES CALLER WANTS HIS DATA IN MAT6 FORMAT STORED IN MATISO.
C     MATERIALS COULD BE ON MAT1 OR ON MAT6. IN EITHER CASE,MATERIALS
C     WILL BE COMPUTED FOR MAT6 OUTPUT. IF NOT FOUND ON MAT1 OR MAT6,
C     FATAL.
C
 2400 IF (TEMPID.EQ.0 .AND. MATID.EQ.MATIDO .AND. INFLAG.EQ.INFLGO)
     1    RETURN
      INFLGO = INFLAG
      MATIDO = MATID
      TDEP   =.FALSE.
C
C     LOOK UP MATID IN MAT1 TABLE
C
      ASSIGN 2420 TO RET1
      ASSIGN 2430 TO RET
      GO TO 820
C
C     MATID NOT IN MAT1. CHECK MAT6
C
 2420 ASSIGN 2470 TO RET
      ASSIGN 1480 TO RET1
      GO TO 907
C
C     MATID FOUND IN MAT1 TABLE. COMPUTE G MATRIX,ETC.
C
 2430 IB(46) = 1
      I = K + 1
      J = 1
      ASSIGN 2440 TO BACK
      GO TO 980
 2440 X(J) = PROP
      I = I + 3
      J = J + 1
      IF (J .LT. KMAT1) GO TO 980
      DD = (1.+NUX)*(1.-2.*NUX)
      IF (DD .NE. 0.) GO TO 2450
      IB(46) = 0
      RETURN
C
 2450 DD   =  EX*(1.-NUX)/DD
      DDN1 = NUX/(1.-NUX)
      DDN2 = 0.5*(1.-2.*NUX)/(1.-NUX)
      DO 2460 IJKL = 1,45
 2460 BUFM6(IJKL) = 0.
      BUFM6( 1) = DD
      BUFM6( 2) = DD*DDN1
      BUFM6( 3) = BUFM6(2)
      BUFM6( 7) = BUFM6(2)
      BUFM6( 8) = BUFM6(1)
      BUFM6( 9) = BUFM6(2)
      BUFM6(13) = BUFM6(2)
      BUFM6(14) = BUFM6(2)
      BUFM6(15) = BUFM6(1)
      BUFM6(22) = DD*DDN2
      BUFM6(29) = BUFM6(22)
      BUFM6(36) = BUFM6(22)
      BUFM6(37) = RHOX
      BUFM6(38) = ALPHX
      BUFM6(39) = ALPHX
      BUFM6(40) = ALPHX
      BUFM6(44) = TOX
      BUFM6(45) = GEX
      RETURN
C
C     MATID FOUND IN MAT6 TABLE. PUT PROPERTIES IN MAT6 FORMAT AND
C     TRANSFORM USING DIRECTION COSINES
C
 2470 IB(46) = 6
      I = K + 1
      J = 1
      ASSIGN 2480 TO BACK
      GO TO 980
 2480 BUFTM6(J) = PROP
      I = I + 3
      J = J + 1
      IF (J .LT. KMAT6) GO TO 980
C
C     PUT SYMMETRIC PORTION OF G INTO A FULL 6 X 6 AND CREATE A 6 X 6
C     DIRECTION COSINE MATRIX BY COOK PP. 212-213. THEN TRANSFORM
C     (U-TRANSPOSE)*G*U
C
      KKK = 0
      LLL = 0
      DO 2500 III = 1,6
      DO 2490 JJJ = III,6
      KKK = KKK + 1
      LLL = LLL + 1
      XY(LLL) = BUFTM6(KKK)
      IF (JJJ .EQ. III) GO TO 2490
      L5   = 5*(JJJ-III)
      ISUB = LLL + L5
      XY(ISUB) = XY(LLL)
 2490 CONTINUE
      LLL = LLL + III
 2500 CONTINUE
      XL1 = BUFTM6(31)
      XM1 = BUFTM6(32)
      XN1 = BUFTM6(33)
      XL2 = BUFTM6(34)
      XM2 = BUFTM6(35)
      XN2 = BUFTM6(36)
      XL3 = BUFTM6(37)
      XM3 = BUFTM6(38)
      XN3 = BUFTM6(39)
      XY(37) = XL1**2
      XY(38) = XM1**2
      XY(39) = XN1**2
      XY(40) = XL1*XM1
      XY(41) = XM1*XN1
      XY(42) = XN1*XL1
      XY(43) = XL2**2
      XY(44) = XM2**2
      XY(45) = XN2**2
      XY(46) = XL2*XM2
      XY(47) = XM2*XN2
      XY(48) = XN2*XL2
      XY(49) = XL3**2
      XY(50) = XM3**2
      XY(51) = XN3**2
      XY(52) = XL3*XM3
      XY(53) = XM3*XN3
      XY(54) = XN3*XL3
      XY(55) = XL1*XL2*2.
      XY(56) = XM1*XM2*2.
      XY(57) = XN1*XN2*2.
      XY(58) = XL1*XM2 + XL2*XM1
      XY(59) = XM1*XN2 + XM2*XN1
      XY(60) = XN1*XL2 + XN2*XL1
      XY(61) = XL2*XL3*2.
      XY(62) = XM2*XM3*2.
      XY(63) = XN2*XN3*2.
      XY(64) = XL2*XM3 + XL3*XM2
      XY(65) = XM2*XN3 + XM3*XN2
      XY(66) = XN2*XL3 + XN3*XL2
      XY(67) = XL3*XL1*2.
      XY(68) = XM3*XM1*2.
      XY(69) = XN3*XN1*2.
      XY(70) = XL3*XM1 + XL1*XM3
      XY(71) = XM3*XN1 + XM1*XN3
      XY(72) = XN3*XL1 + XN1*XL3
C
      CALL GMMATS (XY(1),6,6,0,XY(37),6,6,0,XY(73))
      CALL GMMATS (XY(37),6,6,1,XY(73),6,6,0,BUFM6(1))
C
C     MUST ALSO TRANSFORM THERMAL EXPANSION VECOT= (U-INVERSE)*ALPHA
C     BY COOK P.212, THE INVERSE OF U IS THE TRANSPOSE OF THE
C     MATRIX WHICH TRANSFORMS STRESSES
C
      KKK = 72
      DO 2540 III = 1,6
      DO 2530 JJJ = 1,36,6
      KKK = KKK + 1
      LLL = JJJ + III + 35
      XY(KKK) = XY(LLL)
 2530 CONTINUE
 2540 CONTINUE
      DO 2545 III = 75,87,6
      DO 2545 JJJ = 1,3
      KKK = III + JJJ
      XY(KKK) = XY(KKK)*0.5
 2545 CONTINUE
      DO 2550 III = 90,102,6
      DO 2550 JJJ = 1,3
      KKK = III + JJJ
      XY(KKK) = XY(KKK)*2.0
 2550 CONTINUE
C
      CALL GMMATS (XY(73),6,6,0,BUFTM6(23),6,1,0,BUFM6(38))
C
      BUFM6(37) = BUFTM6(22)
      BUFM6(44) = BUFTM6(29)
      BUFM6(45) = BUFTM6(30)
      RETURN
C
C     INFLAG = 11 IS USED ONLY BY A HYDROELASTIC ANALYSIS TO FIND THE
C     DENSITY FOR THREE DIMENSIONAL FLUID ELEMENTS FROM MATF CARDS.
C
 2600 IF (QMATF .EQ. 0) GO TO 1480
      DO 2610 K = IMATF,NMATF,LMATF
      IF (Z(K) .EQ. MATID) GO TO 2620
 2610 CONTINUE
      GO TO 1480
 2620 RHO = ZZ(K+1)
      RETURN
C
C     INFLAG = 12 IS USED ONLY BY SHELL ELEMENTS QUAD4 AND TRIA3.
C     MAT1 IS FIRST SEARCHED, IF NOT FOUND, MAT2 IS SEARCHED. IF FOUND
C     IN EITHER CASE, /MATOUT/ WILL BE FILLED WITH MAT2 FORMAT DATA.
C     IF NOT FOUND IN MAT1 OR MAT2, MAT8 IS SEARCHED AND MAT8 FORMAT IS
C     USED IN /MATOUT/. FATAL ERROR IF MAT8 IS NOT FOUND.
C
 2700 IF (TEMPID.EQ.0 .AND. MATID.EQ.MATIDO .AND. INFLAG.EQ.INFLGO .AND.
     1    SINTH.EQ.SINTHO .AND. COSTH.EQ.COSTHO .AND. .NOT.PLA)  RETURN
      INFLGO = INFLAG
      MATIDO = MATID
      SINTHO = SINTH
      COSTHO = COSTH
C
C     GO TO INFLAG = 2 CODE TO PICK UP MAT1 OR MAT2 PROPERTIES
C     SET MATSET TO 1.0 IF PROPERTY DATA COMES FROM MAT1, OR
C     TO 2.0 IF FROM MAT2
C
      GO TO 410
C
 2701 MATSET = 1.0
      Y(16)  = EX
      Y(17)  = EX
      RETURN
 2705 MATSET = 2.0
      RETURN
C
C     NOT FOUND IN MAT1 AND MAT2.  LOOK FOR MAT8, ERROR IF NOT FOUND
C
 2710 IF (QMAT8 .EQ. 0) GO TO 1480
      DO 2720 K = IMAT8,NMAT8,LMAT8
      IF (Z(K) .EQ. MATID) GO TO 2730
 2720 CONTINUE
      GO TO 1480
 2730 I = K + 1
      J = 1
      ASSIGN 2740 TO BACK
      GO TO 980
C
C     OUTPUT IN MAT8 FORMAT AND SET MATSET TO 8.0
C
 2740 X(J) = PROP
      I = I + 3
      J = J + 1
      IF (J .LT. KMAT8) GO TO 980
      DO 2760 K = 1,17
      Y(K) = X(K)
 2760 CONTINUE
      Y(2) = X(3)
      Y(3) = X(2)
      Y(5) = X(6)
      Y(6) = X(5)
      MATSET = 8.0
      RETURN
C
C
C     INTERNAL ROUTINE TO SEARCH FOR MATERIAL IN MAT1 TABLE
C
  820 IF (QMAT1 .EQ. 0) GO TO 840
      DO 830 K = IMAT1,NMAT1,LMAT1
      IF (Z(K) .EQ. MATID) GO TO RET, ( 380,430,510,580,630,650,690,930,
     1                                 2010,2430)
  830 CONTINUE
  840 GO TO RET1, (420,500,570,635,790,1420,1450,1480,1500,2420)
C
C     INTERNAL ROUTINE TO SEARCH FOR MATERIAL IN MAT2 TABLE
C
  850 IF (QMAT2 .EQ. 0) GO TO 870
      DO 860 K = IMAT2,NMAT2,LMAT2
      IF (Z(K) .EQ. MATID) GO TO RET, (930,450,610,540,812)
  860 CONTINUE
  870 GO TO RET1, (425,1460,1480)
C
C     INTERNAL ROUTINE TO SEARCH FOR MATERIAL IN MAT3 TABLE.
C
  880 IF (QMAT3 .EQ. 0) GO TO 900
      DO 890 K = IMAT3,NMAT3,LMAT3
      IF (Z(K) .EQ. MATID) GO TO RET, (800,930)
  890 CONTINUE
  900 GO TO RET1, (811)
C
C     PIEZOELECTRIC MATERIALS
C
  901 IF (QMTPZ1 .EQ. 0)  GO TO 903
      DO 902 K = IMTPZ1,NMTPZ1,LMTPZ1
      IF (Z(K) .EQ. MATID) GO TO RET, (2220,930)
  902 CONTINUE
  903 GO TO RET1, (1551,2210)
  904 IF (QMTPZ2 .EQ. 0)  GO TO 906
      DO 905 K = IMTPZ2,NMTPZ2,LMTPZ2
      IF (Z(K) .EQ. MATID) GO TO RET, (2300,930)
  905 CONTINUE
  906 GO TO RET1, (1552,1480,685)
C
C     SEARCH FOR MATERIAL IN MAT6 TABLE(ISOPARAMETRIC SOLIDS)
C
  907 IF (QMAT6 .EQ. 0) GO TO 909
      DO 908 K = IMAT6,NMAT6,LMAT6
      IF (Z(K) .EQ. MATID) GO TO RET, (2470,930)
  908 CONTINUE
  909 GO TO RET1, (1560,1480)
C
C     INTERNAL ROUTINE TO READ MATXI CARDS, MERGE DATA IN MATI TABLE
C     AND STORE TABLE IDS IN CORE.
C
  910 ASSIGN 930 TO RET
  920 CALL READ (*1350,*950,MPT,BUF,N,0,FLAG)
      MATID = BUF(1)
      GO TO PASS, (820,850,880,901,904,907)
  930 DO 940 J = 2,N
      IF (BUF(J) .EQ. 0) GO TO 940
      JX    = K + 3*(J-2) + NX
      Z(JX) = BUF(J)
      Z(I ) = BUF(J)
      Z(I+1)= 0
      I     = I + 11
  940 CONTINUE
      GO TO 920
  950 GO TO BACK, (150,170,180,181,182,183,190)
C
C     INTERNAL ROUTINE TO SEARCH FOR A TABLE IN THE TABLE LIST
C
  960 DO 970 L = ILIST,NLIST,11
      IF (Z(L) .EQ. TABLID) GO TO RET, (260,660,990,1030,2020)
  970 CONTINUE
      GO TO RET1, (280,1490,1520)
C
C     ROUTINE TO TEST FOR DEPENDENCE OF A MATERIAL PROPERTY ON
C     TEMPERATURE OR STRESS. IF DEPENDENT, APPROPRIATE TABLE LOOK UP
C     PROCEDURE IS EMPLOYED. IN EITHER CASE, THE PROPERTY IS RETURNED
C     IN PROP.
C
  980 IF (QMATX .EQ. 0) GO TO 1060
      FLAG   = 0
      TABLID = Z(I+1)
      IF (ELEMID .LT. 0) GO TO 1020
      IF (TABLID .EQ. 0) GO TO 1060
      XX   = TEMP
      TDEP =.TRUE.
      FLAG = 1
      ASSIGN  990 TO RET
      ASSIGN 1490 TO RET1
      GO TO 960
  990 ASSIGN 1010 TO RET
 1000 ITYPE = Z(L+1)
      GO TO (1180,1200,1230,1240), ITYPE
 1010 PROPT = PROP
      GO TO 1070
C
C     SINCE THIS IS NOT A PIECEWISE LINEAR ANALYSIS PROBLEM, NO STRESS
C     DEPENDENT MATERIAL PROPERTIES ARE ALLOWED.  IF AND WHEN THIS
C     RESTRICTION IS LIFTED THE FOLLOWING CODE CAN BE IMPLEMENTED.
C     CURRENTLY A TRANSFER IS ALWAYS MADE TO STATEMENT 1060, SINCE THE
C     ELEMENT ID. IS ALWAYS POSITIVE.
C
 1020 IF (PLA) GO TO 1550
      IF (ELEMID .GT. 0) GO TO 1060
      TABLID = Z(I+2)
      IF (TABLID .EQ. 0) GO TO 1050
      ASSIGN 1030 TO RET
      ASSIGN 1490 TO RET1
      GO TO 960
 1030 ASSIGN 1040 TO RET
      XX = PLAARG
      GO TO 1000
 1040 IF (FLAG .NE. 0) PROP = PROP*PROPT
      GO TO 1070
 1050 IF (FLAG .NE. 0) GO TO 1070
 1060 PROP = ZZ(I)
 1070 GO TO BACK, ( 390,440,460,520,600,700,810,2230,2310,2440,2480,
     1             2740)
C
C     ROUTINE TO PERFORM LINEAR INTERPOLATION FOR FUNCTION IN TABLE.
C     L POINTS TO THE ENTRY IN THE TABLE LIST WHICH DEFINES THE TABLE.
C     ARGUMENT IS XX. FUNCTION VALUE IS RETURNED IN PROP. EXTRAPOLATION
C     IS MADE IF XX IS OUTSIDE THE LIMITS OF THE TABLE.
C
 1080 ITABL = Z(L+2)
      NTABL = Z(L+3)
      UP    = 1.0
      IF (ZZ(ITABL) .GT. ZZ(ITABL+2)) UP = -1.0
      KXX1 = ITABL
      IF ((XX - ZZ(ITABL))*UP .LE. 0.) GO TO 1130
      KXX1 = NTABL - 2
      IF ((XX - ZZ(NTABL))*UP .GE. 0.) GO TO 1130
      KLO = 1
      KHI = (NTABL-ITABL)/2 + 1
 1090 KX  = (KLO+KHI+1)/2
      KXX = (KX-1)*2 + ITABL
      IF ((XX - ZZ(KXX))*UP) 1100,1150,1110
 1100 KHI = KX
      GO TO 1120
 1110 KLO = KX
 1120 IF (KHI-KLO .NE. 1) GO TO 1090
      KXX1 = (KLO-1)*2 + ITABL
      IF (KXX .EQ. KXX1) GO TO 1130
      IF (XX  .EQ. ZZ(KXX1+2)) GO TO 1140
 1130 PROP = (XX - ZZ(KXX1))*(ZZ(KXX1+3) - ZZ(KXX1+1))/(ZZ(KXX1+2)
     1     - ZZ(KXX1)) + ZZ(KXX1+1)
      GO TO IRET, (670,1190,1220)
 1140 KXX = KXX1 + 2
 1150 IF (XX .EQ. ZZ(KXX-2)) GO TO 1160
      IF (XX .EQ. ZZ(KXX+2)) GO TO 1170
      PROP = ZZ(KXX+1)
      GO TO IRET, (670,1190,1220)
 1160 PROP = (ZZ(KXX-1) + ZZ(KXX+1))/2.0
      GO TO IRET, (670,1190,1220)
 1170 PROP = (ZZ(KXX+1) + ZZ(KXX+3))/2.0
C
C     TABLE TYPE = 1
C     ARGUMENT = XX
C
 1180 ASSIGN 1190 TO IRET
      GO TO 1080
 1190 GO TO RET, (1010,1040)
C
C     TABLE TYPE = 2
C     ARGUMENT = (XX-X1)
C
 1200 XX = XX - ZZ(L+4)
 1210 ASSIGN 1220 TO IRET
      GO TO 1080
 1220 PROP = ZZ(I)*PROP
      GO TO RET, (1010,1040)
C
C     TABLE TYPE = 3
C     ARGUMENT = (XX-X1)/X2
C
 1230 XX = (XX - ZZ(L+4))/ZZ(L+5)
      GO TO 1210
C
C     TABLE TYPE = 4
C     PERFORM POLYNOMIAL INTERPOLATION
C
C
C     NOTE...
C         ZZ(L+4) = X1
C         ZZ(L+5) = X2
C         ZZ(L+6) = X3
C         ZZ(L+7) = X4
C         ZZ(L+8) = F((X3-X1)/X2)
C         ZZ(L+9) = F((X4-X1)/X2)
C         WHERE X1 AND X2 ARE TRANSLATION AND SCALE FACTORS RESPECTIVELY
C         AND X3 AND X4 (X3 .LT. X4) ARE THE END POINTS OF THE
C         INTERVAL OVER WHICH THE POLYNOMIAL IS DEFINED.
C
 1240 FACTOR = ZZ(I)
C
C     DETERMINE THE ARGUMENT XX
C
      XX = (XX - ZZ(L+4))/ZZ(L+5)
      IF   (XX - (ZZ(L+6) - ZZ(L+4))/ZZ(L+5)) 1250,1250,1260
 1250 PROP = ZZ(L+8)
      GO TO 1310
 1260 IF   (XX - (ZZ(L+7) - ZZ(L+4))/ZZ(L+5)) 1280,1270,1270
 1270 PROP = ZZ(L+9)
      GO TO 1310
 1280 NN   = Z(L+3)
      PROP = ZZ(NN)
 1290 IF (NN .LE. Z(L+2)) GO TO 1300
      PROP = PROP*XX + ZZ(NN-1)
      NN   = NN - 1
      GO TO 1290
 1300 IF (PART1) GO TO IGOTO, (1330,1340)
 1310 PROP = PROP*FACTOR
      GO TO RET, (1010,1040)
 1330 ZZ(L+8) = PROP
      GO TO 320
 1340 ZZ(L+9) = PROP
      GO TO 250
C
C     FATAL ERROR MESSAGES
C
 1350 N = -2
      DIT = MPT
      GO TO 1400
 1370 N = -1
      GO TO 1400
 1380 N = -2
      GO TO 1400
 1390 N = -3
      GO TO 1400
 1385 WRITE  (NOUT,1386) IMHERE,I,N1MAT,OFFSET,NIMAT
 1386 FORMAT ('0*** NIMAT SPACE TOO SMALL.  ERROR AT',I5,'/PREMAT', /5X,
     1        'I,N1MAT,OFFSET,NIMAT =',3I12,I7,/)
      GO TO 1472
 1398 IF (NIMAT .LE. 2*SYSBUF+4) GO TO 1385
      N = -8
      DIT = I - N1MAT
 1400 CALL MESAGE (N,DIT,NAM)
      N = 16
      BUF(1) = 0
      BUF(2) = 0
      GO TO 1470
 1420 N =  17
 1430 BUF(1) = MATID
      BUF(2) = 0
      GO TO 1470
 1450 N =  19
      GO TO 1430
 1460 N =  20
      GO TO 1430
C
 1470 CALL SSWTCH (20,J)
      IF (J .EQ. 0) GO TO 1475
      WRITE  (NOUT,1471) BUF(1),BUF(2)
 1471 FORMAT (' PREMAT/1471 - BUF(1),BUF(2) =',2I10)
 1472 CALL ERRTRC ('MAT     ',1472)
C
 1475 CALL MESAGE (-30,N,BUF)
C
 1480 N = 42
      BUF(1) = ELEMID
      BUF(2) = MATID
      GO TO 1470
 1485 N = 103
      BUF(1) = 0
      BUF(2) = 0
      GO TO 1470
 1490 N = 112
      BUF(1) = TABLID
      GO TO 1470
 1500 N = 113
      GO TO 1430
 1510 N = 116
      BUF(1) = MATID
      BUF(2) = TABLID
      GO TO 1470
 1520 N = 114
      GO TO 1430
 1530 N = 115
      BUF(1) = TABLID
      BUF(2) = ITYPE
      GO TO 1470
 1540 BUF(1) = TEMPID
 1541 BUF(2) = 0
      N = 117
      GO TO 1470
 1550 BUF(1) = ELEMID
      GO TO 1541
 1551 BUF(2) = 1
      GO TO 1553
 1552 BUF(2) = 2
 1553 N = 216
 1554 BUF(1) = MATID
      GO TO 1470
 1556 N = 214
      GO TO 1554
 1560 N = 217
      GO TO 1554
 1570 N = 219
      GO TO 1554
      END
