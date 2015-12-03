      BLOCK DATA GP3BD
CGP3BD
C
C     BLOCK DATA PROGRAM FOR MODULE GP3.
C
      INTEGER         GEOM3 ,EQEXIN,GEOM2 ,SLT   ,GPTT  ,SCR1  ,SCR2   ,
     1                CARDID,BUF   ,CARDDT,STATUS,PLOAD2,TEMP  ,TEMPD  ,
     2                TEMPP1,TEMPP2,TEMPP3,TEMPRB,PLOAD3,TEMPG ,TEMPP4
C
      COMMON /GP3COM/ GEOM3 ,EQEXIN,GEOM2 ,SLT   ,GPTT  ,SCR1  ,SCR2   ,
     1                BUF1  ,BUF2  ,BUF(50)      ,CARDID(60)   ,IDNO(30)
     2              , CARDDT(60)   ,MASK(60)     ,STATUS(60)   ,NTYPES ,
     3                IPLOAD,IGRAV ,PLOAD2(2)    ,LOAD(2)      ,NOPLD2 ,
     4                TEMP(2)      ,TEMPD(2)     ,TEMPP1(2)    ,
     5                TEMPP2(2)    ,TEMPP3(2)    ,TEMPRB(2)    ,BUF3   ,
     6                PLOAD3(2)    ,IPLD3        ,TEMPG(2)     ,
     7                TEMPP4(2)
C
C     GINO NAMES FOR INPUT, OUTPUT AND SCRATCH FILES.
C
      DATA    GEOM3 , EQEXIN,GEOM2 ,SLT  ,GPTT, SCR1 ,SCR2 /
     1        101   , 102   ,103   ,201  ,202 , 301  ,302  /
C
C     DATA DEFINING LOAD CARDS--
C     CARDID - TWO-WORD RECORD ID DEFINING CARD TYPE.
C     CARDDT - TWO WORDS PER CARD TYPE. 1ST WORD IS NO. OF WORDS PER
C              CARD. 2ND WORD IS POINTER IN MASK TABLE TO ENTRY WHICH
C              DESCRIBES THE NUMBER AND LOCATION OF GRID POINTS ON THE
C              CARD.
C     MASK   - TABLE AS DESCRIBED ABOVE.
C     IDNO   - INTERNAL CARD TYPE ID.
C
C              FORCE1   FORCE2   FORCE    GRAV     RFORCE
C              MOMNT1   MOMNT2   MOMENT   PLOAD    SLOAD
C              PRESAX   QHBDY    QVOL     QBDY1    QBDY2
C              QVECT    PLOAD3   PLOAD1   PLOADX   CEMLOOP
C              SPCFLD   GEMLOOP  REMFLUX  MDIPOLE  PLOAD4
C
      DATA    CARDID/ 4001,40, 4101,41, 4201,42, 4401,44, 5509,55,
     1                4601,46, 4701,47, 4801,48, 5101,51, 5401,54,
     2                5215,52, 4309,43, 5209,52, 4509,45, 4909,49,
     3                5009,50, 7109,71, 6909,69, 7001,70, 3109,31,
     4                3209,32, 3309,33, 3409,34, 3509,35, 6709,67,
     5                0000,00, 0000,00, 0000,00, 0000,00, 0000,00/
C
CWKBR 2/95 SPR94015 DATA    CARDDT/ 5, 3,    7, 7,    7, 1,    6, 0,    8, 1,         
      DATA    CARDDT/ 5, 3,    7, 7,    7, 1,    6, 0,    7, 1,              
     1                5, 3,    7, 7,    7, 1,    6,13,    3, 1,
     2                7,18,    8,21,    3, 0,    3, 0,    6, 0,
     3                6, 0,   39,26,    8, 0,    6,14,   13, 0,
     4                6,28,   49, 0,    6, 0,   10, 0,   12, 0,
     5                0, 0,    0, 0,    0, 0,    0, 0,    0, 0/
C
      DATA    STATUS/ -1, 0,   -1, 0,   -1, 0,   -1, 0,   -1, 0,
     1                -1, 0,   -1, 0,   -1, 0,   -1, 0,   -1, 0,
     2                -1, 0,   -1, 0,   -1, 0,   -1, 0,   -1, 0,
     3                -1, 0,   -1, 0,   -1, 0,   -1, 0,   -1, 0,
     4                -1, 0,   -1, 0,   -1, 0,   -1, 0,   -1, 0,
     5                -1, 0,   -1, 0,   -1, 0,   -1, 0,   -1, 0/
C
      DATA    IDNO  /  3,       5,       1,       8,      10,
     1                 4,       6,       2,       9,       7,
     2                11,      12,      13,      14,      15,
     3                16,      17,      18,      19,      21,
     4                20,      22,      24,      23,      25,
     5                 0,       0,       0,       0,       0/
C
      DATA    MASK  / 1,2,
     1                3,2,4,5,
     2                5,2,4,5,6,7,
     3                4,3,4,5,6,
     4                2,3,4,
     5                4,5,6,7,8,32,-8,1,6,31*0/
C
C     MISCELANEOUS DATA.
C
      DATA    NTYPES/    49/
     1        IGRAV /     7/
     2        IPLOAD/    17/
     3        PLOAD2/  6809,    68/
     4        LOAD  /  4551,    61/
     5        NOPLD2/     0/
     6        TEMP  /  5701,    57/
     7        TEMPD /  5641,    65/
     8        TEMPP1/  8109,    81/
     9        TEMPP2/  8209,    82/
     A        TEMPP3/  8309,    83/
     B        TEMPRB/  8409,    84/
     C        PLOAD3/  7109,    71/
     D        IPLD3 /    33/
     E        TEMPG /  8509,    85/
     F        TEMPP4/  8609,    86/
      END
