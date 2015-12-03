      SUBROUTINE DSMG2
C*****
C THIS MODULE PERFORMS THE FOLLOWING MATRIX OPERATIONS...
C
C     KBAA  = KAA + BETA * KDAA
C     KBFS  = KFS + BETA * KDFS
C     KBSS  = KSS + BETA * KDSS
C     PBL   = BETA * PL
C     PBS   = BETA * PS
C     YBS   = BETA * YS
C     UBOOV = BETA * UOOV
C
C THE VALUE OF BETA LIES IN THE DIFFERENTIAL STIFFNESS COEFFICIENT SET
C NO. SPECIFIED BY THE INPUT PARAMETER DSCSET.  THE PARTICULAR VALUE OF
C BETA TO BE USED ON ANY PASS THROUGH THIS MODULE IS THE NDSKIP(TH)
C VALUE IN THE DSCSET SET.  IREPTD IS SET EQUAL TO -1 AFTER THE LAST
C BETA IN THE SET HAS BEEN ENCOUNTERED, THEREBY TERMINATING THE
C DIFFERENTIAL STIFFNESS RIGID FORMAT DMAP LOOP BEGINNING AT THE
C LABEL DSLOOP AND ENDING AT THE REPT DSLOOP,100$ STATEMENT.
C
C
C  DMAP CALL...
C
C
C     DSMG2    MPT,KAA,KDAA,KFS,KDFS,KSS,KDSS,PL,PS,YS,UOOV/KBAA,KBFS,
C              KBSS,PBL,PBS,YBS,UBOOV/V,N,NDSKIP/V,N,REPEATD/
C              V,N,DSCOSET/ $
C*****
      INTEGER
     1                   MPT                ,SETNO
     2,                  KAA                ,KDAA
     3,                  KFS                ,KDFS
     4,                  KSS                ,KDSS
     5,                  PL                 ,PS
     6,                  YS                 ,UOOV
     7,                  KBAA               ,KBFS
     8,                  KBSS               ,PBL
     9,                  PBS                ,YBS
     T,                  UBOOV              ,SYSBUF
     1,                  BUFFR1             ,BUFFR2
     2,                  FILE1              ,FILE2
     3,                  FILE3              ,CLSRW
     4,                  DSNOS              ,DSCSET
C
C
C
      DIMENSION
     1                   MCB(7)             ,DSNOS(2)
     2,                  NAME(2)            ,BLOCK(11)
     3,                  IBLOCK(11)
C
C
C
      EQUIVALENCE
     1                   (BLOCK(1),IBLOCK(1))
     2,                  (BETASP,IBETA)
C
C MODULE PARAMETERS
C
      COMMON /BLANK/
     1                   NDSKIP             ,IREPTD
     2,                  DSCSET
C
C SYSTEM COMMON
C
      COMMON   /SYSTEM/
     1                   SYSBUF
C
C
C
      COMMON   /ZZZZZZ/
     1                   Z(1)
C
C
C
      DATA
     1                   NAME(1)/4HDSMG/    ,NAME(2)/4H2   /
      DATA
     1                   MPT
     2,                  KAA                ,KDAA
     3,                  KFS                ,KDFS
     4,                  KSS                ,KDSS
     5,                  PL                 ,PS
     6,                  YS                 ,UOOV
     7,                  KBAA               ,KBFS
     8,                  KBSS               ,PBL
     9,                  PBS                ,YBS
     T,                  UBOOV /
     1                   101,102,103,104,105,106,107,108,109,110,
     2                   111,201,202,203,204,205,206,207 /
      DATA
     1                   CLSRW/1/
      DATA
     1                   NEOR,DSNOS(1),DSNOS(2) / 0,53,10/
C
C
C
      IZMAX  = KORSZ (Z)
      BUFFR1 = IZMAX  - SYSBUF
      BUFFR2 = BUFFR1 - SYSBUF
      LEFT   = BUFFR2 - 1
C
C TURN DIFFERENTIAL STIFFNESS LOOPING FLAG ON AND INCREMENT THE INDEX
C OF BETA.  NOTE THAT NDSKIP MUST BE SET TO ZERO IN THE MODULE
C PROPERTIES TABLE.
C
      IREPTD = 1
      NDSKIP = NDSKIP + 1
C
C CALL LOCATE TO FIND THE RECORD OF THE MPT WHERE THE DSFACT CARDS ARE.
C THIS IS DONE ONLY IF A D.S. COEFFICIENT SET NO. IS GIVEN.
C
      IF (DSCSET .NE. (-1)) GO TO 5
C
C THERE IS NO LOOPING.  TURN LOOPING INDICATOR OFF.
C SEE COMMENTS ABOVE FORTRAN STATEMENT NO. 70 RE THE 4 SCALAR MULTIPLI-
C CATIONS WHEN DSCSET = -1.
C
      IREPTD = -1
      GO TO 165
    5 CALL PRELOC(*1030,Z(BUFFR1),MPT)
      CALL LOCATE(*1035,Z(BUFFR1),DSNOS,IDUMMY)
C
C
C
   10 CALL READ(*1040,*1050,MPT,SETNO,1,NEOR,IDUMMY)
      IF (SETNO .EQ. DSCSET) GO TO 30
C
C READ ONE WORD AT A TIME UNTIL A -1 (END OF SET INDICATOR) IS READ.
C
      DO 20 I = 1,32000
      CALL READ(*1060,*1070,MPT,J,1,NEOR,IDUMMY)
      IF (J .EQ. (-1)) GO TO 10
   20 CONTINUE
      CALL MESAGE (-30,84,1)
C
C TEST TO SEE IF WORDS MUST BE SKIPPED.
C
   30 IF (NDSKIP .EQ. 1) GO TO 40
C
C SKIP NDSKIP - 1 WORDS
C
      CALL READ(*1080,*1090,MPT,0,-(NDSKIP-1),NEOR,IDUMMY)
C
C READ THE VALUE OF BETA
C
   40 CALL READ(*1100,*1110,MPT,BETASP,1,NEOR,IDUMMY)
      IF (IBETA .EQ. (-1)) CALL MESAGE (-30,84,2)
C
C IF THE NEXT WORD IS A -1, WE HAVE READ THE LAST BETA.  HENCE SET
C IREPTD = -1
C
      CALL READ(*1120,*1130,MPT,J,1,NEOR,IFLAG)
      IF (J .EQ. (-1)) IREPTD = -1
      CALL CLOSE (MPT,CLSRW)
C
C PERFORM THE 4 SCALAR MULTIPLICATIONS.  N.B.---IF DSCSET = -1, THAT IS,
C ONLY ONE BETA WILL BE USED AND THAT HAS AN ASSUMED VALUE OF 1.0, IT IS
C ASSUMED THAT EQUIVALENCES HAVE BEEN MADE BETWEEN PL AND PBL, PS AND
C PBS, YS AND YBS, AND UOOV AND UBOOV.
C
      IND = 0
      FILE1 = PL
      FILE2 = PBL
   70 MCB(1) = FILE1
      CALL RDTRL (MCB)
C
C A FATAL ERROR OCCURS IF PL IS PURGED.
C
      IF (MCB(1) .LT. 0  .AND.  IND .EQ. 0) CALL MESAGE (-1,FILE1,NAME)
C
C IF THE INPUT FILE IS NOT PURGED AND IS NOT PL, SKIP THE OPERATION.
C
      IF (MCB(1) .LT. 0) GO TO 130
C
C THE INPUT FILE IS NOT PURGED. IF THE OUTPUT FILE IS PURGED, A FATAL
C ERROR OCCURS.
C
      MCB(1) = FILE2
      CALL RDTRL (MCB)
      IF (MCB(1) .LT. 0) CALL MESAGE (-1,FILE2,NAME)
      IBLOCK(1) = 1
      BLOCK (2) = BETASP
      BLOCK (3) = 0.0
      BLOCK (4) = 0.0
      BLOCK (5) = 0.0
      BLOCK (6) = 0.0
      IBLOCK(7) = 1
      BLOCK (8) = 0.0
      BLOCK (9) = 0.0
      BLOCK(10) = 0.0
      BLOCK(11) = 0.0
      CALL SSG2C (FILE1,0,FILE2,0,BLOCK(1))
  130 IND = IND + 1
      GO TO (140,150,160,170), IND
  140 FILE1= PS
      FILE2 = PBS
      GO TO 70
  150 FILE1 = YS
      FILE2 = YBS
      GO TO 70
  160 FILE1 = UOOV
      FILE2 = UBOOV
      GO TO 70
C
C PERFORM MATRIX ADDITIONS
C
  165 BETASP= 1.0
  170 FILE1 = KAA
      FILE2 = KDAA
      FILE3 = KBAA
      IND   = 0
  180 IUNDEF= 0
      MCB(1)= FILE1
      CALL RDTRL (MCB(1))
      IF (MCB(1) .LT. 0)  IF (IND) 190,190,200
      GO TO 210
  190 CALL MESAGE (-1,FILE1,NAME)
  200 IUNDEF = 1
  210 MCB(1) = FILE2
      CALL RDTRL (MCB(1))
      IF (MCB(1) .LT. 0)  IF (IUNDEF) 260,260,220
      IF (IUNDEF .EQ. 1) CALL MESAGE (-30,84,15)
      IBLOCK(1) = 1
      BLOCK (2) = 1.0
      BLOCK (3) = 0.0
      BLOCK (4) = 0.0
      BLOCK (5) = 0.0
      BLOCK (6) = 0.0
      IBLOCK(7) = 1
      BLOCK (8) = BETASP
      BLOCK (9) = 0.0
      BLOCK(10) = 0.0
      BLOCK(11) = 0.0
      CALL SSG2C (FILE1,FILE2,FILE3,0,BLOCK(1))
  220 IND = IND + 1
      GO TO (230,240,250),IND
  230 FILE1 = KFS
      FILE2 = KDFS
      FILE3 = KBFS
      GO TO 180
  240 FILE1 = KSS
      FILE2 = KDSS
      FILE3 = KBSS
      GO TO 180
  250 RETURN
  260 CALL MESAGE (-1,FILE2,NAME )
 1030 I = 3
      GO TO 1099
 1035 I = 4
      GO TO 1099
 1040 I = 5
      GO TO 1099
 1050 I = 6
      GO TO 1099
 1060 I = 7
      GO TO 1099
 1070 I = 8
      GO TO 1099
 1080 I = 9
      GO TO 1099
 1090 I = 10
      GO TO 1099
 1100 I = 11
      GO TO 1099
 1110 I = 12
      GO TO 1099
 1120 I = 13
      GO TO 1099
 1130 I = 14
 1099 CALL MESAGE (-30,84,I)
      RETURN
      END
