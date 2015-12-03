      SUBROUTINE RODS
C
C     THIS ROUTINE PROCESSES ROD ELEMENT DATA TO PRODUCE STIFFNESS AND
C     MASS MATRICES. IF THE HEAT TRANSFER OPTION IS ON, CONDUCTIVITY AND
C     CAPACITY MATRICES ARE PRODUCED
C
C     THIS ROUTINE CAN COMPUTE BOTH CONVENTIONAL AND CONSISTENT
C     MASS MATRICES
C
C     SINGLE PRECISION VERSION
C
C     THIS VERSION WAS SPECIALLY CODED TO ILLUSTRATE A GENERAL
C     USE OF THE IMPROVED MATRIX GENERATOR.
C
C     THE EST ENTRY FOR THIS ELEMENT CONTAINS
C
C     POSITION     NAME       DESCRIPTION
C     *****        *****      *******************************
C     1             EID       ELEMENT ID NO.
C     2             SIL1      SCALAR INDEX OF POINT A
C     3             SIL2      SCALAR INDEX OF POINT B
C     4             MID       MATERIAL DATA ID
C     5             AFACT     AREA OF CROSS SECTION
C     6             JFACT     TORSIONAL STIFFNESS COEFFICIENT
C     7             CFACT     TORSIONAL STRESS RECOVERY DISTANCE
C     8             MU        NON-STRUCTURAL MASS PER LENGTH
C     9-16          BGPDT     BASIC GRID POINT DATA. COORDINATE SYSTEM
C                             NUMBER AND  X,Y,Z LOCATION FOR 2 POINTS
C     17            TBAR      AVERAGE ELEMENT TEMPERATURE
C
C
      LOGICAL         NOGO
      INTEGER         SIL1     ,SIL2     ,IEST(13)  ,EID     ,GE       ,
     1                DICT(7)  ,ELID     ,ESTID
      REAL            JFACT    ,MU       ,KCON     ,EST(200) ,K        ,
     1                EVECT(3) ,EL       ,KE       ,ME       ,TE       ,
     2                HA(3)    ,HB(3)    ,KHA(3)   ,KHB(3)   ,TA(9)    ,
     3                TB(9)    ,SCALE
      REAL            MASSII(9),MASSJJ(9),MASSIJ(9),MASSJI(9),MIJDUM(9),
     1                MJIDUM(9)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /MATIN / MATID    ,INFLAG   ,ELTEMP   ,DUM(3)
      COMMON /MATOUT/ E        ,G        ,NU       ,RHO      ,ALFA     ,
     1                TSUB0    ,GE
      COMMON /HMTOUT/ KCON
      COMMON /EMGPRM/ IXTRA    ,IZR      ,NZR      ,DUMY(12) ,KMBGG(3) ,
     1                IPREC    ,NOGO     ,HEAT     ,ICMBAR
      COMMON /EMGDIC/ DUM2(2)  ,NLOCS    ,ELID     ,ESTID
      COMMON /ZZZZZZ/ K(1)
C
C     THE VARIABLE K IS OPEN CORE. OPEN SPACE EXISTS FROM Z(IZ) TO Z(NZ)
C     THIS IS INTENDED AS AN EXAMPLE. NORMALLY FOR SMALL ARRAYS
C     LOCAL VARIABLES MAY BE USED.
C
      COMMON /EMGEST/ EID      ,SIL1     ,SIL2     ,MID      ,AFACT    ,
     1                JFACT    ,CFACT    ,MU       ,BGPDT(4,2),TBAR
      COMMON /SYSTEM/ KSYSTM(63)
      EQUIVALENCE     (KSYSTM( 2),IOUTPT),(KSYSTM(56),IHEAT) ,
     1                (EID,EST(1),IEST(1)),(CP,KCON)
C
C     FOR DOUBLE PRECISION THE POINTERS TO OPEN CORE MUST BE MODIFIED.
C
      IZ = (IZR-2)/IPREC + 2
      NZ = NZR/IPREC
      IF (NZ -IZ .LE. 144) GO TO 290
      DICT(1) = ESTID
C
C     SUBTRACT BASIC LOCATIONS TO OBTAIN LENGTH ETC.
C
      DO 10 I = 1,3
   10 EVECT(I) = BGPDT(I+1,2) - BGPDT(I+1,1)
C
      EL = SQRT(EVECT(1)**2 + EVECT(2)**2 + EVECT(3)**2)
      IF (EL .LE. 0.0) GO TO 270
C
C     IF HEAT TRANSFER PROBLEM TRANSFER.  CALL MATERIAL SUBROUTINE
C
      INFLAG = 1
      MATID  = MID
      ELTEMP = TBAR
      IF (IHEAT .EQ. 1) GO TO 240
      CALL MAT (EID)
      KE = (E*AFACT)/EL
      ME = (RHO*AFACT+MU)*EL/2.0
      TE = (G*JFACT)/EL
C
C     PROCESS STIFFNESS HERE
C
      IF (KMBGG(1) .EQ. 0) GO TO 220
      IF (KE.EQ.0.0 .AND. TE.EQ.0.) GO TO 220
C
C     GENERATE   HA  =  (E*TA)/EL   AND  HB = (E*TB)/EL
C
      IF (IEST(9).EQ. 0) GO TO 30
      CALL TRANSS (BGPDT(1,1),TA)
      CALL GMMATS (EVECT,1,3,0, TA,3,3,0, HA)
      DO 20 I = 1,3
   20 HA(I) = HA(I)/EL
      GO TO 50
   30 DO 40 I = 1,3
   40 HA(I) = EVECT(I)/EL
   50 IF (IEST(13).EQ. 0) GO TO 70
      CALL TRANSS (BGPDT(1,2),TB)
      CALL GMMATS (EVECT,1,3,0, TB,3,3,0, HB)
      DO 60 I = 1,3
   60 HB(I) = HB(I)/EL
      GO TO 90
   70 DO 80 I = 1,3
   80 HB(I) = EVECT(I)/EL
C
C     THE GENERAL 12X12  MATRIX FOR THE ROD ELEMENT IS
C                             -                            -
C                            1HA K HA1   0  1HA K HB1       1
C                **   **     1 ------1------1-------1-------1
C                *  K  *   = 1   0   1HA T A1       1HA T HB1
C                **   **     1 ------1------1-------1-------1
C                            1HB K HA       1HB K HB1       1
C                            1 ------1------1-------1-------1
C                            1       1HB T A1       1HB T HB1
C                            1       1      1       1       1
C                             -                            -
C                      EACH BLOCK  ABOVE IS A 3 BY 3 MATRIX
C
C     TEST AND SET COMPONENT CODE    111= 7     111000=56
C
   90 ICODE = 0
      NDOF  = 0
      IF (TE .NE. 0.) GO TO 100
      ICODE = 7
      NDOF  = 6
      GO TO 120
  100 IF (KE .NE. 0.) GO TO 110
      ICODE = 56
      NDOF  = 6
      GO TO 120
  110 ICODE = 63
      NDOF  = 12
  120 NSQ   = NDOF**2
      NG    = NDOF/2
      NPART = NG*NDOF
      IZERO = IZ -1
      IPASS = 1
      DO  130 I = 1,NSQ
      IZPI  = IZ + I - 1
  130 K(IZPI) = 0.0
C
C     EXTENSIONAL STIFFNESS TERMS ARE COMPUTED HERE.
C
      IF (ICODE .EQ. 56) GO TO 200
      SCALE = KE
  140 DO 150 I = 1,3
      KHA(I) = SCALE*HA(I)
  150 KHB(I) = SCALE*HB(I)
C
C     THE MATRIX COLUMNS AND ROWS MUST BE IN THE NUMERICAL ORDER
C     OF TH SIL VALUES. THE POINTERS INTO THE MATRIX ARE VARIABLES.
C
      IF (SIL2-SIL1) 160,270,170
  160 IBBZ = IZERO
      IABZ = IZERO + NG
      IBAZ = IZERO + NPART
      IAAZ = IBAZ  + NG
      GO TO 180
  170 IAAZ = IZERO
      IBAZ = IZERO + NG
      IABZ = IZERO + NPART
      IBBZ = IABZ  + NG
  180 CONTINUE
      DO 190 J = 1,3
      DO 190 I = 1,3
      IJ   = NDOF*(J-1) + I
      IAA  = IJ + IAAZ
      K(IAA) = KHA(I)*HA(J)
      IBA  = IJ + IBAZ
      K(IBA) =-KHB(I)*HA(J)
      IAB  = IJ + IABZ
      K(IAB) =-KHA(I)* HB(J)
      IBB  = IJ + IBBZ
      K(IBB) = KHB(I)* HB(J)
  190 CONTINUE
C
C     THE TORSIONAL STIFFNESS TERMS ARE FORMED USING TE INSTEAD OF KE
C     THEY ARE INSERTED IN THE MATRIX WITH  A CONSTANT OFFSET, 3*12+3.
C
  200 IF (IPASS .EQ. 2) GO TO 210
      IF (NDOF .EQ. 12) IZERO = 38 + IZ
      IPASS = 2
      SCALE = TE
      IF (ICODE .NE. 7) GO TO 140
  210 IPART   = IZ
      DICT(2) = 1
      DICT(3) = NDOF
      DICT(4) = ICODE
      DICT(5) = GE
      CALL EMGOUT (K(IPART),K(IPART),NSQ,1,DICT,1,IPREC)
C
C     THE MASS MATRIX TERMS ARE CALCULATED HERE.
C
  220 IF (KMBGG(2).EQ.0 .OR. ME.EQ.0.0) RETURN
      DICT(3) = 6
      DICT(4) = 7
      DICT(5) = 0
C
C     CHECK TO SEE IF CONVENTIONAL OR CONSISTENT MASS MATRIX IS REQUIRED
C
      IF (ICMBAR .GT. 0) GO TO 400
C
C     CONVENTIONAL MASS MATRIX TERMS ARE COMPUTED HERE
C
      DICT(2) = 2
      LDATA = 6
      IZP5  = IZ + 5
      DO 230 I = IZ,IZP5
  230 K(I)  = ME
      GO TO 600
C
C     CONSISTENT MASS MATRIX TERMS ARE COMPUTED HERE
C
  400 DICT(2) = 1
      LDATA   = 36
      DO 420 I = 1,9
      MASSII(I) = 0.0
      MASSJJ(I) = 0.0
      MASSIJ(I) = 0.0
      MASSJI(I) = 0.0
      MIJDUM(I) = 0.0
      MJIDUM(I) = 0.0
  420 CONTINUE
      ME = 2.0*ME
      DO 440 I = 1,9,4
      MASSII(I) = ME/3.0
      MASSJJ(I) = ME/3.0
      MASSIJ(I) = ME/6.0
      MASSJI(I) = ME/6.0
      MIJDUM(I) = ME/6.0
      MJIDUM(I) = ME/6.0
  440 CONTINUE
      IF (SIL2-SIL1) 480,270,460
  460 ITI = 9
      ITJ = 13
      GO TO 500
  480 ITI = 13
      ITJ = 9
  500 IF (IEST(ITI) .EQ. 0) GO TO 520
      CALL TRANSS (IEST(ITI), TA)
      CALL GMMATS (TA,3,3,1, MASSII,3,3,0, K(IZ))
      CALL GMMATS (K(IZ),3,3,0, TA,3,3,0, MASSII)
      CALL GMMATS (TA,3,3,1, MIJDUM,3,3,0, MASSIJ)
      CALL GMMATS (MJIDUM,3,3,0, TA,3,3,0, MASSJI)
  520 IF (IEST(ITJ) .EQ. 0) GO TO 560
      CALL TRANSS (IEST(ITJ), TA)
      CALL GMMATS (TA,3,3,1, MASSJJ,3,3,0, K(IZ))
      CALL GMMATS (K(IZ),3,3,0, TA,3,3,0, MASSJJ)
      CALL GMMATS (MASSIJ,3,3,0, TA,3,3,0, MIJDUM)
      CALL GMMATS (TA,3,3,1, MASSJI,3,3,0, MJIDUM)
      DO 540 I = 1,9
      MASSIJ(I) = MIJDUM(I)
      MASSJI(I) = MJIDUM(I)
  540 CONTINUE
  560 DO 580 I = 1,3
      KZ = IZ + I - 1
      K(KZ   ) = MASSII(I  )
      K(KZ+ 6) = MASSII(I+3)
      K(KZ+12) = MASSII(I+6)
      K(KZ+ 3) = MASSIJ(I  )
      K(KZ+ 9) = MASSIJ(I+3)
      K(KZ+15) = MASSIJ(I+6)
      K(KZ+18) = MASSJI(I  )
      K(KZ+24) = MASSJI(I+3)
      K(KZ+30) = MASSJI(I+6)
      K(KZ+21) = MASSJJ(I  )
      K(KZ+27) = MASSJJ(I+3)
      K(KZ+33) = MASSJJ(I+6)
  580 CONTINUE
  600 CALL EMGOUT (K(IZ),K(IZ),LDATA,1,DICT,2,IPREC)
      RETURN
C
C     HEAT TRANSFER CALCULATIONS ARE PERFORMED HERE
C
  240 INFLAG  = 1
      DICT(2) = 1
      DICT(3) = 2
      DICT(4) = 1
      DICT(5) = 0
      IF (KMBGG(1) .EQ. 0) GO TO 250
      CALL HMAT (EID)
      K(IZ) = (AFACT*KCON)/EL
      IF (K(IZ) .EQ. 0.0) GO TO 250
      K(IZ+1) = -K(IZ)
      K(IZ+2) = -K(IZ)
      K(IZ+3) =  K(IZ)
      CALL EMGOUT (K(IZ),K(IZ),4,1,DICT,1,IPREC)
  250 INFLAG = 4
      IF (KMBGG(1) .EQ. 0) RETURN
      CALL HMAT (EID)
      K(IZ) = (AFACT*CP)*EL/2.0
      IF (K(IZ) .EQ. 0.0) RETURN
      K(IZ+1) = K(IZ)
      DICT(2) = 2
      CALL EMGOUT (K(IZ),K(IZ),2,1,DICT,3,IPREC)
      RETURN
C
  270 NOGO = .TRUE.
      WRITE  (IOUTPT,280) UFM,EID
  280 FORMAT (A23,' 3118, ROD ELEMENT NO.',I9,
     1        ' HAS ILLEGAL GEOMETRY OR CONNECTIONS.')
      RETURN
  290 NOGO = .TRUE.
      WRITE  (IOUTPT,300) UFM
  300 FORMAT (A23,' 3119, INSUFFICIENT CORE TO PROCESS ROD ELEMENTS')
      RETURN
      END
