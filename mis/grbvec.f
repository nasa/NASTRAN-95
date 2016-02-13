      SUBROUTINE GRBVEC
C
C     THIS SUBROUITNE IS THE MAIN DRIVER FOR THE VECGRB MODULE
C     WHICH GENERATES
C
C     (1) THE GEOMETRIC RIGID BODY VECTORS ABOUT THE INDICATED GRID
C         POINT OR ORIGIN.
C         THIS SET OF VECTORS CONSISTS OF UNIT DISPLACEMENTS IN ZERO
C         COORDINATE SYSTEM ABOUT THE SPECIFIED GRID IN GLOBAL COORD.
C         FOR EASE OF ASSEMBLY THE VECTOR IS GENERATED IN THE TRANSPOSED
C         FORM, THAT IS, WITH SIX ROW, ONE FOR EACH OF THE SIX UNIT
C         MOTIONS AND G-SET COLUMNS, ONE FOR EACH DOF'S CORRESPONDING
C         MOTION. THIS SET OF VECTORS WOULD BE EXACTLY EQUAL TO A UNIT
C         DISPLACEMENT CHECK IF ALL THE GRIDS HAD STIFFNESS BUT WERE
C         NOT GROUNDED.
C
C     (2) A g-SET SIZED CSTM FROM BASIC TO GOLBAL
C
C     DMAP SEQUENCE -
C
C     VECGRB    BGPDT,EQEXIN,CSTM/OUTVEC/P1/P2/P3   $
C
C     WHERE     P1 = 1, GENERATE CSTM FROM BASIC TO GLOBAL
C                  = 2, GENERATE PHIRBT
C               P2 = REFERENCE GRID FOR PHIRB (0=BASIC, DEFAULT)
C               P3 = CURRENTLY NOT USED
C
C     EXAMPLES -
C
C     (1) G-SET EQUILIBRIUM CHECK
C     THIS CHECK MULTIPLIES THE STIFFNESS MATRIX TIMES THE GEOMETRIC
C     RIGID BODY SHAPES PENERATED BY VECRGB. THE FORCES OBATINED FROM
C     THIS MULTIPLICATION SHOULD BE ZERO.
C
C     VECGRB   BGPDT,CSTM,EQEXIN/PHIRBT/2/0  $ CREATE TRANSPOSE OF RIGID
C     TRNSP    PHIRBT/PHIRB                  $ BODY VECTORS, THEN TRNSP
C     MPYAD    KGG,PHIRB,/KPHIG/0            $ MULTIPLY BY STIFFNESS.
C     MPYAD    PHIRBT,KPHIG,/KPHG6/0         $ SUM FORCES AND PRINT
C     MATPRN   KPHG6,,,, //                  $ 6X6 SUMMATION. PRINT ALL
C     MATGPR   GPL,USET,SIL,KPHIG//*G*/*G*// $ FORCES OVER 0.0001
C              .0001                         $
C
C     (2) COORDINATE SYSTEM TRANSFORMATION
C
C     VECRGB   BGPDT,CSTM,EQEXIN/BCSTM/1     $ TRANSFORM GLOBAL KGG TO
C     TRNSP    BCSTM/BCSTMT/                 $ BASIC
C     MPYAD    BCSTM,KGG,/BGKGG/0            $
C     MPYAD    BGKGG,BCSTMT,/BBKGG/0         $
C
C     THIS SUBROUTINE WAS ORIGINALLY CALLED CSTMX, AND WAS WRITTEN BY
C     P.KIRCHMAN/SWALES, 2/1993, WITH THE DMAP MODULE OF THE SAME NAME
C
C     THE DMAP MODLUE IS RENAMED TO GEOMETRIC RIGID BODY VECTOR, VECGRB,
C     AND THE SUBROUTINE GRBVEC. THE ORIGINAL SUBROUTINE WAS RE-CODED BY
C     G.CHAN/UNISIS, USING NASTRAN TRADITIONAL FORTRAN STYLE, AND THE
C     SUBSTITUTION OF GMMATD ROUTINE FOR DP3X3M. ALSO, THE ORDER OF 2ND
C     AND 3RD INPUT DATA BLOCKS IS INTERCHANGED.
C
C     THE ORIGINAL CSTMX IS INCLUDED IN THE 1993 RELEASE. IT IS ONLY
C     FOR BACKUP PURPOSE. CSTMX WILL BE DELETED IN NEXT NASTRAN RELEASE
C     (THE ORIGINAL CSTMX ROUTINE PRODUCED HUNDREDS OF FORTRAN ERRORS
C     ON CDC MACHINE WITH FTN5 COMPILER. 3/93)
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL          RBV
      INTEGER          TRL(7),NAME(2),SUB(2)
      REAL             RX(1)
      DOUBLE PRECISION RBVR(3),V3(3),ZERO,ONE,XG,YG,ZG,RAD,XL,TU(9),
     1                 T1(9),T2(9),T(9),RVEC(9),RBVEC(9),V(3),VOUT(6)
      CHARACTER        UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG /  UFM,UWM,UIM,SFM
      COMMON /BLANK /  P1,P2,P3
      COMMON /ZZZZZZ/  IX(1)
      COMMON /SYSTEM/  IBUFF,NOUT
      COMMON /PACKX /  TIN,TOU,II,JJ,INCR
      EQUIVALENCE      (IX(1),RX(1))
      DATA    BGPDT ,  EQEXIN, CSTM / 101,102,103  /,
     1        OUTVEC/  201 /,  SUB  / 4HGRBV,4HC   /
      DATA    ZERO  ,  ONE /  0.0D+0, 1.0D+0       /
      DATA    CSTMX ,  EQE,XIN,       BGP,DT       /
     1        4HCSTM,  4HEQEX,4HIN  , 4HBGPD,4HT   /
C
C     CHECK FOR THE PRESENCE OF OUTPUT DATA BLOCK
C
      TRL(1) = OUTVEC
      CALL RDTRL (TRL)
      IF (TRL(1) .LE. 0) GO TO 1000
C
C     INITIALIZATION
C
      LCOR  = KORSZ(IX(1)) - IBUFF
      BUF1  = LCOR - 1
      TU(1) = ONE
      TU(2) = ZERO
      TU(3) = ZERO
      TU(4) = ZERO
      TU(5) = ONE
      TU(6) = ZERO
      TU(7) = ZERO
      TU(8) = ZERO
      TU(9) = ONE
C
C     CHECK THE PRESENCE OF BGPDT AND CSTM FILES
C
      TRL(1)= BGPDT
      CALL RDTRL (TRL)
      IF (TRL(1) .LE. 0) GO TO 1020
      NENT  = TRL(2)
      TRL(1)= CSTM
      CALL RDTRL (TRL)
      NCST  = TRL(3)
      IF (TRL(1) .LE. 0) NCST = 0
      NENT4 = NENT*4
      NCST14= NCST*14
      IF (NENT4+NCST14 .GT. LCOR) GO TO 1100
      RBV   = P1 .EQ. 2
C
C     CHECK IF THIRD INPUT FILE IS PRESENT, THEN OPEN EQEXIN FILE AND
C     READ THE FIRST TABLE INTO CORE IF APPROPRIEATE
C
      GRDPNT = 0
      IF (P1.NE.2 .OR. P2.EQ.0) GO TO 60
      TRL(1) = EQEXIN
      CALL RDTRL (TRL)
      IF (TRL(1) .LE. 0) GO TO 1020
      FILE = EQEXIN
      CALL FNAME (EQEXIN,NAME)
      IF (NAME(1).NE.EQE .OR. NAME(2).NE.XIN) GO TO 1040
      CALL OPEN (*1200,EQEXIN,IX(BUF1),0)
      CALL FWDREC (*1300,EQEXIN)
      CALL READ (*1300,*20,EQEXIN,IX(1),BUF1-1,1,FLAG)
      CALL MESAGE (-8,0,SUB)
   20 TRL2 = TRL(2)*2
      IF (FLAG .NE. TRL2) GO TO 1320
      J = 1
      DO 30 I = 1,TRL2,2
      IF (P2 .NE. IX(J)) GO TO 30
      GRDPNT = IX(J+1)
      GO TO 50
   30 J = J + 2
      GRDPNT = 0
      WRITE  (NOUT,40) UWM,P2
   40 FORMAT (A25,' - ID ',I8,' IS NOT A GRID POINT.  THE ORIGIN WILL ',
     1       'BE USED.')
   50 CALL CLOSE (EQEXIN,1)
C
C     OPEN AND READ BGPDT TABLE INTO BEGINNING OF CORE
C
   60 FILE = BGPDT
      CALL FNAME (BGPDT,NAME)
      IF (NAME(1).NE.BGP .AND. NAME(2).NE.DT) GO TO 1040
      CALL OPEN (*1200,BGPDT,IX(BUF1),0)
      CALL FWDREC (*1300,BGPDT)
      CALL READ (*1300,*70,BGPDT,IX(1),BUF1-1,1,FLAG)
      CALL MESAGE (-8,0,SUB)
   70 IF (FLAG .NE. NENT4) GO TO 1330
      CALL CLOSE (BGPDT,1)
C
C     OPEN AND READ CSTM FIRST TABLE INOT CORE AFTER BGPDT
C
      IF (NCST .EQ. 0) GO TO 90
      FILE = CSTM
      CALL FNAME (CSTM,NAME)
      IF (NAME(1) .NE. CSTMX) GO TO 1040
      CALL OPEN (*1200,CSTM,IX(BUF1),0)
      CALL FWDREC (*1300,CSTM)
      CALL READ (*1300,*80,CSTM,IX(NENT4+1),BUF1-NENT4-1,1,FLAG)
      CALL MESAGE (-8,0,SUB)
   80 IF (FLAG .NE. NCST14) GO TO 1340
      CALL CLOSE (CSTM,1)
C
C     USE BGPDT INFO TO FIGURE OUT THE g-SET SIZE FOR OUTPUT
C
   90 SIZE = 0
      I    = 1
      DO 100 J = 1,NENT
      IF (IX(I) .LT. 0) SIZE = SIZE + 1
      IF (IX(I) .GE. 0) SIZE = SIZE + 6
  100 I = I + 4
C
C     STORE RIGID BODY REFERENCE VECTOR
C
      RBVR(1) = 0.
      RBVR(2) = 0.
      RBVR(3) = 0.
      IF (.NOT.RBV .OR. GRDPNT.EQ.0) GO TO 110
      RBVR(1) = RX(GRDPNT*4-2)
      RBVR(2) = RX(GRDPNT*4-1)
      RBVR(3) = RX(GRDPNT*4  )
C
C     OPNE OUTPUT FILE AND FILL OUTPUT TRAILER
C
  110 CALL FNAME (OUTVEC,NAME)
      CALL OPEN (*1200,OUTVEC,IX(BUF1),1)
      CALL WRITE (OUTVEC,NAME,2,1)
      IF (     RBV) CALL MAKMCB (TRL(1),OUTVEC,6,2,2)
      IF (.NOT.RBV) CALL MAKMCB (TRL(1),OUTVEC,SIZE,2,2)
C
C     INITIALIZE PACK COMMONS
C
      TIN  = 2
      TOU  = 2
      INCR = 1
      COL  = 1
C
C     BEGIN LOOP FOR NUMBER OF ENTRIES IN BGPDT
C
      E4  = 0
      DO 440 ENTRY = 1,NENT
      E4  = E4 + 4
      OCID= IX(E4-3)
      XG  = RX(E4-2)
      YG  = RX(E4-1)
      ZG  = RX(E4  )
C
C     RBVEC IS A VECTOR BASED ON UNIT ROTATIONS OF A VECTOR FROM THE
C     REFERENCE GRID TO THE GRID IN QUESTION. THE TRANSFORMATION IS
C     FROM BASIC ROTATIONS TO BASIC TRANSLATIONS.
C
      IF (.NOT.RBV) GO TO 130
      RVEC(1) = ZERO
      RVEC(2) = (ZG-RBVR(3))
      RVEC(3) =-(YG-RBVR(2))
      RVEC(4) =-(ZG-RBVR(3))
      RVEC(5) = ZERO
      RVEC(6) = (XG-RBVR(1))
      RVEC(7) = (YG-RBVR(2))
      RVEC(8) =-(XG-RBVR(1))
      RVEC(9) = ZERO
C
C     IF THIS ENTRY IS A SCALAR AND A RIGID BODY VECTOR HAS BEEN
C     REQUESTED, STORE A ZERO COLUMN
C
 130  IF (OCID.NE.-1 .OR. .NOT.RBV) GO TO 140
      II  = 1
      JJ  = 1
      CALL PACK (ZERO,OUTVEC,TRL)
      COL = COL + 1
      GO TO 440
C
C     IF THIS ENTRY IS A SCALAR AND A CSTM HAS BEEN REQUESTED, SIMPLY
C     PLACE A ONE ON THE DIAGONAL AND CONTINUE
C
  140 IF (OCID .NE. -1) GO TO 150
      II  = COL
      JJ  = COL
      CALL PACK (ONE,OUTVEC,TRL)
      COL = COL + 1
      GO TO 440
C
C     IF THIS ENTRY IS ALREADY IN BASIC COORDINATES, STORE AN IDENTITY
C     IN THE APPOPRIATE SIX BY SIX
C
  150 IF (OCID.NE.0 .OR. .NOT.RBV) GO TO 190
      II  = 1
      JJ  = 1
      DO 170 I = 1,3
      I3  = 0
      DO 160 J = 1,3
      VOUT(J  ) =   TU(J+I*3)
  160 VOUT(J+3) = RVEC(J+I*3)
      I3  = I3 + 3
      II  = 1
      JJ  = 6
      CALL PACK (VOUT,OUTVEC,TRL)
  170 COL = COL + 1
      DO 180 I = 1,3
      II  = I + 3
      JJ  = I + 3
      CALL PACK (ONE,OUTVEC,TRL)
  180 CONTINUE
      GO TO 440
C
  190 IF (OCID .NE. 0) GO TO 210
      DO 200 I = 1,6
      II  = COL
      JJ  = COL
      CALL PACK (ONE,OUTVEC,TRL)
  200 COL = COL + 1
      GO TO 440
C
C     CSTM MUST BE MISSING
C
  210 IF (NCST .NE. 0) GO TO 220
      TRL(1) = CSTM
      GO TO 1020
C
C     SET UP VECTORS AND MATRICES COMMON TO ALL COORDINATE SYSTEM
C     TRANSFORMATIONS
C
C     FIND COORDINATE SYSTEM
C
  220 DO 230 ICST = 1,NCST
      IF (IX(ICST*14-13+NENT4) .EQ. OCID) GO TO 240
  230 CONTINUE
      GO TO 1400
C
C     GET COORDINATE SYSTEM TYPE AND
C     TRANSFORMATION FROM BASIC TO COORDINATE SYSTEM ORIGIN TRIAD
C
  240 OCIDT = IX(ICST*14-12+NENT4)
      T1(1) = RX(ICST*14- 8+NENT4)
      T1(4) = RX(ICST*14- 7+NENT4)
      T1(7) = RX(ICST*14- 6+NENT4)
      T1(2) = RX(ICST*14- 5+NENT4)
      T1(5) = RX(ICST*14- 4+NENT4)
      T1(8) = RX(ICST*14- 3+NENT4)
      T1(3) = RX(ICST*14- 2+NENT4)
      T1(6) = RX(ICST*14- 1+NENT4)
      T1(9) = RX(ICST*14   +NENT4)
C
      IK    = ICST*14 + NENT4
      V3(1) = RX(ENTRY*4-2) - RX(IK-11)
      V3(2) = RX(ENTRY*4-1) - RX(IK-10)
      V3(3) = RX(ENTRY*4  ) - RX(IK- 9)
C
      V(1)  = RX(IK-8)*V3(1) + RX(IK-5)*V3(2) + RX(IK-2)*V3(3)
      V(2)  = RX(IK-7)*V3(1) + RX(IK-4)*V3(2) + RX(IK-1)*V3(3)
      V(3)  = RX(IK-6)*V3(1) + RX(IK-3)*V3(2) + RX(IK  )*V3(3)
C
C     SPECIAL CHECKS FOR ZERO RADIUS CYLINDRICAL OR SPHERICAL COORDINATE
C     SYSTEM. IF SO TREAT AS RECTANGULAR.
C
      RAD = SQRT(V(1)**2 + V(2)**2)
      IF (RAD .EQ. 0.) OCIDT = 1
C
C     PERFORM INDIVIDUAL COORDINATE SYSTEM TRANSFORMATION AND GENERATE
C     T2
C
      GO TO (250,330,340), OCIDT
C
C     RECTANGULAR, T = T1
C
  250 IF (.NOT.RBV) GO TO 290
      INDEX = 1
      CALL GMMATD (RVEC,3,3,0, T1,3,3,0, RBVEC)
C
C     ADD RIGID BODY INFORMATION TO LOWER OFF DIAGONAL 3X3 IF REQUESTED
C
      DO 280 I = 1,3
      I3  = 0
      DO 270 J = 1,3
      VOUT(J  ) =    T1(J+I3)
  270 VOUT(J+3) = RBVEC(J+I3)
      I3  = I3 + 3
      II  = 1
      JJ  = 6
      CALL PACK (VOUT,OUTVEC,TRL)
  280 COL = COL + 1
      GO TO 310
C
C     OR SIMPLY PACK THE TRANSFORMATION
C
  290 INDEX = COL
      DO 300 I = 1,3
      II  = INDEX
      JJ  = INDEX + 2
      CALL PACK (T1(I*3-2),OUTVEC,TRL)
  300 COL = COL + 1
C
C    STORE LOWER 3X3, AND GET NEXT GRID
C
  310 DO 320 I = 1,3
      II  = INDEX + 3
      JJ  = INDEX + 5
      CALL PACK (T1(I*3-2),OUTVEC,TRL)
  320 COL = COL + 1
      GO TO 440
C
C     CYLINDRICAL
C
  330 T2(1) = V(1)/RAD
      T2(4) =-V(2)/RAD
      T2(7) = ZERO
      T2(2) =-T2(4)
      T2(5) = T2(1)
      T2(8) = ZERO
      T2(3) = ZERO
      T2(6) = ZERO
      T2(9) = ONE
      GO TO 350
C
C     SPHERICAL
C
  340 XL    = SQRT(V(1)*V(1) + V(2)*V(2) + V(3)*V(3))
      IF (XL .LE. 0.0) GO TO 1060
      T2(1) = V(1)/XL
      T2(4) =(V(1)*V(3))/(RAD*XL)
      T2(7) =-V(2)/RAD
      T2(2) = V(2)/XL
      T2(5) =(V(2)*V(3))/(RAD*XL)
      T2(8) = V(1)/RAD
      T2(3) = V(3)/XL
      T2(6) =-RAD/XL
      T2(9) = ZERO
C
  350 CALL GMMATD (T1,3,3,0, T2,3,3,0, T)
      IF (.NOT.RBV) GO TO 400
C
C     ADD RIGID BODY INFORMATION TO LOWER OFF DIAGONAL 3X3 IF REQUESTED
C     THEN PACK
C
      INDEX = 1
      CALL GMMATD (RVEC,3,3,0, T,3,3,0, RBVEC)
      DO 390 I = 1,3
      I3  = 0
      DO 380 J = 1,3
      VOUT(J  ) =     T(J+I3)
  380 VOUT(J+3) = RBVEC(J+I3)
      I3  = I3 + 3
      II  = 1
      JJ  = 6
      CALL PACK (VOUT,OUTVEC,TRL)
  390 COL = COL + 1
      GO TO 420
C
C     OR SIMPLY PACK THE TRANSFORMATION
C
  400 INDEX = COL
      DO 410 I = 1,3
      II  = INDEX
      JJ  = INDEX + 2
      CALL PACK (T(I*3-2),OUTVEC,TRL)
  410 COL = COL + 1
C
C     STORE LOWER 3X3
C
  420 DO 430 I = 1,3
      II  = INDEX + 3
      JJ  = INDEX + 5
      CALL PACK (T(I*3-2),OUTVEC,TRL)
  430 COL = COL + 1
C
  440 CONTINUE
C
      CALL CLOSE  (OUTVEC,1)
      CALL WRTTRL (TRL)
      RETURN
C
C     ERRORS
C
 1000 WRITE  (NOUT,1010) UFM
 1010 FORMAT (A23,'.  MISSING REQUIRED OUTPUT FILE')
      GO TO  1500
 1020 WRITE  (NOUT,1030) UFM,TRL(1)
 1030 FORMAT (A23,'.  MISSING REQUIRED INPUT FILE',I4)
      GO TO  1500
 1040 WRITE  (NOUT,1050) UFM,NAME
 1050 FORMAT (A23,'. INPUT FILE ',2A4,' ERROR')
      GO TO  1500
 1060 WRITE  (NOUT,1070) UFM
 1070 FORMAT (A23,' FROM GRBVEC. ZERO RADIAL LENGTH, ERROR AT 340')
      GO TO  1500
 1100 J = -8
      GO TO  1490
 1200 J = -1
      GO TO  1490
 1300 J = -2
      GO TO  1490
 1320 J = TRL2
      GO TO  1350
 1330 J = NENT4
      GO TO  1350
 1340 J = NCST14
 1350 WRITE  (NOUT,1360) SFM,NAME,J,FLAG
 1360 FORMAT (A25,'. EXPECTED RECORD LENGTH DOES NOT MATCH ACTUAL ',
     1       ' RECORD LENGTH ON INPUT FILE ',2A4, /5X,2I10)
      GO TO  1500
 1400 WRITE  (NOUT,1410) UFM,OCID
 1410 FORMAT (A23,'. UNABLE TO FIND COORDINATE SYSTEM ',I8)
      GO TO  1500
C
 1490 CALL MESAGE (J,FILE,SUB)
 1500 CALL MESAGE (-61,0,SUB)
      RETURN
      END
