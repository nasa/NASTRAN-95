      SUBROUTINE OPT2A (IP,EL,IEL,PR,IPR,RR)
C
      LOGICAL         FIRST,UNSAFE
      INTEGER         COUNT,ETYP,IEL(1),IP(2,1),IPR(1),IZ(10),NAME(2),
     1                OES1,OUTTAP,PEST,PSTRES,PTELT,ZCOR,OLDTYP,EID(20),
     2                PLUS(5),IY(1)
      REAL            EL(1),PR(1),RR(1),Y(1),PARM(8)
      CHARACTER       UFM*23,UWM*25,UIM*29
      COMMON /XMSSG / UFM,UWM,UIM
      COMMON /BLANK / SKP(2),COUNT,SKQ(2),KORE,SKR(2),NWDSE,NWDSP,SKS,
     1                OES1,SKT(3),NELW,NPRW,SKU,NTOTL,CONV
      COMMON /OPTPW2/ ZCOR,Z(16)
      COMMON /ZZZZZZ/ CORE(1)
      COMMON /NAMES / NRD,NOEOR,NWRT,NEXT
      COMMON /SYSTEM/ SYSBUF,OUTTAP
C     EQUIVALENT ARE  (EL,IEL), (PR,IPR)
      EQUIVALENCE     (Z(1),IZ(1)), (CORE(1),PARM(1),MAX),
     1                (IY(1),Y(1),PARM(8))
      DATA    NAME  / 4H OPT,4H2A   /
      DATA    PLUS  / 4H    , 4H+   , 4H++  , 4H+++ , 4H++++ /
C
      NELR  = 0
      NE    = 0
      PTELT = 0
      IDEL  = 0
      KEL   = KORE
      KCONV = 0
      CONV  = 1.0
      ICP   = NTOTL - 4
      FIRST =.TRUE.
C
C     READ HEADER, ODD RECORDS
C
      GO TO 10
    5 CALL FREAD (OES1,0,0,NEXT)
   10 CALL READ  (*630,*100,OES1,Z(1),10,NEXT,I)
      ETYP  = IZ(3)
      NESW  = IZ(10)
      OLDTYP= PTELT
      PTELT = IY(ETYP)
      IF (PTELT .GT. 0) GO TO 15
C
C     ELEMENT TYPE NOT TO OPTIMIZE
C
      GO TO 5
   15 IF (PTELT.GE.OLDTYP .OR. OLDTYP.EQ.0) GO TO 20
      IF (KEL .NE. -1) KEL = KORE
      IF (NE  .EQ.  0) GO TO 16
      CALL PAGE2 (1)
      WRITE (OUTTAP,580) (EID(J),J=1,NE)
      NE = 0
   16 WRITE  (OUTTAP,17)
   17 FORMAT (/5X,15HNEXT SUBCASE...)
C
C     SET POINTERS TO ELEMENT TYPE AND PROPERTIES IN CORE.
C     L = LOCATION OF FIRST, M = MAX LOCATION
C
   20 LEL = IP(1,PTELT)
      MEL = IP(1,PTELT+1) - 1
      IF (MEL .LE. LEL) GO TO 5
      LOCE  = LEL
      LOCP1 = IP(2,PTELT) - 1
      IF (NESW .GT. ZCOR) GO TO 70
C
C     SEQUENTIALLY READ ONE ELEMENT FROM EVEN NUMBERED RECORDS.
C     LOCE IS CURRENT ELEMENT TO COMPARE TO.
C
   30 CALL READ (*90,*10,OES1,Z(1),NESW,NOEOR,I)
      IDES = IZ(1)/10
   50 IF (IDES .EQ. IEL(LOCE)) GO TO 110
C
C     SCAN THE CORE FILE UNTIL ELEMENT ID .GT. IDES
C
      IF (IDES .LT. IEL(LOCE)) GO TO 30
C
C     CORE ELEMENT NOT TO BE OPTIMIZED
C
      LOCE = LOCE + NWDSE
      IF (LOCE .LT. MEL) GO TO 50
C
C     END OF ELEMENT SEARCH FOR THIS TYPE (EOR NOT READ)
C
      GO TO 5
C
C     ELEMENT TYPE EXCEEDS CORE
C
   70 IER = -8
      IFLE = NESW - ZCOR
      GO TO 105
C
C     ILLEGAL EOF, EOR
C
   90 IER = -2
      GO TO 101
  100 IER = -3
  101 IFLE = OES1
C
  105 CALL MESAGE (IER,IFLE,NAME)
C
C     PROCES THIS ELEMENT
C
  110 CONTINUE
      NELR = NELR + 1
      LOCP = IEL(LOCE+4) + LOCP1
      PEST = IPR(LOCP+1)/100
      MEST = IPR(LOCP+1) - PEST*100
      RC   = 1.0
      X1A  = 0.0
      X2A  = 0.0
      E1   = 999.
      UNSAFE = .FALSE.
C
      GO TO (160,160,180,150,150,150,140,140,140,120,
     1       130,140,140,140,170,150,140,120,140,140), PTELT
C
C     ROD, TUBE
C
  120 LIMIT  = 1
      PSTRES = 4
      ASSIGN 121 TO IRET
      GO TO 500
  121 LIMIT  = 2
      PSTRES = 2
      ASSIGN  540 TO IRET
      GO TO 500
C
C     SHEAR
C
  130 LIMIT  = 1
      PSTRES = 2
      ASSIGN  540 TO IRET
      GO TO 500
C
C     TRBSC, TRPLT, QDPLT, TRIA1, TRIA2, TRIA3, QUAD1, QUAD2, QUAD4
C
  140 IF (MEST .EQ. 1) GO TO 144
      LIMIT  = 2
      PSTRES = 7
      ASSIGN 141 TO IRET
      GO TO 500
  141 PSTRES = 8
      ASSIGN 142 TO IRET
      GO TO 500
  142 PSTRES = 15
      ASSIGN 143 TO IRET
      GO TO 500
  143 PSTRES = 16
      ASSIGN 144 TO IRET
      X1A = AMAX1(ABS(Z( 7)),ABS(Z( 8)))
      X2A = AMAX1(ABS(Z(15)),ABS(Z(16)))
      X1A = AMAX1(X1A,X2A)
      K   = 0
      IF (X1A.EQ.ABS(Z(8)) .OR. X1A.EQ.ABS(Z(15))) K = 1
      X1A = Z( 7+K)
      X2A = Z(16-K)
      GO TO 500
  144 IF (MEST .EQ. 2) GO TO 540
      LIMIT  = 1
      PSTRES = 9
      ASSIGN 145 TO IRET
      GO TO 500
  145 PSTRES = 17
      ASSIGN  540 TO IRET
      GO TO 500
C
C     TRMEM, QDMEM, QDMEM1, QDMEM2
C
  150 IF (MEST .EQ. 1) GO TO 152
      LIMIT  = 2
      PSTRES = 6
      ASSIGN 151 TO IRET
      GO TO 500
  151 PSTRES = 7
      ASSIGN 152 TO IRET
      GO TO 500
  152 IF (MEST .EQ. 2) GO TO 30
      LIMIT  = 1
      PSTRES = 8
      ASSIGN  540 TO IRET
      GO TO 500
C
C     BAR, ELBOW
C
  160 LIMIT  = 2
      PSTRES = 7
      X2A = ABS(Z(7))
      ASSIGN 161 TO IRET
      GO TO 500
  161 PSTRES = 8
      X1A = ABS(Z(8))
      ASSIGN 162 TO IRET
      GO TO 500
  162 PSTRES = 14
      ASSIGN 163 TO IRET
      GO TO 500
  163 PSTRES = 15
      ASSIGN  540 TO IRET
      GO TO 500
C
C     TRIM6
C
  170 IF (IEL(LOCE) .EQ. IDEL) GO TO 172
      IDEL = IEL(LOCE)
      ICP  = ICP + 4
      IF (KEL.NE.-1 .AND. ICP.GE.KEL) CALL MESAGE (-8,0,NAME)
      IY(ICP) = LOCP
      IY(ICP+4) =-1
  172 K  = 0
      M1 =-1
      DO 175 I = 1,3
      M1 = M1 + 7
      II = 3 + LOCE
      S1S = 0.0
      S3S = 0.0
      IF (MEST .NE. 2) S3S = ABS(Z(M1+2)/EL(II))
      II = II - 2
      IF (Z(M1) .LT. 0.0) II = II + 1
      IF (MEST .NE. 1) S1S = ABS(Z(M1)/EL(II))
      II = 1 + LOCE
      IF (Z(M1+1) .LT. 0.0) II = II + 1
      S2S = ABS(Z(M1+1)/EL(II))
      S13 = AMAX1(S1S,S2S)
      S13 = AMAX1(S13,S3S)
      Y(ICP+I) = AMAX1(Y(ICP+I),S13)
      PR(LOCP+4) = AMAX1(PR(LOCP+4),S13)
      E1 = ABS(S13) - 1.0
      IF (ABS(E1) .LE. PARM(2)) K = K + 1
  175 CONTINUE
      ASSIGN 540 TO IRET
      IF (K-3) 550,520,520
C
C     IS2D8
C
  180 M1  = 1
      S1S = 0.0
      S2S = 0.0
      S3S = 0.0
      DO 185 M = 1,8
      M1 = M1 + 5
      II = 3 + LOCE
      IF (MEST .NE. 2) S3S = AMAX1(S3S,ABS(Z(M1+2)/EL(II)))
      II = II - 2
      IF (Z(M1) .LT. 0.0) II = II + 1
      IF (MEST .NE. 1) S1S = AMAX1(S1S,ABS(Z(M1)/EL(II)))
      II = 1 + LOCE
      IF (Z(M1+1) .LT. 0.0) II = II + 1
      S2S = AMAX1(S2S,ABS(Z(M1+1)/EL(II)))
      S13 = AMAX1(S1S,S2S)
      S13 = AMAX1(S13,S3S)
  185 CONTINUE
      E1 = ABS(S13) - 1.0
      PR(LOCP+4) = AMAX1(PR(LOCP+4),S13)
      ASSIGN 540 TO IRET
      GO TO 520
C
C     FUNCTION E1  -  RATIO STRESS MINUS LIMIT DIVIDED BY LIMIT,
C     WITH RESET OF -ALPHA-
C     LOCP   = POINTER TO PID OF PROPERTY.
C     LOCE   = POINTER TO EID OF ELEMENT.
C     LIMIT  = 1=SHEAR, 2= COMPRESSION/TENSION.
C     PSTRES = CORRESPONDING STRESS, POINTER TO Z ARRAY.
C
  500 II = 3 + LOCE
      IF (LIMIT .EQ. 1) GO TO 510
      II = II - 2
      IF (Z(PSTRES) .LT. 0.0) II = II + 1
  510 IF (EL(II) .LE. 0.0) GO TO 530
C
C     POSITIVE LIMIT
C
      PR(LOCP+4) = AMAX1(PR(LOCP+4),ABS(Z(PSTRES)/EL(II)))
C
C                                        I
C                  NEGATIVE E1, SAFE     I    POSITIVE E1, UNSAFE
C                                        I
C   --+------+------+------+------+------+------+------------------- E1
C    UL     4P     3P     2P      P      0      P  (WHERE P=PARM(2),
C      ++++    +++    ++     +    I             I        UL=UNLOADED)
C            OVER DESIGNED        I REGION WHEREI  UNDER DESIGNED
C            REGION               I  AE1 .LE. P I          REGION
C                      (UNSAFE=.FALSE.)         I  (UNSAFE=.TRUE.)
C
      E1 = ABS(Z(PSTRES)/EL(II)) - 1.0
  520 IF (E1 .GT. PARM(2)) UNSAFE = .TRUE.
      IF (UNSAFE) KEL = -1
      AE1 = AMIN1(AE1,ABS(E1))
  530 GO TO IRET, (121,141,142,143,144,145,151,152,161,162,163,540)
C
  540 X1 = ABS(X1A)
      X2 = ABS(X2A)
      IF (X1.EQ.0.0 .OR. X2.EQ.0.0) GO TO 550
      X1A= AMIN1(X1A,X2A)
      X1 = AMIN1(X1,X2)/AMAX1(X1,X2)
      X1 = SIGN(X1,X1A)
      IF (ABS(X1) .GT. 1.0E-8) RC = X1
C
C     SAVE IN RR AN EMPIRICAL ALPHA MODIFIER FOR SPEEDY CONVERGENCE
C
  550 IRR = (LOCP+NWDSP)/NWDSP
      RR(IRR) = RC
C
      IF (UNSAFE) GO TO 30
C
C     PRINT ELEMENT IDS THAT HAVE CONVERGED, OR OVER DESIGNED
C
      IF (.NOT.FIRST) GO TO 570
      FIRST = .FALSE.
      CALL PAGE2 (-3)
      WRITE  (OUTTAP,560) UIM
  560 FORMAT (A29,' 2304A, THE FOLLOWING ELEMENTS EITHER CONVERGED (NO',
     1       ' PLUS) OR OVER-DESIGNED (PLUS(ES))',/5X,'IN ONE OR MORE ',
     2       'SUBCASES,  (EACH PLUS INDICATES AN INCREMENTAL PERCENTAGE'
     3,      ' OF OVER-DESIGN BASED ON CONVERGENCE CRITERION, EPS)',/)
  570 XSTAR = (PR(LOCP+4)-1.0) - PARM(2)
      J  = IFIX(ABS(XSTAR)/PARM(2))
      IF (J .GT. 3) J = 3
      II = 1
      IF (PR(LOCP+4) .LT. 1.0E-8) II = 0
      IF (II .EQ. 0) J = 4
      EID(NE+1) = IEL(LOCE)
      EID(NE+2) = PLUS(J+1)
      NE = NE + 2
      IF (NE .LT. 20) GO TO 590
      NE = 0
      CALL PAGE2 (1)
      WRITE  (OUTTAP,580) EID
  580 FORMAT (5X,10(I8,A4))
  590 IF (KEL .EQ. -1) GO TO 30
      KEL = KEL - 1
CWKBR 9/93 IZK = IZ(KEL)
      IZK = IY(KEL)
      IF (PR(LOCP+3) .LT. 1.0E-6) II = 0
      IF (J.GT.0 .AND. IZK.EQ.-1 .AND. II.NE.0) KCONV = KCONV - 1
      IF (II .EQ. 0) GO TO 600
      IF (AE1 .GT. PARM(2)) GO TO 30
  600 IF (IEL(LOCE) .EQ. IZK) GO TO 30
      IF (AE1.LE.PARM(2) .AND. IZK.EQ.-1) GO TO 610
      IF (II.EQ.0 .AND. IZK.EQ.-1) GO TO 30
CWKBR 9/93 IZ(KEL) = IEL(LOCE)
      IY(KEL) = IEL(LOCE)
CWKBR 9/93 IF (II .EQ. 0) IZ(KEL) = -1
      IF (II .EQ. 0) IY(KEL) = -1
      KCONV = KCONV + 1
      GO TO 30
CWKBR 9/93  610 IZ(KEL) = IEL(LOCE)
  610 IY(KEL) = IEL(LOCE)
      GO TO 30
C
C     EOF
C
  630 CONTINUE
      IF (NE .GT. 0) WRITE (OUTTAP,580) (EID(J),J=1,NE)
C
C     IF KEL=-1 HERE, OR
C     IF NUMBER OF ELEMENTS CONVERGED, KORE-KEL, IS LESS THAN NUMBER OF
C     ELEMENTS IN THE PROBLEM, NELW/NWDSE, CONVERGENCE IS INCOMPLETE
C
      IF (KEL .EQ. -1) GO TO 650
      IF (KCONV .LT. NELW/NWDSE) GO TO 650
CWKBR CALL PAGE (-4)
      CALL PAGE2 (-4)
      WRITE  (OUTTAP,640) UIM
  640 FORMAT (A29,' 2304B, CONVERGENCE ACHIEVED FOR ALL ELEMENTS ',
     1       'REQUESTED, AND IN ALL SUBCASE(S)', /5X,
     2       'FULLY-STRESSED DESIGN COMPLETED',/)
      CONV = 2.0
      GO TO 670
C
C     IF NELR IS ZERO, NO ELEMENT MATCH MADE
C
  650 IF (NELR .GT. 0) GO TO 670
      CALL PAGE2 (-2)
      WRITE  (OUTTAP,660) UFM
  660 FORMAT (A23,' 2295, NO ELEMENTS EXIST FOR OPTIMIZATION.')
      COUNT = MAX + 1
C
  670 RETURN
      END
