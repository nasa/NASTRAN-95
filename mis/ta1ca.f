      SUBROUTINE TA1CA(KOZ)
C*****
C THIS ROUTINE, CALLED BY SUBROUTINE TA1C, COMPUTES THE S MATRIX OF A
C GENERAL ELEMENT FROM INFORMATION IN THE CSTM AND BGPDT DATA BLOCKS.
C SEE FMMS-57 FOR EQUATIONS.
C*****
      DOUBLE PRECISION
     1                   V(3)               ,T(9)
     2,                  E(18)              ,D(42)
     3,                  S(6)               ,DET
     4,                  B(6)               ,INDEX(18)
     5,                  DD(30)             ,DL(25)
     6,                  DU(25)
C
C
C
      INTEGER
     1                   CSTM               ,BGPDT
     2,                  GEI                ,FILE
     3,                  CLSRW              ,EOR
     4,                  BUFR1              ,BUFR2
     5,                  BUFR3
C
C
C
      DIMENSION
     1                   NAME(2)            ,SSP(6)
     2,                  Z(1)
     3,                  LROW(5)            ,ICOL(6)
C
C
C
      COMMON   /TA1COM/
     1                   DUM3(3)            ,BGPDT
     2,                  DUM2(2)            ,CSTM
     3,                  DUM22(2)           ,GEI
C
C OPEN CORE
C
      COMMON   /ZZZZZZ/
     1                   IZ(1)
C
C
C
      COMMON   /NAMES /
     1                   DUMMY1             ,INRW
     2,                  DUMMY2             ,OUTRW
     3,                  CLSRW
C
C
C
      COMMON   /TAC1AX/
     1                   BUFR1              ,BUFR2
     2,                  BUFR3              ,IUI
     3,                  NUI                ,IUD
     4,                  NUD                ,IZZZ
     5,                  NOGO               ,IDGENL
C
C
C
      EQUIVALENCE
     1                   (Z(1),IZ(1))
C
C
C
      DATA     EOR,NEOR /1,0/
      DATA     NAME(1)/4HTA1C/ , NAME(2)/4HA   /
C
C INITIALIZE
C
      NCSTM = 0
      ICSTM = IZZZ
      LEFT  = BUFR3 - ICSTM
C
C ATTEMPT TO OPEN THE CSTM
C
      FILE = CSTM
      CALL OPEN(*20,CSTM,Z(BUFR3),INRW)
      CALL FWDREC(*9020,CSTM)
      CALL READ(*9020,*10,CSTM,Z(ICSTM+1),LEFT,EOR,NCSTM)
      CALL MESAGE (-8,0,NAME(1))
   10 CALL CLOSE (CSTM,CLSRW)
C
C PRETRD SETS UP SUBSEQUENT CALLS TO TRANSD
C
      CALL PRETRD (Z(ICSTM+1),NCSTM)
      LEFT = LEFT - NCSTM
C
C READ THE BGPDT INTO CORE
C
   20 IBGPDT = ICSTM + NCSTM
      FILE = BGPDT
      CALL OPEN(*9010,BGPDT,Z(BUFR3),INRW)
      CALL FWDREC(*9020,BGPDT)
      CALL READ(*9020,*30,BGPDT,Z(IBGPDT+1),LEFT,EOR,NBGPDT)
      CALL MESAGE (-8,0,NAME(1))
   30 CALL CLOSE (BGPDT,CLSRW)
C
C ZERO OUT THE E MATRIX
C
      DO 40 I = 1,18
   40 E(I) = 0.0D0
      E(1)  = 1.0D0
      E(8)  = 1.0D0
      E(15) = 1.0D0
      IND = 0
   50 IND = IND + 1
C*****
C IF IND = 1, THE D MATRIX IS FORMED IN THE DO 200 LOOP.
C IF IND = 2, THE S MATRIX IS FORMED AND OUTPUT A ROW AT A TIME IN THE
C DO LOOP.
C*****
      IF (IND - 2) 60,70,300
   60 CONTINUE
C
C     IF STIFFNESS IS INPUT,CALCULATE LIM
C
      IF (KOZ.EQ.1) GO TO 65
      LIM = 6
      LIMA = 6
      IBEG = IUD
      GO TO 80
   65 LIM = (NUD - IUD) / 4 + 1
      LIMA = LIM
      IBEG = IUD
      GO TO 80
   70 LIM = (NUI - IUI) / 4  +  1
      IBEG = IUI
      IROW = 37
   80 J = IBEG - 2
      I = 1
   85 CONTINUE
      IF (IND .EQ. 1) IROW = 6*I - 5
      J = J + 4
      JJ = IZ(J+1)
      K = IBGPDT + 4*(IZ(J) - 1)
C
C COMPUTE THE V VECTOR
C
      V(1) = 0.0D0
      V(2) = 0.0D0
      V(3) = 0.0D0
      KK = JJ
      IF (JJ .GT. 3) KK = JJ - 3
      IF (IZ(K+1) .EQ. 0) GO TO 120
      CALL TRANSD (IZ(K+1),T)
      GO TO (90,100,110), KK
   90 V(1) = T(1)
      V(2) = T(4)
      V(3) = T(7)
      GO TO 130
  100 V(1) = T(2)
      V(2) = T(5)
      V(3) = T(8)
      GO TO 130
  110 V(1) = T(3)
      V(2) = T(6)
      V(3) = T(9)
      GO TO 130
  120 V(KK) = 1.0D0
C
C FORM THE E MATRIX IF THE DEGREE OF FREEDOM IS A TRANSLATION.
C
  130 IF (JJ .GT. 3) GO TO 150
      E( 5) =  Z(K+4)
      E( 6) = -Z(K+3)
      E(10) = -Z(K+4)
      E(12) =  Z(K+2)
      E(16) =  Z(K+3)
      E(17) = -Z(K+2)
      IF (IZ(K+1) .EQ. 0) GO TO 140
      CALL GMMATD (V,3,1,1, E,3,6,0, D(IROW) )
      GO TO 180
  140 IEROW = 6*JJ - 5
      D(IROW  ) = E(IEROW  )
      D(IROW+1) = E(IEROW+1)
      D(IROW+2) = E(IEROW+2)
      D(IROW+3) = E(IEROW+3)
      D(IROW+4) = E(IEROW+4)
      D(IROW+5) = E(IEROW+5)
      GO TO 180
C
C THE DEGREE OF FREEDOM IS A ROTATION.
C
  150 LL = IROW
      DO 160 L = 1,6
      D(LL) = 0.0D0
  160 LL = LL + 1
      IF (IZ(K+1) .EQ. 0) GO TO 170
      D(IROW+3) = V(1)
      D(IROW+4) = V(2)
      D(IROW+5) = V(3)
      GO TO 180
  170 LL = IROW + JJ - 1
      D(LL) = 1.0D0
C
C IF IND = 2 FORM A ROW OF THE S MATRIX AND WRITE IT OUT.
C
  180 IF (IND .EQ. 1) GO TO 200
C
C     IF STIFFNESS MATRIX INPUT AND LESS THAN 6 RIGID BODY DEGREES OF
C     FREEDOM, BRANCH
C
      IF (KOZ.EQ.1.AND.LIMA.LT.6) GO TO 410
      CALL GMMATD (D(37),6,1,1, D(1),6,6,0, S(1) )
      DO 190 L = 1,6
  190 SSP(L) = S(L)
      CALL WRITE (GEI,SSP,6,NEOR)
  195 CONTINUE
  200 CONTINUE
      I = I + 1
      IF (I.LE.LIM) GO TO 85
      IF (IND .NE. 1) GO TO 300
C
C     IF STIFFNESS MATRIX WAS INPUT AND LESS THAN 6 RIGID BODY DEGREES
C     OF FREEDOM, BRANCH
C
      IF (KOZ.EQ.1.AND.LIM.LT.6) GO TO 310
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ISING = -1
      CALL INVERD (6,D(1),6,B(1),0,DET,ISING,INDEX(1))
      IF (ISING .EQ. 1) GO TO 50
      NOGO = 1
      CALL MESAGE (30,82,IDGENL)
      GO TO 300
  310 CONTINUE
C
C     SWITCH FROM ROW STORED TO COLUMN STORED
C
      DO 320 I=1,LIM
      DO 320 J=1,6
      INDXZ = I+(J-1)*LIM
  320 DD(INDXZ) = D(6*I+J-6)
C
C     DETERMINE RANK OF DD AND EXPRESS MATRIX OF MAXIMAL RANK AS A
C     PRODUCT OF TRIANGULAR FACTORS
C
      CALL DMFGR(DD,LIM,6,1.05E-05,IRANK,LROW,ICOL)
      IF (IRANK.EQ.LIM) GO TO 325
      NOGO = 1
      CALL MESAGE (30,152,IDGENL)
      GO TO 300
C
C     EXTRACT LOWER AND UPPER TRIANGULAR FACTORS, FORM PRODUCT,RESTORE
C     ROWS TO THEIR POSITION BEFORE FACTORIZATION AND INVERT. THEN
C     EXPAND MATRIX TO BE OF DIMENSION  6 BY IRANK
C
  325 IF (IRANK.EQ.1) GO TO 365
      DO 335 I=1,25
      DL(I) = 0.0D0
  335 DU(I) = 0.0D0
      DO 350 I=1,IRANK
      DO 350 J=1,IRANK
      IF (I.GT.J) GO TO 340
      IF (I.EQ.J) GO TO 330
      INDXZ = I+(J-1)*IRANK
      DL(INDXZ) = DD(I*IRANK+J-IRANK)
      GO TO 350
  330 INDXZ = I+(J-1)*IRANK
      DL(INDXZ) = 1.0D0
  340 INDXZ = I+(J-1)*IRANK
      DU(INDXZ) = DD(I*IRANK+J-IRANK)
  350 CONTINUE
      CALL GMMATD (DL(1),LIM,LIM,0,DU(1),LIM,LIM,0,DD)
      DO 360 I=1,LIM
      K = LROW(I)
      DO 360 J=1,LIM
      INDXZ = J+(K-1)*LIM
  360 D(INDXZ) = DD(I*LIM+J-LIM)
C     AGAIN NO NEED TO COMPUTE DETERMINANT
      ISING = -1
      CALL INVERD (LIM,D(1),LIM,B(1),0,DET,ISING,INDEX(1))
      IF (ISING.EQ.1) GO TO 370
      NOGO = 1
      CALL MESAGE (30,153,IDGENL)
      GO TO 300
  365 D(1) = 1.0D0/DD(1)
  370 K = LIM * LIM + 1
      J = LIM * 6
      DO 380  I = K,J
  380 D(I) = 0.0D0
      GO TO 50
  410 CONTINUE
C
C     REARRANGE COLUMNS TO AGREE WITH ORDER OF DD AFTER MATRIX FACTOR-
C     IZATION
C
      DO 420 L = 1,6
      LK = ICOL(L)
  420 B(L) = D(36+LK)
C
C     MULTIPLY DI BY THE EXPANDED INVERSE OF DD
C
      CALL GMMATD (B(1),1,6,0,D(1),6,LIMA,0,S(1))
C
C     WRITE OUT THIS ROW OF THE S MATRIX
C
      DO 430 L =1,LIMA
  430 SSP(L) = S(L)
      CALL WRITE (GEI,SSP,LIMA,NEOR)
      GO TO 195
  300 RETURN
C
C     ERROR MESSAGES
C
 9010 CALL MESAGE (-1,FILE,NAME(1))
 9020 CALL MESAGE (-1,FILE,NAME(1))
      RETURN
      END
