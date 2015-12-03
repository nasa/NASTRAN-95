      SUBROUTINE TRMEMD
C
C     THIS SUBROUTINE CALCULATES THE STIFFNESS AND MASS MATRICES FOR
C     THE  TRIANGULAR MEMBRANE ELEMENT.  CALCULATIONS ARE PERFORMED
C     PRIMARILY BY SUBROUTINES EKTRMS AND EMASTQ.
C     DOUBLE PRECISION VERSION
C
C     ECPT FOR THE TRMEM ELEMENT
C***********************************************************************
C INDEX   DESCRIPTION                                       TYPE
C *****   ***********                                       ****
C   1     ELEMENT ID                                         I
C   2-4   GRID POINTS A,B,AND C                              I
C   5     THETA = ANGLE OF MATERIAL                          R
C   6     MATERIAL ID                                        I
C   7     T                                                  R
C   8     NON-STRUCTURAL MASS                                R
C   9     COORDINATE SYSTEM ID 1                             I
C 10-12   X1,Y1,Z1                                           R
C  13     COORDINATE SYSTEM ID 2                             I
C 14-16   X2,Y2,Z2                                           R
C  17     COORDINATE SYSTEM ID 3                             I
C 18-20   X3,Y3,Z3                                           R
C  21     ELEMENT TEMPERATURE                                R
C***********************************************************************
      DOUBLE PRECISION  K,KOUT,M(9),MOUT(9),KSAVE
     1,                 A,PROD9,TEMP9,XSUB,BFACT,E
      LOGICAL NOGO,HEAT
      INTEGER ELID,ESTID, DICT(10), IPART(3), NECPT(50), NGRID(3)
C
      COMMON /SYSTEM /  KSYSTM (60)
      COMMON /EMGPRM / DM(15),ISMB(3),IPREC,NOGO,HEAT,ICMBAR
      COMMON /EMGDIC /  QQ(3), ELID, ESTID
      COMMON /EMGEST /  ECPT(50)
      COMMON /EMGTRX /  A(225),PROD9(9),TEMP9(9),XSUB(3),BFACT,
     X                  E(18), K(324), KOUT(324),KSAVE(81)
C
      EQUIVALENCE   (ECPT(1),NECPT(1),IELID), (DICT5,DICT(5))
      EQUIVALENCE   (K(1),M(1)),(KOUT(1),MOUT(1)),(KSYSTM(2),IOUTPT)
      EQUIVALENCE   (KSYSTM(56), IHEAT), (ECPT(2), NGRID(1))
C
      DATA  IPART / 1,2, 3/
C
C
C
      IP = IPREC
      DICT(1) = ESTID
C
C     CREATE AN ARRAY POINTING TO GRID POINTS IN INCREASING ORDER
C
  100 DO 140 I=1,2
      IP1 = I+1
      II =  IPART(I)
      DO 120  J=IP1,3
      JJ = IPART(J)
      IF (NGRID(II).LE. NGRID(JJ)) GOTO 120
      IPART(I) =JJ
      IPART(J) =II
      II = JJ
      GO TO 100
  120 CONTINUE
  140 CONTINUE
C
C     IF STIFFNESS MATRIX IS REQUESTED CALL EKTRMS. OTHERWISE GO TO
C     MASS MATRIX CALCULATION SECTION
C
      IF (ISMB(1) .EQ. 0 ) GO TO  300
C
      CALL EKTRMD (0)
C
      IF (NOGO) RETURN
C
C     RE-ORDER  THE STIFFNESS MATRIX BY INCREASING SIL VALUE
C
      IF (HEAT) GO TO 200
      DO 190 I=1,3
      II = IPART(I)
      DO 180 J=1,3
      JJ = IPART(J)
      DO 170 KA=1,3
      DO 160 L=1,3
      ISAVE = (II-1)*27 + (JJ-1) *9 + (KA-1)*3  + L
      IOUT = (I-1)*27 + (J-1)*3  +  (KA-1)*9  + L
  160 K(IOUT)=KSAVE(ISAVE)
  170 CONTINUE
  180 CONTINUE
  190 CONTINUE
C    OUTPUT THE MATRIX
      DICT(2)=1
      DICT(3)=9
      DICT(4)=7
C
      CALL EMGOUT(K,K,81,1,DICT,1,IP)
      GO TO 300
C
C     OUTPUT HEAT MATRIX HERE
C
  200 DO 260 I=1,3
      DO 240 J=1,3
      IOUT = (I-1)* 3+ J
      IK  =  (IPART(I)-1)* 3 + IPART(J)
  240 K(IOUT)=KSAVE(IK)
  260 CONTINUE
C     OUTPUT   HEAT  K
      DICT(2) = 1
      DICT(3) = 3
      DICT(4) = 1
C
      CALL EMGOUT (K,K,9,1,DICT,1,IP)
C
C     PERFORM MASS MATRIX CALCULATIONS HERE
C
  300 IF (ISMB(2)  .EQ.0) RETURN
C
C     CONVENTIONAL MASS MATRIX
C
      CALL EMADTQ (4,M)
C     REORDER THE MASS MATRIX
      IF (HEAT) GO TO 350
      DO 340 I=1,3
      II = (I-1)*3
      IJ = IPART(I)
      JJ = (IJ-1)*3
      DO  320 J=1,3
      IOUT = II + J
      IK = JJ + J
  320 MOUT(IOUT) =  M(IK)
  340 CONTINUE
C
      DICT(2) =2
      DICT(3) = 9
      DICT(4) = 7
C
      CALL EMGOUT (MOUT, MOUT, 9,1,DICT,2,IP)
      RETURN
C
C     HEAT FORMULATION
C
  350 DO 360 I=1,3
      J=IPART(I)
  360 MOUT(I)=M(J)
      DICT(2)=2
      DICT(3)=3
      DICT(4)=1
C
      CALL EMGOUT(MOUT,MOUT,3,1,DICT,2,IP)
      RETURN
C
      END
