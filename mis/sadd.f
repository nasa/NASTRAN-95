      SUBROUTINE SADD (Z,DZ)
C
C     TO COMPUTE MATRIX SUM WITH MULTIPLIERS
C         ACCEPTS 1 TO 5 MATRIX BLOCKS PASSED ON VIA /SADDX/
C     COMMON BLOCK /SADDX/ NOMAT,LCORE,MCBS(60),MC(7)
C         NOMAT - NUMBER OF MATRICES INPUT
C         LCORE - LENGTH OF Z ARRAY (OPEN CORE)
C         MCBS  - MATRIX CONTROL BLOCKS AND MULTIPLIERS
C                 (12 WORDS/MATRIX)
C
C                 1 - FILE NAME            7 - NOT USED
C                 2 - NUMBER OF COLUMN     8 - TYPE OF MULTIPLIER
C                 3 - NUMBER OF ROW        9 - MULTIPLIER   *  LENGTH
C                 4 - FORM OF MATRIX      10 - MULTIPLIER   *  DEPENDS
C                 5 - TYPE OF MATRIX      11 - MULTIPLIER   *  ON THE
C                 6 - MAXIMUM NUMBER OF   12 - MULTIPLIER   *  TYPE
C                     NON-ZERO ELEMENTS
C
C         MC    - MATRIX CONTROL BLOCK OF THE OUTPUT
C
      INTEGER          END    ,EOL    ,HOP    ,NAME(2),ONE    ,PRC    ,
     1                 PREC   ,RC     ,SYSBUF ,TYPE   ,TYPIN  ,TYPOUT
      REAL             AMCB(1),ALPH(1),Z(1)
      DOUBLE PRECISION DA(2)  ,DALPH(10)      ,DMCB(1),DZ(1)
      COMMON /PACKX /  TYPIN  ,TYPOUT ,ONE    ,N      ,INCR
      COMMON /SADDX /  NOMAT  ,LCORE  ,MCBS(60)       ,MC(7)
      COMMON /SYSTEM/  SYSBUF ,NOUT
      COMMON /TYPE  /  PRC(2) ,NWDS(4),RC(4)
      COMMON /ZNTPKX/  A(4)   ,II     ,EOL
      EQUIVALENCE      (AMCB(1),MCBS(9)),(ALPH(1),DALPH(1)),
     1                 (DA(1) ,A(1))  ,(DMCB(1),MCBS(9)),
     2                 (NTYPE ,MC(5)) ,(NROW,   MC(3))
      DATA    NAME  /  4HSADD ,4H    /
C
C
      END   = (NOMAT-1)*12 + 1
      PREC  = -NOMAT*2
      TYPE  = -NOMAT*2
C
C     DETERMINE PRECISION TO BE USED FOR CALCULATIONS
C
C     NOTE - PRC ARRAY IS DIMENSIONED ONLY TO 2
C            PRC(1) = 1, PRC(2) = 2, AND
C            PRC(3) = NWDS(1) = 1, PRC(4) = NWDS(2) = 2
C            WHERE 1 MEANS S.P., 2 D.P.
C          - RC ARRAY = 1,1,2,2, WHERE 1 MEANS REAL, 2 COMPLEX
C
      DO 20 I = 1,END,12
      IF (MCBS(I) .NE. 0) GO TO 10
      PREC  = PREC + 2
      TYPE  = TYPE + 2
      GO TO 20
   10 J     = MCBS(I+4)
      PREC  = PREC  + PRC(J)
      TYPE  = TYPE  + RC(J)
      J     = MCBS(I+7)
      PREC  = PREC  + PRC(J)
      TYPE  = TYPE  + RC(J)
   20 CONTINUE
      TYPIN = 1
      IF (TYPE .GT. 0) TYPIN = 3
      IF (PREC .GT. 0) TYPIN = TYPIN + 1
      NUM = NROW*NWDS(TYPIN)
      IF (LCORE .LT. (NOMAT+1)*SYSBUF+NUM+1) CALL MESAGE (-8,0,NAME)
C
C     MOVE AND CONVERT MULTIPLIERS
C
      IF (PREC .GT. 0) GO TO 60
C
C     SINGLE PRECISION
C
      J = 1
      DO 50 I = 1,END,12
      K = MCBS(I+7)
      IF (PRC(K) .EQ. 2) GO TO 30
      ALPH(J  ) = AMCB(I  )
      ALPH(J+1) = AMCB(I+1)
      GO TO 40
   30 K = I/2 + 1
      ALPH(J  ) = DMCB(K  )
      ALPH(J+1) = DMCB(K+1)
   40 J = J + 1
      IF (TYPE .GT. 0) J = J + 1
   50 CONTINUE
      IF (TYPE .LE. 0) ALPH(J+1) = 0.0
      GO TO 100
C
C     DOUBLE PRECISION
C
   60 J = 1
      DO 90 I = 1,END,12
      K = MCBS(I+7)
      IF (PRC(K) .EQ. 2) GO TO 70
      DALPH(J  ) = AMCB(I  )
      DALPH(J+1) = AMCB(I+1)
      GO TO 80
   70 K = I/2 + 1
      DALPH(J  ) = DMCB(K  )
      DALPH(J+1) = DMCB(K+1)
   80 J = J + 1
      IF (TYPE .GT. 0) J = J + 1
   90 CONTINUE
      IF (TYPE .LE. 0) DALPH(J+1) = 0.0D+0
C
  100 GO TO (110,120,130,140), TYPIN
  110 ASSIGN 300 TO HOP
      GO TO  150
  120 ASSIGN 350 TO HOP
      GO TO  150
  130 ASSIGN 400 TO HOP
      GO TO  150
  140 ASSIGN 450 TO HOP
C
C     OPEN AND ASSIGN FILES
C
  150 IBUF = LCORE
      DO 160 I = 1,END,12
      IBUF = IBUF - SYSBUF
      IF (MCBS(I) .EQ. 0) GO TO 160
      CALL GOPEN (MCBS(I),Z(IBUF),0)
  160 CONTINUE
      IBUF = IBUF - SYSBUF
      CALL GOPEN (MC,Z(IBUF),1)
C
C     SETUP PACK PARAMETERS
C
      ONE    = 1
      N      = NROW
      TYPOUT = NTYPE
      INCR   = 1
      NCOL1  = MC(2)
      MC(2)  = 0
      MC(6)  = 0
      MC(7)  = 0
C
C     ADD MATRICES
C
      DO 1000 I = 1,NCOL1
C
C     CLEAR CORE
C
      DO 210 J = 1,NUM
  210 Z(J) = 0.0
C
      ONE = N
      N   = 1
      DO 900 J = 1,NOMAT
      K   = 12*(J-1) + 1
      IF (MCBS(K  ) .EQ. 0) GO TO 900
      IF (MCBS(K+1) .LT. I) GO TO 900
      CALL INTPK (*900,MCBS(K),0,TYPIN,0)
C
C     READ IN NON ZERO ELEMENT
C
  220 CALL ZNTPKI
      IF (II .GT. NROW) GO TO 500
      ONE = MIN0(ONE,II)
      N   = MAX0(N  ,II)
      GO TO HOP, (300,350,400,450)
  300 Z(II) = Z(II) + ALPH(J)*A(1)
      GO TO 500
  350 DZ(II) = DZ(II) + DALPH(J)*DA(1)
      GO TO 500
  400 II = II + II - 1
      JJ = J  + J  - 1
      Z(II  ) = Z(II)  +  ALPH(JJ)*A(1) - ALPH(JJ+1)*A(2)
      Z(II+1) = Z(II+1)+  ALPH(JJ)*A(2) + ALPH(JJ+1)*A(1)
      GO TO 500
  450 II = II + II - 1
      JJ = J  + J  - 1
      DZ(II  ) = DZ(II  ) + DALPH(JJ)*DA(1) - DALPH(JJ+1)*DA(2)
      DZ(II+1) = DZ(II+1) + DALPH(JJ)*DA(2) + DALPH(JJ+1)*DA(1)
  500 IF (EOL .EQ. 0) GO TO 220
  900 CONTINUE
C
C     END OF COLUMN
C
      ONE = MIN0(ONE,N)
      LL  = (ONE-1)*NWDS(TYPIN) + 1
      CALL PACK (Z(LL),MC(1),MC)
 1000 CONTINUE
C
C     DONE - CLOSE FILES AND RETURN
C
      DO 1010 I = 1,END,12
      IF (MCBS(I) .NE. 0) CALL CLOSE (MCBS(I),1)
 1010 CONTINUE
      CALL CLOSE (MC,1)
      RETURN
      END
