      SUBROUTINE OPT2B (IPR,PR,PL,RR)
C
      INTEGER         COUNT,IPR(1),OUTTAP,SYSBUF,IY(1)
      REAL            PL(1),PR(1),RR(1),Y(1),Z(8)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /BLANK / SKP1(2),COUNT,SKP2(6),NWDSP,
     1                SKP3(6),NPRW,NKLW,NTOTL,CONV
      COMMON /ZZZZZZ/ CORE(1)
      COMMON /SYSTEM/ SYSBUF,OUTTAP
      EQUIVALENCE     (CORE(1),Z(1),MAX), (EPS,Z(2)), (GAMA,Z(3)),
     1                (IPRNT,Z(7)), (IY(1),Y(1),Z(8))
C     EQUIVALENT ARE  (IPR,PR)
C
      NMES = 0
      CH   = 1.0
C
      DO 100 NP = 1,NPRW,NWDSP
      ALPH= PR(NP+4)
      I   = 1
      ICP = NTOTL - 4
    3 ICP = ICP+4
      IF (IY(ICP) .LE.  0) GO  TO 5
      IF (IY(ICP) .NE. NP) GO  TO 3
C
C     SPECIAL HANDLING OF TRIM6
C
    4 ALPH = Y(ICP+I)
C
    5 IF (ALPH) 70,40,10
C
C     POSITIVE ALPHA, CALCULATE PNEW
C
   10 IRR = (NP+NWDSP)/NWDSP
      IF (ABS(GAMA-1.0) .LT. 1.0E-4) CH = 0.25*RR(IRR) + 0.75
      PNEW = PR(NP+3)*((ALPH/(ALPH+(1.0-ALPH)*GAMA))**CH)
      IF (IPR(NP+5) .EQ. 0) GO TO 30
C
C     COMPARE TO LIMIT DATA
C
      KPL  = IPR(NP+5)
      DELP = PNEW/PR(NP+2)
      IF (DELP .LT. PL(KPL)) GO TO 20
      KPL = KPL + 1
      IF (DELP.LE.PL(KPL) .OR. PL(KPL).EQ.0) GO TO 30
C
C     RECALCULATE ALPHA, PNEW  BASED ON THE LIMIT
C
   20 PNEW = PR(NP+2)*PL(KPL)
      ALPH =-PNEW*GAMA/(PNEW*(1.0-GAMA)-PR(NP+3))
C
   30 PR(NP+4) = ALPH
      IF (NP .EQ. IY(ICP)) Y(ICP+I) = ALPH
      GO TO 80
C
C     ZERO STRESS INPUT, CHANGE ALPH TO 0.0001
C
   40 IF (IPRNT.EQ.0 .OR. NMES.GE.100) GO TO 60
      NMES = NMES + 1
      CALL PAGE2 (-2)
      WRITE  (OUTTAP,50) UWM,IPR(NP)
   50 FORMAT (A25,' 2303, FULLY-STRESSED DESIGN DETECTED ZERO STRESS ',
     1       'FOR PROPERTY',I9, /5X,'CHECK PROPERTY CARD OR UNLOADED ',
     2       'ELEMENT(S)')
   60 ALPH = 1.0E-4
      GO TO 10
C
C     NO CHANGE IN ALPH (-1.0 DETECTED)
C
   70 ALPH = -1.0E0
      IF (NP .EQ. IY(ICP)) GO TO 30
C
   80 IF (NP .NE. IY(ICP)) GO TO 100
      I = I + 1
      IF (I .LE. 3) GO TO 4
      ICP = ICP + 4
C
  100 CONTINUE
C
      RETURN
      END
