      SUBROUTINE DISPLA (GPLST,X,S,U,PEN,DEFORM,LABEL,PT,B1)
C
      INTEGER         GPLST(1),PEN,DEFORM,B1,SCR1,ECT2,AXIS,DAXIS,ELID,
     1                GPTS(12),GP,COLOR,OFFSET
      REAL            MAXDEF,X(3,1),U(3,1),S(2,1)
      DIMENSION       SIGN(3),A(4),PT(8),XX(4),YY(4),LABEL(50),MVECT(3),
     1                MSG(13)
      COMMON /BLANK / SKIP(5),NGPSET,SK(6),ECT2,SKP(7),MERR,SKI(6),SCR1
      COMMON /XXPARM/ SKIP1(39),MAXDEF,DEFMAX,AXIS(3),DAXIS(3),
     1                SKIP2(110),NCNTR,CNTR(50),ICNTVL,SKPPAR(6),
     2                SK18(18),COLOR
      COMMON /PLTDAT/ SKIP3(2),XMIN
      DATA    NMSG  / 13   /,
     1        MSG   / 4H(33X, 4H,41H, 4H*** , 4HINCO, 4HMPLE, 4HTE P,
     2                4HLOT , 4HDUE , 4HTO I, 4HNPUT, 4H OR , 4HFILE,
     3                4H.)  /
      DATA    MVECT / 3*0   /,        KBAR,KT3,KQ4 / 2HBR,2HT3,2HQ4 /
C
C
      CALL GOPEN (SCR1,GPLST(B1),1)
      IF (ABS(DEFMAX) .GT. 1.E-8) GO TO 5
      CALL WRTPRT (MERR,MVECT,MSG,NMSG)
      GO TO 120
C
    5 DO 10 I = 1,3
      SIGN(I) = DAXIS(I)/AXIS(I)
   10 CONTINUE
      DO 20 GP = 1,NGPSET
      DO 15 I  = 1,3
      J  = AXIS(I)
      IJ = IABS(J)
      A(IJ) = SIGN(IJ)*U(I,GP)
   15 CONTINUE
      DMAX = MAXDEF
      IF (DMAX .LT. .00001) DMAX = 1.0
      DO 16 I = 1,3
      U(I,GP) = A(I)*(DEFMAX/DMAX)
   16 CONTINUE
   20 CONTINUE
      INDEX = ICNTVL - 9
      NCNTR = MIN0(NCNTR,50)
      IF (CNTR(1) .NE. CNTR(2)) GO TO 40
      IF (INDEX .LE. 3) CONMIN = U(INDEX,1)
      IF (INDEX .GT. 3) CONMIN = SQRT(U(1,1)**2 + U(2,1)**2 + U(3,1)**2)
      CONMAX = CONMIN
      DO 30 GP = 1,NGPSET
      IF (INDEX .GT. 3) GO TO 25
      CONMIN = AMIN1(CONMIN,U(INDEX,GP))
      CONMAX = AMAX1(CONMAX,U(INDEX,GP))
      GO TO 30
   25 D = SQRT(U(1,GP)**2 + U(2,GP)**2 + U(3,GP)**2)
      CONMIN = AMIN1(CONMIN,D)
      CONMAX = AMAX1(CONMAX,D)
   30 CONTINUE
      DELTA = (CONMAX-CONMIN)/FLOAT(NCNTR-1)
      CNTR(1) = CONMIN
      J = NCNTR - 1
      DO 35 I = 2,J
      CNTR(I) = CNTR(I-1) + DELTA
   35 CONTINUE
      CNTR(NCNTR) = CONMAX
   40 CALL LINE (0.,0.,0.,0.,PEN,+1)
      DO 45 I = 1,NCNTR
      LABEL(I) = 3
   45 CONTINUE
   50 CALL READ  (*100,*100,ECT2,ITYPE,1,0,M)
      OFFSET = 0
      IF (ITYPE .EQ. KBAR) OFFSET = 6
      IF (ITYPE.EQ.KT3 .OR. ITYPE.EQ.KQ4) OFFSET = 1
      CALL FREAD (ECT2,NGPPE,1,0)
   55 CALL FREAD (ECT2,ELID,1,0)
      IF (ELID .EQ. 0) GO TO 50
      CALL FREAD (ECT2,0,-1,0)
      CALL FREAD (ECT2,GPTS,NGPPE,0)
      IF (OFFSET .NE. 0) CALL FREAD (ECT2,0,-OFFSET,0)
      IF (NGPPE .LE. 2) GO TO 55
      IJ = 1
      IK = 3
   60 J  = 0
      DO 70 I = IJ,IK
      J  = J + 1
      IG = GPTS(I)
      IG = IABS(GPLST(IG))
      IF (INDEX .LE. 3) A(J) = U(INDEX,IG)
      IF (INDEX .GT. 3) A(J) = SQRT(U(1,IG)**2 +U(2,IG)**2 +U(3,IG)**2)
      IF (DEFORM .NE. 0) GO TO 65
      PT(2*J-1) = X(2,IG)
      PT(2*J  ) = X(3,IG)
      GO TO 70
   65 PT(2*J-1) = S(1,IG)
      PT(2*J  ) = S(2,IG)
   70 CONTINUE
      PT(7) = PT(1)
      PT(8) = PT(2)
      A(4)  = A(1)
      DO 90 I = 1,NCNTR
      IF (COLOR .EQ. 0) GO TO 75
      J = IABS(COLOR)
      IF (NCNTR .LE. J) PEN = I*J/NCNTR
      IF (NCNTR .GT. J) PEN = 1 + I/(NCNTR/J)
      IF (PEN   .GT. J) PEN = J
   75 CONTINUE
      DO 80 J = 1,3
      XX(J) = XMIN - 1.0
      D = A(J) - A(J+1)
      IF (ABS(A(J  )-CNTR(I)).GT.ABS(D) .OR.
     1    ABS(A(J+1)-CNTR(I)).GT.ABS(D)) GO TO 80
      IF (D .EQ. 0.0) D = 1.0
      XX(J) = PT(2*J-1) + (PT(2*J+1)-PT(2*J-1))*(A(J)-CNTR(I))/D
      YY(J) = PT(2*J  ) + (PT(2*J+2)-PT(2*J  ))*(A(J)-CNTR(I))/D
   80 CONTINUE
      XX(4) = XX(1)
      YY(4) = YY(1)
      DO 85 J = 1,3
      IF (XX(J).LT.XMIN .OR. XX(J+1).LT.XMIN) GO TO 85
      CALL LINE (XX(J),YY(J),XX(J+1),YY(J+1),PEN,0)
      LABEL(I) = LABEL(I) + 1
      IF (LABEL(I) .NE. 4) GO TO 85
      LABEL(I) = 0
      CALL WRITE (SCR1,I,1,0)
      CALL WRITE (SCR1,XX(J),1,0)
      CALL WRITE (SCR1,YY(J),1,0)
   85 CONTINUE
   90 CONTINUE
      IF (NGPPE.EQ.3 .OR. IJ.EQ.3) GO TO 55
      GPTS(NGPPE+1) = GPTS(1)
      IJ = 3
      IK = 5
      GO TO 60
  100 CALL BCKREC (ECT2)
      DO 115 GP = 1,NGPSET
      DO 105 I  = 1,3
      J  = AXIS(I)
      IJ = IABS(J)
      A(I) = SIGN(I)*U(IJ,GP)
  105 CONTINUE
      DO 110 I = 1,3
      U(I,GP) = A(I)*(DMAX/DEFMAX)
  110 CONTINUE
  115 CONTINUE
  120 CALL CLOSE (SCR1,1)
      RETURN
      END
