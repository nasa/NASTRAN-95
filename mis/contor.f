      SUBROUTINE CONTOR (GPLST,X,U,DD,Z,IZ,PPEN,DEFORM,B1,OPCOR)
C
      INTEGER         OPCOR,IZ(1),GPLST(1),PPEN,PEN,DEFORM,PARM,EST,
     1                STRESS,SORT,SCR1,BUFSIZ,B1,B2,B3,ELID,ERR,SUB(2),
     2                COLOR,ELMID,GPTS(12),PEDGE,ESYM,OFFSET
      REAL            XB(8),X(3,1),DD(3,1),U(2,1),CENTER,RCNTRL,RCOLOR
      DIMENSION       LABL(50),Z(1),IBEGIN(2),PT(8)
      CHARACTER       UFM*23,UWM*25,UIM*29
      COMMON /XMSSG / UFM,UWM,UIM
      COMMON /BLANK / SKIP(10),PARM,SKP4,EST,SKP8(11),STRESS,SORT,
     1                NEWOES,SCR1
      COMMON /XXPARM/ SKIP2(157),NCNTR,CNTR(50),ICNTVL,SKIP6(5),ISET,
     1                SK18(18),COLOR,LAYER
      COMMON /PLTDAT/ SKIP3(2),XMIN
      COMMON /SYSTEM/ BUFSIZ,NOUT
      COMMON /DRWDAT/ JSET,SKIP7(14),PEDGE
      EQUIVALENCE     (IBEGIN(1),LINES),(IBEGIN(2),IGDPT)
      DATA    KBAR  , KT3,KQ4/ 2HBR,2HT3,2HQ4 /
      DATA    SUB   / 4HCONT , 4HOR           /
C
      B2  = B1 - 2*BUFSIZ
      B3  = B2 - BUFSIZ
      LOPCOR  = OPCOR/5
      ID  = 1
      ERR = 0
      IRR = 0
      NCNTR = IABS(NCNTR)
C
C     COLOR = 0 IS NO COLOR CONTOUR,
C     COLOR = 1 TO  31 IS DRAW CONTOUR LINES IN COLOR.
C     COLOR =-1 TO -31 ID COLOR FILL ELEMENTS BY STRESS
C
C     THIS IS THE CODE FOR THE COLOR BAR SCALE
C     AT THE TOP OF THE PLOT
C
      IF (COLOR .EQ. 0) GO TO 40
      CALL LINE (0.0,0.0,0.0,0.0,32,-1)
      ICOLOR = IABS(COLOR)
      RCOLOR = 535.0/ICOLOR
C              535.0 IS BASED ON 1000X1000 PLOT SCREEN COORDINATE FRAME
      DO 30 IB = 1,ICOLOR
      PEN   = IB + 31
      XB(1) = 368.14 + RCOLOR*(IB-1)
      XB(2) = 959.67
      XB(3) = XB(1) + RCOLOR
      XB(4) = XB(2)
      XB(5) = XB(3)
      XB(6) = 977.51
      XB(7) = XB(1)
      XB(8) = XB(6)
      IK    = 0
      IK1   = 1
      IK2   = 2
      IK3   = 3
      IK4   = 4
      DO 20 II = 1,4
      IK1 = IK1 + IK
      IK2 = IK2 + IK
      IK3 = IK3 + IK
      IK4 = IK4 + IK
      IK  = 2
      IF (II .EQ. 4) PEN = 0
      IF (II .NE. 4) GO TO 10
      IK1 = 7
      IK2 = 8
      IK3 = 1
      IK4 = 2
   10 CALL LINE (XB(IK1),XB(IK2),XB(IK3),XB(IK4),PEN,0)
   20 CONTINUE
   30 CONTINUE
      CALL LINE (0.0,0.0,0.0,0.0,PEN,+1)
   40 ISAV = LOPCOR + ID
      IVAL = LOPCOR + ISAV
      ICEN = LOPCOR + IVAL
      LOPCOR= LOPCOR- 1
      PEN = PPEN
      IF (ICNTVL.GT.9 .AND. ICNTVL.LT.14 .AND. PEDGE.NE.1) GO TO 50
      CALL CLOSE (PARM,2)
      IF (ICNTVL.LE.9 .OR. ICNTVL.GT.13) CALL CREATE (GPLST,X,U,DEFORM,
     1    CONMIN,CONMAX,Z(IVAL),Z(ICEN),LOPCOR,B1,B2)
      IF (ISET .NE. JSET) CALL ORDER (GPLST,Z(IVAL),Z(ISAV),Z(ICEN),
     1    Z(ID),LOPCOR,B1,B2,B3)
      ISET = JSET
   50 IF (ICNTVL.GT.9 .AND. ICNTVL.LT.14)
     1    CALL DISPLA (GPLST,X,U,DD,PEN,DEFORM,LABL,PT,B1)
      IF (ICNTVL.GT.9 .AND. ICNTVL.LT.14) GO TO 420
      IF (CONMIN .EQ. CONMAX) GO TO 470
      IF (COLOR .LT. 0) CALL CLOSE (EST,2)
      CALL GOPEN (SCR1,GPLST(B1),1)
      CALL CLOSE (SCR1,2)
      CALL GOPEN (SORT,GPLST(B2),2)
      CALL GOPEN (STRESS,GPLST(B1),0)
C
C     BUFFERS ASSIGNEMENT HERE -
C     B1 IS USED BY STRESS (SCRATCH1/301), AND BY SCR1 (SCRATCH4/304)
C     FOR SHORT PERIODS OF TIME ONLY
C     B2 IS USED BY SORT (SCRATCH2/302)
C     B3 IS USED BY EST (ELEST/103) AND BY SCR1 (SCRATCH4/304)
C
      NCNTR = MIN0(NCNTR,50)
      IF (CNTR(1) .NE. CNTR(2)) GO TO 90
C
C     IF INTERVALS SPECIFIED, DEFINE CONTOUR VALUES
C
      DELTA = (CONMAX-CONMIN)/FLOAT(NCNTR-1)
      CNTR(1) = CONMIN
      J = NCNTR - 1
      DO 80 I = 2,J
      CNTR(I) = CNTR(I-1) + DELTA
   80 CONTINUE
      CNTR(NCNTR) = CONMAX
   90 CALL LINE (0.,0.,0.,0.,PEN,-1)
      DO 100 I = 1,NCNTR
  100 LABL(I) = 3
C
C     READ AND STORE CONTOUR VALUES AND CENTROIDS
C
      ELID   = 0
      LOPCOX = LOPCOR + 1
      DO 110 I = 1,LOPCOX
      IS = ISAV + I - 1
      IZ(IS) = 0
  110 CONTINUE
      IF (COLOR .GE. 0) GO TO 130
      CALL GOPEN (EST,GPLST(B3),2)
      CALL BCKREC (EST)
      IMHERE = 120
  120 CALL READ (*280,*270,EST,ESYM,1,0,M)
      IRR = 0
      CALL FREAD (EST,NGPPE,1,0)
  130 CALL FWDREC (*415,SORT)
  140 CALL READ (*415,*415,SORT,IFLAG,1,0,M)
      IF (IFLAG .EQ.  0) GO TO 415
      IF (IFLAG .EQ. -2) GO TO 130
      CALL FREAD (SORT,IBEGIN,2,0)
      CALL READ  (*415,*150,SORT,IZ(ID),LINES,1,I)
  150 IREAD = 0
      NEL = 0
      DO 170 I = 1,LINES
      IC = ICEN + 2*(I-1)
      IV = IVAL + I - 1
      ID1= ID   + I - 1
      IS = ISAV + I - 1
      DO 160 J = 1,LOPCOR
      JS = ISAV + J - 1
      JV = IVAL + J - 1
      JC = ICEN + 2*(J-1)
      IF (IZ(JS)  .EQ. 0)  GO TO 170
      IF (IZ(ID1) .NE. IZ(JS)) GO TO 160
      Z(IV  ) = Z(JV)
      Z(IC  ) = Z(JC)
      Z(IC+1) = Z(JC+1)
      IZ(IS ) = IZ(ID1)
      NEL = NEL + 1
      GO TO 170
  160 CONTINUE
  170 CONTINUE
      IF (ELID .GT. 0) GO TO 190
  180 CALL READ (*290,*290,STRESS,ESSYM,1,0,M)
  190 CALL READ (*290,*180,STRESS,ELID,1,0,M)
      IF (ELID .EQ. 0) GO TO 180
      CALL FREAD (STRESS,V,1,0)
      CALL FREAD (STRESS,PT,2,0)
      DO 250 I = 1,LINES
      ID1 = ID  + I - 1
      IS = ISAV + I - 1
      IV = IVAL + I - 1
      IC = ICEN + 2*I - 2
      IF (IZ(ID1) .NE. ELID) GO TO 250
      IF (IZ(IS)  .EQ. ELID) GO TO 250
      Z(IV  ) = V
      Z(IC  ) = PT(1)
      Z(IC+1) = PT(2)
      IZ(IS ) = ELID
      IF (COLOR .GE. 0) GO TO 240
      IMHERE = 200
      ASSIGN 206 TO IRTN
  195 JRR = 0
  200 OFFSET = 0
      IF (ESYM .EQ. KBAR) OFFSET = 6
      IF (ESYM.EQ.KT3 .OR. ESYM.EQ.KQ4) OFFSET = 1
  201 CALL READ (*204,*205,EST,ELMID,1,0,M)
      IF (ELMID .EQ. 0) GO TO 203
      CALL FREAD (EST,0,-1,0)
      CALL FREAD (EST,GPTS,NGPPE,0)
      IF (OFFSET .NE. 0) CALL FREAD (EST,0,-OFFSET,0)
      IF (ELMID .EQ. ELID) GO TO 210
      GO TO 201
C
  203 JRR = JRR + 1
      IF (JRR .LE. 1) CALL BCKREC (EST)
      IMHERE = 203
      ASSIGN 120 TO IRTN
      CALL READ (*204,*204,EST,ESYM,1,0,M)
      CALL FREAD (EST,NGPPE,1,0)
      GO TO 200
C
  204 ERR = ERR + 1
      IF (ERR .GT. 3) GO TO 285
      CALL REWIND (EST)
      CALL SKPREC (EST,1)
      GO TO IRTN, (120,205,206)
C
  205 IMHERE = 205
      ASSIGN 205 TO IRTN
  206 CALL READ (*204,*205,EST,ESYM,1,0,M)
      CALL FREAD (EST,NGPPE,1,0)
      GO TO 195
C
C     START TO CONTOUR FILL HERE
C
  210 RCOLOR = ICOLOR
      RCNTRL = NCNTR
      DO 230 IK = 1,NCNTR
      PEN = 32 + (1.0-(RCNTRL-IK+1)/RCNTRL)*RCOLOR
      IK1 = IK + 1
      IF (IK .EQ. NCNTR) IK1 = IK
      IF (V.LT.CNTR(IK) .OR. V.GT.CNTR(IK1)) GO TO 230
      DO 220 J = 1,NGPPE
      K  = J + 1
      IG = GPTS(J)
      IG = IABS(GPLST(IG))
      IF (J .EQ. NGPPE) K = 1
      IG1 = GPTS(K)
      IG1 = IABS(GPLST(IG1))
      IF (J .EQ. NGPPE) PEN = 0
      CALL LINE (X(2,IG),X(3,IG),X(2,IG1),X(3,IG1),PEN,0)
  220 CONTINUE
      GO TO 240
  230 CONTINUE
  240 NEL = NEL + 1
      GO TO 260
  250 CONTINUE
  260 IF (NEL .GE. LINES) GO TO 300
      GO TO 190
C
  270 CALL BCKREC (EST)
      IRR = IRR + 1
      IF (IRR .LT. 3) GO TO 120
C
C     END OF FILE ON EST
C
  280 ERR = ERR + 1
      IF (ERR .GT. 3) GO TO 285
      CALL REWIND (EST)
      CALL SKPREC (EST,1)
      GO TO 120
  285 WRITE  (NOUT,286) UIM,ELID,IMHERE,ERR,IRR,NGPPE
  286 FORMAT (A29,', CONTOUR FAILED TO LOCATE ELMENT ID =',I8, /5X,
     2        'IMHERE =',I5, 5X,'ERR,IRR,NGPPE =',3I8)
      GO TO 190
C
C     END OF FILE ON STRESS
C
  290 CALL REWIND (STRESS)
      CALL FWDREC (*415,STRESS)
      IF (IREAD .EQ. 1) GO TO 140
      IREAD = 1
      GO TO 180
C
C     END DATA SEARCH
C
  300 L  = LINES
      IS = LINES + ISAV
      IZ(IS) = 0
      IF (LINES .GT. 3) GO TO 310
      XMID   = Z(ICEN+4)
      YMID   = Z(ICEN+5)
      CENVAL = Z(IVAL+2)
      L = 1
      GO TO 350
  310 IG = IABS(GPLST(IGDPT))
      IF (DEFORM .NE. 0) GO TO 320
      XMID = X(2,IG)
      YMID = X(3,IG)
      GO TO 330
  320 XMID = U(1,IG)
      YMID = U(2,IG)
  330 SUM1 = 0.0
      SUM2 = 0.0
      DO 340 I = 1,LINES
      IV = IVAL + I - 1
      IC = ICEN + 2*I - 2
      S  = SQRT((XMID-Z(IC))**2 + (YMID-Z(IC+1))**2)
      SUM1 = SUM1 + Z(IV) * S
  340 SUM2 = SUM2 + S
      CENVAL = SUM1/SUM2
  350 IV = IVAL + LINES
      IC = ICEN + 2*LINES
      Z(IV  ) = Z(IVAL)
      Z(IC  ) = Z(ICEN)
      Z(IC+1) = Z(ICEN+1)
C
C     PLOT CONTOURS.
C
      IF (COLOR .LT. 0) GO TO 140
      RCOLOR = ICOLOR
      RCNTRL = NCNTR
C
      CALL CLOSE (EST,2)
      CALL GOPEN (SCR1,GPLST(B3),3)
C
      DO 410 I = 1,NCNTR
      IF (COLOR .NE. 0) PEN = 1 + (1.0-(RCNTRL-I+1)/RCNTRL)*RCOLOR
      DO 400 J = 1,L
      PT(1) = XMIN - 1.0
      PT(3) = PT(1)
      PT(5) = PT(1)
      JC    = ICEN + 2*J - 2
      JV    = IVAL + J - 1
      D     = (Z(JV) - Z(JV+1))
      IF (ABS(Z(JV)-CNTR(I)).GT.ABS(D) .OR. ABS(Z(JV+1)-CNTR(I))
     1    .GT.ABS(D)) GO TO 360
      IF (D .EQ. 0.0) D = 1.0
      PT(1) = Z(JC  ) + (Z(JC+2)-Z(JC  ))*(Z(JV)-CNTR(I))/D
      PT(2) = Z(JC+1) + (Z(JC+3)-Z(JC+1))*(Z(JV)-CNTR(I))/D
  360 D = Z(JV+1) - CENVAL
      IF (ABS(Z(JV+1)-CNTR(I)).GT.ABS(D) .OR. ABS(CENVAL-CNTR(I))
     1    .GT.ABS(D)) GO TO 370
      IF (D .EQ. 0.0) D = 1.0
      PT(3) = Z(JC+2) + (XMID-Z(JC+2))*(Z(JV+1)-CNTR(I))/D
      PT(4) = Z(JC+3) + (YMID-Z(JC+3))*(Z(JV+1)-CNTR(I))/D
  370 D = CENVAL - Z(JV)
      IF (ABS(CENVAL-CNTR(I)).GT.ABS(D) .OR.
     1   ABS(Z(JV)-CNTR(I)) .GT. ABS(D)) GO TO 380
      IF (D .EQ. 0.0) D = 1.0
      PT(5) = XMID + (Z(JC  )-XMID)*(CENVAL-CNTR(I))/D
      PT(6) = YMID + (Z(JC+1)-YMID)*(CENVAL-CNTR(I))/D
  380 PT(7) = PT(1)
      PT(8) = PT(2)
      DO 390 K = 1,5,2
      IF (PT(K).LT.XMIN .OR. PT(K+2).LT.XMIN) GO TO 390
      CALL LINE (PT(K),PT(K+1),PT(K+2),PT(K+3),PEN,0)
      LABL(I) = LABL(I) + 1
      IF (LABL(I) .NE. 4) GO TO 390
      LABL(I) = 0
      CALL WRITE (SCR1,I,1,0)
      CALL WRITE (SCR1,PT(K),2,0)
  390 CONTINUE
  400 CONTINUE
  410 CONTINUE
C
      CALL CLOSE (SCR1,2)
      CALL GOPEN (EST,GPLST(B3),2)
      GO TO 140
C
  415 CALL CLOSE (SORT,1)
      CALL CLOSE (STRESS,1)
      CALL CLOSE (SCR1,1)
C     IF (COLOR .LT. 0) CALL CLOSE (EST,1)
C     IF (COLOR .GE. 0) CALL GOPEN (EST,GPLST(B3),2)
  420 CALL LINE (0.,0.,0.,0.,PEN,+1)
      IF (COLOR .EQ. 0) GO TO 430
      CALL TYPFLT (0.0,0.0,0,0,0,-1)
      CALL TYPFLT (368.14,990.0,1,CNTR(1),-8,0)
      CENTER = (CNTR(1)+CNTR(NCNTR))/2.0
      CALL TYPFLT (585.90,990.0,1,CENTER,-8,0)
      CALL TYPFLT (796.3,990.0,1,CNTR(NCNTR),-8,0)
      CALL TYPFLT (0.0,0.0,0,0,0,+1)
      IF (COLOR .LT. 0) GO TO 460
  430 CALL GOPEN (SCR1,GPLST(B1),0)
      IF (COLOR .EQ. 0) CALL TYPINT (0.,0.,0,0,0,-1)
  440 CALL READ (*450,*450,SCR1,I,1,0,M)
      CALL FREAD (SCR1,PT,2,0)
      IF (COLOR .EQ. 0) CALL TYPINT (PT(1),PT(2),1,I,1,0)
      GO TO 440
  450 IF (COLOR .EQ. 0) CALL TYPINT (0.,0.,0,0,0,+1)
      CALL CLOSE (SCR1,1)
  460 CALL PLTOPR
  470 IF ((ICNTVL.GT.9 .AND. ICNTVL.LT.14) .AND. PEDGE.NE.1) RETURN
      CALL GOPEN (PARM,GPLST(B2),2)
      RETURN
      END
