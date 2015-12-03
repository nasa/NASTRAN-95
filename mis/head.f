      SUBROUTINE HEAD (DTYP,PLTP,MTYP,IDAT)
C
      INTEGER         IDAT(17),MAXDEF(3),DTYP,PLTP,UNDEF(4),PTYP(2,5),
     1                SUBC(2),MTYPF(2,3),PHAS(3),FPLTIT,PLTITL
      REAL            NT1(5),NT2(4),NT3(3),CSCALE,X,X0
      COMMON /OUTPUT/ TITLE(32,3)
      COMMON /PLTDAT/ SKPPLT(2),XYMIN(2),XYMAX(2),AXYMAX(13),CSCALE,
     1                SKPA(3),CNTX,CNTY
      COMMON /XXPARM/ ISKP(215),FPLTIT,PLTITL(17)
C
      DATA    UNDEF / 4HUNDE, 4HFORM, 4HED S, 4HHAPE /
C ... NUMBER CHAR+2 FOR STATIC - CMODAL ... NOTE, 1 BLANK AT START...
     1,       NT1   / 8., 7., 8., 7., 8.  /
C ... NUMBER CHAR+1 FOR DEFO - ACCEL ...
     2,       NT2   , PTYP / 7., 9., 7., 7.
     A,               4HDEFO,2HR.  , 4HVELO,4HCITY, 4HACCE,2HL.
     B,               4HSTRE,2HSS  , 4HSTRA,2HIN  /
     3,       SUBC  / 4HSUBC,4HASE  /
C ... NUMBER CHAR+1 FOR FREQ, EIGENV., TIME  ... IDENTIFY BY MTYP ...
     4,       NT3   / 6., 8., 5. /
     C,       MTYPF / 4HFREQ,4H.   , 4HEIGE,4HNV. , 4HTIME,1H    /
C ... NUMBER OF SPACES BETWEEN IDENTIFIERS ...
     5,       DELX  / 3.0 /
     6,       MAXDEF/ 4HMAX-,4HDEF.,2H =   /
     7,       PHAS  / 4H PHA,4HSE  ,1H     /
C
      XYMIN(1) = 0.0
      XYMIN(2) = 0.0
      XYMAX(1) = AXYMAX(1)
      XYMAX(2) = AXYMAX(2)
      CALL PRINT (0,0,0,0,0,-1)
      IF (MTYP .LT. 0) GO TO 30
C
C     LEFT-MOST CHARACTER MAY NOT BE COMPETELY DRAWN IF FRACTION OF
C     CSCALE IS IS LESS THAN 0.5. SO MOVE OVER A SMALL SPACE OF X0
C
      J  = IFIX(CSCALE)
      X0 = CSCALE - FLOAT(J)
      IF (X0 .GT. 0.5) X0 = 0.0
C
C     PRINT THE TITLE, SUBTITLE AND LABEL
C
      CALL PRINT (X0,3.0*CNTY,1,TITLE(1,1),17,0)
      CALL PRINT (X0,2.0*CNTY,1,TITLE(1,2),16,0)
      CALL PRINT (X0,CNTY,1,TITLE(1,3),17,0)
C
      X = 25. - 5.*(CSCALE-1.)
      IF (DTYP .EQ. 0) GO TO 10
      X = 40.
      IF (IDAT(1) .LE. 8) GO TO 10
      X = 45.
      IF (IDAT(1) .GE. 12) X = 52.
      IF (IDAT(1) .GE. 15) X = 59.
   10 CONTINUE
      IF (FPLTIT .NE. 0) CALL PRINT (X*CNTX,0.,1,PLTITL,17,0)
C
C     BOTTOM LINE IDENTIFIES PLOT
C
      IF (DTYP .NE. 0) GO TO 20
C
C     UNDEFORMED SHAPE
C
      CALL PRINT (CNTX+X0,0.,1,UNDEF,4,0)
      GO TO 40
C
C     DEFORMED SHAPE
C
   20 CALL PRINT (CNTX+X0,0.,1,IDAT(3),2,0)
      X = NT1(DTYP)
      CALL PRINT (X*CNTX+X0,0.,1,PTYP(1,PLTP),2,0)
      X = X + NT2(PLTP)
      CALL PRINT (X*CNTX+X0,0.,1,SUBC,2,0)
      X = X + 8.
      N = -1
      CALL TYPINT (X*CNTX+X0,0.,1,IDAT(7),N,0)
      X = X + FLOAT(N) + DELX
C
C     LOAD I  OR  MODE I
C
      CALL PRINT (X*CNTX+X0,0.,1,IDAT(9),1,0)
      X = X + 5.
      N = -1
      CALL TYPINT (X*CNTX+X0,0.,1,IDAT(8),N,0)
C
C     FREQUENCY, EIGENVALUE, OR TIME
C
      IF (IDAT(1) .LE. 8) GO TO 40
      X = FLOAT(IFIX(X+DELX+0.1) + N)
      CALL PRINT (X*CNTX+X0,0.,1,MTYPF(1,MTYP),2,0)
      X = X + NT3(MTYP)
      CALL TYPFLT (X*CNTX+X0,0.,1,IDAT(10),-8,0)
C
C     MAGNITUDE  OR  PHASE LAG
C
      IF (IDAT(1) .LE. 12) GO TO 40
      X = X + 7.0 + DELX
      IF (IDAT(14) .NE. PHAS(1)) GO TO 25
      IDAT(15) = PHAS(2)
      IDAT(16) = PHAS(3)
   25 CALL PRINT (X*CNTX+X0,0.,1,IDAT(14),3,0)
C
      IF (IDAT(1) .LE. 15) GO TO 40
      X = X + 7.0
      CALL TYPFLT (X*CNTX+X0,0.,1,IDAT(17),-6,0)
      GO TO 40
C
C     PRINT THE MAXIMUM DEFORMATION AT THE TOP
C
   30 CALL PRINT (20.*CNTX,XYMAX(2),1,MAXDEF,3,0)
      CALL TYPFLT (31.*CNTX,XYMAX(2),1,IDAT(1),-10,0)
C
C
   40 CALL PRINT (0,0,0,0,0,1)
      RETURN
      END
