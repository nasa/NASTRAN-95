      SUBROUTINE SHSETD (*,MM,SIL,BGPDT,IGPDT,GPTH,ELTH,GPTEMP,BGPDM,
     1                   EGPDT,DGPTH,GPNORM,EPNORM,NNODE,MMN,NSIL,
     2                   IORDER,IORDRN,TEB,TUB,CENTE,AVGTHK,TCE,ELID)
C
C     TO SET UP FOR ISOPARAMETRIC SHELL ELEMENTS, CALLED ONLY BY SHHMGD
C
C     DOUBLE PRECISION VERSION
C
C     INPUT :
C           MM       - MAXIMUM NO. OF NODES PER THIS TYPE ELEMENT
C           SIL      - ARRAY OF SIL NUMBERS
C           BGPDT    - BGPDT DATA FROM EST (REAL ARRAY)
C           IGPDT    - BGPDT DATA FROM EST (INTEGER ARRAY)
C           GPTH     - GRID POINT THICKNESS DATA
C           ELTH     - ELEMENT THICKNESS FROM EPT
C           GPTEMP   - GRID POINT TEMPERATURE DATA
C           ELID     - ELEMENT ID
C     OUTPUT:
C           SIL      - ARRAY OF SIL NUMBERS        (REARRANGED)
C           BGPDT    - BGPDT DATA (REAL ARRAY)     (REARRANGED)
C           IGPDT    - BGPDT DATA (INTEGER ARRAY)  (REARRANGED)
C           GPTH     - GRID POINT THICKNESS DATA   (REARRANGED)
C           GPTEMP   - GRID POINT TEMPERATURE DATA (REARRANGED)
C           BGPDM    - BGPDT DATA SAVED IN ORIGINAL FORMAT
C           EGPDT    - BGPDT DATA IN ELEMENT COORD. SYSTEM
C           DGPTH    - GRID POINT THICKNESS DATA
C           GPNORM   - GRID POINT NORMALS
C           EPNORM   - GRID POINT NORMALS IN ELEMENT COORD. SYSTEM
C           NNODE    - THE NO. OF NODES PRESENT IN THE ELEMENT
C           MMN      - ARRAY OF MISSING MIDSIDE NODES
C           NSIL     - INTERNALLY ORDERED SIL ARRAY
C           IORDER   - ARRAY OF ORDER INDICATORS FOR REARRANGED DATA
C           IORDRN   - ARRAY OF ORDER INDICATORS FOR TRIA
C           TEB      - TRANSFORMATION FROM ELEMENT TO BASIC COORD.SYSTEM
C           TUB      - TRANSFORMATION FROM USER TO BASIC COORD. SYSTEM
C           CENTE    - LOCATION OF THE CENTER OF THE ELEMENT
C           AVGTHK   - AVERAGE THICKNESS OF THE ELEMENT
C
      LOGICAL          QUAD
      INTEGER          SIL(8),IORDER(8),KSIL(8),KCID(8),MMN(8),NSIL(8),
     1                 IORDRN(8),IGPDT(4,8),ELID
      REAL             GPTEMP(8),TEMTEM(8),BGPDT(4,8),TGRID(4,8),
     1                 GPTH(8),TMPTHK(8),BGPDM(3,8)
      DOUBLE PRECISION CENT(3),CENTE(3),EGPDT(4,8),GGU(9),GGN(9),TEB(9),
     1                 TEU(9),SMAX,SMIN,SL(3),GGE(9),TUB(9),CC,DGPTH(8),
     2                 GPNORM(4,8),EPNORM(4,8),X31,Y31,X42,Y42,EXI,EXJ,
     3                 AA,BB,UGPDM(3,8),TCE(63),AVGTHK
C
C
      IF (MM.NE.3 .AND. MM.NE.4 .AND. MM.NE.6 .AND. MM.NE.8) GO TO 700
C           TRIA3         QUAD4         TRIA6         QUAD8
C
      QUAD = MM.EQ.8 .OR. MM.EQ.4
      MMX  = 3
      IF (QUAD) MMX = 4
      NNODE = MM
      DO 10 I = 1,MM
      MMN(I) = SIL(I)
      KSIL(I)= SIL(I)
      IF (SIL(I) .GT. 0) GO TO 10
      NNODE = NNODE - 1
   10 CONTINUE
C
C     FILL IN ARRAY GGU WITH THE COORDINATES OF GRID POINTS 1,2 AND 4
C     (3 FOR TRIA). THIS ARRAY WILL BE USED LATER TO DEFINE THE USER
C     COORDINATE SYSTEM WHILE CALCULATING TRANSFORMATIONS INVOLVING
C     THIS COORDINATE SYSTEM.
C
      DO 20 I = 1,3
      II = (I-1)*3
      IJ = I
      IF (QUAD .AND. IJ.EQ.3) IJ = 4
      DO 20 J = 1,3
      JJ = J + 1
   20 GGU(II+J) = DBLE(BGPDT(JJ,IJ))
      CALL BETRND (TUB,GGU,0,ELID)
C
C     STORE INCOMING BGPDT FOR LUMPED MASS AND ELEMENT COORD. SYSTEM
C
      DO 30 I = 1,3
      I1 = I + 1
      DO 30 J = 1,MM
   30 BGPDM(I,J) = BGPDT(I1,J)
C
C     TRANSFORM BGPDM FROM BASIC TO USER COORD. SYSTEM
C
      DO 40 I = 1,3
      IP = (I-1)*3
      DO 40 J = 1,MM
      UGPDM(I,J) = 0.0D0
      DO 40 K = 1,3
      KK = IP + K
   40 UGPDM(I,J) = UGPDM(I,J) + TUB(KK)*(DBLE(BGPDM(K,J))-GGU(K))
C
      IF (QUAD) GO TO 200
C
C     FOR TRIA
C     CALCULATE THE CENTER COORDINATES
C
      CENTE(1) = (GGU(1)+GGU(4)+GGU(7))/3.0D0
      CENTE(2) = (GGU(2)+GGU(5)+GGU(8))/3.0D0
      CENTE(3) = (GGU(3)+GGU(6)+GGU(9))/3.0D0
C
C     ESTABLISH THE INTERNAL COORDINATES:
C     X-AXIS IS ALONG THE MIDDLE-SIZED SIDE AND THE XY-PLANE IS
C     DETERMINED BY IT TOGETHER WITH THE SHORTEST SIDE
C
      CC = (GGU(7)-GGU(4))*(GGU(7)-GGU(4))
     1   + (GGU(8)-GGU(5))*(GGU(8)-GGU(5))
     2   + (GGU(9)-GGU(6))*(GGU(9)-GGU(6))
      IF (CC .LE. 0.0D0) GO TO 700
      SL(1) = DSQRT(CC)
      CC = (GGU(7)-GGU(1))*(GGU(7)-GGU(1))
     1   + (GGU(8)-GGU(2))*(GGU(8)-GGU(2))
     2   + (GGU(9)-GGU(3))*(GGU(9)-GGU(3))
      IF (CC .LE. 0.0D0) GO TO 700
      SL(2) = DSQRT(CC)
      CC = (GGU(4)-GGU(1))*(GGU(4)-GGU(1))
     1   + (GGU(5)-GGU(2))*(GGU(5)-GGU(2))
     2   + (GGU(6)-GGU(3))*(GGU(6)-GGU(3))
      IF (CC .LE. 0.0D0) GO TO 700
      SL(3) = DSQRT(CC)
      SMAX  = SL(1)
      ISMAX = 1
      DO 100 I = 2,3
      IF (SL(I) .LE. SMAX) GO TO 100
      SMAX  = SL(I)
      ISMAX = I
  100 CONTINUE
      SMIN  = SL(1)
      ISMIN = 1
      DO 110 I = 2,3
      IF (SL(I) .GE. SMIN) GO TO 110
      SMIN  = SL(I)
      ISMIN = I
  110 CONTINUE
      IF (ISMAX .EQ. ISMIN) ISMIN = 3
      MIDDL = IABS(ISMAX-ISMIN)
      IF (ISMAX+ISMIN .EQ. 3) MIDDL = 3
C
C     DETECT THE POSSIBLE REVERSAL OF THE INTERNAL Z-AXIS WITH RESPECT
C     TO THE USER Z-AXIS. IF THAT IS THE CASE, SWITCH ISMAX AND ISMIN
C     TO AVOID THE PROBLEM. THE SIDE WITH MEDIUM LENGTH WILL STILL BE
C     THE X-AXIS.
C
      IF (ISMAX .NE. MOD(ISMIN,3)+1) GO TO 120
      III    = ISMIN
      ISMIN  = ISMAX
      ISMAX  = III
C
  120 IS3    = 3*(ISMAX-1)
      GGN(1) = GGU(IS3+1)
      GGN(2) = GGU(IS3+2)
      GGN(3) = GGU(IS3+3)
C
      IS3    = 3*(ISMIN-1)
      GGN(4) = GGU(IS3+1)
      GGN(5) = GGU(IS3+2)
      GGN(6) = GGU(IS3+3)
C
      IS3    = 3*(MIDDL-1)
      GGN(7) = GGU(IS3+1)
      GGN(8) = GGU(IS3+2)
      GGN(9) = GGU(IS3+3)
C
      CALL BETRND (TEB,GGN,0,ELID)
      GO TO 300
C
C     FOR QUAD
C     THE ORIGIN OF THE ELEMENT COORD.SYSTEM IS IN THE MIDDLE OF THE
C     ELEMENT
C
  200 DO 210 J = 1,3
      CENT(J) = 0.0D0
      DO 210 I = 1,MM
  210 CENT(J) = CENT(J) + UGPDM(J,I)/NNODE
C
C     STORE THE CORNER NODE DIFF. IN THE USER COORD. SYSTEM
C
      X31 = UGPDM(1,3) - UGPDM(1,1)
      Y31 = UGPDM(2,3) - UGPDM(2,1)
      X42 = UGPDM(1,4) - UGPDM(1,2)
      Y42 = UGPDM(2,4) - UGPDM(2,2)
      AA  = X31*X31 + Y31*Y31
      IF (AA .LE. 0.0D0) GO TO 700
      AA  = DSQRT(AA)
      BB  = X42*X42 + Y42*Y42
      IF (BB .LE. 0.0D0) GO TO 700
      BB  = DSQRT(BB)
C
C     NORMALIZE XIJ'S
C
      X31 = X31/AA
      Y31 = Y31/AA
      X42 = X42/BB
      Y42 = Y42/BB
      EXI = X31 - X42
      EXJ = Y31 - Y42
C
C     STORE GGE ARRAY, THE OFFSET BETWEEN ELEMENT COORD. SYSTEM AND USER
C     COORD. SYSTEM
C
      GGE(1) = CENT(1)
      GGE(2) = CENT(2)
      GGE(3) = CENT(3)
C
      GGE(4) = GGE(1) + EXI
      GGE(5) = GGE(2) + EXJ
      GGE(6) = GGE(3)
C
      GGE(7) = GGE(1) - EXJ
      GGE(8) = GGE(2) + EXI
      GGE(9) = GGE(3)
C
      CALL BETRND (TEU,GGE,0,ELID)
      CALL GMMATD (TEU,3,3,0, TUB,3,3,0,  TEB)
      CALL GMMATD (TUB,3,3,1, CENT,3,1,0, CENTE)
C
C     THE ARRAY IORDER STORES THE ELEMENT NODE ID IN INCREASING SIL
C     ORDER.
C
C     IORDER(1)  = NODE WITH LOWEST  SIL NUMBER
C     IORDER(MM) = NODE WITH HIGHEST SIL NUMBER
C
C     ELEMENT NODE NUMBER IS THE INTEGER FROM THE NODE LIST
C     G1,G2,G3,G4,G5,G6,G7,G8 .  THAT IS, THE "I" PART OF THE "GI" AS
C     THEY ARE LISTED ON THE CONNECTIVITY BULK DATA CARD DESCRIPTION.
C
  300 KSILD = 99999995
      DO 310 I = 1,MM
      IORDER(I) = 0
      IORDRN(I) = 0
      KSIL(I) = SIL(I)
      IF (SIL(I) .NE. 0) GO TO 310
      KSIL(I) = KSILD
      KSILD   = KSILD + 1
  310 CONTINUE
      DO 330 I = 1,MM
      ITEMP = 1
      ISIL  = KSIL(1)
      DO 320 J = 2,MM
      IF (ISIL .LE. KSIL(J)) GO TO 320
      ITEMP = J
      ISIL  = KSIL(J)
  320 CONTINUE
      IORDER(I) = ITEMP
      IORDRN(I) = ITEMP
      KSIL(ITEMP) = 99999999
  330 CONTINUE
C
C     ADJUST EST DATA
C
C     USE THE POINTERS IN IORDER TO COMPLETELY REORDER THE GEOMETRY DATA
C     INTO INCREASING SIL ORDER.
C     DON'T WORRY!! IORDER ALSO KEEPS TRACK OF WHICH SHAPE FUNCTIONS GO
C     WITH WHICH GEOMETRIC PARAMETERS!
C
      DO 350 I = 1,MM
      KSIL(I)  = SIL(I)
      TMPTHK(I)= GPTH(I)
      IF (MM .NE. 4) TEMTEM(I) = GPTEMP(I)
      KCID(I) = IGPDT(1,I)
      DO 340 J = 2,4
      TGRID(J,I) = BGPDT(J,I)
  340 CONTINUE
  350 CONTINUE
      DO 370 I = 1,MM
      IPOINT  = IORDER(I)
      SIL(I)  = KSIL(IPOINT)
      NSIL(I) = KSIL(IPOINT)
      GPTH(I) = TMPTHK(IPOINT)
      IF (MM .NE. 4) GPTEMP(I) = TEMTEM(IPOINT)
      IGPDT(1,I) = KCID(IPOINT)
      DO 360 J = 2,4
      BGPDT(J,I) = TGRID(J,IPOINT)
  360 CONTINUE
  370 CONTINUE
C
      IF (QUAD) GO TO 500
C
C     FOR TRIA
C     CREATE THE INTERNAL ORDER OF THE NODES OF ELEMENT IN CONNECTION
C     WITH THE INTERNAL COORDINATE SYSTEM THEN CALCULATE NORMALS
C
      DO 400 I = 1,MM
      IF (IORDER(I) .EQ. ISMAX) IORDRN(I) = 1
      IF (IORDER(I) .EQ. ISMIN) IORDRN(I) = 2
      IF (IORDER(I) .EQ. MIDDL) IORDRN(I) = 3
      IF (IORDER(I) .EQ. 4) IND4=I
      IF (IORDER(I) .EQ. 5) IND5=I
      IF (IORDER(I) .EQ. 6) IND6=I
  400 CONTINUE
      IF (MM .NE. 6) GO TO 410
      IF (ISMAX+ISMIN .EQ. 3) IORDRN(IND4) = 4
      IF (ISMAX+ISMIN .EQ. 4) IORDRN(IND6) = 4
      IF (ISMAX+ISMIN .EQ. 5) IORDRN(IND5) = 4
      IF (ISMIN+MIDDL .EQ. 3) IORDRN(IND4) = 5
      IF (ISMIN+MIDDL .EQ. 4) IORDRN(IND6) = 5
      IF (ISMIN+MIDDL .EQ. 5) IORDRN(IND5) = 5
      IF (MIDDL+ISMAX .EQ. 3) IORDRN(IND4) = 6
      IF (MIDDL+ISMAX .EQ. 4) IORDRN(IND6) = 6
      IF (MIDDL+ISMAX .EQ. 5) IORDRN(IND5) = 6
C
  410 DO 420 I = 1,3
      II = I + 1
      IP = (I-1)*3
      DO 420 J = 1,NNODE
      EGPDT(II,J) = 0.0D0
      DO 420 K = 1,3
      KK = IP + K
      EGPDT(II,J) = EGPDT(II,J) + TEB(KK)*(DBLE(BGPDT(K+1,J))-GGN(K))
  420 CONTINUE
C
C     USE THE POINTERS IN IORDER AND IORDRN TO REORDER MMN
C
      DO 430 I = 1,MM
      IPOINT = IORDRN(I)
      JPOINT = IORDER(I)
      MMN(IPOINT) = KSIL(JPOINT)
  430 CONTINUE
C
      IF (MM .NE. 3) GO TO 520
      DO 440 II=1,3
      EPNORM(1,II) = 0.0D0
      EPNORM(2,II) = 0.0D0
      EPNORM(3,II) = 0.0D0
      EPNORM(4,II) = 1.0D0
      GPNORM(1,II) = 0.0D0
      GPNORM(2,II) = TEB(7)
      GPNORM(3,II) = TEB(8)
      GPNORM(4,II) = TEB(9)
  440 CONTINUE
      GO TO 520
C
C     FOR QUAD - COMPUTE NODAL NORMALS
C     THE COORDINATES OF THE ELEMENT GRID POINTS HAVE TO BE TRANSFORMED
C     FROM THE BASIC COORD. SYSTEM TO THE ELEMENT COORD. SYSTEM
C
  500 IFLAG = 0
      IF (MM .EQ. 4) CALL Q4NRMD (BGPDT,GPNORM,IORDER,IFLAG)
      IF (IFLAG .NE. 0) GO TO 700
C
      DO 510 I = 1,3
      II = I + 1
      IP = (I-1)*3
      DO 510 J = 1,NNODE
      EPNORM(II,J) = 0.0D0
      EGPDT (II,J) = 0.0D0
      DO 510 K = 1,3
      KK = IP + K
      K1 = K  + 1
      CC = DBLE(BGPDT(K1,J)) - GGU(K) - CENTE(K)
      EPNORM(II,J) = EPNORM(II,J) + TEB(KK)*GPNORM(K1,J)
      EGPDT (II,J) = EGPDT (II,J) + TEB(KK)*CC
  510 CONTINUE
C
C     SET AVGTHK TO ZERO
C
  520 AVGTHK = 0.0D0
      DO 550 I = 1,NNODE
      IO = IORDER(I)
      IF (IO .GT. MMX) GO TO 550
C
      IF (GPTH(I)) 700,530,540
  530 IF (ELTH .LE. 0.0) GO TO 700
      GPTH(I)  = ELTH
  540 DGPTH(I) = DBLE(GPTH(I))
      AVGTHK = AVGTHK + DGPTH(I)/NNODE
  550 CONTINUE
C
      DO 620 I = 1,NNODE
      IO = IORDER(I)
      IF (IO      .LE. MMX) GO TO 620
      IF (GPTH(I) .GT. 0.0) GO TO 610
      IO1 = IO  - MMX
      IO2 = IO1 + 1
      IF (IO2 .EQ. MMX+1) IO2 = 1
      DO 600 J = 1,MM
      JO = IORDER(J)
      IF (JO .EQ. IO1) IC1 = J
      IF (JO .EQ. IO2) IC2 = J
  600 CONTINUE
      GPTH (I) = (GPTH(IC1)+GPTH(IC2))/2.0
  610 DGPTH(I) = DBLE(GPTH(I))
      AVGTHK = AVGTHK + DGPTH(I)/NNODE
  620 CONTINUE
      RETURN
C
  700 RETURN 1
      END
