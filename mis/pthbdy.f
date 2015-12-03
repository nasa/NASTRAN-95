      SUBROUTINE PTHBDY
C
C     PTHBDY MODIFIES THE SIL,ECT,EQEXIN AND BGBDT FOR CHBDY ELEMENTS
C     SO THEY CAN BE PLOTTED.
C
C     THE SIL BGPDT AND EQEXIN OUTPUT LOOKALIKES ARE ADD ON FILES
C     THE ECT OUTPUT FILE HAS THE CHBDY FLAG SET NEGATIVE
C     SO PLTSET CAN TELL THE ECTS APART IT ALSO HAS THE NEW GRID POINTS
C
      INTEGER         NAME(2),EPT,ECT,SIL,IEQ,BGPDT,HECT,HSIL,OEQ,
     1                HBGPDT,GEOM2,SCR1,SCR2,FLAG,IZ(1),SYSBUF,OUT,
     2                CBS(20),OSIL,FILE1,FILE2,VIEW(2),CHBDY(2),
     3                PHBDY(2),BUF1,BUF2,BUF3,BUF4,BUF5,BUF6,TRL(7)
      DIMENSION       NSIL(7),NEQ(14),TEM(3),E(3),V(3),R21(3)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /BLANK / NHBDY,MESH(2)
      COMMON /SYSTEM/ SYSBUF,OUT,DUM(6),NLPP
      COMMON /CONDAS/ PI
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (IZ(1),Z(1))
      DATA    GEOM2 , ECT, EPT, SIL, IEQ, BGPDT                   /
     1        101   , 102, 103, 104, 105,   106                   /
      DATA    HECT  , HSIL, OEQ, HBGPDT, SCR1, SCR2               /
     1        201   , 202,  203,    204,  301,  302               /
      DATA    IYES  , NO    , NAME  ,         NPHBDY, NVIEW, NCB2 /
     1        4HYES , 4HNO  , 4HPLNB, 4HDY  , 7     , 6    , 15   /
      DATA    VIEW  ,         CHBDY  ,        PHBDY        , NECT /
     1        2606  , 26    , 4208,42,        2502,25      , 15   /
C
C     PRINT FLAG CHBDY FLAG
C
      IPRT  = 0
      IF (MESH(1) .EQ. IYES) IPRT = 1
      NHBDY = -1
      LINE  = NLPP
C
C     INITIALIZE
C
      BUF1 = KORSZ(Z(1)) - SYSBUF
      BUF2 = BUF1 - SYSBUF  - 1
      BUF3 = BUF2 - SYSBUF
      BUF4 = BUF3 - SYSBUF
      BUF5 = BUF4 - SYSBUF
      BUF6 = BUF5 - SYSBUF
      CALL PRELOC (*1000,Z(BUF1),GEOM2)
      CALL LOCATE (*1000,Z(BUF1),CHBDY,N)
C
C     MAKE A SCRATCH FILE WITH EID AF DISLIN FOR CHBDY
C
      IPN  = 0
      IVEW = 0
      CALL PRELOC (*25,Z(BUF2),EPT)
      FILE1 = EPT
      CALL LOCATE (*15,Z(BUF2),PHBDY,N)
      CALL READ (*1002,*10,EPT,Z(1),BUF3,0,N)
      GO TO 1008
   10 IPN = N
   15 IPV = IPN + 1
      NRD = BUF3 - IPV
      CALL LOCATE (*25,Z(BUF2),VIEW,N)
      CALL READ (*1002,*20,EPT,Z(IPV),NRD,0,N)
      GO TO 1008
   20 IVEW = N
   25 CALL CLOSE (EPT,1)
      CALL GOPEN (SCR1,Z(BUF2),1)
      FILE1 = GEOM2
   30 CALL READ (*1002,*70,GEOM2,CBS,NCB2,0,N)
      TEM(1) = 0.0
      TEM(2) = 0.0
      IF (IPN .EQ. 0) GO TO 40
      DO 35 I = 1,IPN,NPHBDY
      IF (CBS(2) .NE. IZ(I)) GO TO 35
      TEM(1) = Z(I+2)
      GO TO 40
   35 CONTINUE
   40 IF (IVEW    .EQ. 0) GO TO 65
      IF (CBS(15) .EQ. 0) GO TO 65
      DO 60 I = 1,IVEW,NVIEW
      IF (CBS(15) .NE. IZ(IPN+I)) GO TO 60
      TEM(2) = Z(IPN+I+5)
      IF (IPRT .EQ.    0) GO TO 65
      IF (LINE .LT. NLPP) GO TO 50
      LINE = 1
      CALL PAGE1
      WRITE  (OUT,45)
   45 FORMAT (1H0,17X,5HIDENT,8X,4HBETA,7X,5HGAMMA,9X,3HCAN,6X,6HCAN BE,
     1       /6X,5HCHBDY,6X,6HNUMBER,8X,4HMESH,8X,4HMESH,7X,5HSHADE,6X,
     2       6HSHADED,5X,7HDISLIN ,/)
   50 NB = IYES
      NS = IYES
      IF (IZ(IPN+I+1) .EQ. 0) NB = NO
      IF (IZ(IPN+I+2) .EQ. 0) NS = NO
      LINE = LINE+1
      WRITE (OUT,55) CBS(1),IZ(IPN+I),IZ(IPN+I+3),IZ(IPN+I+4),NB,NS,
     1               TEM(2)
   55 FORMAT (1H ,4(I10,2X),6X,A4,8X,A4,2X,1P,E10.4)
      GO TO 65
   60 CONTINUE
   65 CALL WRITE (SCR1,TEM,2,0)
      GO TO 30
   70 CALL WRITE (SCR1,0,0,1)
      CALL CLOSE (SCR1 ,1)
      CALL CLOSE (GEOM2,1)
      CALL GOPEN (SCR1,Z(BUF1),0)
      TRL(1) = SIL
      CALL RDTRL (TRL)
      OSIL   = TRL(3)
      TRL(1) = BGPDT
      CALL RDTRL (TRL)
      NIN = TRL(2)
      NRD = 4*TRL(2)
      IF (5*SYSBUF+NRD+50 .GT. BUF1) GO TO 1008
C
C     FIND CHBDY CARDS COPY ECT TO CHBDY CARDS
C
      CALL GOPEN (ECT,Z(BUF2),0)
      CALL GOPEN (HECT,Z(BUF3),1)
      FILE1 = ECT
   80 CALL READ (*1000,*1000,ECT,CBS,3,0,N)
      CALL WRITE (HECT,CBS,3,0)
      IF (CBS(1).EQ.CHBDY(1) .AND. CBS(2).EQ.CHBDY(2)) GO TO 95
C
C     DUPE REST OF RECORD
C
   85 CALL READ (*1002,*90,ECT,Z(1),BUF6-1,0,N)
      CALL WRITE (HECT,Z(1),BUF6-1,0)
      GO TO 85
   90 CALL WRITE (HECT,Z(1),N,1)
      GO TO 80
C
C     COPY SIL EQEXIN TO NEW FILES
C
   95 ICORE = BUF6 - 1
      ILFT  = 1
      FILE1 = SIL
      FILE2 = HSIL
      N     = BUF4
      LEQ   = 0
  100 CALL GOPEN (FILE1,Z(BUF6),0)
      CALL GOPEN (FILE2,Z(N),1)
  105 CALL READ (*1002,*120,FILE1,Z(ILFT),ICORE,0,M)
      IF (FILE1 .NE. IEQ) GO TO 115
      DO 110 I = 1,ICORE,2
      IF (IZ(I) .GT. LEQ) LEQ = IZ(I)
  110 CONTINUE
  115 CALL WRITE (FILE2,Z(ILFT),ICORE,0)
      GO TO 105
  120 IF (FILE1 .NE. IEQ) GO TO 130
      DO 125 I = 1,M,2
      IF (IZ(I) .GT. LEQ) LEQ = IZ(I)
  125 CONTINUE
  130 CALL WRITE (FILE2,Z(ILFT),M,0)
      CALL CLOSE (FILE1,1)
      IF (N .EQ. BUF5) GO TO 150
      FILE1 = IEQ
      FILE2 = OEQ
      N     = BUF5
      GO TO 100
C
C     BRING IN BGPDT
C
  150 CALL GOPEN (BGPDT,Z(BUF6),0)
      FILE1 = BGPDT
      CALL READ (*1002,*1002,BGPDT,Z(1),NRD,0,N)
      CALL CLOSE (BGPDT,1)
C
C     FINALLY TIME TO GO TO WORK
C
      NHBDY = 0
      NNGP  = 0
      NBGP  = NRD + 1
      DO 155 I = 1,24
      IZ(NRD+I) = 0
  155 CONTINUE
      FILE1 = ECT
      CALL GOPEN (SCR2,Z(BUF6),1)
  160 CALL READ (*1002,*350,ECT,CBS,NECT,0,N)
      CBS(NECT) = 0.0
      IF (CBS(3) .GT. 6) CBS(3) = 3
      CALL READ (*1002,*1002,SCR1,TEM,2,0,N)
      FLAG  = CBS(3)
      NHBDY = NHBDY + 1
      GO TO (200,250,260,270,280,260), FLAG
C
C     POINT
C
C
C     BGPDT DATA FOR POINT
C
  200 I1 = (CBS(4)-1)*4 + 2
      ITRY = 1
      E(1) = 0.0
      E(2) = 0.0
      E(3) = 0.0
      CALL SAPB (CBS(12),E,V)
      CALL SANORM (*400,V)
      E(1) = 1.0
  205 XL = SADOTB(V,E)
      R21(1) = E(1) - XL*V(1)
      R21(2) = E(2) - XL*V(2)
      R21(3) = E(3) - XL*V(3)
      XL = SADOTB(R21,R21)
      IF (XL  .GT. .2) GO TO 210
      IF (ITRY .EQ. 2) GO TO 400
      ITRY = 2
      E(1) = 0.0
      E(2) = 1.0
      GO TO 205
  210 CALL SANORM (*215,R21)
  215 CALL SAXB (V,R21,E)
      XL = 0.0
      IF (TEM(1) .NE. 0.0) XL = SQRT(TEM(1)/PI)
      ZS3 = .8660254
      Z(NBGP+ 1) = Z(I1  ) + XL*R21(1)
      Z(NBGP+ 2) = Z(I1+1) + XL*R21(2)
      Z(NBGP+ 3) = Z(I1+2) + XL*R21(3)
      Z(NBGP+ 5) = Z(I1  ) + XL*( .5*R21(1) + ZS3*E(1))
      Z(NBGP+ 6) = Z(I1+1) + XL*( .5*R21(2) + ZS3*E(2))
      Z(NBGP+ 7) = Z(I1+2) + XL*( .5*R21(3) + ZS3*E(3))
      Z(NBGP+ 9) = Z(I1  ) + XL*(-.5*R21(1) + ZS3*E(1))
      Z(NBGP+10) = Z(I1+1) + XL*(-.5*R21(2) + ZS3*E(2))
      Z(NBGP+11) = Z(I1+2) + XL*(-.5*R21(3) + ZS3*E(3))
      Z(NBGP+14) = Z(I1+1) - XL* R21(2)
      Z(NBGP+15) = Z(I1+2) - XL* R21(3)
      Z(NBGP+17) = Z(I1  ) + XL*(-.5*R21(1) - ZS3*E(1))
      Z(NBGP+18) = Z(I1+1) + XL*(-.5*R21(2) - ZS3*E(2))
      Z(NBGP+19) = Z(I1+2) + XL*(-.5*R21(3) - ZS3*E(3))
      Z(NBGP+21) = Z(I1  ) + XL*(+.5*R21(1) - ZS3*E(1))
      Z(NBGP+22) = Z(I1+1) + XL*(+.5*R21(2) - ZS3*E(2))
      Z(NBGP+23) = Z(I1+2) + XL*(+.5*R21(3) - ZS3*E(3))
      Z(NBGP+25) = Z(I1  ) + XL*V(1)
      Z(NBGP+26) = Z(I1+1) + XL*V(2)
      Z(NBGP+27) = Z(I1+2) + XL*V(3)
      NNGP= NNGP + 7
      NS  = 7
      NEA = 14
      NB  = 28
      M   = 7
      N   = 5
  220 NN  = 1
      DO 225 I = 1,M
      LEQ = LEQ + 1
      NIN = NIN + 1
      NSIL(I) = NIN
      NEQ(NN) = LEQ
      NEQ(NN+1) = NIN
      CBS(N   ) = NIN
      NN  = NN + 2
      N   = N  + 1
  225 CONTINUE
      GO TO 300
C
C     LINE
C
C
C     BGPDT DATA FOR LINE
C
  250 I1 = (CBS(4)-1)*4 + 2
      I2 = (CBS(5)-1)*4 + 2
      CALL SAMB (Z(I2),Z(I1),R21)
      XL = SADOTB(R21,R21)
      IF (XL .EQ. 0.0) GO TO 400
      X1 = SADOTB(R21,CBS(12))
      XL = X1/XL
      E(1) = XL*R21(1)
      E(2) = XL*R21(2)
      E(3) = XL*R21(3)
      CALL SAMB (CBS(12),E,V)
      CALL SANORM (*400,V)
      CALL SAXB   (V,R21,E)
      CALL SANORM (*400,E)
      D  = TEM(2)
      AF = TEM(1)*.5
      Z(NBGP+ 1) = Z(I1  ) + D*V(1) - AF*E(1)
      Z(NBGP+ 2) = Z(I1+1) + D*V(2) - AF*E(2)
      Z(NBGP+ 3) = Z(I1+2) + D*V(3) - AF*E(3)
      Z(NBGP+ 5) = Z(I2  ) + D*V(1) - AF*E(1)
      Z(NBGP+ 6) = Z(I2+1) + D*V(2) - AF*E(2)
      Z(NBGP+ 7) = Z(I2+2) + D*V(3) - AF*E(3)
      Z(NBGP+ 9) = Z(I2  ) + D*V(1) + AF*E(1)
      Z(NBGP+10) = Z(I2+1) + D*V(2) + AF*E(2)
      Z(NBGP+11) = Z(I2+2) + D*V(3) + AF*E(3)
      Z(NBGP+13) = Z(I1  ) + D*V(1) + AF*E(1)
      Z(NBGP+14) = Z(I1+1) + D*V(2) + AF*E(2)
      Z(NBGP+15) = Z(I1+2) + D*V(3) + AF*E(3)
      Z(NBGP+17) = Z(I1  ) + D*V(1) + .5*R21(1)
      Z(NBGP+18) = Z(I1+1) + D*V(2) + .5*R21(2)
      Z(NBGP+19) = Z(I1+2) + D*V(3) + .5*R21(3)
      Z(NBGP+21) = Z(NBGP+17) + 2.*AF*V(1)
      Z(NBGP+22) = Z(NBGP+18) + 2.*AF*V(2)
      Z(NBGP+23) = Z(NBGP+19) + 2.*AF*V(3)
      NNGP= NNGP + 6
      NS  = 6
      NEA = 12
      NB  = 24
      M   = 6
      N   = 6
      GO TO 220
C
C     REV  OR  ELIP   DO NOTHING
C
  260 GO TO 310
C
C     AREA3
C
C     BGPDT DATA FOR AREA3
C
  270 I1 = (CBS(4)-1)*4 + 2
      I2 = (CBS(5)-1)*4 + 2
      I3 = (CBS(6)-1)*4 + 2
      CALL SAMB (Z(I2),Z(I1),E)
      CALL SAMB (Z(I3),Z(I1),V)
      CALL SAXB (E,V,E)
      CALL SANORM (*400,E)
      CALL SAMB (Z(I2),Z(I1),V)
      X1 = SADOTB(V,V)
      CALL SAMB (Z(I3),Z(I1),V)
      X2 = SADOTB(V,V)
      CALL SAMB (Z(I3),Z(I2),V)
      X3 = SADOTB(V,V)
      X1 = AMAX1(X1,X2)
      X1 = AMAX1(X1,X3)
      XL = .25* SQRT(X1)
      CALL SAPB (Z(I1),Z(I2),V)
      CALL SAPB (Z(I3),V,V)
      Z(NBGP+1) = V(1)/3.0
      Z(NBGP+2) = V(2)/3.0
      Z(NBGP+3) = V(3)/3.0
      Z(NBGP+5) = Z(NBGP+1) + XL*E(1)
      Z(NBGP+6) = Z(NBGP+2) + XL*E(2)
      Z(NBGP+7) = Z(NBGP+3) + XL*E(3)
  275 NNGP= NNGP + 2
      NS  = 2
      NEA = 4
      NB  = 8
      LEQ = LEQ + 1
      NIN = NIN + 1
      N   = 7
      IF (FLAG .EQ. 5) N = 8
      NSIL(1)= NIN
      NEQ(1) = LEQ
      NEQ(2) = NIN
      CBS(N) = NIN
      LEQ = LEQ + 1
      NIN = NIN + 1
      NSIL( 2) = NIN
      NEQ ( 3) = LEQ
      NEQ ( 4) = NIN
      CBS(N+1) = NIN
      CBS(N+2) = NIN
      CBS(N+3) = NIN
      IF (FLAG .EQ. 4) CBS(N+4) = NIN
      GO TO 300
C
C     AREA4
C
C     BGPDT DATA FOR AREA4
C
  280 I1 = (CBS(4)-1)*4 + 2
      I2 = (CBS(5)-1)*4 + 2
      I3 = (CBS(6)-1)*4 + 2
      I4 = (CBS(7)-1)*4 + 2
      CALL SAMB (Z(I3),Z(I1),E)
      CALL SAMB (Z(I4),Z(I2),V)
      CALL SAXB (E,V,E)
      CALL SANORM (*400,E)
      CALL SAMB (Z(I2),Z(I1),V)
      X1 = SADOTB(V,V)
      CALL SAMB (Z(I3),Z(I2),V)
      X2 = SADOTB(V,V)
      CALL SAMB (Z(I4),Z(I3),V)
      X3 = SADOTB(V,V)
      CALL SAMB (Z(I4),Z(I1),V)
      X4 = SADOTB(V,V)
      X1 = AMAX1(X1,X2)
      X1 = AMAX1(X1,X3)
      X1 = AMAX1(X1,X4)
      XL = .25* SQRT(X1)
      CALL SAPB (Z(I1),Z(I2),V)
      CALL SAPB (V,Z(I3),V)
      CALL SAPB (V,Z(I4),V)
      Z(NBGP+1) = .25*V(1)
      Z(NBGP+2) = .25*V(2)
      Z(NBGP+3) = .25*V(3)
      Z(NBGP+5) = Z(NBGP+1) + XL*E(1)
      Z(NBGP+6) = Z(NBGP+2) + XL*E(2)
      Z(NBGP+7) = Z(NBGP+3) + XL*E(3)
      GO TO 275
C
C     ADD TO HSIL HEQEXIN  HECT
C     BGPDT
C
  300 CALL WRITE (HSIL,NSIL,NS,0)
      CALL WRITE (OEQ,NEQ,NEA,0)
      CALL WRITE (SCR2,Z(NBGP),NB,0)
  310 CBS(3) = -CBS(3)
      CALL WRITE (HECT,CBS,NECT,0)
      GO TO 160
C
C     END CLOSE FILES, WRITE NBGPDT, WRITE TRAILERS THEN FINISH ECT COPY
C
  350 CALL WRITE (HSIL,0,0,1)
      CALL WRITE (OEQ ,0,0,1)
      CALL WRITE (HECT,0,0,1)
      CALL WRITE (SCR2,0,0,1)
      CALL CLOSE (SCR2,1)
      CALL CLOSE (SCR1,1)
      CALL CLOSE (HSIL,1)
      CALL CLOSE (OEQ ,1)
      CALL GOPEN (HBGPDT,Z(BUF1),1)
      CALL WRITE (HBGPDT,Z,NRD,0)
      IF (NNGP .EQ. 0) GO TO 380
      FILE1 = SCR2
      CALL GOPEN (SCR2,Z(BUF6),0)
  360 CALL READ (*1002,*370,SCR2,Z(1),BUF6-1,0,N)
      CALL WRITE (HBGPDT,Z(1),BUF6-1,0)
      GO TO 360
  370 CALL WRITE (HBGPDT,Z(1),N,1)
      CALL CLOSE (SCR2,1)
  380 CALL CLOSE (HBGPDT,1)
      TRL(1) = HBGPDT
      TRL(2) = NRD/4  + NNGP
      CALL WRTTRL (TRL)
      TRL(1) = OEQ
      CALL WRTTRL (TRL)
      TRL(1) = HSIL
      TRL(3) = NNGP + OSIL
      CALL WRTTRL (TRL)
      TRL(1) = ECT
      CALL RDTRL (TRL)
      TRL(1) = HECT
      CALL WRTTRL (TRL)
      FILE1  = ECT
      GO TO 80
C
C     BAD GEOMETRY FOR ELEMENT
C
  400 CBS(3) = -CBS(3)
      NHBDY  = NHBDY - 1
      WRITE  (OUT,410) UWM,CBS(1)
  410 FORMAT (A25,', CHBDY ELEMENT',I9,' HAS NO NORMAL OR BAD GEOMETRY',
     1       ' WHICH MAKES IT UNPLOTTABLE')
      GO TO 310
C
C     RETURN OR ERROR MESSAGES
C
 1000 CALL CLOSE (ECT,1)
      CALL CLOSE (HECT,1)
      CALL CLOSE (GEOM2,1)
      RETURN
C
 1002 CALL MESAGE (-2,0,FILE1)
 1008 CALL MESAGE (-8,0,NAME)
      RETURN
      END
