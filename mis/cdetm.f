      SUBROUTINE CDETM (METHOD,EED,MDD,BDD,KDD,LAMA,PHID,OCEIGS,NFOUND,
     1                  SCR1,SCR2,SCR3,SCR4,SCR5,SCR6,SCR7,SCR8)
C
C     SOLVES COMPLEX EIGENVALUE PROBLEM BY DETERMINANT METHOD
C
      INTEGER          EED,BDD,PHID,OCEIGS,SCR1,SCR2,SCR3,SCR4,SCR5,
     1                 SCR6,SCR7,SYSBUF,IZ(1),FA,FL,FU,SR1,SR2,SR3,POWR,
     2                 IPOLE(2),EIGC(2),IHEAD(50),POIN,DRETN,IPS(3),
     3                 IPDET(3),FILE,NAME(2),SCR8,OTPE,ILUSP(2),
     4                 ISCR2(7),ISP(2)
      REAL             LU
      DOUBLE PRECISION ZD(1),D1,D2,D3,D4,D5,D6,MINDA,RL,PR,DT1,PI,DR,DI,
     1                 PSR(3),PSI(3),DSR(3),DSI(3),PKR(3),PKI(3),
     2                 DETR(3),DETI(3),DT2,DT3,D7,D8,PTR,PTI,D9,D10,
     3                 HK1R,HK1I,HKR,HKI,LAMDKR,LAMDKI,DELTKR,DELTKI,
     4                 GKR,GKI,ROOTR,ROOTI,LAMK1R,LAMK1I,HKP1R,HKP1I,
     5                 H1BAR,H2BAR,H3BAR,TEST,AMCB(2),BMCB(2),CMCB(2),
     6                 DPI,D2PI,RADDEG,DEGRAD,D4PISQ,
     7                 DDISTX,DDIST2,ZZ,ZDKM1,ZDK
      COMMON /MACHIN/  MACH
      COMMON /SYSTEM/  KSYSTM(65)
      COMMON /ZZZZZZ/  Z(1)
      COMMON /CONDAD/  DPI,D2PI,RADDEG,DEGRAD,D4PISQ
      COMMON /OUTPUT/  HEAD(1)
      COMMON /MSGX  /  NMSGX,MAXGX
      COMMON /SADDX /  NOMAT,LCADD,MCBA(12),MCBB(12),MCBC(12),MCBD(12),
     1                 MCBE(12),MC(7)
      COMMON /CDCMPX/  FA(7),FL(7),FU(7),SR1,SR2,SR3,DR,DI,POWR,NX,
     1                 MINDA,IB
      EQUIVALENCE      (KSYSTM(1),SYSBUF), (KSYSTM(2),OTPE),
     1                 (AMCB(1),MCBA(9)), (BMCB(1),MCBB(9)),
     2                 (CMCB(1),MCBC(9)), (Z(1),IZ(1),ZD(1))
      DATA    IPOLE ,  EIGC,  IHEAD,        POIN,   NAME            /
     1        257,4 ,  207,2, 0,1009,1,47*0, 4HPOIN, 4HCDET, 4HM     /
      DATA    NIT,IM1, SIGN, NUMINT, IZ2,IZ3, IZ4,IZ5, IZ6,IZ7, IZ8 /
     1        20, 1,   -1.0, 4,      2,  3,   4,  5,   6,  7,   8   /
C
C     DEFINITION OF VARIABLES
C
C     METHOD   SELECT SET OF POLES AND EIGC CARDS
C     EED      EIGENVALUE EXTRACTION DATA BLOCK
C     MDD      MASS MATRIX      - DIRECT OR MODAL
C     BDD      DAMPING MATRIX   - DIRECT OR MODAL
C     KDD      STIFFNESS MATRIX - DIRECT OR MODAL
C     LAMA     EIGENVALUE FILE
C     PHID     EIGENVECTOR FILE
C     OEIGS    EIGENVALUE SUMMARY FILE
C     NFOUND   TOTAL NUMBER OF EIGENVALUES FOUND IN ALL REGIONS
C     SCR      SCRATCHES
C     IPOLE    LOCATE WORDS FOR POLES
C     EIGC     LOCATE WORDS FOR EIGC CARDS
C     EPSI     CONVERGENCE CRITERION
C     IBUF     POINTER TO BUFFER
C     NPOLE    NUMBER OF POLES TO BE USED
C     IPOLES   POINTER TO START OF POLES - 4 WORDS PER POLE ID,X,Y,MUL
C     NREGN    NUMBER OF REGIONS
C     IREG1    POINTER TO WORDS DESCRIBING REGIONS
C     INORM    NORMALIZATION METHOD - 1 = MAX,  0 = POINT
C     ISIL     POINTER FOR NORM IF  NORM = 0
C     NE       ESTIMATED NUMBER OF ROOTS IN REGION
C     ND       DESIRED NUMBER OF ROOTS
C     LREGN    LENGTH OF BLOCK DESCRIBING REGION
C     NPASS    NUMBER OF PASSES THROUGH STARTING POINTS
C     NCHANG   NUMBER OF CHANGES OF CONVERGENCE CRITERIA
C     NMOVES   NUMBER OF STARTING POINT MOVES
C     NDCOMP   NUMBER OF DECOMPOSITIONS
C     NFAIL    NUMBER OF FAILURES TO INTERATE TO A ROOT
C     NOUTSD   NUMBER OF PREDICTIONS OUTSIDE REGION
C     ITERM    REASON FOR TERMINATION  0 - FOUND REQUESTED NUMBER
C                                      1 - NO MORE IN REGIONS
C     IRGP     POINTER FOR CURRENT REGION
C     ICNT     NUMBER OF INTERATIONS
C     NIT      MAXIMUM NUMBER OF INTERATIONS/ROOT
C     NUMINT   MAXIMUM NUMBER OF CONVERGENCE  CHANGES
C     NROW     ORDER OF PROBLEM
C     ICMPX    SWITCH IF ROOTS FOUND ARE COMPLEX CONJUGATE -0  NOT-1
C     ISPNT    POINTER TO CURRENT 3 STARTING POINTS
C     PS       SORTED 3 STARTING POINTS
C     DS       SORTED 3 DET OF STARTING POINTS
C     IPS      POWERS OF STARTING POINTS
C     P        TRIAL EIGENVALUE
C     D        SCALED SWEPT DETERMINANT AT P
C     IFPASS   FLAG TO SIGNAL ROOT FROUND ON PASS 1, 0 IF NOT
C     IPOINT   NUMBER OF STARTING POINTS USED IN CURRENT REGION
C     ILUSP    INDEX TO LAST USED STARTING POINT (1ST OF 3) IN EACH
C              SUBRGN
C     NSBRGN   NUMBER OF SUBREGIONS IN PROBLEM THIS REGION
C     NSBDON   FLAG MARKING COMPLETED SUBREGION
C     ISP      POINTS NEAREST AND NEXT NEAREST THE ORIGIN
C
C     STRUCTURE OF REGION
C
C     A1,B1,A2,B2,XL,NE,ND,NF,POINTER TO NEXT REGION (ZERO IF LAST),RL,
C     X  (12  WORDS)
C
C     STARTING POINTS  8NE + 8 WORDS  - D.P. COMPLEX
C     DETERMINANTS     8NE + 8 WORDS    D.P. COMPLEX
C     SCALE FACTORS    4NE + 4 WORDS    2 INTEGERS PER STARTING POINT
C
C     ROOTS   4ND WORDS                 D.P. COMPLEX
C
C
C     DEFINE EPSI (CONVERGENCE CRITERION)
C
      EPSI = 1.0E-16
      IF (MACH.EQ.5 .OR. MACH.EQ.21) EPSI = 1.0E-12
C
      LC   = KORSZ(Z)
      IBUF = LC - SYSBUF - 1
      LC   = (IBUF/2)*2 - 1
      NOSING = 1
      CALL SSWTCH (7,IPRT)
      ISING = 0
      FA(1) = KDD
      CALL RDTRL (FA(1))
      IF (FA(1) .LE. 0) GO TO 1
      IDD = KDD
      GO TO 9
    1 FA(1) = MDD
      CALL RDTRL (FA(1))
      IF (FA(1) .LE. 0) GO TO 2
      IDD = MDD
      GO TO 9
    2 FA(1) = BDD
      CALL RDTRL (FA(1))
      IF (FA(1) .LE. 0) GO TO 990
      IDD   = BDD
    9 FA(1) =-SCR2
      FA(5) = 4
      FL(1) = IDD
      CALL RDTRL (FL(1))
      FL(4) = 4
      FL(5) = 4
      FU(1) = IDD
      CALL RDTRL (FU(1))
      FU(4) = 5
      FU(5) = 4
      SR1   = SCR3
      SR2   = SCR4
      SR3   = SCR5
      FL(1) = SCR6
      FU(1) = SCR7
      DO 10 I = 1,7
      MCBA(I) = 0
      MCBB(I) = 0
      MCBC(I) = 0
      MC(I)   = 0
   10 CONTINUE
      MCBA(1) = KDD
      MCBB(1) = BDD
      MCBC(1) = MDD
      CALL RDTRL (MCBA(1))
      CALL RDTRL (MCBB(1))
      CALL RDTRL (MCBC(1))
C
C     MUST HAVE  B OR M MATRICES
C
      IF (MCBB(1) .LT. 0) MCBB(1) = 0
      IF (MCBC(1) .LT. 0) MCBC(1) = 0
      IF (MCBB(1)+MCBC(1) .EQ. 0) GO TO 990
      NROW  = MAX0(MCBA(3),MCBB(3),MCBC(3))
      ICMPX = 0
      IF (MCBA(5).GT.2 .OR. MCBB(5).GT.2 .OR. MCBC(5).GT.2) ICMPX = 1
      AMCB(1) = 1.0D0
      AMCB(2) = 0.D0
      MC(2)   = MCBA(2)
      MC(3)   = MCBA(3)
      MC(4)   = MCBA(4)
      MC(5)   = 4
      MCBA(8) = 4
      MCBC(8) = 4
      MCBB(8) = 4
      NOMAT   = 3
      MC(1)   = SCR2
      NDESRD  = 0
C
C     PICK UP AND STORE ANY POLES
C
      FILE = EED
      CALL PRELOC (*950,IZ(IBUF),EED)
      NPOLE = 0
      CALL LOCATE (*40,IZ(IBUF),IPOLE(1),IFLAG)
C
C     FOUND POLE CARDS
C
   20 LC = LC - 4
   30 CALL READ (*970,*40,EED,IZ(LC),4,0,IFLAG)
      IF (IZ(LC) .NE. METHOD) GO TO 30
      NPOLE = NPOLE + 1
      GO TO 20
   40 IPOLES = LC + 4
C
C     STORE REGIONS
C
      NREGN = 0
      CALL LOCATE (*990,IZ(IBUF),EIGC(1),IFLAG)
   50 CALL READ (*970,*990,EED,IZ(1),10,0,IFLAG)
      IF (METHOD.EQ.IZ(1) .OR. METHOD.EQ.-1) GO TO 70
C
C     SKIP REMAINDER OF EIGC CARD
C
   60 CALL READ (*970,*990,EED,IZ(1),7,0,IFLAG)
      IF (IZ(IZ6) .NE. -1) GO TO 60
      GO TO 50
C
C     EIGC CARD FOUND - ALLOCATE CORE + BUILD UP REGIONS
C
   70 INORM = 0
      IF (IZ(IZ4) .NE. POIN) INORM = 1
      ISIL = IZ(IZ6)
      IF (Z(IZ8) .NE. 0.0) EPSI = Z(IZ8)
C
C     PROCESS EACH REGION DEFINITION
C
   80 CALL READ (*970,*980,EED,Z(1),7,0,IFLAG)
      IF (IZ(IZ7) .LT. 0) GO TO 130
      NREGN = NREGN + 1
      ALPH1 = Z(  1)
      W1    = Z(IZ2)
      ALPH2 = Z(IZ3)
      W2    = Z(IZ4)
      XL    = Z(IZ5)
      NE    = IZ(IZ6)
      ND    = IZ(IZ7)
      IF (ND .EQ. 0) ND = 3*NE
      LREGN = 20*NE + 4*ND + 32
      NDESRD = NDESRD + ND
      IF (NREGN .EQ.1) GO TO 90
      IZ(LC+8) = LC - LREGN
   90 LC = LC - LREGN
      IF (LC    .LE. 0) GO TO 1000
      IF (NREGN .NE. 1) GO TO 100
      IREG1 = LC
C
C     ZERO REGION
C
  100 K = LC - 1
      DO 110 I = 1,LREGN
      K = K + 1
      IF (I .EQ. 9) GO TO 110
      IZ(K) = 0
  110 CONTINUE
C
C     STORE CONSTANTS
      Z(LC  ) = ALPH1
      Z(LC+1) = W1
      Z(LC+2) = ALPH2
      Z(LC+3) = W2
      Z(LC+4) = XL
      IZ(LC+5)= NE
      IZ(LC+6)= ND
C
C     DISTRIBUTE  STARTING POINTS
C
      D1 = ALPH2 - ALPH1
      D2 = W2 - W1
      RL = DSQRT(D1*D1+D2*D2)
      Z(LC+9) = RL
      D1 = D1/RL
      D2 = D2/RL
      J  = (LC+1)/2 + 6
      D3 = RL/FLOAT(4*NE +4)
      ZD(J  ) = D1*D3 + ALPH1
      ZD(J+1) = D2*D3 + W1
      K  = 2*NE + 1
      D3 = RL/FLOAT(K+1)
      D1 = D1*D3
      D2 = D2*D3
      DO 120 I = 1,K
      J  = J + 2
      ZD(J  ) = ZD(J-2) + D1
      ZD(J+1) = ZD(J-1) + D2
  120 CONTINUE
      GO TO 80
  130 LCADD = LC - 1
      NX = LCADD
      IZ(LC+8) = 0
      CALL CLOSE (EED,1)
      IF (LC-4*NROW .LE. 0) GO TO 1000
C
C     INITIALIZE CUMULATIVE POINTERS
      IFAIL  = 0
      NFOUND = 0
      NPASS  =-1
      NCHANG = 0
      NMOVES = 0
      NDCOMP = 0
      NFAIL  = 0
      NOUTSD = 0
      ITERM  = 1
      IFPASS = 1
C
C     RETURN HERE TO SEARCH ALL REGIONS AGAIN
C
  140 NPASS = NPASS + 1
      IF (IFPASS .EQ. 0) GO TO 690
      IFPASS = 0
      IRGP   = IREG1
C
C     FIND REGION WHICH LACKS ROOTS
C
      DO 160 I = 1,NREGN
      IF (IZ(IRGP+6) .GT. IZ(IRGP+7)) GO TO 170
      IRGP = IZ(IRGP+8)
  160 CONTINUE
C
C     ALL REGIONS HAVE ENOUGH ROOTS - EXIT
C
      GO TO 710
C
C     PICK UP REGION POINTERS AND PARAMETERS
C
  170 ALPH1 =  Z(IRGP  )
      W1    =  Z(IRGP+1)
      ALPH2 =  Z(IRGP+2)
      W2    =  Z(IRGP+3)
      XL    =  Z(IRGP+4)
      NE    = IZ(IRGP+5)
      ND    = IZ(IRGP+6)
      NF    = IZ(IRGP+7)
      RL    = Z(IRGP+9)
      XVR   = (ALPH2-ALPH1)/RL
      YVR   = (W2-W1)/RL
      IPOINT= 0
      ISPNT = (IRGP+1)/2 + 4
C
C     FIND POINTS CLOSEST TO AND NEXT CLOSEST TO ORIGIN THUS DIVIDING
C     REGION INTO TWO SUBREGIONS
C
      ISPT1 = (IRGP+13)/2
      LSPT  =  ISPT1 + 2*(2*NE+2) - 2
      DDISTX= 0.
      NXORG = 0
      NRORG = ISPT1
      DDIST2= ZD(ISPT1)*ZD(ISPT1) + ZD(ISPT1+1)*ZD(ISPT1+1)
      ISPT2 = ISPT1 + 2
      DO 173 I = ISPT2,LSPT,2
      ZZ = ZD(I)*ZD(I) + ZD(I+1)*ZD(I+1)
      IF (ZZ .GT. DDIST2) GO TO 175
      NXORG  = NRORG
      NRORG  = I
      DDISTX = DDIST2
      DDIST2 = ZZ
  173 CONTINUE
  175 IF (ZZ .GT. DDISTX) GO TO 178
  177 NXORG  = I
      DDISTX = ZZ
      GO TO 179
  178 IF (NXORG) 177,179,177
C
C     CALCULATE THE NUMBER OR SUBREGIONS, NSBRGN. THERE MUST BE AT LEAST
C     3 POINTS EACH SIDE OF BISECTOR IN ORDER TO HAVE 2 SUBREGIONS.
C          ISPT2+2 .LE. (NRORG+NXORG)/2 .LE. LSPT-4
C
  179 CONTINUE
      IF (2*(ISPT2+2).LE.NRORG+NXORG .AND. NRORG+NXORG.LE.(LSPT-4)*2)
     1    GO TO 185
C
C     ONLY ONE SUBREGION
C     FIND FIRST UNEVALUATED POINT
C
      NSBRGN = 1
      K = IRGP + 16*NE + 32
      L = 2*NE
      DO 180 J = 1,L
      IF (IZ(K) .EQ. 0) GO TO 196
      K = K + 2
      IPOINT = IPOINT + 1
      ISPNT  = ISPNT  + 2
  180 CONTINUE
C
C     ALL TRIED  GO TO BEGINNING
C
      IPOINT = 0
      ISPNT  = (IRGP+1)/2 + 4
      GO TO 196
C
C     TWO SUBREGIONS EXIST. DETERMINE STARTING POINTS FOR EACH
C
  185 NSBRGN = 2
      NSBDON = 0
      ISP(1) = NRORG
      IF (NRORG .LT. NXORG) ISP(1) = NXORG
      ISP(2) = NRORG + NXORG - ISP(1)
      KREG   = 2
      ILUSP(1) = ISP(1)
      ILUSP(2) = ISP(2) - 2
      GO TO 192
C
C     RETURN HERE TO GET NEW STARTING POINT (OR NEW REGION IF NECESSARY)
C     DETINES  ISPNT
C
  190 IF (NSBRGN .EQ. 1) GO TO 196
      IF (NSBDON-1) 192,195,1945
C
C     CHANGE SUBREGIONS
C
  192 KREG = 3 - KREG
      GO TO (194,195), KREG
C
C     PROCESS FIRST SUBREGION
C
  194 ISPNT = ILUSP(1)
      LS    = ISP  (2)
      ALOC1 = ZD(LS  )
      WLOC1 = ZD(LS+1)
      ILUSP(1) = ILUSP(1) + 2
      IF (ISPNT+4 .EQ. LSPT) GO TO 1942
      PR = .45*ZD(ISPNT+4) + .55*ZD(ISPNT+6) - ALOC1
      PI = .45*ZD(ISPNT+5) + .55*ZD(ISPNT+7) - WLOC1
 1940 IPOINT = (ISPNT -ISPT1)/2 + 1
      GO TO 220
C
C     PROCESS LAST SET OF STARTING IN FIRST SUBREGION
C
 1942 NSBDON = NSBDON + 1
      PR = .45*ZD(ISPNT+4) + .55*ALPH1 - ALOC1
      PI = .45*ZD(ISPNT+5) + .55*W1    - WLOC1
      GO TO 1940
C
C     SUBREGION 2 IS COMPLETE.  IS SUBREGION 1 FINISHED AS WELL
C
 1945 IF (NSBDON .EQ. 3) GO TO 680
      GO TO 194
C
C     PROCESS SUBREGION 2
C
  195 ISPNT = ILUSP(2) - 2
      ILUSP(2) = ILUSP(2) - 2
      LS    = ISP(1)
      ALOC1 = ZD(LS)
      WLOC1 = ZD(LS+1)
      IF (ISPNT .EQ. ISPT1) GO TO 1952
      PR = -(.45*ZD(ISPNT-2) + .55*ZD(ISPNT  )) + ALOC1
      PI = -(.45*ZD(ISPNT-1) + .55*ZD(ISPNT+1)) + WLOC1
      GO TO 1940
C
C     LAST SET OF STARTING POINTS IN SUBREGION2 TO PROCESS
C
 1952 NSBDON = NSBDON + 2
      PR = -(.45*ZD(ISPT1  ) + .55*ZD(ISPNT  )) + ALOC1
      PI = -(.45*ZD(ISPT1+1) + .55*ZD(ISPNT+1)) + WLOC1
      GO TO 1940
C
C     ONLY ONE SUBREGION PROCESS FROM END TO END
C
  196 IPOINT = IPOINT + 1
      ISPNT  = ISPNT+2
      IF (IPOINT .GT. 2*NE) GO TO 680
C
C     FIND OUT IF ANY DETERMINT EVALUATIONS ARE NECESSARY
C
C     COMPUTE LOCAL SEARCH REGION DESCRITIONS
C
      ALOC1 = ALPH1
      WLOC1 = W1
      IF (IPOINT .EQ. 2*NE) GO TO 210
      PR = .45*ZD(ISPNT+4) + .55*ZD(ISPNT+6) - ALOC1
      PI = .45*ZD(ISPNT+5) + .55*ZD(ISPNT+7) - WLOC1
      GO TO 220
  210 PR = ALPH2 - ALOC1
      PI = W2 - WLOC1
  220 RLL= DSQRT(PR*PR+PI*PI)
      K  = IRGP + 16*NE + 24 + 2*IPOINT
      I  = 1
      ISING = 0
  230 K  = K + 2
      IF (IZ(K) .NE. 0) GO TO 250
C
C     EVALUATE DETERMINANT
C
      J  = ISPNT + 2*I - 2
      PR = ZD(J  )
      PI = ZD(J+1)
      ASSIGN 240 TO DRETN
      GO TO 810
  240 IZ(K  ) = 1
      IZ(K+1) = POWR
      M = 4*NE + 2 + ISPNT + 2*I
      ZD(M  ) = DR
      ZD(M+1) = DI
  250 I = I + 1
      IF (I .LE. 3) GO TO 230
      IF (ISING.EQ.3 .AND. NPASS.EQ.0) GO TO 701
C
C     SORT STARTING POINTS BY MAGNITUDE OF DET
C
  260 CALL KLOCK (ITIME1)
      K = ISPNT + 4*NE + 4
      L = IRGP + 16*NE + 26 + 2*IPOINT
      CALL CDETM2 (ZD(ISPNT),ZD(K),IZ(L),PSR(1),PSI(1),DSR(1),DSI(1),
     1             IPS(1))
C
C     LOAD STARTING POINTS INTO TRAIL EIGENVALUES
C
      DO 270 I = 1,3
      PKR(I)  = PSR(I)
      PKI(I)  = PSI(I)
      DETR(I) = DSR(I)
      DETI(I) = DSI(I)
      IPDET(I)= IPS(I)
  270 CONTINUE
      DT2 = 1.0D38
C
C     START INTERATION LOOP
C
      ICNT = 1
  280 HK1R = PKR(2) - PKR(1)
      HK1I = PKI(2) - PKI(1)
      HKR  = PKR(3) - PKR(2)
      HKI  = PKI(3) - PKI(2)
      IF (HKR.EQ.0.0D0 .AND. HKI.EQ.0.0D0) GO TO 550
      D1     = HK1R*HK1R + HK1I*HK1I
      LAMDKR = (HKR*HK1R + HKI*HK1I)/D1
      LAMDKI = (HKI*HK1R - HKR*HK1I)/D1
      DELTKR = 1.0D0 + LAMDKR
      DELTKI = LAMDKI
C
C     COMPUTE GK
C
      D1 = LAMDKR*LAMDKR - LAMDKI*LAMDKI
      D2 = 2.0*LAMDKR*LAMDKI
      D3 = D1*DETR(1) - D2*DETI(1)
      D4 = D2*DETR(1) + D1*DETI(1)
      D1 = DELTKR*DELTKR - DELTKI*DELTKI
      D2 = 2.0*DELTKR*DELTKI
      D5 =-D1*DETR(2) + D2*DETI(2)
      D6 =-D2*DETR(2) - D1*DETI(2)
      CALL CSUMM (D3,D4,IPDET(1),D5,D6,IPDET(2),D1,D2,ID1)
      D3 = LAMDKR + DELTKR
      D4 = LAMDKI + DELTKI
      D5 = D3*DETR(3) - D4*DETI(3)
      D6 = D4*DETR(3) + D3*DETI(3)
      CALL CSUMM (D1,D2,ID1,D5,D6,IPDET(3),GKR,GKI,IGK)
C
C     COMPUTE TERM UNDER RADICAL IN EQ. 11
C
      D1 = DETR(1)*LAMDKR - DETI(1)*LAMDKI
      D2 = DETI(1)*LAMDKR + DETR(1)*LAMDKI
      D3 =-DETR(2)*DELTKR + DETI(2)*DELTKI
      D4 =-DETI(2)*DELTKR - DETR(2)*DELTKI
      CALL CSUMM (D1,D2,IPDET(1),D3,D4,IPDET(2),D5,D6,ID1)
      CALL CSUMM (D5,D6,ID1,DETR(3),DETI(3),IPDET(3),D1,D2,ID2)
      D3 = DELTKR*LAMDKR - DELTKI *LAMDKI
      D4 = DELTKI*LAMDKR + DELTKR *LAMDKI
      D5 = D1*D3 - D2*D4
      D6 = D2*D3 + D1*D4
      D1 =-4.0*(DETR(3)*D5 - DETI(3)*D6)
      D2 =-4.0*(DETI(3)*D5 + DETR(3)*D6)
C
C     COMPUTE  GK*GK
C
      D3 = GKR*GKR - GKI*GKI
      D4 = 2.0*GKR*GKI
      CALL CSUMM  (D3,D4,2*IGK,D1,D2,IPDET(3)+ID2,D5,D6,ID1)
      CALL CSQRTN (D5,D6,ID1,ROOTR,ROOTI,IROOT)
      CALL CSUMM  (GKR,GKI,IGK,ROOTR,ROOTI,IROOT,D9,D10,ID3)
      CALL CSUMM  (GKR,GKI,IGK,-ROOTR,-ROOTI,IROOT,D7,D8,ID4)
      IF (ICNT .EQ. 1) GO TO 290
      D1  = D9
      D2  = D10
      ID1 = ID3
      D5  = D9*D9 + D10*D10
      D6  = D7*D7 + D8*D8
      IF (D5 .GE. D6) GO TO 310
      D1  = D7
      D2  = D8
      ID1 = ID4
      GO TO 310
C
C     COMPUTE  NUMERATOR  EQ. 11
C
  290 D1 = D9
      D2 = D10
      ID1= ID3
      M  = 2
      GO TO 310
  300 D1 = D7
      D2 = D8
      ID1= ID4
      M  = 1
  310 D3 =-2.0*(DETR(3)*DELTKR - DETI(3)*DELTKI)
      D4 =-2.0*(DETI(3)*DELTKR + DETR(3)*DELTKI)
      D5 = D1*D1 + D2*D2
      D6 = 10.0**(IPDET(3) - ID1)
      LAMK1R = D6*(D3*D1 + D4*D2)/D5
      LAMK1I = D6*(D4*D1 - D3*D2)/D5
      HKP1R  = LAMK1R*HKR - LAMK1I*HKI
      HKP1I  = LAMK1I*HKR + LAMK1R*HKI
      PR = PKR(3) + HKP1R
      PI = PKI(3) + HKP1I
      IF (ICNT .NE. 1) GO TO 370
      DT3 = 0.0D0
      DO 330 I = 1,3
      DT3 = DT3+DSQRT((PKR(I)-PR)**2 + (PKI(I)-PI)**2)
  330 CONTINUE
      IF (DT3 .GT. DT2) GO TO 340
      PTR = PR
      PTI = PI
      DT2 = DT3
  340 IF (M .EQ. 2) GO TO 300
      PR  = PTR
      PI  = PTI
C
C     DO RANGE CHECKS
C
C
C     COMPUTE U VECTOR
C
  370 XU = PR - ALPH1
      YU = PI - W1
      LU = SQRT(XU*XU + YU*YU)
      IF (LU .EQ. 0.0) GO TO 380
      XU = XU/LU
      YU = YU/LU
      X  = LU*(XU*XVR + YU*YVR)
      Y  = LU*(YU*XVR - XU*YVR)
      IF (ABS(Y).GT.XL/2.0 .OR. X.LT.0.0 .OR. X.GT.RL) GO TO 400
C
C     SEE IF POINT IS IN LOCAL REGION
C
  380 XU = PR - ALOC1
      YU = PI - WLOC1
      LU = SQRT(XU*XU + YU*YU)
      IF (LU .EQ. 0.0) GO TO 390
      XU = XU/LU
      YU = YU/LU
      Y  = LU*(YU*XVR-XU*YVR)
      X  = LU*(XU*XVR+YU*YVR)
      IF (ABS(Y).GT.XL/2.0 .OR. X.LT.0.0 .OR. X.GT.RLL) GO TO 190
C
C     TRY FOR CONVERGENCE
C
  390 ASSIGN 450 TO DRETN
      GO TO 810
C
C     PREDICTED OUTSIDE BIG REGION
C
  400 NOUTSD = NOUTSD + 1
      GO TO 190
C
C     BEGIN CONVERGENCE TESTS
C
  450 IF (ICNT .LE. 2) GO TO 520
      H1BAR = DSQRT(HK1R*HK1R + HK1I*HK1I)
      H2BAR = DSQRT(HKR*HKR + HKI*HKI)
      H3BAR = DSQRT(HKP1R*HKP1R + HKP1I*HKP1I)
  460 TEST  = EPSI*RL
      IF (H1BAR .GT. TEST*1.0E7) GO TO 480
      IF (H2BAR .GT. TEST*1.0E4) GO TO 480
      IF (H3BAR .GT.      H2BAR) GO TO 470
      IF (H3BAR .GT.       TEST) GO TO 480
      GO TO 550
  470 IF (H2BAR .LE.  1.0E-7*RL) GO TO 550
  480 ICNT = ICNT + 1
      IF (ICNT-NIT) 530,500,490
  490 IFAIL = 1
      NFAIL = NFAIL + 1
      GO TO 190
  500 IF (NCHANG.LT.NUMINT .AND. IFAIL.EQ.1) GO TO 510
      GO TO 490
  510 EPSI   = EPSI*10.0
      NCHANG = NCHANG + 1
      GO TO 460
C
C     CONTINUE INTERATIONS
C
  520 ICNT = ICNT + 1
  530 DO 540 I = 1,2
      PKR(I)  = PKR(I+1)
      PKI(I)  = PKI(I+1)
      IPDET(I)= IPDET(I+1)
      DETR(I) = DETR(I+1)
      DETI(I) = DETI(I+1)
  540 CONTINUE
      PKR(3)  = PR
      PKI(3)  = PI
      DETR(3) = DR
      DETI(3) = DI
      IPDET(3)= POWR
      GO TO 280
C
C     ACCEPT CURRENT EIGENVALUE
C
  550 FILE   = LAMA
      NFOUND = NFOUND + 1
      IFPASS = 1
      IF (NFOUND .GT. 1) IM1 = 3
      CALL OPEN (*950,LAMA,IZ(IBUF),IM1)
      ZD(1  ) = PR
      ZD(IZ2) = PI
      CALL WRITE (LAMA,ZD(1),4,1)
      CALL CLOSE (LAMA,2)
C
C     BUILD LOAD FOR FBS
C
      IF (MINDA .EQ. 0.0D0) MINDA = 1.0D-8
      SIGN =-SIGN
      D1   = NROW
      D2   = NFOUND
      J    = 2*NROW
      DO 560 I = 1,J,2
      K    = (I+1)/2
      ZD(I  ) = SIGN*MINDA/(1.0D0+(1.0D0-FLOAT(K)/D1)*D2)
      ZD(I+1) = 0.0D0
  560 CONTINUE
      ISCR2(1) = SR2
      ISCR2(7) = FU(7)
      CALL CDTFBS (ZD(1),ZD(J+1),IZ(IBUF),ISCR2,NROW)
C
C     NORMALIZE
C
      D1 = 0.0D0
      DO 570 I = 1,J,2
      D2 = ZD(I)*ZD(I) + ZD(I+1)*ZD(I+1)
      IF (D2 .LT. D1) GO TO 570
      D3 = ZD(I  )
      D4 = ZD(I+1)
      D1 = D2
  570 CONTINUE
      IF (INORM .EQ. 0) GO TO 600
  580 DO 590 I = 1,J,2
      D5 = (ZD(I)*D3  + ZD(I+1)*D4)/D1
      ZD(I+1) = (D3*ZD(I+1) - D4*ZD(I))/D1
      ZD(I  ) = D5
  590 CONTINUE
      GO TO 610
  600 JJ = 2*ISIL
      D2 = ZD(JJ)*ZD(JJ) + ZD(JJ-1)*ZD(JJ-1)
      IF (D2.EQ.0.0D0 .OR. D1/D2.GT.1.0D6) GO TO 580
      D3 = ZD(JJ-1)
      D4 = ZD(JJ  )
      D1 = D2
      GO TO 580
C
C     WRITE OUT NORMALIZED VECTOR
C
  610 FILE = PHID
      CALL OPEN  (*950,PHID,IZ(IBUF),IM1)
      CALL WRITE (PHID,ZD(1),4*NROW,1)
      CALL CLOSE (PHID,2)
C
C     STORE ACCEPTED VALUE
C
      IZ(IRGP+7) = IZ(IRGP+7) + 1
      NF = NF + 1
      J  = (IRGP+1)/2 + 2*NF + 10*NE + 14
      ZD(J  ) = PR
      ZD(J+1) = PI
      IFAIL   = 0
C
C     CHECK FOR STARTING POINT MOVES
C
      J   = IREG1
      I   = 1
  620 DT1 = 200.0*EPSI*EPSI*Z(J+9)
      M   = 2*IZ(J+5) + 2
      K   = (J+1)/2 + 5
      L   = 1
  630 K   = K + 2
      KKK = J + 16*IZ(J+5) + 26 + 2*L
      IF (DSQRT((ZD(K)-PI)**2+(ZD(K-1)-PR)**2) .GE. DT1) GO TO 650
C
C     SHIFT STARTING POINT
C
      D2  = 1000.0*EPSI*EPSI*Z(J+9)
      ZD(K-1) = DSIGN((Z(J+2)-Z(J  ))/Z(J+9)*D2+ZD(K-1),ZD(K-1))
      ZD(K  ) = DSIGN((Z(J+3)-Z(J+1))/Z(J+9)*D2+ZD(K  ),ZD(K  ))
      NMOVES  = NMOVES + 1
C
C     IF  DETERMINANT EVALUATED - REEVALUATE FOR SHIFT
C
      IF (IZ(KKK) .EQ. 0) GO TO 670
      DT2 = PR
      DT3 = PI
      PR  = ZD(K-1)
      PI  = ZD(K  )
      ASSIGN 640 TO DRETN
      GO TO 810
  640 PR = DT2
      PI = DT3
      KK = K + 4*IZ(J+5) + 4
      ZD(KK  ) = DI
      ZD(KK-1) = DR
      IZ(KKK+1)= POWR
      GO TO 660
C
C     SWEEP ACCEPTED VALUE FROM STORED  DETM-S
C
  650 KK = K + 4*IZ(J+5) + 4
      D2 = ZD(K-1) - PR
      D3 = ZD(K  ) - PI
      D4 = D2*D2 + D3*D3
      D5 = (ZD(KK-1)*D2 + ZD(KK)*D3)/D4
      ZD(KK  ) = (ZD(KK)*D2 - ZD(KK-1)*D3)/D4
      ZD(KK-1) = D5
C
C     SWEEP CONJUGATES S
C
      IF (ICMPX.EQ.1 .OR. DABS(PI).LT.1000.0*Z(J+9)*EPSI) GO TO 660
      D3 = ZD(K) + PI
      D4 = D2*D2 + D3*D3
      D5 = (ZD(KK-1)*D2 + ZD(KK)*D3)/D4
      ZD(KK) = (ZD(KK)*D2 - ZD(KK-1)*D3)/D4
      ZD(KK-1) = D5
  660 ZDKM1 = ZD (KK-1)
      ZDK   = ZD (KK  )
      IZK   = IZ (KKK+1)
      CALL CDETM3 (ZDKM1,ZDK,IZK)
      ZD(KK-1) = ZDKM1
      ZD(KK  ) = ZDK
      IZ(KKK+1)= IZK
  670 L = L + 1
      IF (L .LE. M) GO TO 630
      J = IZ(J+8)
      I = I + 1
      IF (I .LE. NREGN) GO TO 620
      CALL KLOCK  (ITIME2)
      CALL TMTOGO (ITLEFT)
      IF (2*(ITIME2-ITIME1).GT.ITLEFT .AND. NFOUND.NE.NDESRD) GO TO 700
      IF (NF .LT. ND) GO TO 260
C
C     FIND NEXT REGION LACKING ROOTS
C
  680 IF (IZ(IRGP+8) .EQ. 0) GO TO 140
      IRGP = IZ(IRGP+8)
      IF (IZ(IRGP+6) .GT. IZ(IRGP+7)) GO TO 170
      GO TO 680
  690 ITERM = 2
      GO TO 710
C
C     INSUFFICIENT TIME
C
  700 IF (NMSGX .GE. MAXGX) NMSGX = MAXGX - 1
      CALL MESAGE (45,NDESRD-NFOUND,NAME)
      ITERM = 3
      GO TO 710
C
C     SINGULAR MATRIX
C
  701 ITERM = 4
      GO TO 710
C
C     END OF ROUTINE  PUT OUT SUMMARY
C
  710 CALL GOPEN (OCEIGS,IZ(IBUF),1)
      CALL WRITE (OCEIGS,IHEAD(1),10,0)
      IZ(  1) = NFOUND
      IZ(IZ2) = NPASS
      IZ(IZ3) = NCHANG
      IZ(IZ4) = NMOVES
      IZ(IZ5) = NDCOMP
      IZ(IZ6) = NFAIL
      IZ(IZ7) = NOUTSD
      IZ(IZ8) = ITERM
      CALL WRITE (OCEIGS,IZ(1),40,0)
      CALL WRITE (OCEIGS,HEAD(1),96,1)
      IHEAD(3) = 3
      IHEAD(10)= 6
      CALL WRITE (OCEIGS,IHEAD,50,0)
      CALL WRITE (OCEIGS,HEAD,96,1)
      J  = IREG1
      DO 800 I = 1,NREGN
      NE = IZ(J+5)
      K  = (J+1)/2+6
      KK = 4*NE + 4
      KD = J + 27 + 16*NE
      NE = 2*NE + 2
      DO 790 L = 1,NE
      IZ(1)  = L
      Z(IZ2) = ZD(K  )
      Z(IZ3) = ZD(K+1)
      M  = K  + KK
      KD = KD + 2
      IZ(IZ6) = IZ(KD)
C
C     CONVERT TO MAGNITUDE AND PHASE  SCALE ON MAGNITIDE
C     PHASE IN DEGRESS BETWEEN 0 AND 360
C
      D1 = DSQRT(ZD(M)*ZD(M) + ZD(M+1)*ZD(M+1))
      IF (D1 .EQ.  0.0D0) GO TO 760
  720 IF (D1 .GT. 10.0D0) GO TO 740
  730 IF (D1 .LT.  1.0D0) GO TO 750
      GO TO 770
  740 D1 = D1*0.1D0
      IZ(IZ6) = IZ(IZ6) + 1
      GO TO 720
  750 D1 = D1*10.0D0
      IZ(IZ6) = IZ(IZ6) - 1
      GO TO 730
C
C     NOT  EVALUATED
C
  760 Z(IZ4) = 0.0
      Z(IZ5) = 0.0
      GO TO 780
  770 Z(IZ4) = D1
C
C     COMPUTE PHASE
C
      Z(IZ5) = DATAN2(ZD(M+1),ZD(M))*RADDEG
C
C     DETERMINE QUADRANT
C
      IF (Z(IZ5) .LT. 0.) Z(IZ5) = Z(IZ5) + 360.0
  780 CONTINUE
      CALL WRITE (OCEIGS,IZ(1),6,0)
      K = K + 2
  790 CONTINUE
      J = IZ(J+8)
  800 CONTINUE
      CALL CLOSE (OCEIGS,1)
      FA(1) = OCEIGS
      CALL WRTTRL (FA(1))
      RETURN
C
C     INTERNAL SUBROUTINE TO EVALUATE DR,DI AT PR,PI
C
  810 NDCOMP = NDCOMP + 1
C
C     SET UP FOR ADD
C
      BMCB(1) = PR
      BMCB(2) = PI
      CMCB(1) = PR*PR - PI*PI
      CMCB(2) = 2.*PR*PI
      CALL SADD (Z(1),Z(1))
      FA(1) = -IABS(FA(1))
      IF (NOSING .EQ. 0) GO TO 821
      ISAVE = SR2
      SR2   = SCR8
      SCR8  = ISAVE
  821 CALL TMTOGO (KK)
      IF (KK .LE. 0) GO TO 700
      IB = 0
      CALL CDCOMP (*930,Z(1),Z(1),Z(1))
      NOSING = 1
      IF (IPRT .NE. 0) WRITE (OTPE,831) PR,PI,DR,DI,POWR
  831 FORMAT (10X,4D16.7,I8)
C
C     SCALE DETERMINANT BY POLES AND EIGENVALUES PREVIOUSLY FOUND
C
      ID1 = IREG1
      DO 880 ID = 1,NREGN
      ID2 = IZ(ID1+5)
      KK  = IZ(ID1+7)
      IF (KK .EQ. 0) GO TO 870
      KD = 14 + 10*ID2 + (ID1+1)/2
      DO 860 LL = 1,KK
      KD = KD + 2
      D1 = PR - ZD(KD  )
      D2 = PI - ZD(KD+1)
      D3 = D1*D1  + D2*D2
      D4 = (DR*D1 + DI*D2)/D3
      D5 = (DI*D1 - DR*D2)/D3
      DR = D4
      DI = D5
      IF (ICMPX .EQ. 1) GO TO 850
C
C     SWEEP COMPLEX CONJUGATE ROOTS
C
      IF (DABS(ZD(KD+1)) .LT. 1000.0*Z(ID1+9)*EPSI) GO TO 850
      D2 = PI + ZD(KD+1)
      D3 = D1*D1  + D2*D2
      D4 = (DR*D1 + DI*D2)/D3
      D5 = (DI*D1 - DR*D2)/D3
      DR = D4
      DI = D5
  850 CALL CDETM3 (DR,DI,POWR)
  860 CONTINUE
  870 ID1 = IZ(ID1+8)
  880 CONTINUE
C
C     SWEEP POLES
C
      IF (NPOLE .EQ. 0) GO TO 940
      ID1 = IPOLES
      DO 900 ID = 1,NPOLE
      D1 = PR - Z(ID1+1)
      D2 = PI - Z(ID1+2)
      D3 = 1.0D0
      D4 = 0.0D0
      KD = IZ(ID1+3)
      DO 890 ID2 = 1,KD
      D5 = D1*D3 - D2*D4
      D6 = D2*D3 + D1*D4
      D3 = D5
      D4 = D6
  890 CONTINUE
      D1 = D3*D3  + D4*D4
      D2 = (DR*D3 + DI*D4)/D1
      D5 = (DI*D3 - DR*D4)/D1
      DR = D2
      DI = D5
      ID1= ID1 + 4
C
C     SCALE AGAIN
C
      CALL CDETM3 (DR,DI,POWR)
  900 CONTINUE
      GO TO 940
C
C     SINGLULAR MATRIX
C
  930 DR    = 0.0D0
      DI    = 0.0D0
      POWR  = 0
      ISING = ISING + 1
      MINDA = 1.0E-11
      IF (NOSING .EQ. 0) GO TO 940
      NOSING= 0
      ISAVE = SR2
      SR2   = SCR8
      SCR8  = ISAVE
C
C     RETURN
C
  940 IF (IPRT .NE. 0) WRITE (OTPE,831) PR,PI,DR,DI,POWR
      GO TO DRETN, (240,450,640)
C
C     ERROR  MESAGES
C
  950 IP1 = -1
  960 CALL MESAGE (IP1,FILE,NAME)
  970 IP1 = -2
      GO TO 960
  980 IP1 = -3
      GO TO 960
  990 IP1 = -7
      GO TO 960
 1000 IP1 = -8
      GO TO 960
      END
