      SUBROUTINE TRLGA (CASECC,USETD,DLT,SLT,BGPDT,SIL,CSTM,AP,TMLDTB,
     1                 ITRL,ISCR1,ISCR2,ISCR3,EST,NEWSLT,MGG,ISCR4,MPT1)
C
C     THE PURPOSE OF THIS ROUTINE IS TO CONSTRUCT THE AP MATRIX
C     WHICH HAS 1 COLUMN FOR EACH FUNCTION OF TIME
C     AND TO BUILD THE TIME FUNCTION TABLE (FORMAT SHOWN IN TRLGC)
C
      EXTERNAL        ANDF
      INTEGER         CASECC,USETD,DLT,SLT,BGPDT,SIL,CSTM,AP,TMLDTB,
     1                SYSBUF,ANDF,PG(7),NAME(2),SLT1,BGPDT1,CSTM1,SIL1,
     2                MCB(7),IZ(38),FILE,NAMT(2),GVECT(30),TWO1,IZB(4),
     3                MINUS(2),EST,EST1
      COMMON /BLANK / NG
      COMMON /ZZZZZZ/ Z(1)
      COMMON /LOADX / LC,SLT1,BGPDT1,OLD,CSTM1,SIL1,ISIL,EST1,MPT,GPTT,
     1                EDT,N(3),LODC,MASS,NOBLD,IDIT
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /BITPOS/ ISK(11),IUE
      COMMON /ZBLPKX/ ZA(4),IIB
      COMMON /ZNTPKX/ ZB(4),III,IEOL,IEOR
      COMMON /TWO   / TWO1(32)
      COMMON /QVECT / ITRAN,IQVECT
      EQUIVALENCE     (KSYSTM(1),SYSBUF),(Z(1),IZ(1)),(ZB(1),IZB(1))
      DATA    NAME  / 4HTRLG,4HA   /,  NAMT/ 4HDLT ,4HTRLG /
      DATA    ITRAN1, MINUS /4HTRAN,-1,-1  /
C
C     CORE IS ALLOCATED AS FOLLOWS -
C     . EXTERN PHASE (BUILD STATIC LOADS)
C                                                           POINTER
C     DLOAD STUFF--TLOAD ID,RECORD NO.IN DLT,SCALE FACTOR   ILLST
C     EXTERN LOAD LIST IN SLT ((NEX LENGTH)                 ISLLST
C     2  BUFFERS
C     1  G VECTOR (NG)  COMING FROM TOP
C        N.B.  EXTER WILL OPEN NEWSLT,BGPDT,CSTM,SIL
C
C     . DYNAMIC PHASE
C     DLOAD STUFF                                           ILLST
C     EXTERN LOAD LIST                                      ISLLST
C     SIL TO SILD CONVERTER (NG LENGTH)                     ISILD
C     4  BUFFERS
C     2  P SIZE VECTORS
C     COMPRESSED LIST  SILD,A,TAU                           ICLST
C
C     BRING IN DATA FROM CASECC(DLOAD ID -- TSTEP ID)
C
      NSUBL = 0
      NZ    = KORSZ(IZ)
      IBUF1 = NZ - SYSBUF + 1
      NX    = IBUF1 - 1
      CALL GOPEN (CASECC,IZ(IBUF1),0)
      CALL FREAD (CASECC,IZ(1),166,1)
      IDLOAD = IZ(13)
      ITRL   = IZ(38)
      CALL CLOSE (CASECC,1)
      IF (IDLOAD .EQ. 0) GO TO 1020
C
C     BUILD NEW SLT
C
      CALL SSGSLT (SLT,NEWSLT,EST)
C
C     FIND DLOAD, TLOAD
C
      FILE = DLT
      CALL OPEN (*900,DLT,IZ(IBUF1),0)
      CALL READ (*910,*10,DLT,IZ(1),NX,0,IFLAG)
      GO TO 980
C
C     IS IT A DLOAD SET
C
   10 NDLOAD = IZ(3)
      NSIMPL = IFLAG - 3 - NDLOAD
      IF (NDLOAD .EQ. 0) GO TO 100
      K = 3
      DO 20 I = 1,NDLOAD
      K = K + 1
      IF (IZ(K) .EQ. IDLOAD) GO TO 30
   20 CONTINUE
C
C     ITS  A SIMPLE LOAD
C
      GO TO 100
C
C     PROCESS DLOAD SET
C     FORMAT OF DLOAD = SET ID,SCALE,SCALE,ID,SCALE,ID .... -1,-1
C
   30 NZ1 = NX - IFLAG
C
C     BRING  IN  ALL  DLOADS
C
      L =  IFLAG + 1
      CALL READ (*910,*40,DLT,IZ(L),NZ1,0,I)
      GO TO 980
C
C     FIND SELECTED ID
C
   40 ISEL = L
   50 IF (IZ(ISEL) .EQ. IDLOAD) GO TO 70
   60 ISEL = ISEL + 2
      IF (IZ(ISEL+1) .NE. -1) GO TO 60
      ISEL = ISEL + 2
      IF (ISEL-L .GT. I) GO TO 990
      GO TO 50
C
C     FOUND DLOAD SELECTED
C
   70 SCALE = Z(ISEL+1)
C
C     CONVERT SCALE FACTORS TO OVERALL SCALE FACTORS
C     BUILD LIST OF TRIPLES-- TLOAD ID,RECORD NO.IN DLT, SCALE FACTOR
C
      L = ISEL + 2
      M = ISEL + I
      IFLAG  = M
      NSUBL  = 0
   80 IDLOAD = IZ(L+1)
      Z(L)   = Z(L)*SCALE
      K = NDLOAD + 3
      DO 90 I = 1,NSIMPL
      K = K + 1
      IF (IZ(L+1) .EQ. IZ(K)) GO TO 95
C
   90 CONTINUE
      GO TO 990
C
C     FOUND SIMPLE ID
C
   95 IZ(M  ) = IZ(L+1)
       Z(M+1) = Z(L)
      IZ(M+2) = I
      L = L + 2
      M = M + 3
      NSUBL = NSUBL + 1
      IF (IZ(L+1) .GE. 0) GO TO 80
      GO TO 150
C
C     PROCESS SIMPLE LOAD REQUEST
C
  100 M = IFLAG + 1
      IFLAG  = M
      IZ(M ) = IDLOAD
      Z(M+1) = 1.0
      L = NDLOAD + 3
      DO 110 I = 1,NSIMPL
      L = L + 1
      IF (IZ(L) .EQ. IDLOAD) GO TO 120
  110 CONTINUE
      GO TO 990
C
C     FOUND SIMPLE LOAD
C
  120 IF (NDLOAD .NE. 0) I = I + 1
      IZ(M+2) = I - 1
      NSUBL   = 1
C
C     MOVE STUFF TO BOTTOM OF CORE
C
  150 CALL CLOSE(DLT,1)
      ILLST = NZ - NSUBL*3 + 1
      NZ    = NZ - NSUBL*3
      IBUF1 = NZ - SYSBUF + 1
      L     = IFLAG
      K     = ILLST
      DO 160 I = 1,NSUBL
      CALL GOPEN  (DLT,IZ(IBUF1),0)
      CALL SKPREC (DLT,IZ(L+2))
      CALL FREAD  (DLT,IZB,2,0)
      IZ(K) = IZB(2)
      CALL CLOSE (DLT,1)
      IZ(K+1) = IZ(L+2)
      IZ(K+2) = IZ(L+1)
      L = L + 3
      K = K + 3
  160 CONTINUE
C
C     SET UP FOR EXTERN
C
      FILE   = NEWSLT
      NX     = IBUF1 - 1
      ISLLST = ILLST
      NOSLT  = 0
      MCB(1) = SLT
      CALL RDTRL (MCB)
      IF (MCB(1) .LE. 0) NOSLT = -1
      MCB(1) = SIL
      MCB(3) = 0
      CALL RDTRL (MCB)
      NG     = MCB(3)
      IF (NOSLT .NE. 0) GO TO 191
      CALL OPEN (*900,NEWSLT,IZ(IBUF1),0)
      CALL READ (*910,*170,NEWSLT,IZ(1),NX,0,IFLAG)
      GO TO 980
  170 CALL CLOSE (NEWSLT,1)
      M = ILLST
      DO 180 I = 1,NSUBL
      DO 175 J = 3,IFLAG
      IF (IZ(M) .NE. IZ(J)) GO TO 175
C
C     FOUND LOAD TO BUILD
C
      IZ(J) = -IABS(IZ(J))
      GO TO 179
  175 CONTINUE
  179 M = M + 3
  180 CONTINUE
C
C     ZERO LOADS NOT TO BUILD
C
      M = ILLST - IFLAG + 2
      ISLLST = M
      DO 190 J = 3,IFLAG
      IF (IZ(J) .LT. 0) GO TO 185
      IZ(M) = 0
      GO TO 189
  185 IZ(M) = IABS(IZ(J))
  189 M     = M + 1
  190 CONTINUE
      NEX   = IFLAG - 2
      NZ    = NZ - NEX
      NGRAV = 0
      IHARM = 0
      N1    = NEX
      IBUF1 = NZ - SYSBUF + 1
      IBUF2 = IBUF1 - SYSBUF
C
C     SET UP SCRATCH FILE FOR QLOADL
C
      ITRAN  = ITRAN1
      IQVECT = ISCR1
      CALL GOPEN (ISCR1,IZ(IBUF1),1)
      CALL MAKMCB (PG,ISCR2,NG,2,1)
      SLT1   = NEWSLT
      BGPDT1 = BGPDT
      CSTM1  = CSTM
      SIL1   = SIL
      EST1   = EST
      MASS   = MGG
      MPT    = MPT1
      CALL GOPEN (PG,IZ(IBUF2),1)
      LC     = IBUF2 - 1
      CALL EXTERN (NEX,NGRAV,GVECT,IZ(ISLLST),PG,N1,IHARM)
      CALL CLOSE  (PG,1)
      CALL WRTTRL (PG)
      CALL WRITE  (ISCR1,MINUS,2,1)
      CALL CLOSE  (ISCR1,1)
      IF (NGRAV .EQ. 0) GO TO 191
C
C     DO GRAVITY LOADS
C
      MCB(1) = MGG
      CALL RDTRL (MCB)
      IF (MCB(1) .LE. 0) CALL MESAGE (-56,0,NAVE)
C
C     SAVE LOAD LIST IN CORE
C
      CALL GOPEN (ISCR4,IZ(IBUF2),1)
      CALL WRITE (ISCR4,IZ(ISLLST),3*NSUBL+NEX,1)
      CALL CLOSE (ISCR4,1)
      CALL GRAVL1 (NGRAV,GVECT,ISCR3,IHARM)
      CALL SSG2B (MGG,ISCR3,0,TMLDTB,0,1,1,AP)
      CALL GRAVL2 (NGRAV,TMLDTB,PG)
      N1 = N1 + NGRAV
C
C     RESTORE LOAD LIST TO CORE
C
      CALL GOPEN (ISCR4,IZ(IBUF2),0)
      CALL FREAD (ISCR4,IZ(ISLLST),3*NSUBL+NEX,1)
      CALL CLOSE (ISCR4,1)
C
C     BUILD SIL  TO SILD CONVERTER
C
  191 CONTINUE
      FILE   = USETD
      CALL GOPEN (USETD,IZ(IBUF1),0)
      MCB(1) = USETD
      CALL RDTRL (MCB)
      LUSETD = MCB(2)
      CALL FREAD (USETD,IZ(1),LUSETD,1)
      CALL CLOSE (USETD,1)
      ISILD  = ISLLST - NG
      MSKUE  = TWO1(IUE)
      L    = ISILD
      DO  200 I = 1,LUSETD
      IF (ANDF(IZ(I),MSKUE) .NE. 0) GO TO 200
      IZ(L)= I
      L    = L + 1
  200 CONTINUE
      NZ   = NZ - NG
C
C     BEGIN LOOP ON EACH TLOAD CARD
C
      IBUF1 = NZ - SYSBUF + 1
      ICLST = 2*LUSETD + 1
      IBUF2 = IBUF1 - SYSBUF
      IBUF3 = IBUF2 - SYSBUF
      CALL MAKMCB (MCB,AP,LUSETD,2,1)
      CALL GOPEN (AP,IZ(IBUF2),1)
      ITERM = 0
      CALL GOPEN (TMLDTB,IZ(IBUF3),1)
      IQVRN = 0
      IBUF4 = IBUF3 - SYSBUF
      CALL GOPEN (ISCR3,IZ(IBUF4),1)
      NZ    = IBUF4 - 1
      IF (NZ .LT. 5*LUSETD) GO TO 980
      DO 1000 ILOOP = 1,NSUBL
C
C     ZERO AP AND TAU AREA
C
      K = 2*LUSETD
      DO 210 I = 1,K
      Z(I) = 0.0
  210 CONTINUE
C
C     FIND APPROPRIATE STATIC LOAD
C
      K = ILLST + (ILOOP-1)*3
      SCALE  = Z(K+2)
      IDLOAD = IZ(K )
      IDLTR  = IZ(K+1)
      IF (NOSLT .NE. 0) GO TO 300
      K = ISLLST - 1
      M = 0
      DO 220 I = 1,NEX
      L = K + I
      IF (IZ(L) .EQ. IDLOAD) GO TO 221
      IF (IZ(L) .NE. 0) M = M + 1
  220 CONTINUE
      GO TO 300
C
C     POSITION TO PROPER AP RECORD
C
  221 FILE = PG(1)
      CALL GOPEN (PG,IZ(IBUF1),0)
      CALL SKPREC (PG,M)
      CALL INTPK (*290,PG,0,1,0)
  240 IF (IEOL .NE. 0) GO TO 290
      CALL  ZNTPKI
      ZB(1) = ZB(1)*SCALE
      K = ISILD + III - 1
      K = IZ(K)
      Z(K) = ZB(1)
      GO TO 240
  290 CALL CLOSE (PG,1)
C
C     PROCESS DLT STUFF
C
  300 CALL GOPEN (DLT,IZ(IBUF1),0)
      FILE = DLT
      CALL SKPREC (DLT,IDLTR)
      CALL FREAD (DLT,GVECT,8,0)
C
C     READS AND BUILDS COMPRESSED LIST SILD,AI,TAU,FOR ALL AI.S
C
  320 CALL READ (*910,*330,DLT,IZB,4,0,IFLAG)
      L    = IZB(1)
      Z(L) = ZB(2) + Z(L)
      Z(L+LUSETD) = ZB(3)
      GO TO 320
  330 CALL CLOSE (DLT,1)
      IQR = 0
      ASSIGN 370 TO IRETN
      M = 0
      K = ICLST
      DO 336 I = 1,LUSETD
      IF (Z(I) .EQ. 0.0) GO TO 336
      Z(I)   = Z(I)*SCALE
      M      = M + 1
      IZ(K ) = I
      Z(K+1) = Z(I)
      Z(K+2) = Z(I+LUSETD)
      K      = K + 3
  336 CONTINUE
C
C     SORT ON TAU
C
  339 K = 3*M + ICLST - 4
      IF (ICLST .GT. K) GO TO 1335
      DO 335  L = ICLST,K,3
      IF (Z(L+5).GT.Z(L+2) .OR. (Z(L+5).EQ.Z(L+2) .AND.
     1   IZ(L+3).GE.IZ(L))) GO TO 335
      LL      = L
      IZ(K+4) = IZ(L+3)
      Z(K+5)  = Z(L+4)
      Z(K+6)  = Z(L+5)
  338 IZ(LL+3)= IZ(LL)
      Z(LL+4) = Z(LL+1)
      Z(LL+5) = Z(LL+2)
      LL      = LL - 3
      IF (LL.GE.ICLST .AND. (Z(K+6).LT.Z(LL+2) .OR. (Z(K+6).EQ.Z(LL+2)
     1   .AND. IZ(K+4).LT.IZ(LL)))) GO TO 338
      IZ(LL+3)= IZ(K+4)
      Z(LL+4) = Z(K+5)
      Z(LL+5) = Z(K+6)
  335 CONTINUE
 1335 CONTINUE
C
C     OUTPUT PVECTOR FOR EACH UNIQUE TAU
C
      L    = ICLST
CWKBR 8/94 ALPHA  341 TAUO = Z(L+2)              
  341 ITAUO = IZ(L+2)            
      CALL BLDPK (1,1,AP,0,0)
  345 ZA(1)= Z(L+1)
      IIB  = IZ(L)
      CALL ZBLPKI
      L = L +3
CWKBR 8/94 ALPHA IF (L.LT.3*M+ICLST .AND. Z(L+2).EQ.TAUO) GO TO 345           
      IF (L.LT.3*M+ICLST .AND. IZ(L+2).EQ.ITAUO) GO TO 345     
      CALL BLDPKN (AP,0,MCB)
C
C     PUT OUT LINE OF TIME TABLE
C
      ITERM = ITERM + 1
      CALL WRITE (TMLDTB,ITERM,1,0)
      CALL WRITE (TMLDTB,IDLOAD,1,0)
      CALL WRITE (TMLDTB,GVECT,1,0)
CWKBR 8/94 ALPHA CALL WRITE (TMLDTB,TAUO,1,0)                    
      CALL WRITE (TMLDTB,ITAUO,1,0)              
      CALL WRITE (TMLDTB,GVECT(3),6,0)
      CALL WRITE (TMLDTB,IQR,1,0)
      IF (L .GE. ICLST+3*M) GO TO IRETN, (370,390)
      GO TO 341
C
C     FIND PROPER QVEC RECORD
C
  370 CONTINUE
      IF (NOSLT .NE. 0) GO TO 1000
      CALL GOPEN (ISCR1,IZ(IBUF1),0)
      FILE = ISCR1
  380 CALL READ (*450,*920,ISCR1,IQVID,1,0,IFLAG)
      IF (IQVID .EQ.     -1) GO TO 450
      IF (IQVID .EQ. IDLOAD) GO TO 390
      CALL FWDREC (*910,ISCR1)
      GO TO 380
C
C     BUILD LIST OF SILD,AI,TAU FROM QVEC STUFF
C
  390 CALL FREAD (ISCR1,M,1,0)
      K = ICLST
      IF (M .EQ. -1) GO TO 450
      DO 400 I = 1,M
      CALL FREAD (ISCR1,ZB,2,0)
      ZB(2) = ZB(2)*SCALE
      J = ISILD + IZB(1) - 1
      J = IZ(J)
      IZ(K)  = J
      Z(K+1) = ZB(2)
      Z(K+2) = Z(J+LUSETD)
      K = K + 3
  400 CONTINUE
      IQVRN = IQVRN + 1
      IQR   = IQVRN
      CALL FREAD (ISCR1,IZ(K),9,0)
      CALL WRITE (ISCR3,IZ(K),9,0)
      ASSIGN 390 TO IRETN
      GO TO 339
C
C     END OF QVECT PROCESSING
C
  450 CALL CLOSE (ISCR1,1)
C
C     END OF TLOAD CARD LOOP
C
 1000 CONTINUE
      CALL CLOSE (AP,1)
      CALL WRTTRL (MCB)
      CALL CLOSE (ISCR3,1)
C
C     APPEND QVECT STUFF TO TMLDTB
C
      CALL GOPEN (ISCR3,IZ(IBUF1),0)
      FILE = ISCR3
      CALL WRITE (TMLDTB,0,0,1)
      CALL READ (*1010,*1010,ISCR3,IZ(1),NZ,0,IFLAG)
      GO TO 980
 1010 CALL WRITE (TMLDTB,IZ(1),IFLAG,1)
      CALL CLOSE (TMLDTB,1)
      MCB(1) = TMLDTB
      MCB(2) = ITERM
      MCB(3) = IFLAG
      CALL WRTTRL (MCB)
      CALL CLOSE (ISCR3,1)
 1020 CONTINUE
      RETURN
C
C     FATAL ERRORS
C
  900 IP1 = -1
  901 CALL MESAGE (IP1,FILE,NAME)
      RETURN
  903 CALL MESAGE (-61,0,NAME)
      RETURN
  910 IP1 = -2
      GO TO 901
  920 IP1 = -3
      GO TO 901
  980 CALL MESAGE (-8,0,NAME)
      GO TO 903
  990 CALL MESAGE (-31,IDLOAD,NAMT)
      RETURN
      END
