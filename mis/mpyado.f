      SUBROUTINE MPYADO (ZZ      ,Z      ,ZD      )
C
C     MPYAD PERFORMS THE MATRIX OPERATION
C       (+/-)A    * B (+/-)C = D   OR
C       (+/-)A(T) * B (+/-)C = D
C
C     LAST REVISED  1/92 BY G.CHAN/UNISYS
C     . NEW METHOD 4T WAS ADDED WHICH IS FASTER THAN METHOD 2T UNDER
C       CERTAIN CONDITIONS.
C     . NEW ROUTINE FOR DIAGONAL, IDENTITY, AND ROW VECTOR MATRICES
C     . USER CAN REVERT TO ORIGINAL MPYAD ALL METHODS, BY DIAG 41
C
C
C     LEGEND:
C
C     +---+ + +  IS A MATRIX       +-            \    AN ELEMENT OF
C     |   | | |  BY COLUMNS        |  IS A        \   A, B, OR C IN
C     |   | | |  IN MULTIPLE       |  COLUMN       \     AND
C     |   | | |  PASSES            |             OR   /  AN ELEMENT
C     +---+ + +                    +-                /   OF D OUT
C
C     +-------+  IS A MATRIX
C     |       |  BY ROWS           => OR   INDICATES MATRICES C AND D
C     +-------+  IN MULTIPLE       <=      ARE USING SAME CORE SPACE
C     +-------+  PASSES
C     +-------+
C
C     UPPER CASE LETTER INDICATES UNPACKED MATRIX OR COLUMN
C     LOWER CASE LETTER INDICATES MATRIX OR COLUMN IN STRINGS FORM
C
C
C     METHOD 1NT AND 1T       METHOD 2NT               METHOD 2T
C                                        B                        +-
C         +----+ + +                    /                         |
C         |    | | |                   /                          |B
C     a   | B  | | |          + + +---+ +-                        |
C      \  |    | | |          | | |   | |            +----------+ +-
C       \ +----+ + +          | | | a | |C           |    a     |
C         +----+              | | |   | |            +----------+
C         |    |              + + +---+ +- +-        +----------+ \
C         | D  | <= C                      |         +----------+  \
C         |    |                        => |D                     \ C
C         |    |                           |                       \
C         +----+                           +-                       D
C
C      METHOD 3T     +-                  METHOD 4T     +-
C                    |                                 |
C                    |b                                |b(BANDED)
C                    |                                 |
C                    +-                                +-
C         + + +----+ +-    +-              +---------+ +-
C         | | |    | |     |               |         | |
C         | | | A  | |D  + |C              |    a    | |C
C         | | |    | |     |               |         | |
C         + + +----+ +-    +-              +---------+ +-  +-
C                        ADD ON            +---------+     |
C                        LAST              +---------+     |D(FULL)
C                        PASS                           => |
C                                                          +-
C
      LOGICAL         LAST  ,NULL
      EXTERNAL        ANDF  ,ORF   ,LSHIFT
CWKBI 9/93
      INTEGER PRNTYP(4), NAMEA(2), NAMEB(2), NAMEC(2), NAMED(2), PRCA
      INTEGER         ZZ(6) ,P     ,Q     ,R     ,T     ,OP    ,OPA   ,
     1                OPB   ,OPC   ,OPBC  ,OP2   ,ONE1  ,ONE2  ,P1    ,
     2                PP1   ,PP2   ,PRC   ,PREC  ,PREC1 ,BCD   ,RCB   ,
     3                RCD   ,RC    ,RD    ,RDREW ,WRT   ,WRTREW,CLS   ,
     4                CLSREW,ANDF  ,ORF   ,EOL   ,EOR   ,ACOL  ,ACOL1 ,
     5                ACOLN ,ACORE ,APOINT,POINT ,BCOL  ,BUF1  ,BUF2  ,
     6                BUF3  ,BUF4  ,BUFI  ,BLK   ,BLOCK ,ROW   ,ROWA  ,
     7                AROW  ,AROW1 ,AROWN ,CROW  ,DROW  ,TYPE  ,TYPEA ,
     8                TYPEB ,TYPEC ,TYPED ,TYPEBD,TYPD  ,TYPD1 ,FILE  ,
     9                FILEA ,FILEB ,FILEC ,FILED ,CFILE ,DFILE ,EFILE ,
     O                SCRTCH,SIGNAB,SIGNC ,FIRSTL,SYSBUF,FORM  ,FLAG  ,
     1                DENSC
      DOUBLE PRECISION       AD(2) ,BD(2) ,DD(2) ,ZD(1) ,XND
      DIMENSION       B(4)  ,Z(1)  ,MPY(3),BCD(2),ZERO(4)      ,XNS(1),
     1                NAME(2)      ,BLK(15)      ,METHOD(6)
CNVXNB
      COMMON /LOGOUT/ LOUT
CNVXNE
      COMMON /MACHIN/ MACH  ,IHALF ,JHALF
     1       /MPYQT4/ QT(2) ,LL4   ,JMP(2)
      COMMON /MPYADX/ FILEA(7)     ,FILEB(7)     ,FILEC(7)     ,
     1                FILED(7)     ,NZ    ,T     ,SIGNAB,SIGNC ,PREC1 ,
     2                SCRTCH,TIME
     3       /SYSTEM/ KSYSTM(152)
     4       /TYPE  / PRC(2),NWDS(4)      ,RC(4)
     5       /NAMES / RD    ,RDREW ,WRT   ,WRTREW,CLSREW,CLS
     6       /ZBLPKX/ D(4)  ,DROW
     7       /ZNTPKX/ A(4)  ,IP    ,EOL   ,EOR
     8       /PACKX / TYPED ,TYPD1 ,ONE1  ,PP1   ,INCR1
     9       /UNPAKX/ TYPEBD,ONE2  ,PP2   ,INCR2
      COMMON /NTIME / NITEMS,TMIO  ,TMBPAK,TMIPAK,TMPAK ,TMUPAK,TMGSTR,
     1                TMPSTR,TMT(4),TML(4)
     2       /MPYADZ/ RCB   ,RCD   ,LL    ,LLL   ,JB    ,NBX   ,NDX   ,
     3                JMAX1X,ACOL  ,ACOL1 ,ACOLN ,ACORE ,APOINT,BCOL  ,
     4                CROW  ,FIRSTL,NA    ,NB    ,ND    ,NWDA  ,NWDB  ,
     5                NWDD  ,PREC  ,JMAX  ,INCRA ,BLOCK(20)
     6       /ZZZZZZ/ XND(8500)
      EQUIVALENCE     (KSYSTM( 1),SYSBUF) , (KSYSTM( 2),MOUT ) ,
     1                (KSYSTM(58),KSYS58) , (KSYSTM(40),NBPW )
CWKBI 10/93
     2               ,(KSYSTM(55),IPREC )
      EQUIVALENCE     (A(1)    ,AD(1)   ) , (B(1)    ,BD(1)  ) ,
     1                (D(1)    ,DD(1)   ) , (FILEA(2),M      ) ,
     2                (FILEA(3),N,ROWA  ) , (FILEA(5),TYPEA  ) ,
     3                (FILEB(2),Q       ) , (FILEB(3),R      ) ,
     4                (FILEB(5),TYPEB   ) , (FILEC(5),TYPEC  ) ,
     6                (FILED(5),TYPD    ) , (NZZ     ,BUF1   ) ,
     7                (ACOLN   ,AROWN   ) , (FILEC(7),DENSC  )
      EQUIVALENCE     (BLOCK(2),TYPE    ) , (BLOCK(3),FORM   ) ,
     1                (BLOCK(4),ROW     ) , (BLOCK(5),POINT  ) ,
     2                (BLOCK(6),NBRSTR  ) ,
     3                (BLOCK(8),FLAG    ) , (XND(1)  ,XNS(1) ) ,
     4                (ACOL1   ,AROW1   ) , (ACOL    ,AROW   ) ,
     5                (MPY(1)  ,NAME(1) )
C
      DATA    NAME  / 4HMPYA, 4HD   /, JBEGN /  4HBEGN/, JEND  / 3HEND/
     1        TIME1 / 0. /  , TIME2 /  0.    /, ZERO  /  4*0   /,
     2        METHOD/ 4H1 NT, 4H1 T ,  4H2 NT,  4H2 T  , 4H3 T , 3H4 T/
CWKBI 9/93
      DATA PRNTYP / 2HRS, 2HRD, 2HCS, 2HCD /
CNVXNB
      IF (TYPEA .EQ. 0) TYPEA = IPREC
      IF (TYPEB .EQ. 0) TYPEB = IPREC
      IF (TYPEC .EQ. 0) TYPEC = IPREC
CNVXNE
CWKBNB 7/94 SPR94008
      ITYPEA = TYPEA
      ITYPEB = TYPEB
      ITYPEC = TYPEC
CWKBNE 7/94 SPR94008
C
C     CHECK TO SEE IF THE INPUT MATRICES ARE CONFORMABLE
C
      CALL SSWTCH (19,L19)
      CALL SSWTCH (41,L41)
      NOGO = 0
      FILE = 0
      NOAB = 0
      IF (FILEA(6).EQ.0 .OR. FILEB(6).EQ.0) NOAB = 1
      IROWB = FILEA(2)
      IROWC = FILEA(3)
      IF (T .NE. 0) T = 1
      IF (T .EQ. 0) GO TO 30
      IROWB = FILEA(3)
      IROWC = FILEA(2)
   30 IF (NOAB .EQ. 1) GO TO 50
      IF (FILEB(3) .NE. IROWB) NOGO = 1
      IF (FILEC(1) .LE. 0) GO TO 40
      IF (FILEC(2).NE.FILEB(2) .OR. FILEC(3).NE.IROWC) NOGO = 1
   40 IF (NOGO .EQ. 1) GO TO 560
C
C     PERFORM GENERAL INITIALIZATION
C
   50 MPY(3) = JBEGN
      IF (FILED(1) .GT. 0) CALL CONMSG (MPY,3,0)
      NOUT  = LOUT
C
C  -- USE SINGLE PRECISION ON MACHINES WITH 60 OR 64 BITS PER WORD
C
      IF (NBPW .GE. 60) PREC1 = 1
      OPB   = RDREW
      OPC   = RDREW
      OP2   = WRTREW
      OP    = CLS
      CFILE = FILEC(1)
      IF (CFILE .EQ. 0) TYPEC = 1
      B(2)  = 0.
      B(3)  = 0.
      B(4)  = 0.
      TYPD1 = TYPD
      ONE1  = 1
      ONE2  = 1
      P     = N
      IF (T .NE. 0) P = M
      PP1   = P
      INCR1 = 1
      IF (CFILE.EQ.0  .OR. FILEC(6).EQ.0) CFILE = 0
      IF (FILEB(6).EQ.0 .AND. CFILE.EQ.0) PP1   = 1
      INCR2    = 1
      FILED(2) = 0
      FILED(6) = 0
      FILED(7) = 0
      MPASS3   = 0
      TIME3    = 1.0E+10
      PREC     = PREC1
      IF (PREC .NE. 2) PREC = 1
      IF (PREC1.EQ.0 .AND. (PRC(TYPEA).EQ.2 .OR. PRC(TYPEB).EQ.2 .OR.
     1    PRC(TYPEC).EQ.2)) PREC = 2
C
C     ELIMINATE METHOD THREE FROM SELECTION FOR THIS BAD CASE
C     (I.E. TRANSPOSE AND MIXED MATRIX PRECISION)
C
      IT = T
      IF (IT.NE.0 .AND. PREC.EQ.1 .AND. PRC(TYPEB).EQ.2) IT = 0
      IF (IT.NE.T .AND. L19.NE.0) WRITE (NOUT,60) TYPEA,TYPEB,TYPEC
   60 FORMAT ('0METHOD 3T IS ELIMINATED FROM SELECTION/MPYAD@60',/1X,
     1        'MATRIX TYPES A,B,C =',3I3)
C
C     COMPUTE TYPE AND PRECISION OF D MATRIX
C     RCD    = 1 FOR REAL,   2 FOR COMPLEX
C     PREC   = 1 FOR SINGLE, 2 FOR DOUBLE
C     TYPED  = 1 FOR RSP, 2 FOR RDP, 3 FOR CSP, AND 4 FOR CDP
C     PRC(1) = 1 FOR S.P.   PRC(2) = 2 FOR D.P.
C
      RCD = 0
      IF (PREC .EQ. 2) GO TO 70
      IF (ANDF(TYPEA,1) .EQ. 0) TYPEA = TYPEA - 1
      IF (ANDF(TYPEB,1) .EQ. 0) TYPEB = TYPEB - 1
      IF (ANDF(TYPEC,1) .EQ. 0) TYPEC = TYPEC - 1
   70 IF (TYPEA.GT.2 .OR. TYPEB.GT.2 .OR. TYPEC.GT.2) RCD = 2
      TYPED = RCD + PREC
      IF (RCD .EQ. 0) RCD = 1
C
C     RCA/B/D   = 1 IF A/B/D IS REAL, = 2 IF A/B/D IS COMPLEX
C     NWDA/B/D  = NUMBER OF WORDS PER ELEMENT OF A/B/D
C     NBX/DX    = NUMBER OF ELEMENTS PER COLUMN OF B/C ORD
C     NB/D      = NUMBER OF WORDS PER COLUMN OF B/C OR D
C     NZZ       = BUF1 = POINTER TO FIRST GINO BUFFER
C     BUF2/3    = POINTER TO SECOND AND THIRD GINO BUFFERS
C     JJ        = MAX. NO. OF COLNS OF B AND D THAT MAY BE HELD IN CORE
C     MPASS1/2/3 = NUMBER OF PASSES REQUIRED FOR METHOD ONE/TWO/THREE
C     JZB/JZDB  = POINTER TO FIRST ELEMENT OF B FOR SP/DP REFERENCE
C     JB        = POINTER TO FIRST ELEMENT OF B FOR PRECISION OF PROBLEM
C     ACORE     = POINTER TO FIRST WORD FOR STORAGE OF PACKED COLUMNS
C                 OF A MATRIX FOR METHOD TWO
C     KSYS58    = SYSTEM(58), METHOD REQUESTED BY USER IF IT IS NON-ZERO
C
C
C     TURN TRANSPOSE FLAG OFF IF INPUT MATRIX A IS SYMMETRIC, AND SURELY
C     THAT COLUMNS EQUEL ROWS, AND DIAG 41 IS OFF.
C
C     IF INPUT A OR B IS DIAGONAL, ROW VECTOR, OR IDENTITY MATRICES,
C     MATRICES ARE NOT IN MIXED PRECISTION TYPES, AND DIAG 41 FLAG IS
C     OFF AND SYSTEM(94) IS NOT 1, BRANCH OFF TO SPECIAL SUBROUTINE
C     MPY-D-R-I
C
      K = FILEA(4)
      IF (K.EQ.6 .AND. M.EQ.N .AND. L41.EQ.0) T = 0
C        SYMMETRIC    COLN=ROW     DIAG41 OFF
      IF (L41.EQ.1 .OR. MOD(KSYSTM(94),10).EQ.1) GO TO 80
      J = FILEB(4)
      IF (K.NE.3 .AND. K.NE.7 .AND. K.NE.8 .AND.
     1    J.NE.3 .AND. J.NE.7 .AND. J.NE.8) GO TO 80
C         DIAGONAL     ROW VCTR     IDENTITY
C
      IF (TYPEA.NE.TYPEB .OR. TYPEA.NE.TYPD) GO TO 80
      K = MAX0(M,N,Q,R)
      J = K*2 + 1
      K = K + 1
      CALL MPYDRI (Z,Z,Z(K),Z(K),Z(J),Z(J))
      GO TO 380
C
   80 RCB   = RC(TYPEB)
      NBX   = R*RCB
      NWDB  = NWDS(TYPEB)
      NWDB1 = NWDB + 1
      NB    = R*NWDB
      NDX   = P*RCD
      ND    = P*NWDS(TYPED)
      NZZ   = IABS(NZ) - SYSBUF + 1
      BUF2  = BUF1 - SYSBUF
      BUF3  = BUF2 - SYSBUF
      BUF4  = BUF3 - SYSBUF
      JJ    = (NZZ-1)/(NB+ND)
      ICRQ  = NB + ND - NZZ + 1
      IF (ICRQ .GT. 0) GO TO 530
      MPASS1= (Q-1)/JJ + 1
      JZB   = JJ*ND  + 1
      JZDB  = JJ*NDX + 1
      JB    = JZB
      IF (PRC(TYPEB) .EQ. 2) JB = JZDB
      NWDA  = NWDS(TYPEA)
      PRCA  = PRC(TYPEA)
      NA    = NWDA*N
      NWDA1 = NWDA + 1
      NWDD  = NWDS(TYPED)
      ACORE = ND + 1
      IF (T .NE. 0) ACORE = NB + 1
      ACORE = ((ACORE+1)/2)*2 + 1
      IF (SIGNAB.EQ.0 .AND. PREC.EQ.1 .AND. (PRC(TYPEA).EQ.2 .OR.
     1    PRC(TYPEB).EQ.2)) TYPED = RCD + 1
      IF (NOAB.EQ.1   .OR. SIGNAB.EQ. 0) GO TO 1100
      IF (SIGNAB.EQ.1 .OR. SIGNAB.EQ.-1) GO TO 100
      WRITE  (MOUT,90)
   90 FORMAT ('0*** USER FATAL MESSAGE 2398, MPYAD REQUIRES SIGN OF ',
     1        'A*B TO BE -1, 0, OR +1')
      GO TO 540
  100 CALL MPYQ (Z)
C
C     CALCULATE ESTIMATED EXECUTION TIMES AND SELECT METHOD.
C
      NCORE = BUF3 - ACORE
      ICRQ  = -NCORE
      IF (ICRQ .GT. 0) GO TO 530
      CORE  = FLOAT(NCORE/NWDA)
      FN    = FILEA(2)
      FM    = FILEA(3)
      FP    = FILEB(2)
      RHOA  = AMIN1(1.E-4*FLOAT(FILEA(7)),1.0)
      RHOB  = AMIN1(1.E-4*FLOAT(FILEB(7)),1.0)
      RHOC  = AMIN1(1.E-4*FLOAT(FILEC(7)),1.0)
      RHOD  = AMAX1(RHOA,RHOB)
      ARITH = FM*FN*(TMT(TYPED) + (1.0-RHOA)*TML(TYPED))
      ATERM = (FM*RHOA+5.0)*FN*TMIPAK
      BTERM = FLOAT(R)*FP*0.5*(1.0+RHOB)*TMUPAK
      DTERM = FM*FP*0.5*(1.0+RHOD)*TMPAK
      CTERM = 0
      IF (CFILE .NE. 0) CTERM = FM*FP*0.5*(1.0+RHOC)*TMUPAK
      TIME1 = (FM*FN*FP*RHOA*TMT(TYPED) + FLOAT(MPASS1)*ATERM + BTERM
     1      +  DTERM + CTERM)*1.0E-6
C
      MPASS2= (2.0-RHOA)*FM*FN*RHOA/CORE + 1.0
      FR    = MPASS2
      IF (T .NE. 0) GO TO 110
      TIME2 = (FP*RHOA*RHOB*ARITH + ATERM
     1      + (FR+1.0)/2.0*(FN*RHOB+10.0)*FP*TMIPAK + FR*DTERM
     2      + (FR-1.0)*0.5*FM*FP*(1.0+RHOD)*TMUPAK
     3      + CTERM)*1.0E-6
      GO TO 120
C
  110 FNT   = FN*FM*RHOB
      P1    = AMIN1((FNT/FLOAT(FILEB(6))+FP)/2.0,FNT,FP)
      FP1   = P1
      CTERM2= 0.
      IF (CFILE .NE. 0) CTERM2 = (FN*RHOC+5.0)*FP*TMIPAK
      BTERM = FM*FP*0.5*(1.0+RHOB)*TMUPAK
      DTERM2= (FN*RHOD+5.0)*FP
      TIME2 = (FP1*RHOA*ARITH + (FM*RHOA+5.0)*FN*TMIPAK + FR*BTERM
     1      + (FR+1.0)/2.0*DTERM2*TMBPAK + (FR-1.0)/2.0*DTERM2*TMIPAK
     2      + CTERM2)*1.0E-6
C
      BUFI  = BUF4
      IF (FILEC(1) .EQ. 0) BUFI = BUF3
      NBRROW= MIN0((BUFI-ORF(ND+1,1))/NA,M)
      MPASS3= (M-1)/NBRROW + 1
      FR    = MPASS3
      TIME3 = (FM*FN*FP*RHOB*TMT(TYPED) + FM*FN*0.5*(1.0+RHOA)*TMUPAK
     1      + FR*FP*(FN*RHOB+5.0 )*TMIPAK
     2      + (FR+1.0)/4.0*FN*FP*(1.0+RHOD)*TMPAK
     3      + (FR-1.0)/4.0*FN*FP*(1.0+RHOD)*TMUPAK + CTERM2)*1.E-6
  120 CALL TMTOGO (ITIMGO)
      IF (CORE .LE. 0.0) TIME2 = AMAX1(TIME1,TIME3) + 1.0
      TIME  = AMIN1(TIME1,TIME2,TIME3)
      ITIME = TIME + 1
      IF (ITIMGO.LE.ITIME .AND. FILED(1).GT.0) GO TO 550
C
C     PRINT TIMING MESSAGE AND IF OUTPUT FILE IS PURGED RETURN
C
      IELEMS = FN*FM*RHOA + 0.5
      JELEMS = FLOAT(R)*FP*RHOB
CWKBNB 9/93
      IF(L19.EQ.0) GO TO 137
      CALL FNAME ( FILEA, NAMEA )
      CALL FNAME ( FILEB, NAMEB )
      CALL FNAME ( FILEC, NAMEC )
      CALL FNAME ( FILED, NAMED )
      WRITE( NOUT,136, IOSTAT=IERR )
CWKBR 7/94/SPR 94008 *         NAMEA, N, M, IELEMS, RHOA, PRNTYP( TYPEA )
CKWBR 7/94 SPR 94008 *,        NAMEB, R, Q, JELEMS, RHOB, PRNTYP( TYPEB )
     *         NAMEA, N, M, IELEMS, RHOA, PRNTYP( ITYPEA )
     *,        NAMEB, R, Q, JELEMS, RHOB, PRNTYP( ITYPEB )
  136 FORMAT(
     & '  /-----------------------------------------------------------/'
     &,/
     &,'  /     MATRIX      ROWS   COLS     TERMS  DENS    TYPE       /'
     &,/
     &,'  /-----------------------------------------------------------/'
     &,/
     &,'  /  A- ',2A4,I8,I7,I10,F7.4, 5X, A2
     &,/
     &,'  /  B- ',2A4,I8,I7,I10,F7.4, 5X, A2 )
      IELEMS = FN*FM*RHOC + .5
      IF (CFILE .EQ. 0) GO TO 11140
      WRITE( NOUT,11136, IOSTAT=IERR )
     *        NAMEC, FILEC(3), FILEC(2), IELEMS, RHOC, PRNTYP(ITYPEC)
11136 FORMAT(
     &   '  /  C- ',2A4,I8,I7,I10, F7.4, 5X, A2 )
11140 WRITE( NOUT, 11137 ) NAMED, PRNTYP(TYPED)
11137 FORMAT('  /  D- ',2A4,8X, 7X, 10X, 7X,   5X, A2 )
      WRITE( NOUT, 11138 ) SIGNAB, SIGNC, T, CORE, MPASS1,MPASS2,
     &                     MPASS3, TIME1, TIME2, TIME3
11138 FORMAT('  /  SIGNAB =',I4,'  SIGNC =',I4,'  TIME EST=',I9
     &,      ' MEMORY =',F8.0
     &,/,    '  /  MPASS1 =',I4, '  MPASS2=',I4, '  MPASS3=',I4
     &,/,    '  /  TIME1  =',E9.2,' TIME2=',E9.2,' TIME3=',E9.2,/
     &,'  /-----------------------------------------------------------/'
     &)
  137 CONTINUE
CWKBNE 9/93
C
  180 IF (FILED(1) .LT. 0) GO TO 1600
C
      J = KSYS58
      IF (J.LT.0 .OR. J.GT.3 .OR. (J.EQ.3 .AND. IT.EQ.0)) J = 0
      IF (J  .NE. 0) GO TO (200,600,1300), J
      IF (IT .NE. 0) GO TO 190
CWKBNB 2/95 NCL93004
      IF ( MPASS1 .LT. MPASS2 ) GO TO 200
      IF ( MPASS2 .LT. MPASS1 ) GO TO 600
CWKBNE 2/95 NCL93004
      IF (TIME1 .LT. TIME2) GO TO 200
      GO TO 600
CWKBD 2/95 NCL93004 190 IF (TIME1.LT.TIME2 .AND. TIME1.LT.TIME3) GO TO 200
CWKBNB 2/95 NCL93004
  190 CONTINUE
      IF ( MPASS1 .LT. MPASS2 .AND. 
     &   ( TIME1 .LT. TIME3 .OR. MPASS1 .LT. MPASS3 ) ) GO TO 200
      IF ( MPASS2 .LT. MPASS1 .AND. 
     &   ( TIME2 .LT. TIME3 .OR. MPASS2 .LT. MPASS3 ) ) GO TO 200
      IF ( TIME1 .LT. TIME2 .AND. TIME1 .LT. TIME3 ) GO TO 200
      IF (TIME2 .LT. TIME3) GO TO 600
CWKBNE 2/95 NCL93004
      GO TO 1300
C
C               *********************
C               *                   *
C               *    METHOD  ONE    *
C               *    MPY1NT $ 1T    *
C               *                   *
C               *********************
C
C     BUILD MATRIX PRODUCT JMAX COLUMNS PER PASS OF A MATRIX
C     WHERE JMAX=JJ EXCEPT ON FINAL PASS
C
  200 JCOL = 1
  230 WRITE  (NOUT,240) METHOD(T+1),MPASS1,TIME1
  240 FORMAT ('    METHOD TO BE USED:',A4,', NBR PASSES =',I4,
     1        ',  EST. TIME =',F9.1)
  250 JMAX  = MIN0(JCOL+JJ-1,Q)
      IF (JMAX .EQ. Q) OP = CLSREW
      JMAX1 = JMAX  - JCOL
      JMAX  = JMAX1 + 1
      JMAX1X= JMAX1*NDX
      IF (FILEB(6) .EQ. 0) GO TO 270
C
C     READ AND UNPACK JMAX COLUMNS OF THE B MATRIX
C
      FILE = FILEB(1)
      JZ   = JZB
      TYPEBD = TYPEB*SIGNAB
      NBD  = NB
      OPBC = OPB
      PP2  = R
      ASSIGN 270 TO MM
      GO TO 400
C
C     READ AND UNPACK JMAX COLUMNS OF THE C MATRIX
C
  270 FILE = FILEC(1)
      JZ   = 1
      TYPEBD = TYPED*SIGNC
      NBD  = ND
      OPBC = OPC
      PP2  = P
      ASSIGN 280 TO MM
      GO TO 400
C
C     OPEN AND POSITION A MATRIX TO FIRST COLUMN
C
  280 IF (FILEB(6) .EQ. 0) GO TO 340
      FILE = FILEA(1)
      CALL OPEN (*500,FILEA,Z(NZZ),RDREW)
  290 CALL FWDREC (*510,FILEA)
C
C     SET POINTERS
C     L   = COLUMN NUMBER
C     LL  = POINTER TO LTH ROW OF B MATRIX
C     LLL = POINTER TO LTH ROW OF D MATRIX
C
      L   = 1
      LL  = JB
      LLL = 1
C
C     CALL INTPK TO INITIATE READING THE LTH COLUMN OF THE A MATRIX
C     IF COLUMN IS NULL, BYPASS ARITHMETIC
C
  310 CALL INTPK (*320,FILEA,0,TYPED,0)
C
C     FORM EITHER  A(I,L)*B(L,J) + D(I,J)
C              OR  A(L,I)*B(I,J) + D(L,J)
C           WHERE  J RUNS ACROSS COLUMNS OF B AND D NOW IN CORE
C
      CALL MPY1V (ZZ,Z,ZD)
C
C     POSITION POINTERS FOR NEXT COLUMN OF A
C
  320 LL  = LL + RCB
      LLL = LLL+ RCD
      L   = L  + 1
      IF (L .LE. M) GO TO 310
C
C     CLOSE AND REWIND FILE CONTAINING A MATRIX
C
      CALL CLOSE (FILEA,CLSREW)
C
C     OPEN FILE CONTAINING D MATRIX TO WRITE
C
  340 FILE = FILED(1)
      CALL OPEN (*500,FILED,Z(NZZ),OP2)
C
C     IF FIRST COLUMNS OF D, WRITE HEADER
C
      IF (OP2 .EQ. WRT) GO TO 360
      CALL FNAME (FILED,BCD)
      CALL WRITE (FILED,BCD,2,1)
C
C     PACK AND WRITE JMAX COLUMNS OF THE D MATRIX
C
  360 JZ = 1
      DO 370 J = 1,JMAX
      CALL PACK (Z(JZ),FILED,FILED)
  370 JZ = JZ + ND
C
C     TEST FOR END OF MULTIPLICATION
C     CLOSE FILE CONTAINING D MATRIX
C
      CALL CLOSE (FILED,OP)
C
C     SET OP FLAGS FOR OPEN CALLS FOR NEXT PASS
C
      OPB = RD
      OPC = RD
      OP2 = WRT
C
      JCOL = JCOL + JJ
      IF (JCOL .LE. Q) GO TO 250
  380 MPY(3) = JEND
      CALL CONMSG (MPY,3,0)
      GO TO 1600
C
C     INTERNAL SUBROUTINE TO READ JMAX COLUMNS OF THE B OR C MATRICES
C     ELEMENTS ARE SET TO ZERO IF COLUMN IS NULL OR MATRIX ABSENT
C
C     OPEN AND POSITION FILE IF MATRIX IS PRESENT
C
  400 IF (FILE) 410,420,410
  410 CALL OPEN (*500,FILE,Z(NZZ),OPBC)
      IF (JCOL .NE. 1) GO TO 420
      CALL FWDREC (*510,FILE)
C
C     LOOP THROUGH JMAX COLUMNS OF MATRIX
C
  420 DO 470 J = 1,JMAX
C
C     UNPACK THE JTH COLUMN IF MATRIX IS PRESENT
C
      IF (FILE) 440,450,440
  440 CALL UNPACK (*450,FILE,Z(JZ))
      GO TO 470
C
C     ZERO COLUMN
C
  450 K2 = JZ + NBD - 1
      DO 460 K = JZ,K2
  460 Z(K) = 0.
C
C     POSITION POINTERS TO NEXT COLUMN OF MATRIX
C
  470 JZ = JZ + NBD
C
C     CLOSE FILE IF MATRIX IS PRESENT
C
      IF (FILE) 480,490,480
  480 CALL CLOSE (FILE,OP)
C
C     RETURN
C
  490 GO TO MM, (270,280)
C
C
C     ERROR CONDITIONS
C
  500 MM = -1
      GO TO 520
  510 MM = -2
  520 CALL MESAGE (MM,FILE,NAME)
      GO TO 1600
C
  530 MM = -8
      FILE = ICRQ
      GO TO 520
  540 MM = -37
      GO TO 520
  550 MM = -50
      FILE = ITIME
      GO TO 520
  560 CALL FNAME (FILEA,ZZ(1))
      CALL FNAME (FILEB,ZZ(3))
      CALL FNAME (FILEC,ZZ(5))
      IF (FILEC(2).NE.FILEB(2) .OR. FILEC(3).NE.IROWC) NOGO = 1
      WRITE (MOUT,570) ZZ(1),ZZ(2),FILEA(2),FILEA(3),ZZ(3),ZZ(4),
     1                 FILEB(2),FILEB(3),ZZ(5),ZZ(6),FILEC(2),IROWC
  570 FORMAT (3(4X,2A4,2I7))
      MM = -55
      GO TO 520
C
C
C               *********************
C               *                   *
C               *    METHOD  TWO    *
C               *    MPY2NT $ 2T    *
C               *    AND   MPY4T    *
C               *                   *
C               *********************
C
  600 CONTINUE
C
C     INITIALIZE FOR METHODS 2NT, 2T AND 4T.
C     METHOD 4T DOES NOT HANDLE COMPLEX MATRIX-D FROM REAL MATRICES A
C     AND B (LL4 = 6).
C
      MT4   = 0
      MT2   = T
      IF (MOD(KSYSTM(94),100)/10 .EQ. 1) GO TO 620
      IF (T.EQ.0 .OR. L41.EQ.1 .OR. LL4.EQ.6) GO TO 620
      IF (MPASS2.LE.2 .OR. CFILE.EQ.0 .OR. DENSC.LT.700) GO TO 620
      MT2   = 0
      MT4   = 2
      ACORE = NB + ND + 1
      ACORE = ((ACORE+1)/2)*2 + 1
      JZB   = ND + 1
      JB    = ND/PREC1 + 1
  620 DFILE = FILED(1)
      EFILE = SCRTCH
      BLOCK(1) = FILEA(1)
      CFILE = FILEC(1)
      OPA   = RDREW
      TYPEC = TYPED*SIGNC
      FIRSTL= BUF3 - 1
  640 WRITE (NOUT,240) METHOD(T+3+MT4),MPASS2,TIME2
C
C     BEGIN PASS
C
C     OPEN DFILE TO WRITE.
C     READ AS MANY COLUMNS (OR ROWS) OF A AS CAN BE HELD
C     IN CORE IN PACKED FORM ON THIS PASS.
C
      ACOL1 = 1
  650 FILE = DFILE
      CALL OPEN  (*500,DFILE,Z(BUF3),WRTREW)
      CALL FNAME (FILED(1),BCD)
      CALL WRITE (DFILE,BCD,2,1)
      FILED(2) = 0
      FILED(6) = 0
      FILED(7) = 0
      FILE = FILEA(1)
      CALL GOPEN (FILEA,Z(BUF2),OPA)
      APOINT = ACORE
      L    = FIRSTL
      ACOL = ACOL1
CWKBR 9/94   660 IF ( (APOINT+NA+2) .GE. L-2) GO TO 530   
C ABOVE CHECK WAS OVER-ZEALOUS IN CHECKING FOR AVAILABLE MEMORY
C BECAUSE OF THE CHECK TWO LINES AFTER STATEMENT 670
  660 IF ( (APOINT+2) .GE. L-2) GO TO 750   
      ZZ(L  ) = 0
      ZZ(L-1) = 0
      BLOCK(8)=-1
      CALL GETSTR (*730,BLOCK)
      INCRA = 1
      IF (PRC(TYPE).EQ.2 .AND. PRC(TYPEA).EQ.1) INCRA = 2
      ZZ(L) = APOINT
  670 KR1 = APOINT + 2
      KRN = KR1 + NBRSTR*NWDA - 1
      IF (KRN .GE. L-2) GO TO 740
C
C     MOVE STRING FROM BUFFER TO CORE AND COMPLETE STRING DEFINITION
C     WORDS
C
      IF (PRC(TYPE).NE.2 .OR. PRC(TYPEA).NE.1) GO TO 690
C
C  -- THIS CODE NECESSARY FOR UNIVAC DOUBLE PRECISION TO SINGLE PRC.
C
      INC   = 1
      INCRA = 1
      IF (TYPE .EQ. 4) INC = 2
      KRN = KR1 + NBRSTR*INC - 1
      DO 680 II = KR1,KRN
      Z(II) = XND(POINT)
      POINT = POINT + INCRA
  680 CONTINUE
      GO TO 710
  690 IF (PRC(TYPE) .EQ. 2) POINT = POINT*2 - 1
      DO 700 II = KR1,KRN
      Z(II) = XNS(POINT)
      POINT = POINT + INCRA
  700 CONTINUE
  710 ZZ(APOINT  ) = ROW
      ZZ(APOINT+1) = NBRSTR
      ZZ(L-1) = ZZ(L-1) + 1
      APOINT  = KRN + 1
C
C     GET NEXT STRING DEFINITION
C
      CALL ENDGET (BLOCK)
      CALL GETSTR (*730,BLOCK)
      GO TO 670
C
C     END-OF-COLUMN -
C     SAVE LAST NON-ZERO TERM POSTION FOR MTHOD 4T, THEN
C     TEST FOR ALL COLUMNS
C
C     SINCE GINO IS MDS, MAKE SURE THAT THE LAST VALUES IN NBRSTR AND
C     ROW HERE ARE STILL VALID. OTHERWISE THEY MUST BE SAVED FIRST (AT
C     710) AND USED ON NEXT LINE.
C
  730 IF (MT4 .EQ. 2) ZZ(L-1) = ORF(ZZ(L-1),LSHIFT(ROW+NBRSTR-1,IHALF))
C                                    NBR + LAST NON-ZERO TERM COLUMN NO.
C
      L = L - 2
      ACOL = ACOL + 1
      IF (ACOL .LE. M) GO TO 660
C
C     ALL COLUMNS OF A ARE IN - THIS IS THE LAST PASS
C
      ACOLN = M
      CALL CLOSE (FILEA(1),CLSREW)
      GO TO 760
C
C     ALL COLUMNS OF A WILL NOT FIT ON THIS PASS.
C
  740 CALL BCKREC (FILEA(1))
  750 CALL CLOSE  (FILEA(1),CLS)
      ACOLN = ACOL - 1
C
C     IF CFILE IS PRESENT, OPEN IT.
C     IF THIS IS THE FIRST PASS, SKIP HEADER RECORD.
C     OPEN BFILE AND SKIP HEADER RECORD.
C     INITIALIZE COLUMN (OR ORW) COUNTER, BCOL, TO 1, AND BRANCH ON T.
C
  760 IF (CFILE .EQ. 0) GO TO 770
      FILE = CFILE
      CALL OPEN (*500,CFILE,Z(BUF1),RDREW)
      CALL FWDREC (*510,CFILE)
  770 FILE = FILEB(1)
      CALL OPEN (*500,FILEB(1),Z(BUF2),RDREW)
      CALL FWDREC (*510,FILEB(1))
      BCOL = 1
  780 IF (MT2 .EQ. 1) GO TO 900
C
C     UNPACK A COLUMN OF C.
C
      IF (CFILE .EQ. 0) GO TO 810
      TYPEBD = TYPEC
      IF (MT4 .NE. 0) ONE2 = 1
      PP2    = P
      CALL UNPACK (*810,CFILE,Z)
      GO TO 830
  810 DO 820 II = 1,ND
      Z(II) = 0.
  820 CONTINUE
  830 IF (MT4 .NE. 0) GO TO 850
C
C     NON-TRANSPOSE CASE, METHOD 2NT
C     ==============================
C
C     INITIATE INTERPRETATION OF A COLUMN OF B.
C
C     ITYPSG = TYPED*SIGNAB
      ITYPSG = TYPEB*SIGNAB
      CALL INTPK (*860,FILEB(1),0,ITYPSG,0)
C
C     FOR EACH NON-ZERO ELEMENT B(I) IN THE CURRENT COLMN OF B SUCH
C     THAT FOR I.GE.ACOL1 .AND I.LE.ACOLN, FORM ALL PRODUCTS OF
C     D(K,I) = A(K,I)*B(I) + C(K,I)
C
      CALL MPY2NV (ZZ,Z,ZD)
      GO TO 860
C
C     TRNASPOSE CASE, METHOD 4T
C     =========================
C
C     UNPACK A BANDED COLUMN OF MATRIX B, RANGING FROM ONE2 THRU PP2.
C     FOR THE RANGE  MAX0(ONE2,ACOL1) THRU MIN0(PP2,ACOLN), FORM ALL
C     PRODUCTS
C     D(I,K) = A(I,J)*B(J,K) + C(I,K)
C
  850 TYPEBD = TYPEB*SIGNAB
      ONE2   = 0
      CALL UNPACK (*860,FILEB,Z(JZB))
C
C     WE HAVE HERE -
C     ACLO1, ACOLN = COLUMNS OF MATRIX A IN CORE
C     BCOL = CURRENTLY WE ARE WORKING ON THE BCOL COLUMN OF MATRIX B,
C            WHICH IS ALSO THE WORKING COLUMNS OF MATRIX D AND MATRIX C
C     Z(JZB) THRU Z(ACORE-1) CONTAIN THE BCOL COLUMN OF MATRIX B
C
      CALL MPY4T (Z,Z,Z)
C
C     PACK CURRENT COLUMN ONTO DFILE FOR BOTH 2NT AND 4T METHOD, AND
C     GO TO TEST FOR END OF PASS.
C
  860 CALL PACK (Z,DFILE,FILED)
      GO TO 980
C
C     TRANSPOSE CASE, METHOD 2T
C     =========================
C
C     INITIATE BUILDING OF A PACKED COLUMN OF D.
C     UNPACK A COLUMN OF B IN CORE. IF NULL, COPY COLUMN FROM C TO D.
C     INITIATE INTERPRETATION OF A COLUMN OF C.
C
  900 CALL BLDPK (TYPED,TYPD,DFILE,0,0)
      TYPEBD = TYPEB*SIGNAB
      PP2  = R
      CALL UNPACK (*910,FILEB(1),Z)
      EOL  = 1
      CROW = 16777215
C            16777215 = 2**24 - 1
C
      IF (CFILE .EQ. 0) GO TO 960
      CALL INTPK (*960,CFILE,0,TYPEC,0)
      CROW = 0
      GO TO 960
  910 IF (CFILE .EQ. 0) GO TO 970
      CALL INTPK (*970,CFILE,0,TYPEC,0)
  920 CALL ZNTPKI
      CROW = IP
  930 DO 940 II = 1,NWDD
      D(II) = A(II)
  940 CONTINUE
      DROW = CROW
      CALL ZBLPKI
  950 IF (EOL .EQ. 0) GO TO 920
      GO TO 970
C
C     FOR ALL NON-NULL ROWS OF A IN CORE, FORM A(I,J)*B(J) + C(I)
C
  960 CALL MPY2TV (ZZ,Z,ZD)
      IF (AROWN.EQ.M .OR. CROW.EQ.16777215) GO TO 970
      IF (CROW .GT. AROWN) GO TO 930
      GO TO 950
C
C     TERMINATE CURRENT COLUMN OF D.
C
  970 CALL BLDPKN (DFILE,0,FILED)
C
C     BOTH TRANSPOSE (2T AND 4T) AND NON-TRANSPOSE (2NT) CASES
C
C     TEST FOR COMPLETION OF PASS. IF COMPLETE, TEST ALL PASSES.
C
  980 BCOL = BCOL + 1
      IF (BCOL .LE. Q) GO TO 780
      CALL CLOSE (FILEB,CLSREW)
      IF (CFILE .NE. 0) CALL CLOSE (CFILE,CLSREW)
      CALL CLOSE (DFILE,CLSREW)
      IF (ACOLN .EQ. M) GO TO 1010
C
C     NOT LAST PASS - SWITCH C AND D FILES AND CONTINUE
C
      OPA   = RD
      TYPEC = TYPED
      IF (ACOL1 .EQ. 1) GO TO 990
      K     = CFILE
      CFILE = DFILE
      DFILE = K
      GO TO 1000
  990 CFILE = DFILE
      DFILE = EFILE
 1000 ACOL1 = ACOLN + 1
      GO TO 650
C
C     LAST PASS -
C     MAKE SURE D MATRIX IS ON PROPER FILE.
C     IF NOT, SWITCH FIST AND FIAT UNIT NBRS IN /XFIAT/
C
 1010 IF (DFILE .NE. FILED(1)) CALL FILSWI (DFILE,FILED)
      GO TO 380
C
C     A MATRIX OR B MATRIX IS NULL - COPY C MATRIX TO D MATRIX
C
 1100 TIME = 0.0
      IF (FILED(1) .LT. 0) GO TO 1600
      IF (Q .LE. 0) Q = FILEC(2)
      FILED(2) = 0
      FILED(6) = 0
      FILED(7) = 0
      WRITE  (NOUT,1140)
 1140 FORMAT ('             MPYAD - NULL MATRIX PRODUCT')
      CALL GOPEN (FILED,Z(BUF1),WRTREW)
      IF (CFILE .EQ. 0) GO TO 1150
      IF (TYPEC .EQ. SIGNC*TYPD) GO TO 1170
      GO TO 1200
C
C     PACK NULL COLUMNS BECAUSE C MATRIX IS NULL
C
 1150 PP1 = 1
      DO 1160 ACOL = 1,Q
      CALL PACK (ZERO,FILED,FILED)
 1160 CONTINUE
      GO TO 1190
C
C     USE CPYSTR TO COPY C TO D
C
 1170 BLOCK(1) = CFILE
      BLK(1)   = FILED(1)
      CALL GOPEN (CFILE,Z(BUF2),RDREW)
      DO 1180 II = 1,Q
      CALL CPYSTR (BLOCK,BLK,0,0)
 1180 CONTINUE
      CALL CLOSE (CFILE,CLSREW)
      FILED(2) = Q
      FILED(5) = FILEC(5)
      FILED(6) = FILEC(6)
      FILED(7) = FILEC(7)
 1190 IF (FILEC(1) .GT. 0) FILED(4) = FILEC(4)
      CALL CLOSE (FILED,CLSREW)
      GO TO 380
C
C     USE INTPK/BLDPK TO COPY C TO D BECAUSE TYPES CONFLICT
C
 1200 CALL GOPEN (CFILE,Z(BUF2),RDREW)
      DO 1230 II = 1,Q
      CALL BLDPK (TYPD,TYPD,FILED,BLOCK,1)
      ITYPSG = SIGNC*TYPD
      CALL INTPK (*1220,FILEC,0,ITYPSG,0)
 1210 CALL ZNTPKI
      CALL BLDPKI (A,IP,FILED,BLOCK)
      IF (EOL .EQ. 0) GO TO 1210
 1220 CALL BLDPKN (FILED,BLOCK,FILED)
 1230 CONTINUE
      CALL CLOSE (CFILE,CLSREW)
      GO TO 1190
C
C
C               *********************
C               *                   *
C               *    METHOD THREE   *
C               *       MPY3T       *
C               *                   *
C               *********************
C
C     TRANSPOSE CASE ONLY, METHOD 3T
C     ==============================
C
 1300 CONTINUE
      WRITE (NOUT,240) METHOD(5),MPASS3,TIME3
      BLOCK(1) = FILEB(1)
      ACORE = ORF(ND+1,1)
      CFILE = SCRTCH
      DFILE = FILED(1)
      IF (MOD(MPASS3,2) .NE. 0) GO TO 1340
      CFILE = FILED(1)
      DFILE = SCRTCH
 1340 AROW1 = 1
      LAST  = .FALSE.
      OPA   = RDREW
C
C     BEGIN PASS BY FILLING CORE WITH UNPACKED COLUMNS OF A
C
 1350 AROWN = MIN0(AROW1+NBRROW-1,M)
      IF (AROWN .EQ. M) LAST = .TRUE.
      CALL GOPEN (FILEA,Z(BUF1),OPA)
      TYPEBD = TYPEA*SIGNAB
      PP2    = N
      APOINT = ACORE
      DO 1390 AROW = AROW1,AROWN
      CALL UNPACK (*1360,FILEA,Z(APOINT))
      GO TO 1380
 1360 K2 = APOINT + NA - 1
      DO 1370 II = APOINT,K2
      Z(II) = 0.
 1370 CONTINUE
 1380 APOINT = APOINT + NA
 1390 CONTINUE
      II = CLS
      IF (LAST) II = CLSREW
      CALL CLOSE (FILEA,II)
      INCRA = (AROWN-AROW1)*NA
C
C     PREPARE TO PASS B MATRIX AND C MATRIX FROM LAST PASS
C
      IF (AROW1 .NE. 1) CALL GOPEN (CFILE,Z(BUF2),RDREW)
      CALL GOPEN (DFILE,Z(BUF3),WRTREW)
      CALL GOPEN (FILEB,Z(BUF1),RDREW )
      IF (LAST .AND. FILEC(1).NE.0) CALL GOPEN (FILEC,Z(BUF4),RDREW)
      FILED(2) = 0
      FILED(6) = 0
      FILED(7) = 0
      TYPEBD   = TYPED
      PP2 = AROWN
      K2  = AROWN*NWDD
C
      DO 1530 BCOL = 1,Q
      IF (AROW1 .NE. 1) GO TO 1420
C
C     FIRST PASS OR NULL COLUMN ON CFILE - SET COLUMN OF D TO ZERO
C
 1400 DO 1410 II = 1,K2
 1410 Z(II) = 0.
      NULL  = .TRUE.
      IF (LAST) GO TO 1430
      GO TO 1500
C
C     INTERMEDIATE PASS OR LAST PASS - UNPACK COLUMN FROM PREVIOUS PASS
C
 1420 CALL UNPACK (*1400,CFILE,Z)
      NULL = .FALSE.
      IF (.NOT.LAST) GO TO 1500
C
C     LAST PASS - ADD COLUMN FROM C MATRIX (IF PRESENT)
C
 1430 IF (FILEC(1) .EQ. 0) GO TO 1500
      ITYPSG = TYPED*SIGNC
      CALL INTPK (*1500,FILEC,0,ITYPSG,0)
      NULL = .FALSE.
 1440 CALL ZNTPKI
      GO TO (1450,1460,1470,1480), TYPED
 1450 Z(IP) = Z(IP) + A(1)
      GO TO 1490
 1460 ZD(IP) = ZD(IP) + AD(1)
      GO TO 1490
 1470 Z(2*IP-1) = Z(2*IP-1) + A(1)
      Z(2*IP  ) = Z(2*IP  ) + A(2)
      GO TO 1490
 1480 ZD(2*IP-1) = ZD(2*IP-1) + AD(1)
      ZD(2*IP  ) = ZD(2*IP  ) + AD(2)
 1490 IF (EOL .EQ. 0) GO TO 1440
C
C     FOR EACH NON-ZERO TERM B(J) IN THE CURRENT COLUMN OF B FORM
C     D(I,K) = D(I,K) + A(I,J)*B(J,K)
C
 1500 CALL MPY3T (*1510,Z(ACORE),Z(ACORE),Z(1),Z(1))
      GO TO 1520
C
C     PACK NULL COLUMN
C
 1510 IF (.NOT.NULL) GO TO 1520
      PP1 = 1
      CALL PACK (ZERO,DFILE,FILED)
      GO TO 1530
C
C     PACK NON-NULL COLUMN
C
 1520 PP1 = AROWN
      CALL PACK (Z,DFILE,FILED)
C
C     TEST FOR END OF CURRENT PASS
C
 1530 CONTINUE
C
      IF (AROW1 .NE. 1) CALL CLOSE (CFILE,CLSREW)
      CALL CLOSE (DFILE,CLSREW)
      CALL CLOSE (FILEB,CLSREW)
      IF (LAST) GO TO 1540
C
C     NOT LAST PASS - SWITCH FILES AND CONTINUE
C
      II    = CFILE
      CFILE = DFILE
      DFILE = II
      AROW1 = AROWN + 1
      OPA   = RD
      GO TO 1350
C
C     LAST PASS - SIGNAL END AND RETURN
C
 1540 IF (FILEC(1) .NE. 0) CALL CLOSE (FILEC,CLSREW)
      GO TO 380
C
 1600 RETURN
      END
