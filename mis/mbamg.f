      SUBROUTINE MBAMG (INPUT,AJJL,SKJ)
C
C     DRIVER FOR MACH BOX THEORY
C
      LOGICAL         CNTRL2,CNTRL1,CRANK1,CRANK2,ASYM
      INTEGER         SYSBUF,AJJL,SKJ,NAME(2),IZ(1),BUF1,SCR2
      REAL            MACH
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /MBOXA / X(12),Y(12),TANG(10),ANG(10),COTANG(10)
      COMMON /MBOXC / NJJ ,CRANK1,CRANK2,CNTRL1,CNTRL2,NBOX,
     1                NPTS0,NPTS1,NPTS2,ASYM,GC,CR,MACH,BETA,EK,EKBAR,
     2                EKM,BOXL,BOXW,BOXA ,NCB,NSB,NSBD,NTOTE,KC,KC1,KC2,
     3                KCT,KC1T,KC2T
      COMMON /SYSTEM/ SYSBUF,NOUT
      COMMON /AMGMN / MCB(7),NROW,ND,NE,REFC,FMACH,RFK,TSKJ(7),ISK,NSK
      COMMON /PACKX / ITI,IT0,II,NN,INCR
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (Z(1),IZ(1))
      DATA    NAME  / 4HMBAM,4HG    /
      DATA    NHCORE, NHCAPF,NHCONT /4HCORE,4HCAPF,4HCONT/
      DATA    SCR2  / 302 /
C
C     SCR2 CONTAINS THE INTERPOLATED POINTS
C
C     2 * KCT FOR NPTS0 POINTS
C     2 * KC1T FOR NPTS1 POINTS
C     2 * KC2T FOR NPTS2 POINTS
C
C
C     OPEN CORE POINTERS FIXED DIMENSIONS
C
      NW1   = 1
      NWN   = 51
      NC21  = 101
      NC2N  = 151
      NC1   = 201
      NCN   = 251
      ND1   = 301
      NDN   = 351
      NXK   = 401
      NYK   = 601
      NXK1  = 801
      NYK1  = 926
      NXK2  = 1051
      NYK2  = 1176
      NXWTE = 1301
      NYWTE = 1351
      NKTE  = 1401
      NKTE1 = 1451
      NKTE2 = 1501
      NPAREA=1551
      ICORR = 9051
C
C     INITITALIZE  PUT HEADER DATA IN MBOXC
C
      ICORE = KORSZ(IZ) - 4*SYSBUF
      BUF1  = ICORE - SYSBUF
      CALL FREAD (INPUT,NJJ,9,0)
      ASYM = .FALSE.
      IF( ND .EQ. -1 ) ASYM = .TRUE.
      MACH = FMACH
      BETA = SQRT((MACH*MACH)-1.0)
      CALL FREAD (INPUT,Z,24,0)
C
C     MOVE X AND Y TO MBOXA
C
      L = 0
      DO 10 I = 1,23,2
      L = L + 1
      X(L)  = Z(I)
      Y(L)  = Z(I+1)
   10 CONTINUE
      CALL MBGEOD
      EK    = (2.0*CR/REFC)*RFK
      CMAX  =  AMAX1(X(4),X(5),X(6))
      BOXL  =  CMAX/(FLOAT(NBOX) + 0.50)
      BOXW  =  BOXL/BETA
      NSB   =  Y(3)/BOXW + 0.5
      NSB   =  MIN0(NSB,50)
      BOXW  =  Y(3)/(FLOAT(NSB) - 0.50)
      BOXL  =  BOXW*BETA
      NCB   =  CMAX/BOXL + 0.999
C
C     CALL MBREG TO GENERATE BOXES
C
      ICRQ = ICORR - BUF1
      IF (ICORR .GT. BUF1) GO TO 996
   20 CALL MBREG (IREG,Z(NW1),Z(NWN),Z(NC21),Z(NC2N),Z(NC1),Z(NCN),
     1            Z(ND1),Z(NDN),Z(NXK),Z(NYK),Z(NXK1),Z(NYK1),Z(NXK2),
     2            Z(NYK2),Z(NXWTE),Z(NYWTE),Z(NKTE),Z(NKTE1),Z(NKTE2),
     3            Z(NPAREA))
      IF (IREG .NE. 2) GO TO 30
      IF (NBOX .LT. 2) GO TO 999
      NBOX = NBOX - 1
      GO TO 20
   30 CALL MBPLOT (Z(NW1),Z(ND1),Z(NWN),Z(NC21),Z(NC2N),Z(NC1),
     1             Z(NCN),Z(NDN))
C
C     CALL MBMODE TO GENERATE MODE LIKE DATA
C
      CALL GOPEN (SCR2,Z(BUF1),1)
      CALL MBMODE (INPUT,SCR2,ICORR,BUF1,Z,NPTS0,KCT,Z(NXK),Z(NYK),IS,
     1             CR)
      IF (IS .EQ. 2) GO TO 997
      IF (CNTRL1) CALL MBMODE (INPUT,SCR2,ICORR,BUF1,Z,NPTS1,KC1T,
     1                         Z(NXK1),Z(NYK1),IS,CR)
      IF (IS .EQ. 2) GO TO 997
      IF (CNTRL2) CALL MBMODE (INPUT,SCR2,ICORR,BUF1,Z,NPTS2,KC2T,
     1                         Z(NXK2),Z(NYK2),IS,CR)
      IF (IS .EQ. 2) GO TO 997
      CALL CLOSE (SCR2,1)
      EKBAR = (EK*BOXL*MACH*MACH)/(BETA*BETA)
      EKM   = EKBAR/MACH
      CALL FREAD (INPUT,0,0,1)
      CALL BUG (NHCORE,80,Z,NYK1-1)
      CALL BUG (NHCORE,80,Z(NYK1),NPAREA-NYK1)
      CALL DMPFIL (SCR2 ,Z(ICORR),BUF1-ICORR)
C
C     MORE DIMENSIONS
C
      IF (MOD(ICORR,2) .EQ. 0) ICORR = ICORR + 1
      NCAP  = ICORR
      NCAPH = NCB*(NCB+1)/2
C
C     COMPLEX PHIS
C
      ICORR = NCAP + NCAPH*2
      ICRQ  = ICORR - BUF1
      IF (ICORR .GT. BUF1) GO TO 996
      CALL MBCAP (NCAPH,Z(NCAP))
      ICORR = NCAP + NCAPH*2
      CALL BUG (NHCAPF,80,Z(NCAP),NCAPH*2)
C
C     PUT OUT SKJ
C
      ITI = 1
      IT0 = 3
      II  = ISK
      NSK = NSK + 1
      NN  = NSK
      RM  = 1.0
      DO 100 I = 1,NJJ
      CALL PACK (RM,SKJ,TSKJ)
      II  = II + 1
      IF (I .EQ. NJJ) GO TO 100
      NN  = NN + 1
  100 CONTINUE
      ISK = II
      NSK = NN
C
C     SET UP FOR COLUMN OF AJJL
C
      ITI = 3
      IT0 = 3
      II  = NROW + 1
      NN  = NROW + NJJ
C
C     GET AJJL MATRIX TERMS
C     MORE DIMENSIONS
C
      NPHIT = ICORR
      NDSS  = NPHIT + (3*NSBD)*2
      NQ    = NDSS  + (NCB*NSBD)*2
      NQ1   = NQ + KCT*2
      NQ2   = NQ1 + KC1T*2
      NA    = NQ2 + KC2T*2
      ICORR = NA + NJJ*2
      CALL BUG (NHXECT,100,X,54)
      CALL BUG (NHCONT,100,NJJ,30)
      ICRQ  = ICORR - BUF1
      IF (ICORR .GT. BUF1) GO TO 996
      CALL MBDPDH (AJJL,Z(NXK),Z(NYK),Z(NXK1),Z(NYK1),Z(NXK2),Z(NYK2),
     1             Z(NXWTE),Z(NYWTE),Z(NPAREA),Z(NCAP),Z(NPHIT),Z(NDSS),
     2             Z(NQ),Z(NQ1),Z(NQ2),Z(NDN),Z(ND1),Z(NW1),Z(NWN),
     3             Z(NKTE),Z(NKTE1),Z(NKTE2),Z(NC1),NCB,NSBD,SCR2,
     4             Z(BUF1),Z(NA))
      NROW = NROW + NJJ
 1000 RETURN
C
C     ERROR MESSAGES
C
  997 WRITE  (NOUT,9971) UFM
 9971 FORMAT (A23,' 2424, MACH BOX CONTROL POINTS IMPROPER SINGULAR ',
     1       'MATRIX RESULTED')
      GO TO 998
  999 WRITE  (NOUT,9991) UFM
 9991 FORMAT (A23,' 2425, MACH BOX GENERATION OF BOXES FAILED')
  998 CALL MESAGE (-37,0,NAME)
  996 CALL MESAGE (-8,ICRQ,NAME)
      GO TO 1000
      END
