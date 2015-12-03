      SUBROUTINE RCOVEM (NOEXCL,NROWE)
C
C     THIS SUBROUTINE CALCULATES THE ENERGIES ON THE MODAL COORDINATES
C     THAT WERE EXCLUDED FROM THE MODAL REDUCTION PROCESSING
C
      LOGICAL         NOEXCL
      INTEGER         RSS        ,RC         ,RULE       ,SOF1       ,
     1                SOF2       ,SOF3       ,BUF1       ,BUF2       ,
     2                Z          ,QA         ,PA         ,SCR3       ,
     3                SCR4       ,SCR5       ,SCR6       ,SCR7       ,
     4                SCR8       ,PHIS       ,SOLN       ,TYPA       ,
     5                TYPB       ,TFLAG      ,SIGNAB     ,SIGNC      ,
     6                BUF3       ,RSP        ,CSP        ,NAME(2)    ,
     7                RFNO
      REAL            RZ(5)
      COMPLEX         SC         ,CZ(2)      ,SC2        ,DKDC       ,
     1                DKSC
      CHARACTER       UFM*23     ,UWM*25
      COMMON /XMSSG / UFM        ,UWM
      COMMON /BLANK / DRY        ,LOOP       ,STEP       ,FSS(2)     ,
     1                RFNO       ,NEIGV      ,LUI        ,UINMS(2,5) ,
     2                NOSORT     ,UTHRES     ,PTHRES     ,QTHRES
      COMMON /RCOVCR/ ICORE      ,LCORE      ,BUF1       ,BUF2       ,
     1                BUF3       ,BUF4       ,SOF1       ,SOF2       ,
     2                SOF3
      COMMON /RCOVCM/ MRECVR     ,UA         ,PA         ,QA         ,
     1                IOPT       ,RSS(2)     ,ENERGY     ,UIMPRO     ,
     2                RANGE(2)   ,IREQ       ,LREQ       ,LBASIC
      COMMON /CONDAS/ PHI        ,TWOPHI
      COMMON /SYSTEM/ SYSBUF     ,NOUT
      COMMON /ZZZZZZ/ Z(1)
      COMMON /PACKX / ITINP      ,ITOUTP     ,IRP        ,NRP        ,
     1                INCRP
      COMMON /UNPAKX/ ITINU      ,IRU        ,NRU        ,INCRU
      COMMON /NAMES / RD         ,RDREW      ,WRT        ,WRTREW     ,
     1                REW        ,NOREW      ,EOFNRW     ,RSP        ,
     2                RDP        ,CSP        ,CDP        ,SQUARE     ,
     3                RECT
      COMMON /PARMEG/ MCBP(7)    ,MCBP11(7)  ,MCBP21(7)  ,MCBP12(7)  ,
     1                MCBP22(7)  ,MRGZ       ,RULE
      COMMON /SADDX / NOMAT      ,LCOREZ     ,MCBAA(7)   ,TYPA       ,
     1                ALPHA      ,ALP(3)     ,MCBBB(7)   ,TYPB       ,
     2                BETA       ,BET(3)     ,DUM(36)    ,MCBXX(7)
      COMMON /MPYADX/ MCBA(7)    ,MCBB(7)    ,MCBC(7)    ,MCBD(7)    ,
     1                MPYZ       ,TFLAG      ,SIGNAB     ,SIGNC      ,
     2                MPREC      ,MSCR
      EQUIVALENCE     (Z(1),RZ(1),CZ(1))     ,(CZ(2),SC)
      DATA    LAMS  , PHIS,SOLN  /4HLAMS,4HPHIS,4HSOLN /
      DATA    SCR3  , SCR4,SCR5,SCR6,SCR7,SCR8 /303,304,305,306,307,308/
      DATA    NAME  / 4HRCOV,4HEM   /
C
C     INITILIZE
C
      LCOREZ = KORSZ(Z)
C
C     FROM THE LAST GROUP ON LAMS CREATE A PARTITIONING VECTOR TO
C     DIFFERENTIATE THE INCLUDED AND EXCLUDED MODES
C
      NROWE = 0
      ITEM  = LAMS
      CALL SFETCH (RSS,LAMS,1,RC)
      IF (RC .NE. 1) GO TO 6000
      N = 2
      CALL SJUMP (N)
      IF (N .LT. 0) GO TO 6200
      I = 0
C
   10 CALL SUREAD (ICODE,1,N,RC)
      IF (RC .NE. 1) GO TO 20
      I = I + 1
      IF (I .GT. BUF1) GO TO 9008
      RZ(I) = 1.0
      IF (ICODE .EQ. 1) GO TO 10
      RZ(I) = 0.0
      NROWE = NROWE + 1
      GO TO 10
C
   20 CONTINUE
      IF (NROWE .EQ. 0) GO TO 900
      IF (QA+PA .EQ. 0) GO TO 9200
      ITINP  = RSP
      ITOUTP = RSP
      IRP   = 1
      NRP   = I
      INCRP = 1
      CALL MAKMCB (MCBA,SCR8,NRP,RECT,RSP)
      CALL GOPEN  (SCR8,Z(BUF1),WRTREW)
      CALL PACK   (RZ(1),SCR8,MCBA)
      CALL CLOSE  (SCR8,REW)
      CALL WRTTRL (MCBA)
C
C     PARTITION THE EIGENVECTOR TO GET THE EXCLUDED MODES OUT
C
      ITEM = PHIS
      CALL MTRXI (SCR7,RSS,PHIS,0,RC)
      IF (RC .NE. 1) GO TO 6000
      RULE = 0
      MCBP(1) = SCR7
      CALL RDTRL (MCBP)
      IF (MCBP(2) .NE. NRP) GO TO 6372
      CALL MAKMCB (MCBP11,SCR6,MCBP(3),RECT,MCBP(5))
      MCBP11(2) = NROWE
      MCBP21(1) = 0
      MCBP12(1) = 0
      MCBP22(1) = 0
C
C     SETUP NULL COLUMN PARTITONING VECTOR
C
      CALL MAKMCB (MCBB,0,MCBP(3),RECT,RSP)
      MCBB(2) = 1
      MRGZ = LCOREZ
      CALL SOFCLS
C
      CALL PARTN (MCBA,MCBB,Z(1))
C
      CALL WRTTRL (MCBP11)
C
C     IF BOTH LOADS AND SINGLE POINT CONSTRAINT FORCES EXIST, ADD
C     THEM TOGETHER
C
      IRH = QA + PA
      IF (QA.EQ.0 .OR. PA.EQ.0) GO TO 100
      NOMAT = 2
      TYPA  = 1
      ALPHA = 1.0
      MCBAA(1) = QA
      CALL RDTRL (MCBAA)
      TYPB = 1
      BETA = 1.0
      MCBBB(1) = PA
      CALL RDTRL (MCBBB)
      CALL MAKMCB (MCBXX,SCR7,MCBAA(3),RECT,MCBAA(5))
      MCBXX(2) = MCBAA(2)
C
      CALL SADD (Z(1),Z(1))
C
      CALL WRTTRL (MCBXX)
      IRH = SCR7
C
C     MULTIPLY   PK = QK(T)*(PA + QA)
C
  100 DO 110 I = 1,7
  110 MCBA(I) = MCBP11(I)
      MCBB(I) = IRH
      CALL RDTRL (MCBB)
      MCBC(1) = 0
      MPYZ   = LCOREZ
      TFLAG  = 1
      SIGNAB = 1
      SIGNC  = 1
      MPREC  = 0
      MSCR   = SCR8
      CALL MAKMCB (MCBD,SCR5,NROWE,RECT,MCBA(5))
C
      CALL MPYAD (Z(1),Z(1),Z(1))
C
C     READ MODAL MASS AND TWOPHI*FREQUENCY FOR EACH OF THE EXCLUDED
C     MODES MODES FROM LAMS
C     IF MODE WAS EXCLUDED BECAUSE OF NON-PARTICIPATION, SET ITS
C     FREQUENCY TO ZERO
C
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
      ITEM = LAMS
      CALL SFETCH (RSS,LAMS,1,RC)
      IF (RC .NE. 1) GO TO 6000
      N = 1
      CALL SJUMP (N)
      IF (N .LE. 0) GO TO 6200
      IMODE = 8
      CALL SUREAD (Z(IMODE),-1,N,RC)
      IF (RC .NE. 2) GO TO 6100
      NMODE = IMODE + N - 1
      IF (NMODE .GT. BUF3) GO TO 9008
      ICODE = NMODE + 1
      CALL SUREAD (Z(ICODE),-1,N,RC)
      IF (RC.NE.2 .AND. RC.NE.3) GO TO 6100
      NCODE = ICODE + N - 1
      IF (NCODE .GT. BUF3) GO TO 9008
C
      I1 = IMODE - 7
      I2 = IMODE - 2
      DO 250 I = ICODE,NCODE
      I1 = I1 + 7
      IF (Z(I) .EQ. 1) GO TO 250
      I2 = I2 + 2
      RZ(I2) = RZ(I1+3)
      IF (Z(I).EQ.2 .OR. RZ(I2).LE.0.001) RZ(I2) = 0.0
      RZ(I2+1) = RZ(I1+5)
  250 CONTINUE
      NMODE = I2 + 1
C
C     POSITION SOLN ITEM TO SOLUTION DATA
C
      ITEM = SOLN
      CALL SFETCH (RSS,SOLN,1,RC)
      IF (RC .NE. 1) GO TO 6000
      N = 1
      CALL SJUMP (N)
      IF (N .LT. 0) GO TO 6200
C
C     SET UP TO LOOP OVER COLUMNS
C
      NCOL  = MCBD(2)
      NWORD = 1
      IF (MCBD(5) .GE. 3) NWORD = 2
      IVEC1 = (NMODE/2)*2 + 3
      ICVEC1= IVEC1/2 + 1
      IVEC2 = IVEC1 + (NROWE*NWORD/2) * 2 + 1
      IF (IVEC2+NROWE .GT. BUF3) GO TO 9008
C
      CALL GOPEN (SCR5,Z(BUF1),RDREW)
      CALL GOPEN (SCR3,Z(BUF2),WRTREW)
      CALL GOPEN (SCR4,Z(BUF3),WRTREW)
      CALL MAKMCB (MCBA,SCR3,NROWE,RECT,RSP)
      CALL MAKMCB (MCBB,SCR4,NROWE,RECT,RSP)
C
      ITINU = RSP
      IF (MCBD(5) .GE. 3) ITINU = CSP
      IRU = 1
      NRU = NROWE
      INCRU = 1
      NRP = NROWE
C
C     LOOP OVER EACH SOLUTION STEP
C
      DO 700 ICOL = 1,NCOL
C
C     GET FREQUENCY OR POLE FROM SOLN ITEM FOR THIS STEP
C
      IF (RFNO .GT. 3) GO TO 310
      CALL SUREAD (RZ(1),7,N,RC)
      IF (RC .NE. 1) GO TO 6100
      IF (MCBD(5) .GE. 3) GO TO 300
      FREQ = RZ(5)
      S2   = -(TWOPHI*FREQ)**2
      GO TO 320
C
  300 SC  = CZ(2)
      SC2 = SC*SC
      GO TO 320
C
  310 CALL SUREAD (RZ(1),1,N,RC)
      IF (RC .NE. 1) GO TO 6100
      SC  = TWOPHI*RZ(1)*(0.0,1.0)
      SC2 = SC*SC
C
C     UNPACK THE NEXT COLUMN
C
  320 CALL UNPACK (*330,SCR5,RZ(IVEC1))
      GO TO 350
  330 DO 340 I = 1,NROWE
      J = I - 1
      RZ(IVEC1+J) = 0.0
  340 RZ(IVEC2+J) = 0.0
      GO TO 600
C
  350 IF (MCBD(5) .GE. 3) GO TO 500
C
C     CALCULATE ENERGIES FOR REAL MATRICIES
C
      IM = IMODE - 2
      DO 410 I = 1,NROWE
      IM = IM + 2
      J  = I - 1
      IF (RZ(IM).EQ.0.0 .OR. (TWOPHI*FREQ).GT.RZ(IM)) GO TO 400
      WK2 = RZ(IM)**2
C
      DKD =-S2*RZ(IVEC1+J)/(RZ(IM+1)*WK2**2*(1.0 + S2/WK2))
      DKS = RZ(IVEC1+J)/(RZ(IM+1)*WK2)
C
      RZ(IVEC2+J) = .5*RZ(IM+1)*WK2*ABS((2.0*DKS+DKD)*DKD)
      RZ(IVEC1+J) = ABS(S2/WK2)*RZ(IVEC2+J)
      GO TO 410
C
  400 RZ(IVEC1+J) = 0.0
      RZ(IVEC2+J) = 0.0
C
  410 CONTINUE
      GO TO 600
C
C     CALCULATE ENERGIES FOR COMPLEX VECTORS
C
  500 IM = IMODE - 2
      DO 520 I = 1,NROWE
      IM = IM + 2
      J  = I - 1
      IF (RZ(IM).EQ.0.0 .OR. AIMAG(SC).GT.RZ(IM)) GO TO 510
      WK2 = RZ(IM)**2
C
      DKDC =-SC2*CZ(ICVEC1+J)/(RZ(IM+1)*WK2**2*(1.0+SC2/WK2))
      DKSC = CZ(ICVEC1+J)/(RZ(IM+1)*WK2)
C
      RZ(IVEC2+J) = .5*RZ(IM+1)*WK2*CABS((2.0*DKSC+DKDC)*DKDC)
      RZ(IVEC1+J) = CABS(SC**2/WK2)*RZ(IVEC2+J)
      GO TO 520
C
  510 RZ(IVEC1+J) = 0.0
      RZ(IVEC2+J) = 0.0
C
  520 CONTINUE
C
C     PACK OUT THE KENETIC AND POTENTIAL ENERGIES
C
  600 CALL PACK (RZ(IVEC1),SCR3,MCBA)
      CALL PACK (RZ(IVEC2),SCR4,MCBB)
C
  700 CONTINUE
C
      CALL CLOSE (SCR5,REW)
      CALL CLOSE (SCR3,REW)
      CALL CLOSE (SCR4,REW)
      CALL WRTTRL (MCBA)
      CALL WRTTRL (MCBB)
C
C     NORMAL RETURN
C
      RETURN
C
C     NO EXECLUDED MODES EXIST
C
  900 NOEXCL = .TRUE.
      RETURN
C
C     ERRORS
C
 6000 CALL SMSG (RC-2,ITEM,RSS)
      GO TO 9200
 6100 CALL SMSG (RC+4,ITEM,RSS)
      GO TO 9200
 6200 CALL SMSG (7,ITEM,RSS)
      GO TO 9200
 6372 WRITE (NOUT,6373) UWM,RSS
      GO TO 9200
 9008 CALL MESAGE (8,0,NAME)
 9200 WRITE (NOUT,6371) UWM,RSS
      NOEXCL = .TRUE.
      CALL CLOSE (SCR3,REW)
      CALL CLOSE (SCR4,REW)
      CALL CLOSE (SCR5,REW)
      RETURN
C
C     FORMAT STATEMENTS
C
 6371 FORMAT (A25,' 6371, CALCULATIONS FOR EXCLUDED MODE ENERGIES FOR',
     1       ' SUBSTRUCTURE ',2A4,' ABORTED.')
 6373 FORMAT (A25,' 6372, THE PHIS AND LAMS ITEMS ARE INCONSISTANT FOR',
     2       ' SUBSTRUCTURE ',2A4)
      END
