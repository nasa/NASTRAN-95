      SUBROUTINE REIG
C
C     READ   KAA,MAA,MR,DM,EED,USET,CASECC/LAMA,PHIA,MI,OEIGS/C,N,IPROB
C            /V,N,NUMMOD/C,N,ICASE/C,N,XLAMDA $
C
      INTEGER         SYSBUF   ,EIGR(4)  ,ICORE(12),CASECC   ,FILE     ,
     1                ERROR(3) ,FEERX    ,SDET     ,UDET     ,INV      ,
     2                SINV     ,UINV     ,GIVI     ,STURM    ,GIVN(7)  ,
     3                DM       ,EED      ,LAMA     ,PHIA     ,MI       ,
     4                USET     ,POUT     ,SCR1     ,SCR2     ,SCR3     ,
     5                SCR4     ,SCR5     ,SCR6     ,SCR7     ,OPTION   ,
     6                SUBNAM(2),IX(7)    ,OPTN2
      REAL            LMIN     ,LMAX     ,LFREQ    ,MB(1)
      DOUBLE PRECISION DCORE(1)
      CHARACTER       UFM*23   ,UWM*25   ,UIM*29
      COMMON /XMSSG / UFM      ,UWM      ,UIM
      COMMON /BLANK / IPROB(2) ,NUMMOD   ,ICASE    ,XLAMDA
     1       /INVPWX/ IFILK(7) ,IFILM(7) ,IFILLM(7),IFILVC(7),ISCR1    ,
     2                ISCR2    ,ISCR3    ,ISCR4    ,ISCR5    ,ISCR6    ,
     3                ISCR7    ,ISCR8    ,IDUMP    ,LMIN     ,LMAX     ,
     4                NOEST    ,NDPLUS   ,NDMNUS   ,EPS      ,NOVECT
     5       /GIVN  / DUM(100) ,N        ,LFREQ    ,ORDER    ,D1       ,
     6                HFREQ    ,D2       ,NV       ,NPRT     ,D4       ,
     7                NFR
      COMMON /FEERCX/ IFKAA(7) ,IFMAA(7) ,IFLELM(7),IFLVEC(7),SR1FLE   ,
     1                SR2FLE   ,SR3FLE   ,SR4FLE   ,SR5FLE   ,SR6FLE   ,
     2                SR7FLE   ,SR8FLE   ,DMPFLE   ,NORD     ,XLMBDA   ,
     3                NEIG     ,MORD     ,IBK      ,CRITF    ,NORTHO   ,
     4                IFLRVA   ,IFLRVC   ,IEPX
     5       /NTIME / LNTIME   ,TCONS(15)
     6       /REGEAN/ IM(7)    ,IK(7)    ,IEV(7)   ,SCR1     ,SCR2     ,
     7                SCR3     ,SCR4     ,SCR5     ,LCORE    ,RMAX     ,
     8                RMIN     ,MZ       ,NEV      ,EPSI     ,RMINR    ,
     9                NE       ,NIT      ,NEVM     ,SCR6     ,SCR7     ,
     O                NFOUND   ,LAMDA    ,IBUCK    ,NSYM
      COMMON /STURMX/ STURM    ,SHFTPT   ,KEEP     ,PTSHFT   ,NR       ,
     1                SHFTZO
     2       /REIGKR/ OPTION   ,OPTN2
     3       /SYSTEM/ SYSBUF   ,NOUT     ,NOGO     ,KSYS(51) ,JPREC
      COMMON /CONDAS/ PI       ,TWOPI    ,RADEG    ,DEGRA    ,
     1                FPS
      COMMON /PACKX / ITP1     ,ITP2     ,IIP      ,JJP      ,INCRP
      COMMON /UNPAKX/ ITU      ,IIU      ,JJU      ,INCRU
     1       /ZZZZZZ/ CORE(1)
      EQUIVALENCE     (GIVN(1) ,CORE(1))
      EQUIVALENCE     (TCONS(4),APC    ) ,(TCONS(5),APU      ) ,
     1                (TCONS(8),MB(1)  ) ,(ERROR(2),SUBNAM(1)) ,
     2                (DCORE(1) ,CORE(1) ,ICORE (1))
      DATA            EIGR                                   ,CASECC/
     1                307      ,3        ,107      ,1        ,107   /,
     2                SDET     ,UDET     ,INV      ,SINV     ,I0    /
     3                4HSDET   ,4HUDET   ,4HINV    ,4HSINV   ,0     /,
     4                UINV     ,GIVI     ,KAA      ,MAA      ,MR    /
     5                4HUINV   ,4HGIV    ,101      ,102      ,103   /,
     6                DM       ,EED      ,USET     ,LAMA     ,PHIA  /
     7                104      ,105      ,106      ,201      ,202   /,
     8                MI       ,POUT     ,ICR1     ,ICR2     ,MODE  /
     9                203      ,204      ,301      ,302      ,4HMODE/,
     O                ERROR                        ,FEERX    ,MGIV  /
     1                4HEED    ,4HREIG   ,4H       ,4HFEER   ,4HMGIV/
C
C
      IBUCK  = 1
      LCORE  = KORSZ(CORE) - SYSBUF - 3
      LLCORE = LCORE - SYSBUF
      CALL GOPEN (LAMDA,CORE(LCORE+1),1)
      CALL CLOSE (LAMDA,2)
      IF (IPROB(1) .NE. MODE) IBUCK = 3
      STURM  = -1
      KEEP   = 0
      SHFTPT = 0.0
      PTSHFT = 0.0
      NR     = 0
      SHFTZO = 0.0
      CALL OPEN (*10,CASECC,CORE(LCORE+1),0)
      CALL SKPREC (CASECC,ICASE)
      CALL FREAD (CASECC,ICORE,166,1)
      CALL CLOSE (CASECC,1)
      METHOD = ICORE(5)
      GO TO 20
   10 METHOD = -1
   20 FILE   = EED
      CALL PRELOC (*170,CORE(LCORE+1),EED)
      CALL LOCATE (*40,CORE(LCORE+1),EIGR(IBUCK),IFLAG)
   30 CALL READ (*40,*40,EED,CORE(1),18,0,IFLAG)
      IF (METHOD.EQ.ICORE(1) .OR. METHOD.EQ.-1) GO TO 50
      GO TO 30
C
C     NO SET NUMBER FOUND
C
   40 CALL MESAGE (-32,METHOD,ERROR)
C
C     FOUND DATA CARD
C
   50 NORM = ICORE(10)
      CALL CLOSE (EED,1)
C
C     TEST THE SIZE OF THE K AND M MATRICES VIA THEIR TRAILERS
C
      CALL RDTRL (IK(1))
      CALL RDTRL (IM(1))
      IF (IM(2).EQ.IK(2) .AND. IM(3).EQ.IK(3)) GO TO 51
C
C     K AND M MATRICES ARE NOT OF THE SAME SIZE
C
      WRITE  (NOUT,200) UFM
  200 FORMAT (A23,' 3131, INPUT STIFFNESS AND MASS MATRICES ARE NOT ',
     1       'COMPATIBLE.')
      CALL MESAGE (-37,0,ERROR(2))
C
C     K AND M MATRICES ARE COMPATIBLE
C
   51 CONTINUE
C
C     CHECK TO SEE IF THE INPUT STIFFNESS AND/OR MASS MATRIX IS NULL
C
      IF (IK(6).EQ.0 .OR. IM(6).EQ.0) CALL MESAGE (-60,0,0)
C
C     SET FLAG FOR THE METHOD OF ANALYSIS AND THE PROPER
C     TYPE OF DECOMPOSITION
C
      OPTION = ICORE(2)
      OPTN2  = ICORE(3)
      IF (OPTION.EQ.GIVI .OR. OPTION.EQ.UDET .OR. OPTION.EQ.UINV)
     1    GO TO 53
      IF (OPTION.EQ.FEERX .OR. OPTION.EQ.MGIV) GO TO 53
      IF (OPTION.EQ.SDET  .OR. OPTION.EQ.SINV) GO TO 52
      OPTION = UDET
      IF (ICORE(2) .EQ. INV) OPTION = UINV
      IF (IM(4).NE.6 .OR. IK(4).NE.6) GO TO 53
      OPTION = SDET
      IF (ICORE(2) .EQ. INV) OPTION = SINV
      GO TO 53
   52 IF (IM(4).EQ.6 .AND. IK(4).EQ.6) GO TO 53
      WRITE (NOUT,2100) UWM
      OPTION = UDET
      IF (ICORE(2) .EQ. SINV) OPTION = UINV
   53 ISIL  = ICORE(12)
      I     = 9
      EPSII = CORE(I)
      IF (IBUCK .EQ. 3) GO TO 60
C
C     CONVERT FREQUENCY TO LAMDA
C
      IF ((ICORE(2).EQ.GIVI .OR. ICORE(2).EQ.MGIV) .AND. ICORE(7).GT.0)
     1    GO TO 55
      IF (CORE(I0+4) .GE. 0.0) GO TO 55
      WRITE (NOUT,2000) UWM
      CORE(I0+4) = 0.0
   55 CORE(I0+4) = FPS*CORE(I0+4)*CORE(I0+4)
      IF (ICORE(2) .NE. FEERX) CORE(I0+5) = FPS*CORE(I0+5)*CORE(I0+5)
   60 CONTINUE
      CORE4  = CORE(I0+4)
      CORE5  = CORE(I0+5)
      ICORE6 = ICORE(6)
      ICORE7 = ICORE(7)
      ICORE8 = ICORE(8)
      IF (ICORE(2).EQ.GIVI .OR. ICORE(2).EQ.MGIV) GO TO 70
      IF (ICORE(2) .EQ. FEERX) GO TO 65
      IF (ICORE(7) .EQ. 0) ICORE(7) = 3*ICORE(6)
      ICORE7 = ICORE(7)
C
C     FEER, INVERSE POWER AND DETERMINANT METHODS
C
C     CHECK IF IT IS A NORMAL MODES PROBLEM OR A BUCKLING PROBLEM
C
   65 IF (IBUCK .EQ. 3) GO TO 80
C
C     NORMAL MODES PROBLEM
C
C     CHECK FOR APPEND
C
      IF (NUMMOD .LE. 0) GO TO 70
      IX(1) = PHIA
      CALL RDTRL (IX)
      IF (IX(1).LE.0 .OR. IX(2).LE.0) GO TO 70
C
C     NEW EIGENVALUES AND EIGENVECTORS WILL BE APPENDED TO THOSE
C     PREVIOUSLY CHECKPOINTED
C
      NR = IX(2)
      IF (NUMMOD .LT. NR) NR = NUMMOD
      WRITE (NOUT,2200) UIM,NR
C
C     RETRIEVE EIGENVALUES AND EIGENVECTORS PREVIOUSLY CHECKPOINTED.
C
C     COPY OLD EIGENVALUES FROM LAMA FILE TO ICR1 FILE.
C
C     COPY OLD EIGENVECTORS FROM PHIA FILE TO ICR2 FILE.
C
      CALL READ7 (NR,LAMA,PHIA,ICR1,ICR2)
      GO TO 80
C
C     NO APPEND
C
C     CHECK IF RIGID BODY MODES ARE TO BE COMPUTED SEPARATELY
C
   70 IX(1) = MR
      CALL RDTRL (IX)
      IF (IX(1) .LT. 0) GO TO 75
C
C     COMPUTE RIGID BODY MODES
C
      CALL READ1 (DM,MR,SCR4,SCR5,SCR3,ICR2,USET,NR,ICR1,SCR6)
C
C     RIGID BODY EIGENVALUES ARE ON ICR1
C
C     RIGID BODY EIGENVECTORS ARE ON ICR2
C
   75 IF (OPTION.EQ.GIVI .OR. OPTION.EQ.MGIV) GO TO 100
   80 IF (OPTION .EQ.FEERX) GO TO  95
      IF (OPTION .EQ. SDET) GO TO 109
      IF (OPTION .EQ. UDET) GO TO 110
C
C
C     INVERSE POWER METHOD
C     ********************
C
      LMIN   = CORE4
      LMAX   = CORE5
      NOEST  = ICORE6
      NDPLUS = ICORE7
      NDMNUS = 0
      IF (IBUCK .EQ. 3) NDMNUS = ICORE8
      EPS = EPSII
      IF (EPS .LE.      0.) EPS = .0001
      IF (EPS .LT. .000001) EPS = .000001
      CALL RDTRL (IFILK(1))
      CALL RDTRL (IFILM(1))
      NOVECT = NR
      CALL INVPWR
      METHOD = 2
      NUMMOD = NOVECT
      GO TO 140
C
C
C     FEER METHOD
C     ***********
C
   95 IFLRVA = ICR1
      IFLRVC = ICR2
      XLMBDA = CORE4
      NEIG   = ICORE7
      IEPX   = ICORE8
      IF (IBUCK .EQ. 3) NEIG = ICORE6
      NORTHO = NR
      CRITF  = CORE5
      IX(1)  = KAA
      CALL RDTRL (IX)
      N = IX(2)
      IF (CRITF .EQ. 0.) CRITF = .001/N
      CALL FEER
      METHOD = 2
      NUMMOD = MORD + NR
      CALL SSWTCH (26,L26)
      IF (NUMMOD.GT.NEIG .AND. L26.NE.0) NUMMOD = NEIG
      IFILK(2) = NORD
      GO TO 140
C
C
C     GIVENS METHOD
C     *************
C
  100 LFREQ = CORE4
      HFREQ = CORE5
      METHOD= 3
      NFR   = NR
      NPRT  = ICORE6
      NV    = ICORE7
      GIVN(   1) = KAA
      GIVN(I0+2) = MAA
      GIVN(I0+3) = PHIA
      DO 105 I = 1,4
  105 GIVN(I+3) = EIGR(I)
      CALL GIVENS
      NNV = GIVN(1)
      NUMMOD = N
      GO TO 145
C
C
C     DETERMINANT METHOD
C     ******************
C
  109 NSYM  = 1
  110 METHOD= 4
      RMIN  = CORE4
      RMAX  = CORE5
      IF (RMIN .EQ. 0.0 ) RMIN = RMAX*1.0E-4
      RMINR = -.01*RMIN
      NEV   = ICORE6
      IF (IBUCK.EQ.3 .AND. EPSII.NE.0.0) EPSI = EPSII
      NEVM  = ICORE7
      CALL RDTRL (IM(1))
      IEV(3) = IK(3)
      IF (NEVM .GT. IK(3)) NEVM = IK(3)
      MZ = NR
C
C     PICK UP UNREMOVED FREE BODY MODES
C
      IF (ICORE8 .GT. NR) MZ = -ICORE8
      IEV(2) = NR
      CALL DETM
      NUMMOD = NFOUND + NR
      IFILK(2) = IEV(3)
C
C     SORT EIGENVECTORS AND VALUES
C
  140 IF (NUMMOD .EQ. 0) GO TO 160
      CALL READ3 (NUMMOD,IFILK(2),LAMDA,IEV,PHIA,LAMA)
  145 IF (METHOD.EQ.2 .OR. NUMMOD.EQ.1) GO TO 150
C
C     CHECK ORTHOGONALITY
C
      IFILVC(1) = PHIA
      CALL RDTRL (IFILVC(1))
      CALL READ4 (LAMA,IFILVC(1),SCR1,EPSII,MAA)
  150 CONTINUE
C
C     SET FLAG FOR GIVENS METHOD FOR USE IN READ2 ROUTINE
C
      DUM(1) = 0.0
      IF (METHOD .EQ. 3) DUM(1) = 1.0
      NV = NNV
C
C     FORM MODAL MASS, NORMALIZE AND FORM SUMMARY FILE.
C
      CALL READ2 (MAA,PHIA,SCR1,NORM,ISIL,XXX,MI,LAMA,POUT,ICR2,
     1            EPSII,SCR6)
      GO TO 165
  160 NUMMOD = -1
      CALL READ5 (POUT)
  165 IF (NOGO .EQ. 14) WRITE (NOUT,166)
  166 FORMAT ('0*** THIS NASTRAN JOB WILL BE TERMINATED')
      RETURN
C
  170 IP1 = -1
  180 CALL MESAGE (IP1,FILE,SUBNAM)
      GO TO 180
C
C     ERROR MESSAGES
C
 2000 FORMAT (A25,' 2367, FREQUENCY F1 (FIELD 4) ON THE EIGR BULK DATA',
     1       ' CARD IS NEGATIVE', /5X,
     2        'IT IS ASSUMED TO BE ZERO FOR CALCULATION PURPOSES.',/)
 2100 FORMAT (A25,' 2368, SYMMETRIC DECOMPOSITION IS SPECIFIED ON THE ',
     1        'EIGR BULK DATA CARD, BUT', /5X,
     2        'UNSYMMETRIC DECOMPOSITION WILL BE USED AS THIS IS THE ',
     3        'PROPER TYPE OF DECOMPOSITION FOR THIS PROBLEM.')
 2200 FORMAT (A29,' 3143, THE EIGENVALUES AND EIGENVECTORS FOUND IN ',
     1        'THIS ANALYSIS WILL BE APPENDED', /5X,'TO THE',I8,
     2        ' EIGENVALUES AND EIGENVECTORS COMPUTED EARLIER.')
C
      END
