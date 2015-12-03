      SUBROUTINE FEER4 (IT)
C
C     FEER4 OBTAINS FROM THE REDUCED TRIDIAGONAL MATRIX THE EIGENVALUES
C     AND EIGENVECTORS
C
      LOGICAL           INCORE
      INTEGER           SYSBUF    ,CNDFLG   ,SR2FLE    ,SR6FLE   ,
     1                  SR7FLE    ,SR8FLE   ,IZ(1)     ,NAME(2)
CWKBNB NCL93007 11/94
      INTEGER           SR4FLE    ,SR5FLE   ,REW       ,EOFNRW
     1,                 WRTREW    ,RDREW    ,WRT
CWKBNE NCL93007 11/94
      DOUBLE PRECISION  LAMBDA    ,LMBDA    ,DZ(1)     ,B(2)     ,
     1                  DSM       ,DSCE
      DIMENSION         MCBC(7)   ,ICR(2)   ,SB(2)
      COMMON   /MACHIN/ MACH
      COMMON   /FEERCX/ IFKAA(7)  ,IFMAA(7) ,IFLELM(7),IFLVEC(7) ,
     1                  SR1FLE    ,SR2FLE   ,SR3FLE   ,SR4FLE    ,
     2                  SR5FLE    ,SR6FLE   ,SR7FLE   ,SR8FLE    ,
     3                  DMPFLE    ,NORD     ,XLMBDA   ,NEIG      ,
     4                  MORD      ,IBK      ,CRITF    ,NORTHO    ,
     5                  IFLRVA    ,IFLRVC
      COMMON   /FEERXX/ LAMBDA    ,CNDFLG   ,ITER     ,TIMED     ,
     1                  L16       ,IOPTF    ,EPX      ,ERRC      ,
     2                  IND       ,LMBDA    ,IFSET    ,NZERO     ,
     3                  NONUL     ,IDIAG    ,MRANK    ,ISTART
      COMMON   /ZZZZZZ/ Z(1)
      COMMON   /SYSTEM/ SYSBUF    ,IO       ,SYSTM(52),IPREC
      COMMON   /OPINV / MCBLT(7)  ,MCBSMA(7),MCBVEC(7),MCBRM(7)
      COMMON   /UNPAKX/ IPRC      ,II       ,NN       ,INCR
      COMMON   /PACKX / ITP1      ,ITP2     ,IIP      ,NNP       ,
     1                  INCRP
      COMMON   /NAMES / RD        ,RDREW    ,WRT      ,WRTREW    ,
     1                  REW       ,NOREW    ,EOFNRW
      EQUIVALENCE       (IZ(1),Z(1),DZ(1)), (SB(1),B(1)), (DSCE,SCE)
      DATA      NAME  / 4HFEER,4H4     /,    ICR   / 4HPASS,4HFAIL /
C
C     SR4FLE CONTAINS THE EIGENVECTORS OF THE REDUCED PROBLEM
C     SR5FLE CONTAINS THE TRIDIAGONAL ELEMENTS AND SCRATCH IN FQRWV
C     SR6FLE CONTAINS THE G VECTORS
C     SR7FLE CONTAINS THE ORTHOGONAL VECTORS
C
      CALL SSWTCH (26,L26)
      MDIM = MORD + 1
      DSM  = 10.0D+0**(-2*IT/3)
      SM   = DSM
      IPRC = MCBRM(5)
      NZ   = KORSZ(Z)
      CALL MAKMCB (MCBC(1),SR4FLE,MDIM,2,IPRC)
      MCBC(2) = 0
      MCBC(6) = 0
      M    = 0
C
C     INITIALIZE ALLOCATIONS
C
      IBUF1 = NZ    - SYSBUF
      IBUF2 = IBUF1 - SYSBUF
      IBUF3 = IBUF2 - SYSBUF
      IV1   = 1
      IV2   = IV1 + MDIM
      IV3   = IV2 + MDIM
      IV4   = IV3 + MDIM
      IV5   = IV4 + MDIM
      IV6   = IV5 + MDIM
      IV7   = IV6 + MDIM
      IV8   = IV7 + MDIM
      IV9   = IV8 + MDIM
      IX3   = IV3 - 1
      IX4   = IV4 - 1
      IEND  = IPRC*(8*MDIM+1) + MDIM
      IF (IEND .GT. IBUF3) CALL MESAGE (-8,IEND-IBUF3,NAME)
      CALL GOPEN (SR5FLE,Z(IBUF2),RDREW)
      IF (IPRC .EQ. 2) DZ(IV4+MORD) = ERRC
      IF (IPRC .EQ. 1)  Z(IV4+MORD) = ERRC
      NW = IPRC*2
      DO 10 I = 1,MORD
      CALL READ (*420,*430,SR5FLE,B(1),NW,1,M)
      IF (IPRC .EQ. 1) GO TO 5
      DZ(IX3+I) = B(1)
      DZ(IX4+I) = B(2)
      GO TO 10
    5 Z(IX3+I) = SB(1)
      Z(IX4+I) = SB(2)
   10 CONTINUE
      CALL CLOSE (SR5FLE,REW)
      CALL GOPEN (SR4FLE,Z(IBUF2),WRTREW)
      IF (IPRC .EQ. 1) GO TO 12
      CALL FQRWV (MORD,DZ(IV1),DZ(IV2),DZ(IV3),DZ(IV4),DZ(IV5),DZ(IV6),
     1                 DZ(IV7),DZ(IV8),DZ(IV9),Z(IBUF1),SR5FLE,MCBC(1))
C                                                              SR4FLE
      GO TO 15
   12 CALL FQRW (MORD,Z(IV1),Z(IV2),Z(IV3),Z(IV4),Z(IV5),Z(IV6),
     1                Z(IV7),Z(IV8),Z(IV9),Z(IBUF1),SR5FLE,MCBC(1))
C                                                          SR4FLE
   15 CALL CLOSE (SR4FLE,NOREW)
C
C     RECONFIGURE VECTOR INDEX TO OBTAIN PHYSICAL EIGENVECTORS
C
      IX1 = IV1 - 1
      IX2 = IV2 - 1
      IX3 = IV3 - 1
      IX4 = IV4 - 1
      IX5 = IX4 + NORD
      ISRV = MCBRM(1)
      IFLVEC(1) = IFLRVC
      IFLELM(1) = IFLRVA
      IF (NZERO .NE. 0) GO TO 20
C
C     PREPARE FILES WHEN NO RESTART AND/OR RIGID BODY VECTORS
C
      IFLVEC(2) = 0
      IFLVEC(6) = 0
      CALL GOPEN (IFLRVC,Z(IBUF3),WRTREW)
      CALL CLOSE (IFLRVC,NOREW)
      CALL GOPEN (IFLRVA,Z(IBUF3),WRTREW)
      CALL CLOSE (IFLRVA,NOREW)
   20 ITP1 = IPRC
      ITP2 = 1
      INCRP= 1
      II   = 1
      CALL GOPEN (IFLRVA,Z(IBUF1),WRT)
      MRED = 0
      MFLG = 1
      DO 30 M = 1,MORD
      IF (IPRC .EQ. 1) GO TO 22
      DSCE = 1.0D+0/DZ(IX1+M) - LAMBDA
      IF (L16 .EQ. 0) GO TO 24
      ERF  = 0.0D+0
      IF (DABS(DSCE) .GT. DSM)
     1    ERF = 100.D0*DZ(IX2+M)/DABS(1.D0-DZ(IX1+M)*LAMBDA)
      DZ(IX2+M) = DSCE
      GO TO 23
   22 SCE = 1.0/Z(IX1+M) - LAMBDA
      IF (L16 .EQ. 0) GO TO 24
      ERF = 0.0D+0
      IF (ABS(SCE) .GT. SM)
     1    ERF = 100.0D+0*Z(IX2+M)/DABS(1.0D+0-Z(IX1+M)*LAMBDA)
      Z(IX2+M) = SCE
   23 IF (ERF  .GT. CRITF) MFLG = 2
   24 IF (MFLG .EQ.     2) GO TO 25
      MRED = MRED + 1
      CALL WRITE (IFLRVA,DSCE,IPREC,1)
   25 IF (L16 .EQ. 0) GO TO 30
      CALL PAGE2 (1)
      IF (IPRC .EQ. 2) WRITE (IO,26) M,DSCE,ERF,ICR(MFLG)
      IF (IPRC .EQ. 1) WRITE (IO,26) M, SCE,ERF,ICR(MFLG)
   26 FORMAT (10X,'PHYSICAL EIGENVALUE',I5,1P,E16.8,
     1        '  THEOR ERROR ',E16.8,'  PERCENT',5X,A4)
   30 CONTINUE
      CALL CLOSE (IFLRVA,EOFNRW)
      IF (MORD .EQ. 0) RETURN
C
      CALL GOPEN (ISRV,Z(IBUF1),RDREW)
      CALL GOPEN (SR4FLE,Z(IBUF2),RDREW)
      CALL GOPEN (IFLRVC,Z(IBUF3),WRT)
CWKBNB NCL93007 11/94
      INCORE = .FALSE.
      CALL SSWTCH ( 43, L43 )
      IF ( L43 .NE. 0 ) GO TO 42
      IVW    = IX5 + NORD + 1
      ICREQ  = NORD*MORD*IPRC
      ICAVL  = IBUF3 - IVW - 1
      IF ( ICAVL .GT. ICREQ ) INCORE = .TRUE.
      IF ( .NOT. INCORE ) GO TO 42
      NN = NORD
      DO 41 I = 1, MORD
      IVR = IVW + (I-1)*NORD
      IF ( IPRC .EQ. 1 ) CALL UNPACK ( *41, ISRV,  Z(IVR+1) )  
      IF ( IPRC .EQ. 2 ) CALL UNPACK ( *41, ISRV, DZ(IVR+1) )  
   41 CONTINUE
   42 CONTINUE
CWKBNE NCL93007 11/94
C
C     IF DIAG 26 IS OFF, LIMIT EIGENSOLUTIONS TO NUMBER REQUESTED
C
      IF (MRED.GE.NEIG .AND. L26.NE.0) MRED = NEIG
      IF (IPRC .EQ. 1) GO TO 200
      DO 100 M = 1,MRED
      DO 50  L = 1,NORD
   50 DZ(IX5+L) = 0.0D+0
      NN = MORD
      CALL UNPACK (*75,SR4FLE,DZ(IV3))
      NN = NORD
CWKBI NCL93007 11/94
      IF ( INCORE ) GO TO 72
      DO 70 I = 1,MORD
      CALL UNPACK (*100,ISRV,DZ(IV4))
      DO 60 J = 1,NORD
   60 DZ(IX5+J) = DZ(IX5+J) + DZ(IX4+J)*DZ(IX3+I)
   70 CONTINUE
CWKBNB NCL93007 11/94
      GO TO 73
   72 CONTINUE
      DO 61 I = 1, MORD
      IVR = IVW + (I-1)*NORD
      DO 61 J = 1, NORD
   61 DZ(IX5+J) = DZ(IX5+J) + DZ(IVR+J)*DZ(IX3+I)
   73 CONTINUE
CWKBNE NCL93007 11/94
   75 CONTINUE
      IF (IOPTF .EQ. 0) GO TO 90
      DSCE = 1.0D+0/DSQRT(DABS(DZ(IX1+M)))
      DO 80 L = 1,NORD
   80 DZ(IX5+L) = DSCE*DZ(IX5+L)
   90 CONTINUE
      IIP = 1
      NNP = NORD
      CALL PACK (DZ(IX5+1),IFLRVC,IFLVEC(1))
CWKBI NCL93007 11/94
      IF ( INCORE ) GO TO 100
      CALL REWIND (MCBRM)
      CALL SKPREC (MCBRM,1)
  100 CONTINUE
      GO TO 400
  200 DO 300 M = 1,MRED
      DO 250 L = 1,NORD
  250 Z(IX5+L) = 0.0
      NN = NORD
      CALL UNPACK (*275,SR4FLE,Z(IV3))
      NN = NORD
CWKBI NCL93007 11/94
      IF ( INCORE ) GO TO 272
      DO 270 I = 1,MORD
      CALL UNPACK (*300,ISRV,Z(IV4))
      DO 260 J = 1,NORD
  260 Z(IX5+J) = Z(IX5+J) + Z(IX4+J)*Z(IX3+I)
  270 CONTINUE
CWKBNB NCL93007 11/94
      GO TO 273
  272 CONTINUE
      DO 261 I = 1, MORD
      IVR = IVW + (I-1)*NORD
      DO 261 J = 1, NORD
  261 Z(IX5+J) = Z(IX5+J) + Z(IVR+J)*Z(IX3+I)
  273 CONTINUE
CWKBNE NCL93007 11/94
  275 CONTINUE
      IF (IOPTF .EQ. 0) GO TO 290
      SCE = 1.0/SQRT(ABS(Z(IX1+M)))
      DO 280 L = 1,NORD
  280 Z(IX5+L) = SCE*Z(IX5+L)
  290 CONTINUE
      IIP = 1
      NNP = NORD
      CALL PACK (Z(IX5+1),IFLRVC,IFLVEC(1))
CWKBI NCL93007 11/94
      IF ( INCORE ) GO TO 300
      CALL REWIND (MCBRM)
      CALL SKPREC (MCBRM,1)
  300 CONTINUE
C
  400 CALL CLOSE (IFLRVC,EOFNRW)
      CALL CLOSE (ISRV,REW)
      CALL CLOSE (SR4FLE,REW)
      MORD = MRED
      GO TO 500
  420 IER = 2
      GO TO 440
  430 IER = 3
  440 CNDFLG = 4
      CALL MESAGE (IER,SR5FLE,NAME)
  500 IOPN = IBUF3 - IEND
      IF (L16 .EQ. 1) WRITE (IO,510) IOPN,NAME
  510 FORMAT ('  OPEN CORE NOT USED',I10,2X,2A4)
      RETURN
      END
