      SUBROUTINE INITL2 (OFFSET,DELTT)
C
C     INITL2 WILL COMPUTE THE STARTING VALUES FOR THE INTEGRATION
C     ROUTINE
C
C     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
C
      INTEGER          OFFSET   ,RSP      ,FILEM    ,FILEB    ,
     1                 FILEK    ,SQR      ,FILE     ,IFILA(7) ,
     2                 IFILB(7) ,IFILC(7) ,NAME(2)  ,RDP
      DOUBLE PRECISION DET      ,MINDIA   ,ALPHA(2) ,BETA(2)
      COMMON /SADDX /  NOMAT    ,NZ       ,MCBS(67)
      COMMON /NAMES /  RD       ,RDREW    ,WRT      ,WRTREW   ,
     1                 REW      ,NOREW    ,EOFNRW   ,RSP      ,
     2                 RDP      ,CSP      ,CDP      ,SQR
      COMMON /SFACT /  IFA(7)   ,IFL(7)   ,IFU(7)   ,ISC1     ,
     1                 ISC2     ,NXX      ,ID(5)    ,ISC3     ,
     2                 ID1(2)   ,ICHL
      COMMON /DCOMPX/  IA(7)    ,IL(7)    ,IU(7)    ,ISCR10   ,
     1                 ISCR20   ,ISCR30   ,DET      ,POWER    ,
     2                 NX       ,MINDIA
      COMMON /TRDXX /  FILEK(7) ,FILEM(7) ,FILEB(7) ,
     1                 ISCR1    ,ISCR2    ,ISCR3    ,ISCR4    ,
     2                 ISCR5    ,ISCR6    ,IOPEN    ,ISYM
      COMMON /ZZZZZZ/  Z(1)
      EQUIVALENCE      (MCBS(1),IFILA(1)) ,(MCBS(8),ITYPAL)   ,
     1                 (MCBS(9),ALPHA(1)) ,(MCBS(13),IFILB(1)),
     2                 (MCBS(20),ITYPBT)  ,(MCBS(21),BETA(1)) ,
     3                 (MCBS(61),IFILC(1))
      DATA    NAME  /  4HINIT,4HL2  /
C
      NOMAT   = 2
      IPREC   = RDP
      ALPHA(2)= 0.0D0
      BETA(2) = 0.0D0
      NX      = KORSZ(Z) - OFFSET
      NZ      = NX
C
C     FORM AND DECOMPOSE THE LEFT HAND MATRIX
C
      ITYPAL   = RDP
      ITYPBT   = RDP
      ALPHA(1) = 1.0D0/DELTT**2
      BETA(1)  = 0.5D0/DELTT
      IFILC(4) = 6
      DO 10 I = 1,7
      IFILA(I) = FILEM(I)
   10 IFILB(I) = FILEB(I)
      IFILC(2) = FILEK(2)
      IFILC(1) = ISCR2
      IF (FILEK(1) .LE. 0) IFILC(1) = ISCR1
      IFILC(3) = FILEK(2)
      IF (IFILA(1).NE.0 .AND. IFILA(4).NE.6) IFILC(4) = SQR
      IF (IFILB(1).NE.0 .AND. IFILB(4).NE.6) IFILC(4) = SQR
      IFILC(5) = IPREC
      IF (FILEM(1).LE.0 .AND. FILEB(1).LE.0) GO TO 60
      CALL SADD (Z,Z)
      IF (FILEK(1) .LE. 0) GO TO 21
   11 DO 20 I = 1,7
      IFILA(I) = IFILC(I)
   20 IFILB(I) = FILEK(I)
      IF (IFILB(4) .NE. 6) IFILC(4) = SQR
      IFILC(1) = ISCR1
      ALPHA(1) = 1.0D0
      BETA(1)  = 1.0D0/3.0D0
      CALL SADD (Z,Z)
   21 CONTINUE
      CALL WRTTRL (IFILC)
      IF (IFILC(4) .NE. 6) GO TO 31
C
C     SET UP FOR SYMMETRIC DECOMPOSITION
C
      DO 32  I = 1,7
      IFA(I) = IFILC(I)
   32 CONTINUE
      IFL(1) = ISCR2
      IFU(1) = ISCR3
      ISC1   = ISCR4
      ISC2   = ISCR5
      ISC3   = ISCR6
      IFL(5) = IPREC
      ICHL   = 0
      NXX    = NX
      FILE   = IFA(1)
      CALL SDCOMP (*1030,Z,Z,Z)
      CALL WRTTRL (IFL)
      ISYM   = 0
      GO TO 33
C
C     SET UP FOR UNSYMMETRIC DECOMPOSITION
C
   31 CONTINUE
      ISYM = 1
      DO 30 I = 1,7
   30 IA(I)  = IFILC(I)
      IL(1)  = ISCR2
      IU(1)  = ISCR3
      ISCR10 = ISCR4
      ISCR20 = ISCR5
      ISCR30 = ISCR6
      IL(5)  = IPREC
      FILE   = IA(1)
      CALL DECOMP (*1030,Z(1),Z(1),Z(1))
      CALL WRTTRL (IL)
      CALL WRTTRL (IU)
C
C     FORM FIRST RIGHT HAND MATRIX
C
   33 CONTINUE
      DO 40 I = 1,7
   40 IFILA(I) = FILEM(I)
      ALPHA(1) = 2.0D0/DELTT**2
      BETA(1)  = -1.0D0/3.0D0
      IFILC(1) = ISCR1
      CALL SADD (Z,Z)
C
C     FORM SECOND RIGHT HAND MATRIX
C
      ALPHA(1) = -1.0D0/DELTT**2
      IFILC(1) = ISCR5
      CALL SADD (Z,Z)
      DO 50 I = 1,7
      IFILA(I) = IFILC(I)
   50 IFILB(I) = FILEB(I)
      ALPHA(1) = 1.0D0
      BETA(1)  = 0.5D0/DELTT
      IFILC(1) = ISCR4
      CALL SADD (Z,Z)
      RETURN
C
C     ERRORS
C
 1030 IP1 = -5
 1031 CALL MESAGE (IP1,FILE,NAME(1))
C
C     NO BDD OR MDD
C
   60 IF (FILEK(1) .LE. 0) GO TO 70
      IFILC(1) =0
      GO TO 11
C
C     ILLEGAL INPUT.   NO MATRICES
C
   70 IP1 = -7
      GO TO 1031
      END
