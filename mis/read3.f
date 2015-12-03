      SUBROUTINE READ3 (NOVECT,NCOL,SR1FIL,SR2FIL,FILC,KDBLM)
C
C     READ3 PACKS THE EIGENVECTORS AND EIGENVALUES AND PUTS THEM OUT IN
C     ASCENDING ORDER
C
C     LAST REVISED  1/92, BY G.CHAN/UNISYS
C     ZERO OUT RIGID BODY FREQUENCIES IF METHOD IS 'FEER' (NOT 'FEER-X'
C     NOR 'FEER-Q')
C
      INTEGER            SYSBUF    ,IZ(1)    ,RSP      ,RDP      ,
     1                   FILELM    ,FILEVC   ,SR1FIL   ,SR2FIL   ,
     2                   FILC      ,OPTION   ,OPTN2    ,FEER     ,
     3                   DASHZ     ,STURM
      INTEGER            RDREW
      DOUBLE PRECISION   DXX(2)
      DIMENSION          NAM(2)    ,FILEVC(7),FILELM(7)
      COMMON   /ZZZZZZ/  Z(1)
      COMMON   /STURMX/  STURM     ,SHFTPT
      COMMON   /REIGKR/  OPTION    ,OPTN2
      COMMON   /SYSTEM/  SYSBUF    ,NOUT     ,SYSTM(52),IPREC
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP
      COMMON   /PACKX /  ITYPA     ,ITYPB    ,IPAK     ,JPAK     ,
     1                   INCR
      COMMON   /UNPAKX/  ITYPU     ,IUNP     ,JUNP     ,INCRU
      EQUIVALENCE        (IZ(1),Z(1))
      DATA      FEER  ,  DASHZ /4HFEER,   4H-X   /
      DATA      NAM   /  4HREAD,4H3   /,  I2 / 2 /
C
C     FILELM (=KDBLM=LAMA=201) WILL HOLD THE EIGENVALUES  UPON RETURN
C     FILEVC (=FILC =PHIA=202) WILL HOLD THE EIGENVECTORS UPON RETURN
C
      FILELM(1) = KDBLM
      FILEVC(1) = FILC
      ITYPA = RSP
      ITYPB = RSP
      INCR  = 1
      IPAK  = 1
      JPAK  = NCOL
      NCOL2 = IPREC*NCOL
      ITYPU = RSP
      INCRU = 1
      NOCL  = 2*NCOL + 2
      NZ    = KORSZ(Z)
      IBUF1 = NZ    - SYSBUF
      IBUF2 = IBUF1 - SYSBUF
C
C     READ IN ALL EIGENVALUES
C
      IFILE = SR1FIL
      CALL GOPEN (SR1FIL,Z(IBUF1),RDREW)
      I = 1
   10 CALL FREAD (SR1FIL,DXX,IPREC,1)
      Z(I+1) = DXX(1)
      I = I + 1
      IF (I .LE. NOVECT) GO TO 10
      CALL CLOSE (SR1FIL,REW)
C
C     SET UP AN INDEX VECTOR AND SORT THE EIGENVALUES
C
      J = NCOL + 2
      K = J + NCOL - 1
      II = 1
      DO 20 I = J,K
      IZ(I) = II
   20 II = II + 1
      Z(1) = Z(I2)
      J = 2
      K = J + NOVECT - 1
      DO 25 I = J,K
      IF (Z(I) .LT. Z(1)) Z(1) = Z(I)
   25 CONTINUE
      DO 40 I = 1,NOVECT
      K = I
   30 IF (Z(K+1) .GE. Z(K)) GO TO 40
      ZZ     = Z(K  )
      Z(K  ) = Z(K+1)
      Z(K+1) = ZZ
      J  = K + NCOL
      II = IZ(J)
      IZ(J  ) = IZ(J+1)
      IZ(J+1) = II
      K = K - 1
      GO TO 30
   40 CONTINUE
C
C     ZERO OUT RIGID BODY EIGENVALUES IF THEY ARE PRESENT AND METHOD IS
C     'FEER-Z'
C     I.E. ZERO FREQUENCIES BELOW PTSHFT AND KEEP, AS CHECKED BY STURM
C     SEQUENCE
C
      IF (STURM .LT. 0) GO TO 45
      DO 43 I = 2,NOVECT
      IK = I + STURM
      IF (Z(IK).GE.SHFTPT .OR. IK.GT.NOVECT) GO TO 45
      IF (Z(I).LT.0. .AND. OPTION.EQ.FEER .AND. OPTN2.EQ.DASHZ) Z(I)= 0.
   43 CONTINUE
C
C     READ THE EIGENVECTORS AND PACK THEM IN ASCENDING ORDER
C
   45 CALL GOPEN (FILEVC,Z(IBUF1),1)
      IFILE = SR2FIL
      CALL GOPEN (SR2FIL,Z(IBUF2),RDREW)
      IPOS = 1
      CALL MAKMCB (FILEVC(1),FILC,NCOL,2,RSP)
C
      DO 110 I = 1,NOVECT
      K  = I + NCOL + 1
      NO = IZ(K)
      IF (NO-IPOS) 50,80,70
   50 CALL REWIND (SR2FIL)
      IPOS = NO
      IF (NO .LE. 0) GO TO 120
   60 CALL SKPREC (SR2FIL,NO)
      GO TO 80
   70 NO   = NO - IPOS
      IPOS = IPOS + NO
      GO TO 60
   80 IUNP = 0
      CALL UNPACK (*90,SR2FIL,Z(NOCL))
      IPOS = IPOS + 1
      IPAK = IUNP
      JPAK = JUNP
      GO TO 100
   90 IPAK = 1
      JPAK = 1
      Z(NOCL) = 0.0
  100 CALL PACK (Z(NOCL),FILEVC,FILEVC)
  110 CONTINUE
C
      CALL CLOSE  (FILEVC(1),REW)
      CALL CLOSE  (SR2FIL,REW)
      CALL WRTTRL (FILEVC)
C
C     OUTPUT THE EIGENVALUES, 1ST DATA RECORD
C
      CALL GOPEN (FILELM,Z(IBUF1),1)
      CALL WRITE (FILELM,Z(I2),NOVECT,1)
C
C     SAVE ORDER FOUND IN 2ND DATA RECORD
C
      CALL WRITE (FILELM,IZ(NCOL+2),NOVECT,1)
      CALL CLOSE (FILELM(1),REW)
      FILELM(2) = NOVECT
      CALL WRTTRL (FILELM)
      RETURN
C
  120 CALL MESAGE (-7,FILE,NAM)
      RETURN
C
      END
