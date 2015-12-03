      SUBROUTINE CFEER4
C
C     CFEER4 OBTAINS THE EIGENVALUES AND EIGENVECTORS FROM THE
C     REDUCED TRIDIAGONAL MATRIX FOR THE COMPLEX FEER METHOD
C
      LOGICAL           NO B     ,DECREM   ,QPR      ,LZ(1)     ,
     1                  DPMACH
      INTEGER           NAME(2)  ,IZ(1)    ,EOR
      INTEGER           WRTREW
      DOUBLE PRECISION  LAMBDA   ,EPS      ,DZ(1)    ,D(4)      ,
     1                  LAM1(2)
      DIMENSION         S(8)     ,DMP1(2)  ,ALAM(2)  ,DM(2)     ,
     1                  STATUS(2),ACCEPT(2),REJECT(2)
      CHARACTER         UFM*23   ,UWM*25   ,UIM*29
      COMMON  /XMSSG /  UFM      ,UWM      ,UIM
      COMMON  /FEERAA/  IKMB(21) ,ILAM(7)  ,IPHI(7)  ,IDMPFL    ,
     1                  ISCR(11) ,DUMAA(84),MCBVEC(7)
      COMMON  /FEERXC/  LAMBDA(2),SWDUM    ,MREDUC   ,NORD      ,
     1                  IDIAG    ,EPS      ,NORTHO   ,NORD2     ,
     2                  NORD4    ,NORDP1   ,NSWP(2)  ,NO B      ,
     3                  IT       ,TEN2MT   ,TENMHT   ,NSTART    ,
     4                  QPR      ,JREG     ,NOREG    ,NZERO     ,
     5                  TENMTT   ,MINOPN
      COMMON  /ZZZZZZ/  Z(1)
      COMMON  /UNPAKX/  IPRC     ,II       ,NN       ,INCR
      COMMON  /SYSTEM/  KSYSTM(65)
      COMMON  /NAMES /  RD       ,RDREW    ,WRT      ,WRTREW    ,
     1                  REW      ,NOREW    ,EOFNRW
      EQUIVALENCE       (KSYSTM(2 ),NOUT ) ,(NROW,MREDUC)       ,
     1                  (KSYSTM(55),IPREC) ,(D(1),S(1)  )       ,
     2                  (Z(1),IZ(1),LZ(1),DZ(1))
      DATA     NAME  /  4HCFEE,4HR4  /
      DATA     ACCEPT,  REJECT/4H  AC,4HCEPT,4H -RE,4HJECT      /
C
C     CORE ALLOCATION FOR ALLMAT
C
C     CONTENTS                SIZE             POINTER   TYPE   NAME
C     --------                ----             -------   ----   ----
C     INPUT MATRIX--VECTORS   2*NROW*NROW      IA        COMP   A
C     EIGENVALUES             2*NROW           IL        COMP   LAM
C     H  MATRIX               2*NROW*NROW      IH        COMP   H
C     HL MATRIX               2*NROW*NROW      IHL       COMP   HL
C     VECTOR STORAGE          2*NROW           IV        COMP   VEC
C     MULTIPLIERS             2*NROW           IM        COMP   MULT
C     INTH                    NROW             INTH      INTG   INTH
C     INTQ                    NROW             INTQ      LOGL   INTQ
C
C     CORE ALLOCATION AFTER ALLMAT IS FINISHED
C
C     ALLMAT OUTPUT EIGENVECTORS               IA
C     EIGENVALUES                              IL
C     ORDER OF EXTRACTION                      IH
C     THEORETICAL ERRORS                       IHL
C     NOT USED                                 IV,IM
C     STATUS OF SOLUTIONS                      INTH
C     DISTANCES FROM CENTER                    INTQ
C     VARIABLE PRECISION PHYSICAL EIGENVECTORS IV1
C     VARIABLE PRECISION ORTHOGONAL VECTORS    IV2
C
C     DEFINITION OF INTERNAL PARAMETERS
C
C     DMP1     = D-SUB-M-PLUS-1 = EXTRANEOUS OFF-DIAGONAL ELEMENT
C                OF REDUCED TRIDIAGONAL MATRIX, USED FOR COMPUTING
C                THEORETICAL ERRORS
C     DM       = FINAL OFF-DIAGONAL ELEMENT OF REDUCED TRIDIAGONAL
C                MATRIX
C     NO B     = LOGICAL INDICATOR FOR ABSENCE OF DAMPING MATRIX B
C     DECREM   = LOGICAL INDICATOR FOR DECREMENTED SIZE OF REDUCED
C                PROBLEM
C     NROW     = SIZE OF THE REDUCED PROBLEM (EQUIVALENT TO MREDUC)
C     RMS      = ROOT-MEAN-SQUARE OF EIGENVALUES, USED IN RIGID-BODY
C                ERROR TEST
C     NOTE.....SEE LISTING OF CFCNTL FOR ADDITIONAL DEFINITIONS
C
      IF (QPR) WRITE (NOUT,600)
      DPMACH = IPREC .EQ. 2
      NORD8  = 2*NORD4
      DECREM = .FALSE.
    4 NROW2  = 2*NROW
      NROWSQ = NROW*NROW2
C
C     ALLOCATE CORE FOR ALLMAT
C
      IA  = 1
      IL  = IA  + NROWSQ
      IH  = IL  + NROW2
      IHL = IH  + NROWSQ
      IV  = IHL + NROWSQ
      IM  = IV  + NROW2
      INTH= IM  + NROW2
      INTQ= INTH+ NROW
C
C     ALLOCATE CORE FOR PHYSICAL EIGENVECTORS (LEFT FOLLOWS RIGHT)
C
      IV1 = INTQ + NROW
      IV2 = IV1  + NORD8
      IF (DPMACH .AND. MOD(IV2,2).EQ.0) IV2 = IV2 + 1
      IV1X = IV1 - 1
C
C     TEST FOR INSUFFICIENT CORE
C
      NZ    = KORSZ(Z(1))
      IBUF1 = NZ    - KSYSTM(1)
      IBUF2 = IBUF1 - KSYSTM(1)
      IOPN  = IBUF2 - (IV2 + NORD8)
      IF (IDIAG .NE. 0) WRITE (NOUT,610) IOPN
      IF (IOPN  .LE. 0) CALL MESAGE (-8,0,NAME(1))
      IF (IOPN .LT. MINOPN) MINOPN = IOPN
      IF (NSWP(2)  .LT.  0) GO TO 209
C
C     CONSTRUCT REDUCED TRIDIAGONAL MATRIX
C
      DO 10 I = IA,IL
   10 Z(I) = 0.
      NROW22 = NROW2 + 2
      CALL GOPEN (ISCR(5),Z(IBUF1),RDREW)
      NW  = 4*IPREC
      EOR = 1
      M   = 0
      NROW1 = NROW - 1
C
C     ENTER LOOP
C
      DO 20 I = 1,NROW
      I1 = I - 1
      CALL READ (*420,*430,ISCR(5),S(1),NW,EOR,M)
      IF (QPR .AND. .NOT.DPMACH) WRITE (NOUT,620) I,(S(J),J=1,4)
      IF (QPR .AND.      DPMACH) WRITE (NOUT,630) I,(D(J),J=1,4)
C
C     ALLMAT ACCEPTS ONLY SINGLE PRECISION ARRAY
C
      J = IA + NROW22*I1
      IF (.NOT.DPMACH) GO TO 15
C
C     LOAD MAIN DIAGONAL ELEMENT
C
      Z(J  ) = D(3)
      Z(J+1) = D(4)
      IF (I .NE. NROW1) GO TO 12
C
C     SAVE LAST OFF-DIAGONAL ELEMENT
C
      DM(1) = D(1)
      DM(2) = D(2)
   12 IF (I .EQ. NROW) GO TO 20
C
C     LOAD OFF-DIAGONAL ELEMENTS
C
      Z(J+2) = D(1)
      Z(J+3) = D(2)
      J  = J + NROW2
      Z(J  ) = D(1)
      Z(J+1) = D(2)
      GO TO 20
C
C     LOAD MAIN DIAGONAL ELEMENT
C
   15 Z(J  ) = S(3)
      Z(J+1) = S(4)
      IF (I .NE. NROW1) GO TO 16
C
C     SAVE LAST OFF-DIAGONAL ELEMENT
C
      DM(1) = S(1)
      DM(2) = S(2)
   16 IF (I .EQ. NROW) GO TO 20
C
C     LOAD OFF-DIAGONAL ELEMENTS
C
      Z(J+2) = S(1)
      Z(J+3) = S(2)
      J  = J + NROW2
      Z(J  ) = S(1)
      Z(J+1) = S(2)
   20 CONTINUE
C
C     SAVE ERROR ELEMENT FROM TRIDIAGONAL MATRIX
C
      IF (.NOT.DPMACH) GO TO 25
      DMP1(1) = D(1)
      DMP1(2) = D(2)
      GO TO 26
   25 DMP1(1) = S(1)
      DMP1(2) = S(2)
   26 CONTINUE
      IF (QPR) WRITE (NOUT,640) (Z(I),I=1,NROWSQ)
      CALL CLOSE (ISCR(5),REW)
      IF (DECREM) GO TO 30
C
C     DECREMENT THE REDUCED PROBLEM SIZE IF THE ERROR ELEMENT IS NULL
C
      IF (DMP1(1).NE.0. .OR. DMP1(2).NE.0.) GO TO 30
      MREDUC = MREDUC - 1
      WRITE (NOUT,570) UWM,MREDUC
      IF (MREDUC .EQ. 0) GO TO 440
      IF (DM(1).NE.0. .OR. DM(2).NE.0.) GO TO 29
C
C     NEW ERROR ELEMENT IS ALSO NULL. RESTORE ORIGINAL REDUCED SIZE.
C
      MREDUC  = MREDUC + 1
      DMP1(1) = SNGL(EPS)
      WRITE (NOUT,590) UWM,MREDUC,DMP1
      GO TO 30
   29 DECREM = .TRUE.
      GO TO 4
C
   30 CALL ALLMAT (Z(IA),Z(IL),Z(IH),Z(IHL),Z(IV),Z(IM),Z(INTH),Z(INTQ),
     1             NROW,NROW,INIDUM)
C
C     --------------- SPECIAL PRINT -------------------------
C
      IF (.NOT.QPR) GO TO 4429
      WRITE  (NOUT,4408)
 4408 FORMAT (1H0,10X,15HALLMAT EXECUTED,/,1H0)
      J = IH - 1
      WRITE  (NOUT,4420) (Z(I),I=IL,J)
 4420 FORMAT (1H0,11HEIGENVALUES, //,(1H ,2E16.8))
      WRITE  (NOUT,4422)
 4422 FORMAT (1H0,12HEIGENVECTORS,//)
      DO 4428 I = 1,NROW
      L = IA + NROW2*(I-1)
      K = L  + NROW2 - 1
      WRITE (NOUT,4424) (Z(J),J=L,K)
C
C     CHECK NORMALITY
C
      SUMR = 0.
      SUMI = 0.
      DO 7760 J = L,K,2
      JJ   = J + 1
      SUMR = SUMR + Z(J)**2 - Z(JJ)**2
 7760 SUMI = SUMI + 2.*Z(J)*Z(JJ)
      WRITE  (NOUT,7770) SUMR,SUMI
 7770 FORMAT (//,35H SELF INNER-PRODUCT OF ABOVE VECTOR, /,1H ,6X,
     1        11HREAL PART =,E16.8,8X,16HIMAGINARY PART =,E16.8)
 4424 FORMAT (//,(1H ,6E16.8))
 4428 CONTINUE
 4429 CONTINUE
C     -------------------------------------------------------
C
C     NORMALIZE THE EIGENVECTORS OUTPUT FROM ALLMAT
C
      IF (QPR) WRITE (NOUT,4422)
      DO 36 I = 1,NROW
      L = IA + NROW2*(I-1)
      K = L  + NROW2 - 1
      SUMR = 0.
      SUMI = 0.
      DO 33 J = L,K,2
      JJ   = J + 1
      SUMR = SUMR + Z(J)**2 - Z(JJ)**2
   33 SUMI = SUMI + 2.*Z(J)*Z(JJ)
      RSQRT= SQRT(SQRT(SUMR**2 + SUMI**2))
      IF (RSQRT .GT. 0.) GO TO 34
      WRITE (NOUT,560) UWM,NAME
      GO TO 36
   34 THETA2= .5*ATAN2(SUMI,SUMR)
      SUMR  = RSQRT*COS(THETA2)
      SUMI  = RSQRT*SIN(THETA2)
      THETA2= 1./(SUMR**2 + SUMI**2)
      SUMR  = SUMR*THETA2
      SUMI  =-SUMI*THETA2
      DO 35 J = L,K,2
      JJ    = J + 1
      THETA2= Z(J)
      Z(J ) = SUMR*Z(J)   - SUMI*Z(JJ)
   35 Z(JJ) = SUMI*THETA2 + SUMR*Z(JJ)
C
C     -------------- SPECIAL PRINT --------------------------
C
      IF (.NOT.QPR) GO TO 36
      WRITE (NOUT,4424) (Z(J),J=L,K)
C
C     CHECK NORMALITY
C
      SUMR = 0.
      SUMI = 0.
      DO 1008 J = L,K,2
      JJ   = J + 1
      SUMR = SUMR + Z(J)**2 - Z(JJ)**2
 1008 SUMI = SUMI + 2.*Z(J)*Z(JJ)
      WRITE (NOUT,7770) SUMR,SUMI
C     -------------------------------------------------------
C
   36 CONTINUE
C
C     COMPUTE THEORETICAL EIGENVALUE ERRORS
C
      IF (QPR) WRITE (NOUT,650) DMP1
      IHL1 = IHL - 1
      DO 50 I = 1,NROW
      K = IL + 2*(I-1)
      DENOM = SQRT(Z(K)**2 + Z(K+1)**2)
      IF (DENOM .GT. 0.) GO TO 40
      WRITE (NOUT,550) UIM,I
      DENOM = 1.E-10
   40 DENOM = 1./DENOM
      K  = IA + NROW2*I - 2
      KK = K  + 1
      J  = IHL1 + I
      Z(J) = DENOM*SQRT((DMP1(1)*Z(K) - DMP1(2)*Z(KK))**2
     1     + (DMP1(1)*Z(KK) + DMP1(2)*Z(K))**2)
      IF (QPR) WRITE (NOUT,660) I,Z(J),Z(K),Z(KK),DENOM
   50 CONTINUE
C
C     RECOVER PHYSICAL EIGENVALUES
C
      RMS = 0.
      IF (NO B) GO TO 54
      ALAM(1) = LAMBDA(1)
      ALAM(2) = LAMBDA(2)
      GO TO 55
   54 ALAM(1) = LAMBDA(1)**2 - LAMBDA(2)**2
      ALAM(2) = 2.D0*LAMBDA(1)*LAMBDA(2)
   55 DO 70 I = 1,NROW
      K  = IL + 2*(I-1)
      KK = K  + 1
      DENOM = Z(K)**2 + Z(KK)**2
      IF (DENOM .EQ. 0.) DENOM = 1.E-20
      DENOM = 1./DENOM
      Z( K) = DENOM*Z( K) + ALAM(1)
      Z(KK) =-DENOM*Z(KK) + ALAM(2)
      IF (NO B) GO TO 60
      GO TO 70
C
C     DAMPING MATRIX ABSENT
C
   60 RSQRT  = SQRT(SQRT(Z(K)**2 + Z(KK)**2))
      THETA2 = .5*ATAN2(Z(KK),Z(K))
      Z( K)  = RSQRT*COS(THETA2)
      Z(KK)  = RSQRT*SIN(THETA2)
      IF (Z(KK) .GE. 0.) GO TO 70
      Z( K)  =-Z( K)
      Z(KK)  =-Z(KK)
C
C     COMPUTE RMS FOR RIGID-BODY ERROR TEST
C
   70 RMS = RMS + SQRT((Z(K)**2-Z(KK)**2)**2 + 4.*(Z(K)*Z(KK))**2)
      RMS = SQRT(RMS)/FLOAT(NROW)
      IF (QPR) WRITE (NOUT,800) RMS
      J  = IH - 1
      IF (QPR) WRITE (NOUT,4420) (Z(I),I=IL,J)
C
C     PERFORM RIGID-BODY ERROR TEST
C
      IF (RMS .LT. 1.E-20) RMS = 1.E-20
      RMS = 1./RMS
      DO 80 I = 1,NROW
      K = IL + 2*(I-1)
      J = IHL1 + I
      IF (RMS*SQRT(Z(K)**2+Z(K+1)**2) .LE. TENMTT) Z(J) = 0.
   80 CONTINUE
C
C     COMPUTE DISTANCES OF EIGENVALUES TO CENTER OF NEIGHBORHOOD
C
      ALAM(1) = LAMBDA(1)
      ALAM(2) = LAMBDA(2)
      JJ = INTQ - 1
      KK = IH   - 1
      LL = INTH - 1
      DO 90 I = 1,NROW
      J  = JJ + I
      K  = IL + 2*(I-1)
      Z(J) = SQRT((ALAM(1) - Z(K))**2 + (ALAM(2)-Z(K+1))**2)
C
C     LOAD ORDER OF EXTRACTION
C
      K = KK + I
      IZ(K) = I
C
C     LOAD STATUS OF EACH SOLUTION
C
      K = LL + I
      LZ(K) = .FALSE.
      J = IHL1 + I
      IF (Z(J) .LT. SNGL(EPS)) LZ(K) = .TRUE.
   90 CONTINUE
C
C     SORT EIGENVALUES ACCORDING TO DISTANCE FROM CURRENT CENTER
C
      IF (NROW .EQ. 1) GO TO 150
      LL = NROW - 1
      DO 140 I = 1,LL
      K  = JJ + I
      I1 = KK + I
      LLL= I  + 1
      DO 130 J = LLL,NROW
      L  = JJ + J
      IF (Z(K) .LT. Z(L)) GO TO 130
      UNIDUM = Z(L)
      Z(L)   = Z(K)
      Z(K)   = UNIDUM
      I2     = KK + J
      INIDUM = IZ(I1)
      IZ(I1) = IZ(I2)
      IZ(I2) = INIDUM
  130 CONTINUE
  140 CONTINUE
  150 LLL = IL - 1
      LL  = INTH - 1
      IF (IDIAG .EQ. 0) GO TO 170
C
C     PRINT OUT FULL SUMMARY FOR CURRENT NEIGHBORHOOD
C
      WRITE (NOUT,670) JREG,NOREG,ALAM
      WRITE (NOUT,680)
      WRITE (NOUT,690)
      DO 160 I = 1,NROW
      K   = KK  + I
      IZZ = 2*IZ(K) - 1
      J   = JJ  + I
      L   = LLL + IZZ
      L1  = L   + 1
      I1  = IHL1+ IZ(K)
      Z(I1) = 100.*Z(I1)
      I2  = LL  + IZ(K)
      STATUS(1) = ACCEPT(1)
      STATUS(2) = ACCEPT(2)
      IF (LZ(I2)) GO TO 160
      STATUS(1) = REJECT(1)
      STATUS(2) = REJECT(2)
  160 WRITE (NOUT,700) I,IZ(K),Z(J),Z(L),Z(L1),Z(I1),STATUS
C
C     DECREMENT COUNTERS SO THAT ONLY ACCEPTABLE SOLUTIONS ARE RETAINED
C
  170 MSAVE = NROW
      DO 180 I = 1,MSAVE
      I2 = LL + I
      IF (LZ(I2)) GO TO 180
      NROW   = NROW - 1
      NORTHO = NORTHO - 1
      IF (NROW .EQ. 0) GO TO 450
  180 CONTINUE
      NFOUND = NZERO + NROW
      IF (NROW .EQ. MSAVE) WRITE (NOUT,720) UIM,MSAVE
      IF (IDIAG.EQ.0 .OR. NROW.EQ.MSAVE) GO TO 200
C
C     PRINT OUT SUMMARY WITH REJECTED SOLUTIONS DELETED
C
      WRITE (NOUT,670) JREG,NOREG,ALAM
      WRITE (NOUT,730)
      WRITE (NOUT,690)
      M  = 0
      DO 190 I = 1,MSAVE
      K  = KK + I
      I2 = LL + IZ(K)
      IF (.NOT.LZ(I2)) GO TO 190
      M  = M + 1
      IZZ= 2*IZ(K) - 1
      J  = JJ  + I
      L  = LLL + IZZ
      L1 = L   + 1
      I1 = IHL1+ IZ(K)
      WRITE (NOUT,700) M,IZ(K),Z(J),Z(L),Z(L1),Z(I1),ACCEPT
  190 CONTINUE
  200 M = MSAVE - NROW
      IF (M .GT. 0) WRITE (NOUT,740) UIM,NROW,M
C
C     WRITE EIGENVALUES TO OUTPUT FILE
C
      CALL GOPEN (ILAM(1),Z(IBUF1),WRT)
      DO 210 I = 1,MSAVE
      K  = KK + I
      I2 = LL + IZ(K)
      IF (.NOT.LZ(I2)) GO TO 210
      IZZ = 2*IZ(K) - 1
      L   = LLL + IZZ
      LAM1(1) = DBLE(Z(L  ))
      LAM1(2) = DBLE(Z(L+1))
      CALL WRITE (ILAM(1),LAM1(1),4,1)
  210 CONTINUE
      CALL CLOSE (ILAM(1),EOFNRW)
      IF (JREG.LT.NOREG .AND. NFOUND.LT.NORD) GO TO 214
      IF (NZERO .EQ. 0) GO TO 214
C
C     IF THIS IS THE FINAL (BUT NOT THE FIRST) NEIGHBORHOOD, THEN
C     RE-WRITE THE EIGENVECTOR FILE PERTAINING TO ALL PRIOR
C     NEIGHBORHOODS (ELIMINATE LEFT-HAND VECTORS)
C
  209 IF (IDIAG .NE. 0) WRITE (NOUT,810) NZERO,NORTHO
      INIDUM = ISCR(10)
      CALL OPEN  (*455,ISCR(10),Z(IBUF2),WRTREW)
      CALL CLOSE (ISCR(10),REW)
      J = NORD2
      IF (NO B) J = 2*J
      INIDUM = IPHI(1)
      CALL OPEN (*455,IPHI(1),Z(IBUF1),0)
      DO 212 I = 1,NZERO
      CALL READ (*460,*211,IPHI(1),Z(IV2),NORD8+10,0,N3)
      GO TO 470
  211 CALL GOPEN (ISCR(10),Z(IBUF2),WRT)
      CALL WRITE (ISCR(10),Z(IV2),J,1)
  212 CALL CLOSE (ISCR(10),NOREW)
      CALL CLOSE (IPHI(1),NOREW)
      CALL OPEN  (*455,IPHI(1),Z(IBUF1),WRTREW)
      CALL CLOSE (IPHI(1),REW)
      INIDUM = ISCR(10)
      CALL OPEN  (*455,ISCR(10),Z(IBUF2),0)
      DO 213 I = 1,NZERO
      CALL READ  (*460,*206,ISCR(10),Z(IV2),J+10,0,N3)
      GO TO 470
  206 CALL GOPEN (IPHI(1),Z(IBUF1),WRT)
      CALL WRITE (IPHI(1),Z(IV2),J,1)
  213 CALL CLOSE (IPHI(1),EOFNRW)
      CALL CLOSE (ISCR(10),NOREW)
      IF(NSWP(2) .LT. 0) GO TO 500
C
C     RECOVER PHYSICAL EIGENVECTORS, PRINT, AND WRITE TO OUTPUT FILE
C
  214 IPRC = IPREC + 2
      II   = 1
      NN   = NORD2
      INCR = 1
      IA1  = IA - 1
      IF (QPR) WRITE (NOUT,750)
      ISHFT = NORD2*IPREC
      I1  = 0
C
C     ENTER LOOP
C
      DO 300 I = 1,MSAVE
      K  = KK + I
      I2 = LL + IZ(K)
      IF (.NOT. LZ(I2)) GO TO 300
      CALL GOPEN (ISCR(7),Z(IBUF2),RDREW)
      IF (NZERO .GT. 0) CALL SKPREC (ISCR(7),NZERO)
      DO 215 J = 1,NORD8
      M = IV1X + J
  215 Z(M) = 0.
C
C     SET POINTER TO ALLMAT OUTPUT VECTOR
C
      IB = IA1 + 2*MSAVE*(IZ(K)-1)
C
C     CYCLE THRU ALL ORTHOGONAL VECTORS
C
      DO 225 J = 1,MSAVE
C
C     NOTE.... Z(IV2) MAY BE LOADED DOUBLE-PRECISION....HIGHER DIGITS
C              ARE NOT USED
C     (HIGHER DIGITS MUST BE INCLUDED FOR THE D.P.MACHINES.  G.C/UNISYS)
C
      CALL UNPACK (*225,ISCR(7),Z(IV2))
      KR = IB + 2*J - 1
      KI = KR + 1
      DO 220 MM = 1,NORD2,2
      MR = IV2 + (MM-1)*IPREC
      MI = MR  + IPREC
      JR = IV1X+ MM
      JI = JR  + 1
      IF (.NOT.DPMACH) GO TO 216
      MRD = (MR+1)/2
      MID = MRD + 1
C
C     RECOVER RIGHT-HAND PHYSICAL EIGENVECTOR
C
      Z(JR) = Z(JR) + DZ(MRD)*Z(KR) - DZ(MID)*Z(KI)
      Z(JI) = Z(JI) + DZ(MID)*Z(KR) + DZ(MRD)*Z(KI)
      GO TO 217
  216 Z(JR) = Z(JR) + Z(MR)*Z(KR) - Z(MI)*Z(KI)
      Z(JI) = Z(JI) + Z(MI)*Z(KR) + Z(MR)*Z(KI)
  217 MR = MR + ISHFT
      MI = MR + IPREC
      JR = JR + NORD4
      JI = JR + 1
      IF (.NOT.DPMACH) GO TO 218
      MRD = (MR+1)/2
      MID = MRD + 1
C
C     RECOVER LEFT-HAND PHYSICAL EIGENVECTOR
C
      Z(JR) = Z(JR) + DZ(MRD)*Z(KR) - DZ(MID)*Z(KI)
      Z(JI) = Z(JI) + DZ(MID)*Z(KR) + DZ(MRD)*Z(KI)
      GO TO 220
  218 Z(JR) = Z(JR) + Z(MR)*Z(KR) - Z(MI)*Z(KI)
      Z(JI) = Z(JI) + Z(MI)*Z(KR) + Z(MR)*Z(KI)
  220 CONTINUE
  225 CONTINUE
      CALL CLOSE (ISCR(7),EOFNRW)
      IF (.NOT.QPR) GO TO 230
      I1  = I1 + 1
      IZZ = 2*IZ(K) - 1
      L   = LLL  + IZZ
      MM  = IV1X + NORD8
      WRITE (NOUT,760) I1,IZ(K),Z(L),Z(L+1),(Z(J),J=IV1,MM)
      WRITE (NOUT,770)
C
C     EXPAND PHYSICAL EIGENVECTORS TO DOUBLE PRECISION FOR OUTPUT
C
  230 LIM1 = IV1  + NORD2
      LIM2 = LIM1 + NORD4
      INIDUM = IV1X + NORD4
      DO 240 J = 1,NORD2
      KI  = LIM1 - J
      MI  = 2*KI - IV1X
      MR  = MI - 1
      MRD = (MR+1)/2
C
C     EXPAND RIGHT-HAND VECTOR
C
      Z(MI) = 0.
      Z(MR) = Z(KI)
      IF (DPMACH) DZ(MRD) = Z(KI)
      KI  = LIM2 - J
      MI  = 2*KI - INIDUM
      MR  = MI - 1
      MRD = (MR+1)/2
C
C     EXPAND LEFT -HAND VECTOR
C
      Z(MI) = 0.
      Z(MR) = Z(KI)
      IF (DPMACH) DZ(MRD) = Z(KI)
  240 CONTINUE
      IF (.NOT.QPR) GO TO 250
      WRITE (NOUT,770)
      LIM1 = IV1X + NORD4
      WRITE (NOUT,780) (Z(J),J=IV1,LIM1)
      WRITE (NOUT,770)
      LIM2 = LIM1 + NORD4
      LIM1 = LIM1 + 1
      WRITE (NOUT,780) (Z(J),J=LIM1,LIM2)
      WRITE (NOUT,770)
C
C     PERFORM SPECIAL NORMALIZATION OF VECTORS FOR OUTPUT
C
  250 CALL CNORM1 (Z(IV1),IKMB(2))
      IF (QPR) WRITE (NOUT,790)
      INIDUM = INIDUM + 1
      CALL CNORM1 (Z(INIDUM),IKMB(2))
      IF (QPR) WRITE (NOUT,790)
      CALL GOPEN (IPHI(1),Z(IBUF1),WRT)
      IF (JREG.LT.NOREG .AND. NFOUND.LT.NORD) GO TO 260
      J = NORD2
      IF (NO B) J = 2*J
      CALL WRITE (IPHI(1),Z(IV1),J,1)
      CALL CLOSE (IPHI(1),EOFNRW)
      GO TO 300
C
C     MUST USE NORD8 TO WRITE FULL RIGHT AND LEFT EIGENVECTORS
C
  260 CALL WRITE (IPHI(1),Z(IV1),NORD8,1)
      CALL CLOSE (IPHI(1),NOREW)
  300 CONTINUE
      GO TO 500
  420 WRITE (NOUT,530) NAME
      GO TO 500
  430 WRITE (NOUT,540) M,NAME
      GO TO 500
  440 WRITE (NOUT,580) UWM
      IF(NZERO.GT.0 .AND. JREG.EQ.NOREG) NSWP(2) = -1
      GO TO 500
  450 WRITE (NOUT,710) UWM,MSAVE
      GO TO 500
  455 CALL MESAGE (-1,INIDUM,NAME)
  460 CALL MESAGE (-2,INIDUM,NAME)
  470 CALL MESAGE (-8,INIDUM,NAME)
  500 RETURN
C
  530 FORMAT (27H UNEXPECTED EOF ENCOUNTERED,2X,2A4)
  540 FORMAT (22H UNEXPECTED WORD COUNT,I5,2X,2A4)
  550 FORMAT (A29,' 3152', //5X,'SUBROUTINE ALLMAT OUTPUT EIGENVALUE',
     1       I4,' IS NULL.',//)
  560 FORMAT (A25,' 3153', //5X,'ATTEMPT TO NORMALIZE NULL VECTOR IN ',
     1       'SUBROUTINE ',A4,A2,'. NO ACTION TAKEN.',//)
  570 FORMAT (A25,' 3154', //5X,'SIZE OF REDUCED PROBLEM DECREMENTED ',
     1       'ONCE (NOW',I6,') DUE TO NULL ERROR ELEMENT.',//)
  580 FORMAT (A25,' 3155', //5X,'REDUCED PROBLEM HAS VANISHED. NO ',
     1       'ROOTS FOUND.',//)
  590 FORMAT (A25,' 3156', //5X,'SIZE OF REDUCED PROBLEM RESTORED TO',
     1       I8,' BECAUSE NEXT ERROR ELEMENT WAS ALSO NULL.', /5X,
     3       'ERROR ELEMENT SET = ',2E16.8,//)
  600 FORMAT (1H0,//7H CFEER4,//)
  610 FORMAT (1H ,I10,36H SINGLE PRECISION WORDS OF OPEN CORE,
     1       29H NOT USED (SUBROUTINE CFEER4))
  620 FORMAT (4H ROW,I5,2(4X,2E16.8))
  630 FORMAT (4H ROW,I5,2(4X,2D16.8))
  640 FORMAT (1H0,26HREDUCED TRIDIAGONAL MATRIX, /(1H ,6E16.8))
  650 FORMAT (1H0,//30H THEORETICAL EIGENVALUE ERRORS,
     1       20X,18HD-SUB-M-PLUS-ONE =,2E16.8,/)
  660 FORMAT (1H ,I5,E16.8,20X,2E16.8,10X,E16.8)
  670 FORMAT (1H1,27X,39H*****  F E E R  *****  (FAST EIGENVALUE,
     1       27H EXTRACTION ROUTINE)  *****, //4X,
     2       24HSUMMARY FOR NEIGHBORHOOD,I3,3H OF,I3,1H.,10X,
     3       21HNEIGHBORHOOD CENTER =,2E16.8,/)
  680 FORMAT (4X,43HALL SOLUTIONS FOUND IN CURRENT NEIGHBORHOOD,
     1       12H ARE LISTED.,/)
  690 FORMAT (4X,7X,8HSOLUTION,7X,8HORDER OF,7X,8HDISTANCE,
     1       10X,10HEIGENVALUE,14X,11HTHEORETICAL, /4X,
     2       9X,6HNUMBER,5X,10HEXTRACTION,4X,11HFROM CENTER,
     3       6X,4HREAL,9X,9HIMAGINARY,9X,5HERROR,12X,6HSTATUS,/)
  700 FORMAT (4X,I12,I15,1P,E18.8,1P,3E15.7,7X,2A4)
  710 FORMAT (A25,' 3163', //5X,'ALL',I6,' SOLUTIONS HAVE FAILED ',
     1       'ACCURACY TEST. NO ROOTS FOUND.',//)
  720 FORMAT (A29,' 3164',//5X,'ALL',I6,' SOLUTIONS ARE ACCEPTABLE.',//)
  730 FORMAT (4X,37HREJECTED SOLUTIONS HAVE BEEN DELETED.,/)
  740 FORMAT (A29,' 3165', //4X,I6,' SOLUTIONS HAVE BEEN ACCEPTED AND',
     1       I4,' SOLUTIONS HAVE BEEN REJECTED.',//)
  750 FORMAT (1H1,27X,39H*****  F E E R  *****  (FAST EIGENVALUE,
     1       27H EXTRACTION ROUTINE)  *****,//
     2       42X,37HE I G E N V E C T O R   S U M M A R Y,//1H ,
     3       32(4H----),2H--)
  760 FORMAT (1H ,8HSOLUTION,I4,8X,16HEXTRACTION ORDER,I4,
     1       10X,10HEIGENVALUE,2X,1P,2E16.8, /(1H ,3(4X,1P,2E16.8)))
  770 FORMAT (3H --,32(4H----))
  780 FORMAT ((1H ,3(3X,2E16.8)))
  790 FORMAT (1H  ,12HAFTER CNORM1)
  800 FORMAT (1H  ,10X,5HRMS =,E16.8)
  810 FORMAT (1H  ,33HLEFT-HAND EIGENVECTORS ELIMINATED,20X,2I8)
      END
