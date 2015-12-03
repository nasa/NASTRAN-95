      SUBROUTINE SOFCLS
C
C     WRITES OUT AT THE TERMINATION OF A MODULE ALL THE IN CORE BUFFERS
C     AND COMMON BLOCKS.
C
      LOGICAL         DITUP,MDIUP,NXTUP,OPNSOF,NXTRST
      INTEGER         BUF,A,B,FILNAM,FILSIZ,PSSWRD,DIT,DITPBN,DITLBN,
     1                MDI,MDIPBN,MDILBN
      COMMON /ZZZZZZ/ BUF(1)
      COMMON /SOF   / A(37)
      COMMON /SYS   / B(6)
      COMMON /ITEMDT/ NITEM,ITEM(7,1)
      COMMON /SYSTEM/ NBUFF
      COMMON /SOFCOM/ NFILES,FILNAM(10),FILSIZ(10),STATUS,PSSWRD(2),
     1                FIRST,OPNSOF
      EQUIVALENCE     (DIT   ,A(1) ),(DITPBN,A(2) ),(DITLBN,A(3) ),
     1                (MDI   ,A(15)),(MDIPBN,A(16)),(MDILBN,A(17)),
     2                (NXT   ,A(19)),(NXTPBN,A(20)),(NXTLBN,A(21)),
     3                (DITUP ,A(34)),(MDIUP ,A(35)),(NXTUP ,A(36)),
     4                (NXTRST,A(37))
      DATA    IWRT  / 2 /
C
      IF (.NOT.OPNSOF) RETURN
      IF (DITPBN .EQ. 0) GO TO 20
      IF (.NOT.DITUP) GO TO 20
      CALL SOFIO (IWRT,DITPBN,BUF(DIT-2))
      DITUP = .FALSE.
      GO TO 40
   20 IF (NXTPBN .EQ. 0) GO TO 40
      IF (.NOT.NXTUP) GO TO 40
      CALL SOFIO (IWRT,NXTPBN,BUF(NXT-2))
      NXTUP = .FALSE.
   40 IF (MDIPBN .EQ. 0) GO TO 60
      IF (.NOT.MDIUP) GO TO 60
      CALL SOFIO (IWRT,MDIPBN,BUF(MDI-2))
      MDIUP = .FALSE.
C
C     WRITE OUT COMMON BLOCKS.
C
   60 LAST = NBUFF - 4
      DO 62 I = 1,LAST
   62 BUF(DIT+I) = 0
      BUF(DIT+1) = PSSWRD(1)
      BUF(DIT+2) = PSSWRD(2)
      BUF(DIT+4) = NFILES
      DO 70 I = 1,NFILES
      BUF(DIT+ 4+I) = FILNAM(I)
      BUF(DIT+14+I) = FILSIZ(I)
      BUF(DIT+33+I) = A(22+I)
   70 CONTINUE
      DO 80 I = 1,4
      BUF(DIT+24+I) = B(I)
   80 CONTINUE
      BUF(DIT+29) = A(4)
      BUF(DIT+30) = A(5)
      BUF(DIT+31) = A(6)
      BUF(DIT+32) = A(18)
      BUF(DIT+33) = A(22)
      BUF(DIT+44) = A(33)
      NXTRST      = .FALSE.
      BUF(DIT+45) = A(37)
      BUF(DIT+46) = B(5)
      BUF(DIT+47) = B(6)
C
      BUF(DIT+100) = NITEM
      K = 100
      DO 92 I = 1,NITEM
      DO 90 J = 1,7
   90 BUF(DIT+K+J) = ITEM(J,I)
   92 K = K + 7
      IBL = 1
      DO 100 I = 1,NFILES
      BUF(DIT+3) = I
      CALL SOFIO (IWRT,IBL,BUF(DIT-2))
      IBL = IBL + FILSIZ(I)
  100 CONTINUE
      CALL SOFIO (7, 0, 0)
      OPNSOF = .FALSE.
      RETURN
      END
