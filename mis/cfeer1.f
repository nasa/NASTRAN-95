      SUBROUTINE CFEER1
C
C     CFEER1 INITIALIZES AND CALLS SUBROUTINE SADD FOR CFCNTL
C
      LOGICAL           NO B     ,QPR
      INTEGER           SCR1     ,SCR2     ,SCR11    ,SQR       ,
     1                  TYPOUT   ,IFILA(7) ,IFILB(7) ,IFILC(7)
      DOUBLE PRECISION  ALPHA(2) ,BETA(2)  ,LAMBDA   ,DZ(1)
      DIMENSION         SALPHA(4),SBETA(4)
      COMMON  /FEERAA/  IK(7)    ,IM(7)    ,IB(7)    ,DUM(15)   ,
     1                  SCR1     ,SCR2     ,SCR(8)   ,SCR11     ,
     2                  DUMAA(91),MCBLMB(7)
      COMMON  /FEERXC/  LAMBDA(2),DUMXC(12),NO B     ,DUMXC2(4) ,
     1                  QPR
      COMMON  /SADDX /  NOMAT    ,NZ       ,MCBS(67)
      COMMON  /NAMES /  DUMM(10) ,CDP      ,SQR
      COMMON  /ZZZZZZ/  Z(1)
      COMMON  /UNPAKX/  TYPOUT   ,IROW     ,NLAST    ,INCR
      COMMON  /SYSTEM/  KSYSTM(65)
C
      EQUIVALENCE      (MCBS( 1), IFILA(1)), (MCBS( 8), ITYPAL  ),
     1                 (MCBS(61), IFILC(1)), (MCBS(13), IFILB(1)),
     2                 (MCBS(20), ITYPBT  ), (MCBS(21), BETA(1) ),
     3                 (MCBS( 9), ALPHA(1)), (IPREC,  KSYSTM(55)),
     4                 (ALPHA(1),SALPHA(1)), (BETA(1),  SBETA(1)),
     5                 (Z(1)    , DZ(1)   ), (NOUT,   KSYSTM(2) )
C
C     FORM   -(B + LAMBDA*M)  ON SCR2
C
      ITYPE    = IPREC + 2
      NOMAT    = 2
      DO 10 I  = 1,7
      IFILA(I) = IM(I)
   10 IFILB(I) = IB(I)
      IF (IPREC .EQ. 2) GO TO 15
      SALPHA(1)=-SNGL(LAMBDA(1))
      SALPHA(2)=-SNGL(LAMBDA(2))
      SALPHA(3)= 0.
      SALPHA(4)= 0.
      SBETA(1) =-1.
      SBETA(2) = 0.
      SBETA(3) = 0.
      SBETA(4) = 0.
      GO TO 16
   15 ALPHA(1) =-LAMBDA(1)
      ALPHA(2) =-LAMBDA(2)
      BETA(1)  =-1.D0
      BETA(2)  =  0.D0
   16 ITYPAL   = ITYPE
      ITYPBT   = ITYPE
      NZ       = KORSZ(Z)
      IFILC(1) = SCR2
      IFILC(2) = IK(2)
      IFILC(3) = IK(3)
      IFILC(4) = 1
      IFILC(5) = ITYPE
      IF (NO B) GO TO 100
      CALL SADD (Z,Z)
C
C---------- SPECIAL PRINT ------------------------------
C
      IF (.NOT.QPR) GO TO 25
      WRITE  (NOUT,2)
    2 FORMAT (1H0,//7H CFEER1,//)
      TYPOUT= ITYPE
      IROW  = 1
      NLAST = IK(2)
      LIMIT = 2*NLAST
      INCR  = 1
      IBUF  = NZ - KSYSTM(1) - 2
      CALL GOPEN (IFILC(1),Z(IBUF),0)
      DO 20 I = 1,NLAST
      WRITE  (NOUT,1) I
    1 FORMAT (7H COLUMN,I4)
      CALL UNPACK (*20,IFILC(1),Z)
      IF (IPREC .EQ. 2) WRITE (NOUT,3) (DZ(J),J=1,LIMIT)
      IF (IPREC .NE. 2) WRITE (NOUT,5) ( Z(J),J=1,LIMIT)
   20 CONTINUE
      CALL CLOSE (IFILC(1),1)
    3 FORMAT (1H ,13(10H----------)/(1H ,4D25.16))
    5 FORMAT (1H ,13(10H----------)/(1H ,4E25.16))
   25 CONTINUE
C
C
C     FORM  (LAMBDA**2*M + LAMBDA*B + K)  ON SCR1
C
      DO 30 I  = 1,7
   30 IFILA(I) = IK(I)
      IFILB(1) = IFILC(1)
      IFILB(2) = IK(2)
      IFILB(3) = IK(3)
      IFILB(4) = SQR
      IFILB(5) = ITYPE
      IF (IPREC .EQ. 2) GO TO 35
      SALPHA(1) = 1.
      SALPHA(2) = 0.
      SALPHA(3) = 0.
      SALPHA(4) = 0.
      SBETA(1)  =-SNGL(LAMBDA(1))
      SBETA(2)  =-SNGL(LAMBDA(2))
      SBETA(3)  = 0.
      SBETA(4)  = 0.
      GO TO 50
   35 ALPHA(1) = 1.D0
      ALPHA(2) = 0.D0
      BETA(1)  =-LAMBDA(1)
      BETA(2)  =-LAMBDA(2)
   50 IFILC(1) = SCR1
      CALL SADD (Z,Z)
C
C---------- SPECIAL PRINT ------------------------------
C
      IF (.NOT.QPR) GO TO 75
      WRITE  (NOUT,4)
    4 FORMAT (1H ,13(10H----------),//,19H THE DYNAMIC MATRIX,//)
      CALL GOPEN (IFILC(1),Z(IBUF),0)
      DO 70 I = 1,NLAST
      WRITE (NOUT,1) I
      CALL UNPACK (*70,IFILC(1),Z)
      IF (IPREC .EQ. 2) WRITE (NOUT,3) (DZ(J),J=1,LIMIT)
      IF (IPREC .NE. 2) WRITE (NOUT,5) ( Z(J),J=1,LIMIT)
   70 CONTINUE
      CALL CLOSE (IFILC(1),1)
   75 CONTINUE
C
C-------------------------------------------------------
C     MCBLMB NOT USED WHEN DAMPING MATRIX ABSENT
C
      DO 40 I = 1,7
   40 MCBLMB(I) = IFILB(I)
      GO TO 200
C
C     DAMPING MATRIX ABSENT
C
  100 DO 110 I = 1,7
  110 IFILB(I) = IK(I)
      IF (IPREC .EQ. 2) GO TO 120
      SALPHA(1) = SNGL(LAMBDA(1)**2 - LAMBDA(2)**2)
      SALPHA(2) = 2.*SNGL(LAMBDA(1)*LAMBDA(2))
      SBETA(1)  = 1.
      GO TO 130
  120 ALPHA(1) = LAMBDA(1)**2 - LAMBDA(2)**2
      ALPHA(2) = 2.D0*LAMBDA(1)*LAMBDA(2)
      BETA(1)  = 1.D0
C
C----------- LOGIC FOR SPECIAL PRINT -------------------------
C
  130 IF (.NOT.QPR) GO TO 50
      TYPOUT= ITYPE
      IROW  = 1
      NLAST = IK(2)
      LIMIT = 2*NLAST
      INCR  = 1
      IBUF  = NZ - KSYSTM(1) - 2
C-------------------------------------------------------------
C
      GO TO 50
C
  200 RETURN
      END
