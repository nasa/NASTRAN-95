      SUBROUTINE SSG3A (A,LLL,B,X,SR1,SR2,ITR1,RES)
C
C     SSG3A SOLVES AX = B USING A = L*LT
C
C     ON OPTION COMPUTES RESIDUAL VECTOR RES = A*X - B
C     AND EPSI= X(T)*RES/B(T)*X
C
      INTEGER          A,        B,        X,        SR1,
     1                 FILL,     FILLT,    FILB,     SR2,
     2                 FILX,     PREC,     RES,      SYSBUF,
     3                 NAME(2)
      DOUBLE PRECISION DCORE(1), DNUM,     DNOM,     DX
      COMMON /BLANK /  N,        IRES,     NSKIP,    IEPSI
      COMMON /FBSX  /  FILL(7),  FILLT(7), FILB(7),  FILX(7),
     1                 NZ,       PREC,     ISIGN
      COMMON /ZZZZZZ/  CORE(1)
      COMMON /SYSTEM/  KSYSTM(55)
      COMMON /UNPAKX/  ITB,      I,        J,        INCUR
      COMMON /ZNTPKX/  DX(2),    IK,       IEOL,     IEOR
      EQUIVALENCE      (CORE(1),DCORE(1)), (KSYSTM(1),SYSBUF),
     1                 (KSYSTM(55),IPREC)
      DATA    NAME  /  4HSSG3,   4HA       /
C
      FILL(1) = LLL
      CALL RDTRL (FILL)
      IF (FILL(1) .LE. 0) CALL MESAGE (-1,LLL,NAME)
      FILB(1) = B
      CALL RDTRL (FILB)
      NLOAD = FILB(2)
      NLEN  = FILB(3)
      ISIGN = 1
      PREC  = 2
      NZ    = KORSZ(CORE)
      DO 10 I = 2,7
   10 FILX(I) = FILB(I)
      FILX(1) = X
C
C     SAVE DISPLACEMENT VECTOR IN DOUBLE PRECISION
C
      FILX(5) = 1
      IF (FILB(5) .GT. 2) FILX(5) = 3
      FILX(5) = FILX(5) + IPREC - 1
      CALL FBS (CORE,CORE)
      CALL WRTTRL (FILX)
      IF (ITR1 .LT. 0) GO TO 130
      FILL(1) = RES
      CALL RDTRL (FILL)
      IF (FILL(1) .LE. 0) GO TO 130
C
C     COMPUTE RESIDUAL VECTOR
C
      CALL SSG2B (A,X,B,RES,0,2,-2,SR1)
C
C     COMPUTE EPSI
C
      NZ = NZ - SYSBUF
      CALL GOPEN (X,CORE(NZ+1),0)
      NZ = NZ - SYSBUF
      CALL GOPEN (RES,CORE(NZ+1),0)
      NZ = NZ - SYSBUF
      CALL GOPEN (B,CORE(NZ+1),0)
      IF (NZ .LT. 2*NLEN) GO TO 180
      ITB = 2
      INCUR = 1
      I = 1
      J = NLEN
      DO 120 L = 1,NLOAD
      CALL UNPACK (*80,X,CORE)
      DNUM = 0.0D0
      DNOM = 0.0D0
      CALL INTPK (*90,RES,0,2,0)
   20 IF (IEOL) 40,30,40
   30 CALL ZNTPKI
      DNUM = DNUM + DX(1)*DCORE(IK)
      GO TO 20
   40 CALL INTPK (*100,B,0,2,0)
   50 IF (IEOL) 70,60,70
   60 CALL ZNTPKI
      DNOM = DNOM + DX(1)*DCORE(IK)
      GO TO 50
   70 EPSI = DNUM/DNOM
      GO TO 110
   80 CALL FWDREC (*160,RES)
   90 CALL FWDREC (*170,B)
  100 EPSI = 0.0
  110 CALL MESAGE (35,NSKIP+L-1,EPSI)
      IF (ABS(EPSI) .LT. 1.0E-3) GO TO 120
      IEPSI = -1
      CALL MESAGE (58,1.0E-3,NSKIP+L-1)
  120 CONTINUE
      CALL CLOSE (X,1)
      CALL CLOSE (RES,1)
      CALL CLOSE (B,1)
  130 RETURN
C
  150 CALL MESAGE (-1,IPM,NAME)
  160 IPM = RES
      GO TO 150
  170 IPM = B
      GO TO 150
  180 CALL MESAGE (-8,0,NAME)
      RETURN
C
      END
