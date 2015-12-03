      SUBROUTINE CNORM(X,DIV,Y)
C
C     CNORM WILL NORMALIZE X TO THE MAXIMUM ELEMENT EQUAL TO A MODULUS
C     OF ONE AND STORE THE DIVISOR IN MAX (X MAY BE COMPLEX)
C
      DOUBLE PRECISION X(1),DIV(2),MAX,TEMP,Y(1),SIGN,COSANG,XO,D,R,RI
      COMMON /SYSTEM/  IBUF,NOUT
      COMMON /CINVPX/  FILEK(7)
      COMMON /CINVXX/  DUM(30),IND1,ITER
      EQUIVALENCE      (NCOL,FILEK(2))
C
      NCOL2 = NCOL + NCOL
      MAX   = 0.D0
      SIGN  = 1.0D0
      IND   = 0
      DO 20 I = 1,NCOL2,2
      IF (X(I)**2+X(I+1)**2 .LE. MAX) GO TO 20
      MAX = X(I)**2 + X(I+1)**2
      IND = I
   20 CONTINUE
      IF (IND  .EQ.   0) GO TO 80
      IF (ITER .EQ.   1) GO TO 60
      IF (IND .EQ. IND1) GO TO 50
      CALL SSWTCH (12,IDIAG)
      IF (IDIAG .EQ.  0) GO TO 40
      WRITE  (6,30) IND,IND1
   30 FORMAT (10H CHANGE     ,2I5)
   40 CONTINUE
   50 CONTINUE
      D  = X(IND)**2 + X(IND+1)**2
      R  = (X(IND1)*X(IND) + X(IND1+1)*X(IND+1))/D
      RI = (X(IND1+1)*X(IND) - X(IND1)*X(IND+1))/D
      COSANG = XO*R/DSQRT(R**2 + RI**2)
      IF (DABS(COSANG+1.D0) .LE. 0.1D0) SIGN = -1.0D0
   60 I  = IND
      DIV(1) = X(I  )*SIGN
      DIV(2) = X(I+1)*SIGN
      IND1 = IND
      MAX  = 1.0D0/MAX
      DO 70 I = 1,NCOL2,2
      TEMP   = (X(I)*DIV(1)+X(I+1)*DIV(2))*MAX
      X(I+1) = (X(I+1)*DIV(1)-X(I)*DIV(2))*MAX
   70 X(I) = TEMP
      XO = X(IND)
      RETURN
C
   80 WRITE  (NOUT,90)
   90 FORMAT (//5X,37HCONOR  RECEIVED A VECTOR OF ALL ZEROS)
      CALL MESAGE (-37,0,0)
      RETURN
      END
