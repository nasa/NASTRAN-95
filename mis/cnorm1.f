      SUBROUTINE CNORM1 (X,N)
C
C     CNORM1 WILL SEARCH A VECTOR FOR THE LARGEST VALUE AND NORMALIZE
C     THE VECTOR TO LARGEST ELEMENT EQUAL TO ONE
C
      INTEGER          NAME(2)
      DOUBLE PRECISION X(1),DUM,MAX,DIV(2)
      COMMON /SYSTEM/  IBUF,NOUT
      DATA    NAME  /  4HCNOR,4HM1   /
C
      NN   = N + N
      MAX  = 0.D0
      INDEX= 0
      DO 10 I = 1,NN,2
      DUM = X(I)*X(I) + X(I+1)*X(I+1)
      IF (DUM .LE. MAX) GO TO 10
      MAX   = DUM
      INDEX = I
   10 CONTINUE
      IF (INDEX .EQ. 0) GO TO 30
      DIV(1) = X(INDEX  )
      DIV(2) = X(INDEX+1)
      MAX    = DIV(1)*DIV(1) + DIV(2)*DIV(2)
      DO 20 I = 1,NN,2
      DUM    = (X(I)*DIV(1) + X(I+1)*DIV(2))/MAX
      X(I+1) = (X(I+1)*DIV(1) - X(I)*DIV(2))/MAX
   20 X(I)   = DUM
      RETURN
C
   30 WRITE  (NOUT,40)
   40 FORMAT (//5X,37HCNORM1 RECEIVED A VECTOR OF ALL ZEROS)
      CALL MESAGE (-37,0,NAME)
      RETURN
      END
