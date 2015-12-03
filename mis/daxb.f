      SUBROUTINE DAXB(A,B,C)
      DOUBLE PRECISION A(3), B(3), C(3), D(3)
C*****
C  DOUBLE PRECISION VERSION
C
C  THIS ROUTINE PERFORMS A X B INTO C  (C MAY OVERLAP A OR B IN CORE)
C*****
      D(1) = A(2)*B(3) - A(3)*B(2)
      D(2) = A(3)*B(1) - A(1)*B(3)
      D(3) = A(1)*B(2) - A(2)*B(1)
      C(1) = D(1)
      C(2) = D(2)
      C(3) = D(3)
      RETURN
      END
