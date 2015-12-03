      FUNCTION SADOTB( A, B )
      REAL A(3), B(3)
C*****
C  SINGLE-PRECISION VERSION
C
C  DOT PRODUCT A . B
C*****
      SADOTB = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
      RETURN
      END
