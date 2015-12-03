      SUBROUTINE IFTE4(THA,RP,CP)
      DATA THAO,EPSI /.1,1.E-9 /
      IF(ABS(THA) .LT. THAO) GO TO 100
      D = THA**4/24.
      RP = ((.5*(THA*THA))-1.+ COS(THA))/D
      CP = ((THA**3/6.)-THA+SIN(THA))/D
      RETURN
C
C  EVALUATE SERIES
C
  100 CONTINUE
      RN = 1.0
      D = 1.0
      SIGN = -1.
      RPS = 1.
      TSQ = THA*THA
      T1 = 5.
      T2 = 6.
      IT = 1
  101 CONTINUE
      DO 110 I=1,50
      RN = RN*TSQ
      D = D*T1*T2
      TRM = RN/D*SIGN
      RPS = RPS + TRM
      IF(ABS(TRM) .LT. EPSI) GO TO 120
      SIGN = -SIGN
      T1 = T1+2.
      T2 = T2+2.
  110 CONTINUE
  120 CONTINUE
      IF(IT .EQ. 2) GO TO 125
      RP = RPS
      RN = THA
      D = 5.0
      SIGN = -1.
      RPS = THA/5.
      T1 = 6.
      T2 = 7.
      IT = 2
      GO TO 101
  125 CONTINUE
      CP = RPS
      RETURN
      END
