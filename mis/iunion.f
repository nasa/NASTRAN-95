      FUNCTION IUNION(I1,I2)
C
C     I1 AND I2 ARE .GT. 0 BUT .LE. 654321 AND CONSIST OF ANY UNIQUE
C     COMBINATION OF THE DIGITS 1 THRU 6
C
      INTEGER K(6,2) , KK(6) , R
C
C              DECODE I1 INTO K(*,1)
      I=1
      II=I1
      ASSIGN 10 TO R
      GO TO 100
C
C              DECODE I2 INTO K(*,2)
   10 I=2
      II=I2
      ASSIGN 20 TO R
      GO TO 100
C
C              FORM UNION OF K(*,1) AND K(*,2) IN KK(*)
   20 DO 30 I=1,6
      KK(I)=0
      IF(K(I,1).EQ.I .OR. K(I,2).EQ.I) KK(I)=I
   30 CONTINUE
C
C              PACK KK(*) INTO IUNION
      J=1
      L=0
      DO 40 I=1,6
      IF(KK(I).EQ.0) GO TO 40
      IF(L.GT.0) J=10*J
      L=L+J*I
   40 CONTINUE
C
      IUNION=L
C
      RETURN
C
C
  100 DO 110 J=1,6
  110 K(J,I)=0
      DO 120 J=1,6
      L=II-10*(II/10)
      II=(II-L)/10
      IF(L.EQ.0) GO TO 130
  120 K(L,I)=L
  130 GO TO R,(10,20)
C
      END
