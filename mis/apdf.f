      FUNCTION APDF (F,IN,NS)
C
      REAL F(1)
C
C     IF (NS .EQ. 0) GO TO 10
C     APDF = FLOAT(IN-1)/FLOAT(NS)
C     RETURN
C  10 APDF = F(IN)
C     RETURN
C
      APDF = F(IN)
      IF (NS .NE. 0) APDF = FLOAT(IN-1)/FLOAT(NS)
      RETURN
      END
