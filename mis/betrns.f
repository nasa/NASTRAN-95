      SUBROUTINE BETRNS (TBE,GG,KFLAG,ELID)
C     &    ENTRY BETRND (TBD,GD,KFLAG,ELID)
C
C*****
C     SUBROUTINE WHICH CALCULATES THE TBE TRANSFORMATION
C     MATRIX WHICH RELATES THE ELEMENT TO THE BASIC C.S.
C
C     GG(9) OR GD(9) IS A 9X1 ARRAY WHICH STORES THE GRID PT. COORD.
C     X(G1),Y(G1),Z(G1),X(G2),Y(G2),Z(G2),X(G3),Y(G3),Z(G3)
C     GG(1),GG(2),GG(3),GG(4),GG(5),GG(6),GG(7),GG(8),GG(9), OR
C     GD(1),GD(2),GD(3),GD(4),GD(5),GD(6),GD(7),GD(8),GD(9)
C
C     KFLAG = 0, TBE (OR TBD) IS OUTPUT WITHOUT TRANSPOSING
C           = 1, TBE (OR TBD) IS OUTPUT AFTER IT IS TRANSPOSED
C*****
C
      INTEGER          ELID
      REAL             GG(9),TBE(9),RSSTR(3),RSTR(3),R12(3),RV(3),LEN
      DOUBLE PRECISION GD(9),TBD(9),DSSTR(3),DSTR(3),D12(3),DV(3),
     1                 DTEMP,LED
C
C     SINGLE PRECISION VERSION
C
C*****
C     CALCULATE APPROPRIATE LENGTH QUANTITIES
C*****
      LEN = SQRT((GG(4)-GG(1))**2 + (GG(5)-GG(2))**2
     1          +(GG(6)-GG(3))**2)
      IF (LEN .EQ. 0.0) GO TO 40
C*****
C     CALCULATE APPROPRIATE VECTOR QUANTITIES
C*****
      R12(1) = (GG(4)-GG(1))/LEN
      R12(2) = (GG(5)-GG(2))/LEN
      R12(3) = (GG(6)-GG(3))/LEN
      RV(1)  = (GG(7)-GG(1))
      RV(2)  = (GG(8)-GG(2))
      RV(3)  = (GG(9)-GG(3))
C*****
C     CALCULATE ENTRIES INTO THE TRANSFORMATION MATRIX
C*****
      RSTR(1) = (R12(2)*RV(3) - R12(3)*RV(2))
      RSTR(2) = (R12(3)*RV(1) - R12(1)*RV(3))
      RSTR(3) = (R12(1)*RV(2) - R12(2)*RV(1))
C
      LEN = SQRT (RSTR(1)**2 + RSTR(2)**2 + RSTR(3)**2)
      IF (LEN .EQ. 0.0) GO TO 40
      DO 10 I=1,3
   10 RSTR(I) = RSTR(I)/LEN
C
      RSSTR(1)= (RSTR(2)*R12(3) - RSTR(3)*R12(2))
      RSSTR(2)= (RSTR(3)*R12(1) - RSTR(1)*R12(3))
      RSSTR(3)= (RSTR(1)*R12(2) - RSTR(2)*R12(1))
      TBE(1)  = R12(1)
      TBE(2)  = R12(2)
      TBE(3)  = R12(3)
      TBE(4)  = RSSTR(1)
      TBE(5)  = RSSTR(2)
      TBE(6)  = RSSTR(3)
      TBE(7)  = RSTR(1)
      TBE(8)  = RSTR(2)
      TBE(9)  = RSTR(3)
      IF (KFLAG .EQ. 0) GO TO 30
C*****
C     TRANSPOSE TBE(9) SINCE KFLAG.NE.ZERO
C*****
      TEMP   = TBE(2)
      TBE(2) = TBE(4)
      TBE(4) = TEMP
      TEMP   = TBE(3)
      TBE(3) = TBE(7)
      TBE(7) = TEMP
      TEMP   = TBE(6)
      TBE(6) = TBE(8)
      TBE(8) = TEMP
      GO TO 30
C
      ENTRY BETRND (TBD,GD,KFLAG,ELID)
C     ================================
C
C     DOUBLE PRECISION VERSION
C
C*****
C     CALCULATE APPROPRIATE LENGTH QUANTITIES
C*****
      LED = DSQRT((GD(4)-GD(1))**2 + (GD(5)-GD(2))**2
     1           +(GD(6)-GD(3))**2)
      IF (LED .EQ. 0.0D+0) GO TO 40
      D12(1) = (GD(4)-GD(1))/LED
      D12(2) = (GD(5)-GD(2))/LED
      D12(3) = (GD(6)-GD(3))/LED
      DV(1)  = (GD(7)-GD(1))
      DV(2)  = (GD(8)-GD(2))
      DV(3)  = (GD(9)-GD(3))
C*****
C     CALCULATE ENTRIES INTO THE TRANSFORMATION MATRIX
C*****
      DSTR(1) = (D12(2)*DV(3) - D12(3)*DV(2))
      DSTR(2) = (D12(3)*DV(1) - D12(1)*DV(3))
      DSTR(3) = (D12(1)*DV(2) - D12(2)*DV(1))
C
      LED = DSQRT(DSTR(1)**2 + DSTR(2)**2 + DSTR(3)**2)
      IF (LED .EQ. 0.0D+0) GO TO 40
      DO 20 I=1,3
   20 DSTR(I) = DSTR(I)/LED
C
      DSSTR(1)= (DSTR(2)*D12(3) - DSTR(3)*D12(2))
      DSSTR(2)= (DSTR(3)*D12(1) - DSTR(1)*D12(3))
      DSSTR(3)= (DSTR(1)*D12(2) - DSTR(2)*D12(1))
      TBD(1)  = D12(1)
      TBD(2)  = D12(2)
      TBD(3)  = D12(3)
      TBD(4)  = DSSTR(1)
      TBD(5)  = DSSTR(2)
      TBD(6)  = DSSTR(3)
      TBD(7)  = DSTR(1)
      TBD(8)  = DSTR(2)
      TBD(9)  = DSTR(3)
      IF (KFLAG .EQ. 0) GO TO 30
C*****
C     TRANSPOSE TBD(9) SINCE KFLAG.NE.ZERO
C*****
      DTEMP  = TBD(2)
      TBD(2) = TBD(4)
      TBD(4) = DTEMP
      DTEMP  = TBD(3)
      TBD(3) = TBD(7)
      TBD(7) = DTEMP
      DTEMP  = TBD(6)
      TBD(6) = TBD(8)
      TBD(8) = DTEMP
   30 RETURN
C*****
C     ZERO LENGTH ERROR, BAD GEOMETRY
C*****
   40 CALL MESAGE (-30,31,ELID)
      RETURN
      END
