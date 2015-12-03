      SUBROUTINE GIGTKG
C
      EXTERNAL        WRITE
      INTEGER         GSIZE,SCR2,SCR3,TRL(7),IZ(1),SYSBUF,OUT,BUF1,
     1                BUF2,NAM(2),SDTAB(6,5),CTYPE
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ SYSBUF,OUT
      COMMON /PACKX / ITI,ITO,II,NN,INCR
      COMMON /GICOM / SPLINE,DUM(8),KSIZE,GSIZE,SCR1,SCR2,SCR3
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE    (Z(1),IZ(1))
      DATA    NAM   / 4HGIGT,4HKG     /
      DATA    SDTAB / 9, 9, 0, 9, 1, 9,
     1                9, 0, 1, 9, 2, 3,
     2                9, 9, 0, 9, 9, 9,
     3                9, 9, 0, 9, 1, 2,
     4                9, 9, 0, 9, 1, 2/
C
      NCORE  = KORSZ(Z) - 2*SYSBUF
      BUF1   = NCORE
      BUF2   = BUF1 + SYSBUF
      ITI    = 1
      ITO    = 1
      II     = 1
      INCR   = 1
      TRL(1) = SCR2
      TRL(2) = 0
      TRL(3) = GSIZE
      TRL(4) = 2
      TRL(5) = 1
      TRL(6) = 0
      TRL(7) = 0
C
C     BUILD A G BY K MATRIX PUT OUT SPLINE3 COLUMNS WHEN NECESSARY
C
      CALL GOPEN (SCR2,Z(BUF1),1)
      CALL GOPEN (SCR3,Z(BUF2),0)
      ISS  = GSIZE + 1
      NCORE= NCORE - ISS
      KCOL = 0
      DO 80 I = 1,KSIZE
      IF (KCOL .LT. I) GO TO 20
   10 IF (KCOL .EQ. I) GO TO 50
      NN   = 1
      Z(1) = 0.0
      CALL PACK (Z,SCR2,TRL)
      GO TO 80
   20 CALL READ (*30,*40,SCR3,Z(ISS),NCORE,0,NWR)
      GO TO 90
   30 KCOL = KSIZE +1
      GO TO 10
   40 KST  = IZ(ISS+2)
      CTYPE= IZ(ISS+NWR-9)
      ICM  = IZ(ISS+3)
      K    = SDTAB(ICM,CTYPE)
      IF(K.EQ.9) GO TO 100
      KCOL = KST + K
      GO TO 10
C
C     BUILD COLUMN FOR SPLINE CARD
C
   50 DO 60 J = 1,GSIZE
   60 Z(J) = 0.0
      NN   = GSIZE
      JJ   = ISS+4
      JJJ  = ISS+NWR-19
      DO 70 J = JJ,JJJ,3
      K    = IZ(J) + IZ(J+1) -1
      Z(K) = Z(J+2)
   70 CONTINUE
      CALL PACK (Z,SCR2,TRL)
   80 CONTINUE
      CALL CLOSE (SCR2,1)
      CALL CLOSE (SCR3,1)
      CALL WRTTRL (TRL)
      GO TO 120
C
C     ERROR MESSAGES
C
   90 CALL MESAGE (-8,NCORE,NAM)
  100 WRITE  (OUT,110) UFM,IZ(ISS),CTYPE,ICM
  110 FORMAT (A23,' 2263, SPLINE3',I9,' FOR CAERO',I1,
     1       ' HAS ILLEGAL COMPONENT',I6)
      CALL MESAGE (-37,0,NAM)
  120 RETURN
      END
