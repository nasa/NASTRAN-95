      SUBROUTINE COM12  (*,IX,X,DX,ITERMM)
C
C*******
C      PROGRAM TO SOLVE A MATRIX OF ORDER ONE OR TWO FOR CDCOMP
C*******
      DOUBLE PRECISION DX(12),DET,MINDIA,DZ,DA
      INTEGER         SYSBUF,RDP,DUM
      INTEGER         TYPEL
      INTEGER         CDP
      INTEGER         SCRFLG,JPOSL,BBAR,CBCNT,R,BBBAR1,BBBAR,
     1                SR2FL,SR2FIL
      DIMENSION       SUB(2),X(1),IX(1)
      COMMON /SYSTEM/ SYSBUF
      COMMON /CDCMPX/ IFILA(7),IFILL(7),IFILU(7),DUM(3),DET(2),POWER,
     1                NX,MINDIA
      COMMON /NAMES / RD,RDREW,WRT,WRTREW,REW,NOREW,EOFNRW ,RSP,RDP,
     1                CSP,CDP
      COMMON /ZBLPKX/ DZ(2),JJ
      COMMON /PACKX / ITYPE1,ITYPE2,IY,JY,INCRY
      COMMON /UNPAKX/ ITYPEX,IXY,JXY,INCRX
      EQUIVALENCE     (IFILA(2),NCOL),(IFILL(5),TYPEL),(SR2FIL,DUM(2))
      DATA    SUB(1), SUB(2)  / 4HCOM1,4H2    /
C
      IBUF1 = NX-SYSBUF
      IBUF2 = IBUF1-SYSBUF
      CALL CLOSE(SR2FIL,REW)
      IBUF3 = IBUF2-SYSBUF
      IFILE = IFILU(1)
      IF(ITERMM .EQ. 1) IFILE = SR2FIL
      CALL GOPEN(IFILE,IX(IBUF3),WRTREW)
      CALL GOPEN(IFILA(1),IX(IBUF1),RDREW)
      ITYPEX = CDP
      ITYPE1 = CDP
      ITYPE2 = TYPEL
      INCRX  = 1
      INCRY  = 1
      IF(NCOL .EQ. 2) GO TO 100
      IF(NCOL .NE. 1) GO TO 5000
C*******
C     SOLVE A (1X1)
C*******
      IXY = 1
      JXY = 1
      CALL UNPACK(*5060,IFILA(1),DX)
      DET(1) = DX(1)
      DET(2) = DX(2)
      MINDIA = DSQRT(DX(1)**2+DX(2)**2)
      IY = 1
      JY = 1
      CALL PACK (DX,IFILE,IFILU)
      DX(1) = 0.D0
      DX(2) = 0.D0
      CALL PACK (DX,IFILL(1),IFILL)
   90 CALL CLOSE(IFILE,REW)
   95 CALL CLOSE(IFILA(1),REW)
      CALL CLOSE(IFILL(1),REW)
      RETURN
  100 IXY = 1
C*******
C     SOLVE A (2X2)
C*******
      JXY = 2
      CALL UNPACK(*5060,IFILA(1),DX   )
      CALL UNPACK(*5060,IFILA(1),DX(5))
      A = 1.
      IF(DX(1)**2+DX(2)**2 .GE. DX(3)**2+DX(4)**2) GO TO 150
C*******
C     PERFORM INTERCHANGE
C*******
      DET(1) = DX(1)
      DX(1)  = DX(3)
      DX(3)  = DET(1)
      DET(1) = DX(2)
      DX(2)  = DX(4)
      DX(4)  = DET(1)
      DET(1) = DX(5)
      DX(5)  = DX(7)
      DX(7)  = DET(1)
      DET(1) = DX(6)
      DX(6)  = DX(8)
      DX(8)  = DET(1)
      A      = -1.
  150 CONTINUE
      DET(1) = (DX(3)*DX(1)+DX(4)*DX(2))/(DX(1)**2+DX(2)**2)
      DX(4)  = (DX(4)*DX(1)-DX(3)*DX(2))/(DX(1)**2+DX(2)**2)
      DX(3)  = DET(1)
      DX(7)  = DX(7)-DX(3)*DX(5)+DX(4)*DX(6)
      DX(8)  = DX(8)-DX(3)*DX(6)-DX(4)*DX(5)
      DET(1) = DX(1)*DX(7)-DX(2)*DX(8)
      DET(2) = DX(2)*DX(7)+DX(1)*DX(8)
      IF((DX(1).EQ.0.D0 .AND. DX(2).EQ.0.D0) .OR. (DX(7).EQ.0.D0.AND.
     1    DX(8).EQ.0.D0)) GO TO 5060
      MINDIA = DMIN1(DSQRT(DX(1)**2+DX(2)**2),DSQRT(DX(7)**2+DX(8)**2))
      IY = 1
      JY = 2
      DX( 9) = 0.0D0
      DX(10) = 0.0D0
      IF(A .LT. 0.) DX(9) = 1.D0
      DX(11) = DX(3)
      DX(12) = DX(4)
      CALL PACK(DX(9),IFILL(1),IFILL)
      DX( 9) = 0.D0
      DX(10) = 0.D0
      JY = 1
      CALL PACK(DX(9),IFILL(1),IFILL)
      IF(ITERMM .EQ. 1) GO TO 160
      DX(3) = DX(5)
      DX(5) = DX(7)
      DX(7) = DX(3)
      DX(3) = DX(6)
      DX(6) = DX(8)
      DX(8) = DX(3)
      JY    = 2
      CALL PACK(DX(5),IFILU(1),IFILU)
      IY = 2
      CALL PACK (DX,IFILU(1),IFILU)
      GO TO 90
  160 CALL PACK(DX,IFILE,IFILU)
      JY = 2
      CALL PACK(DX(5),IFILE,IFILU)
      CALL CLOSE(IFILE,EOFNRW)
      GO TO 95
C
C
      ENTRY COMFIN (ITERM,SCRFLG,SR2FL,JPOSL,I1SP,BBAR,I1,CBCNT,
     1              IPAK,R,BBBAR1,BBBAR,I6SP,I4,I4SP,IX,DX,X,LCOL)
C
      IBUF1 = NX-SYSBUF
      IBUF2 = IBUF1-SYSBUF
      IBUF3 = IBUF2-SYSBUF
      CALL CLOSE(IFILA(1),REW)
      CALL OPEN(*5010,SR2FIL,IX(IBUF1),WRT)
      CALL CLOSE(SR2FIL,EOFNRW)
      K=0
      NAME =  IFILL(1)
      CALL OPEN(*5010,IFILL(1),IX(IBUF2),WRT)
      IF(SCRFLG.EQ.0) GO TO 2005
      NAME = SR2FL
      CALL OPEN(*5010,SR2FL,IX(IBUF3),RD)
 2005 LL = 0
 2010 JPOSL = JPOSL+1
      CALL BLDPK(CDP,TYPEL,IFILL(1),0,0)
      IN1 = I1SP+K
      JJ  = JPOSL
      DZ(1) = IX(IN1)
      DZ(2) = 0.D0
      CALL ZBLPKI
      KK   = 0
      IEND = MIN0(BBAR,NCOL-JJ)
      IF(IEND .EQ. 0) GO TO 2030
      IN1 = I1 +LL*BBAR*2
 2020 JJ  = JJ+1
      IN2 = IN1+KK+KK
      DZ(1) = DX(IN2)
      DZ(2) = DX(IN2+1)
      CALL ZBLPKI
      KK = KK+1
      IF(KK-IEND)2020,2030,5050
 2030 IF(CBCNT.EQ.0) GO TO 2050
C*******
C     PACK ACTIVE ROW ELEMENTS ALSO
C*******
      KK  = 0
 2035 IN1 = I6SP + KK
      IN2 = I4 +(IX(IN1)*BBBAR+K)*2
      DZ(1) = DX(IN2)
      DZ(2) = DX(IN2+1)
      IF(DZ(1) .EQ. 0.D0 .AND. DZ(2) .EQ. 0.D0) GO TO 2040
      IN1 = I4SP + IX(IN1)
      JJ  = IX(IN1)
      CALL ZBLPKI
 2040 KK = KK + 1
      IF(KK .LT. CBCNT) GO TO 2035
 2050 CALL BLDPKN(IFILL(1),0,IFILL)
      LL = LL + 1
      K  = K + 1
      IF (K .EQ. LCOL) GO TO 2080
      IF(K-R+1)2010,2060,2070
 2060 IF(R-BBBAR1)2070,2010,5050
 2070 LL  = LL-1
      IN1 = I1+LL*BBAR*2
      IBBAR4 = 4 * BBAR
      CALL READ(*5020,*5030,SR2FL,DX(IN1),IBBAR4,0,NO)
      GO TO 2010
 2080 CALL CLOSE(IFILL(1),REW)
      IF(SCRFLG.GT.0)CALL CLOSE(SR2FL,REW)
      IF(ITERM .NE. 0) RETURN
C*******
C     RE-WRITE THE UPPER TRIANGLE WITH THE RECORDS IN THE REVERSE ORDER
C*******
      INCRX  = 1
      INCRY  = 1
      ITYPE1 = TYPEL
      ITYPE2 = TYPEL
      ITYPEX = TYPEL
      IFILU(2) = 0
      IFILU(6) = 0
      IFILU(7) = 0
      NAME = SR2FIL
      CALL OPEN(*5010,SR2FIL,IX(IBUF1),RD)
      CALL GOPEN(IFILU(1),IX(IBUF2),WRTREW)
      DO 2300 I = 1,NCOL
      IXY = 0
      CALL BCKREC(SR2FIL)
      CALL UNPACK(*5060,SR2FIL,IX)
      CALL BCKREC(SR2FIL)
      KK = JXY-IXY+1
      K  = KK/2
      KK = KK + 1
      IF(TYPEL .EQ. 1) GO TO 2095
      IF(TYPEL .EQ. 4) GO TO 2061
      DO 2090 J = 1,K
      L  = KK-J
      DA = DX(J)
      DX(J) = DX(L)
 2090 DX(L) = DA
      GO TO 2100
 2061 KK = KK+KK-1
      K  = K+K
      DO 2092 J = 1,K,2
      L  = KK-J-1
      DA = DX(L)
      DX(L) = DX(J)
      DX(J) = DA
      DA = DX(L+1)
      DX(L+1) = DX(J+1)
 2092 DX(J+1) = DA
      GO TO 2100
 2095 DO 2097 J = 1,K
      L    = KK-J
      A    = X(J)
      X(J) = X(L)
 2097 X(L) = A
 2100 IY = NCOL-JXY+1
      JY = NCOL-IXY+1
      CALL PACK(IX,IFILU(1),IFILU)
 2300 CONTINUE
      CALL CLOSE(IFILU(1),REW)
      CALL CLOSE(SR2FIL,REW)
      RETURN
 5000 NO = -8
      GO TO 5500
 5010 NO = -1
      GO TO 5500
 5020 NO = -2
      GO TO 5500
 5030 NO = -3
      GO TO 5500
 5050 NO = -25
      GO TO 5500
 5060 RETURN 1
 5500 CALL MESAGE(NO,NAME,SUB(1))
      RETURN
      END
