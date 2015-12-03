      SUBROUTINE LAMX
C
C     LAMX MAKES OR EDITS THE LAMA DATA BLOCK
C
C     LAMX  EDIT,LAMA/LAMB/C,Y,NLAM=0 $
C     IF NLAM LT 0 MAKE LAMB A MATRIX OF 5 COLUMNS
C     LAMA  OMEGA FREQ GM GS
C     UNTIL GM = 0.0
C
C
      INTEGER SYSBUF,IST(10),TRL(7),BUFA,BUFB,BUFE,EDIT
C
      DIMENSION D(3),Z(7)
C
      COMMON /BLANK / NLAM
      COMMON /ZZZZZZ/ IZ(1)
      COMMON /SYSTEM/ SYSBUF,NOUT
      COMMON /CONDAS/ PI,TWOPI
      COMMON /UNPAKX/ ITO,II,IE,INCR
      COMMON /PACKX / ITYIN,ITYOUT,III,NNN,INCR1
      COMMON /OUTPUT/ HDG(96)
C
      EQUIVALENCE     (D(1),A),(D(2),B),(D(3),C)
      EQUIVALENCE     (Z(1),IZ(1))
C
      DATA    EDIT  , LAMA,LAMB /101,102,201/
      DATA    IST   / 21,6,7*0,7/
      DATA    LMA   / 1/,   IED /1/,  IZ2 /2/
      DATA    NAM   / 4HLAMX    /
C
C     INITILIZE AND DECIDE MODE OF OPERATIONS
C
      ICORE  = KORSZ(Z)
      TRL(1) = LAMA
      CALL RDTRL (TRL)
      IF (TRL(1) .LT. 0) LMA = 0
      TRL(1) = EDIT
      CALL RDTRL (TRL)
      IF (TRL(1) .LT. 0) IED = 0
      NCOL = TRL(2)
      IF (NCOL .EQ. 0) IED = 0
      IF (LMA.EQ.0 .AND. IED.EQ.0) GO TO 1000
      ITO = 1
      II  = 1
      INCR= 1
      IE  = TRL(3)
      IF (IE .GT. 3) IE = 3
      B = 0.0
      C = 0.0
      BUFB = ICORE - SYSBUF
      CALL GOPEN (LAMB,Z(BUFB),1)
      IF (LMA .EQ. 0) GO TO 200
      BUFA = BUFB - SYSBUF
      CALL GOPEN (LAMA,Z(BUFA),0)
      IF (NLAM .LT. 0) GO TO 500
      BUFE = BUFA - SYSBUF
      IF (IED .EQ. 0) GO TO 5
C
C      EDITING LAMA FROM EDIT
C
      CALL GOPEN (EDIT,Z(BUFE),0)
C
C     WRITE HEADER
C
    5 CALL READ (*10,*10,LAMA,Z,BUFE,1,NWR)
   10 CALL WRITE (LAMB,Z,NWR,1)
      IF (IED .EQ. 0) GO TO 100
C
C     MAKE RECORDS
C
      J = 0
      DO 50 I = 1,NCOL
      CALL READ (*60,*60,LAMA,Z,7,0,NWR)
      CALL UNPACK (*40,EDIT,D)
      IF (A.EQ.0.0 .AND. B.EQ.0.0 .AND. C.EQ.0.0) GO TO 40
      IF (C .LT. 0.0) GO TO 50
      Z(5) = Z(5)*(1.0+B) + A
      Z(4) = Z(5)*TWOPI
      Z(3) = Z(4)*Z(4)
      IF (C .NE. 0.0) Z(6) = C
      Z(7) = Z(6)*Z(3)
   40 J    = J+1
      IZ(1)= J
      IF (NLAM .LE. 0) GO TO 45
      IF (J .GT. NLAM) GO TO 180
   45 CALL WRITE (LAMB,Z,7,0)
   50 CONTINUE
   60 GO TO 180
C
C     COPY LAMA TO LAMB FOR NLAM RECORDS
C
  100 IF (NLAM .EQ. 0) GO TO 190
      J = NLAM
      M = 7*NLAM
      CALL READ (*180,*110,LAMA,Z,M,0,NWR)
      CALL WRITE (LAMB,Z,7*NLAM,0)
      GO TO 180
  110 CALL WRITE (LAMB,Z,NWR,0)
  180 TRL(1) = LAMB
      TRL(2) = J
      CALL WRTTRL (TRL)
  190 CALL CLOSE (LAMA,1)
      CALL CLOSE (LAMB,1)
      CALL CLOSE (EDIT,1)
      GO TO 1000
C
C      MAKE A NEW LAMB
C
  200 BUFE = BUFB - SYSBUF
      CALL GOPEN (EDIT,Z(BUFE),0)
      IF (NLAM .GT. 0) NCOL = MIN0(NCOL,NLAM)
C
C     WRITE HEADER
C
      CALL WRITE (LAMB,IST,50,0)
      CALL WRITE (LAMB,HDG,96,1)
C
C     MAKE RECORDS
C
      DO 220 I = 1,NCOL
      CALL UNPACK (*310,EDIT,D)
      GO TO 210
  310 D(1) = 0.0
      D(2) = 0.0
      D(3) = 0.0
  210 IZ(  1) = I
      IZ(IZ2) = I
      Z(5) = A
      Z(4) = TWOPI*A
      Z(3) = Z(4)*Z(4)
      Z(6) = C
      Z(7) = C*Z(3)
      CALL WRITE (LAMB,Z,7,0)
  220 CONTINUE
      J = NCOL
      GO TO 180
C
C     BUILD LAMB AS A MATRIX
C
  500 TRL(1) = LAMB
      TRL(2) = 0
      TRL(4) = 1
      TRL(5) = 1
      TRL(6) = 0
      TRL(7) = 0
      ITYIN  = 1
      ITYOUT = 1
      III    = 1
      INCR1  = 7
      CALL FWDREC (*190,LAMA)
      CALL READ (*190,*510,LAMA,Z,BUFA,0,NWR)
      CALL MESAGE (8,0,NAM)
      GO TO 190
  510 NLOOP = 0
      DO 520 I = 1,NWR,7
      IF (Z(I+5) .EQ.0.0) GO TO 530
      NLOOP = NLOOP +1
  520 CONTINUE
  530 IF (NLOOP .EQ. 0) GO TO 190
      TRL(3) = NLOOP
      NNN = NLOOP
      L   = 3
      DO 540 I = 1,5
      CALL PACK (Z(L),LAMB,TRL)
      L = L + 1
  540 CONTINUE
      CALL WRTTRL (TRL)
      GO TO 190
 1000 RETURN
      END
