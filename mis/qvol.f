      SUBROUTINE QVOL
C
C     CALCULATES THERMAL LOADS DUE TO QVOL CARDS
C
      INTEGER         IP(3),NSIL(8),MAP(4,14),SLT,REASON,TYPE,BG,OLD,
     1                ORDER(8)
      REAL            R(4,8),DATA4(4,9),P(8),D12(3),D13(3),D14(3),
     1                CARD(12)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /CONDAS/ CONSTS(5)
      COMMON /LOADX / LC,SLT,BG,OLD,NX(12), IFM
      COMMON /ZZZZZZ/ CORE(1)
      COMMON /SYSTEM/ SYSBUF,IOUT
      EQUIVALENCE     (CONSTS(2),TWOPI),(NPTS,CARD(1)),(ID,CARD(2)),
     1                (NSIL(1),CARD(3)),(COEF,CARD(11)),(TYPE,CARD(12)),
     2                (R(1,1),DATA4(2,1)),(I1,IP(1)),(I2,IP(2)),
     3                (I3,IP(3))
      DATA    MAP   / 1  ,2  ,3  ,4  ,
     1                1  ,2  ,3  ,6  ,
     2                1  ,2  ,6  ,5  ,
     3                1  ,4  ,5  ,6  ,
     4                1  ,2  ,3  ,6  ,
     5                1  ,3  ,4  ,8  ,
     6                1  ,3  ,8  ,6  ,
     7                1  ,5  ,6  ,8  ,
     8                3  ,6  ,7  ,8  ,
     9                2  ,3  ,4  ,7  ,
     O                1  ,2  ,4  ,5  ,
     1                2  ,4  ,5  ,7  ,
     2                2  ,5  ,6  ,7  ,
     3                4  ,5  ,7  ,8  /
C
C     READ AND PROCESS ONE ELEMENT OF ONE QVOL CARD PER CALL
C     THE LOAD COEFFICIENTS ARE GENERATED AND INSERTED HERE
C
C     THE INPUT DATA ON FILE SLT IS
C
C     FIELD       DATA
C       1         NO. OF POINTS
C       2         EL. ID.
C      3-10       1 TO 8 SILS
C                        *  A*Q  FOR  TYPE=1 (RODS,ETC)
C       11        COEF = *  T*Q  FOR  TYPE=2 (TRIANGLES ETC)
C                        *    Q  FOR  TYPE=3 (BELL) OR 4 (SOLID)
C       12        TYPE
C
      CALL FREAD (SLT,CARD,12,0)
      REASON = 1
      IF (NPTS .LE. 1) GO TO 240
      CALL PERMUT (NSIL(1),ORDER(1),NPTS,OLD)
      REASON = 2
      DO 10 I = 1,NPTS
      L = ORDER(I)
      CALL FNDPNT (DATA4(1,L),NSIL(L))
      N = NSIL(L)
      CALL FNDSIL (N)
      IF (N .NE. NSIL(L)) GO TO 240
      P(I) = 0.0
   10 CONTINUE
      REASON = 3
      IF (TYPE.LT.1 .OR. TYPE.GT.4) GO TO 240
      GO TO (20,40,40,120), TYPE
C
C     RODS, CONRODS, TUBES, BARS
C
   20 EL = 0.0
      DO 30 I = 1,3
   30 EL = EL + (R(I,1) - R(I,2))**2
      P(1) = COEF*SQRT(EL)*0.5
      P(2) = P(1)
      GO TO 200
C
C     MEMBRANES, PLATES, AND AXISYMMETRIC SOLIDS
C
   40 IF (NPTS .EQ. 3) GO TO 50
      IF (NPTS .EQ. 4) GO TO 60
      REASON = 4
      GO TO 240
   50 NEL  = 1
      FACT =  COEF/6.0
      GO TO 70
   60 NEL  = 4
      FACT =  COEF/12.0
   70 DO 110 IEL = 1,NEL
      DO 80 I = 1,3
      IP(I) = I + IEL - 1
      IF (IP(I) .GT. 4) IP(I) = IP(I) - 4
   80 CONTINUE
      DO 90 I = 1,3
      D12(I) = R(I,I2) - R(I,I1)
   90 D13(I) = R(I,I3) - R(I,I1)
      CALL SAXB (D12(1),D13(1),D12(1))
      EL =  FACT*SQRT(D12(1)**2 + D12(2)**2 + D12(3)**2)
      IF (TYPE .EQ. 2) GO TO 100
C
C     SPECIAL FACTOR FOR  AXISYMMETRIC ELEMENTS
C
      EL    = EL*TWOPI*(R(1,I1) + R(1,I2) + R(1,I3))/3.0
  100 P(I1) = P(I1) + EL
      P(I2) = P(I2) + EL
      P(I3) = P(I3) + EL
  110 CONTINUE
      GO TO 200
C
C     SOLID ELEMENTS
C
  120 IF (NPTS .EQ. 4) GO TO 130
      IF (NPTS .EQ. 6) GO TO 140
      IF (NPTS .EQ. 8) GO TO 150
      REASON = 5
      GO TO 240
  130 NEL  = 1
      FACT = COEF/24.0
      IMAP = 1
      GO TO 160
  140 NEL  = 3
      IMAP = 2
      FACT = COEF/24.0
      GO TO 160
  150 IMAP = 5
      NEL  = 10
      FACT = COEF/48.0
  160 DO 190 IEL = 1,NEL
      IM = IMAP + IEL - 1
      I1 =  MAP(1,IM)
      I2 =  MAP(2,IM)
      I3 =  MAP(3,IM)
      I4 =  MAP(4,IM)
      DO 170  I = 1,3
C
       D12(I) = R(I,I2) - R(I,I1)
       D13(I) = R(I,I3) - R(I,I1)
  170  D14(I) = R(I,I4) - R(I,I1)
C
      CALL SAXB (D12(1),D13(1),D12(1))
      EL = FACT*ABS(D12(1)*D14(1) + D12(2)*D14(2) + D12(3)*D14(3))
      DO 180 I = 1,4
      L = MAP(I,IM)
  180 P(L) = P(L) + EL
  190 CONTINUE
C
C     INSERT THE LOADS
C
  200 DO 210 I = 1,NPTS
      ISIL = NSIL(I)
      CORE(ISIL) = CORE(ISIL) + P(I)
  210 CONTINUE
      RETURN
C
C     ERROR MESSAGE
C
  240 WRITE  (IOUT,250) SFM,ID,REASON
  250 FORMAT (A25,' 3093, ELEMENT =',I9,'.   REASON =',I7)
      CALL MESAGE (-61,0,0)
      RETURN
      END
