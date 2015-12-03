      SUBROUTINE DADD
C
C     DMAP DRIVER FOR ADD--
C
C     ADD    A,B/C/V,N,ALPHA/V,N,BETA/V,N,DALPHA/V,N,DBETA/V,N,ECHO  $
C
C            MATRIX C = ALPHA*MATRIX A + BETA*MATRIX B
C
C     MATRIX C IS COMPLEX IF ANY ONE OF THE MATRIX A, MATRIX B, SCALE
C     ALPHA, OR SCLAE BETA IS COMPLEX
C
      LOGICAL           DBLEA    ,DBLEB
      INTEGER           FN(2)    ,ECHO     ,AA(2)     ,BB(2)
      DOUBLE PRECISION  DALPHA   ,DBETA    ,DALP(2)   ,DBTA(2)  ,
     1                  ZERO     ,ONE      ,XX
      CHARACTER         UFM*23   ,UWM*25   ,UIM*29
      COMMON /XMSSG /   UFM      ,UWM      ,UIM
      COMMON /SYSTEM/   IBUF     ,NOUT
      COMMON /BLANK /   ALPHA(2) ,BETA(2)  ,DALPHA(2) ,DBETA(2) ,
     1                  ECHO
      COMMON /SADDX /   NOMAT    ,LCORE    ,IA(7)     ,ITA      ,
     1                  ALP(4)   ,IB(7)    ,ITB       ,BTA(4)   ,
     2                  CDE(3,12),IC(7)
      COMMON /ZZZZZZ/   CORE(1)
      EQUIVALENCE       (ALP(1),DALP(1))   ,(BTA(1),DBTA(1))
      DATA              IN1,IN2,IOUT1,ZERO /101,102,201, 0.0D+0 /
      DATA              ONE,XX ,X    / 1.0D+0, 1.0D+37, 1.0E+37 /
C
C
C     SCALE FACTORS ALPHA, DALPHA, BETA AND DBETA WERE INITIALLY SET TO
C     (1.1+37, 1.1+37) BY XMPLDD
C
C     IN THIS ROUTINE -
C     IF ALPHA, DALPHA, BETA AND DBETA ARE NOT SPECIFIED BY USER, THEY
C     WILL BE SET TO -
C     ALPHA AND DALPHA TO (1.0, 0.0), AND
C     BETA  AND DBETA  TO (1.0, 0.0), SAME DEFAULTS AS 88 AND EARLIER
C                                     NASTRAN VERSIONS.
C     NOTE - DEFAULTS WERE ALL ZEROS IN 89 NASTRAN VERSION
C
C     NOTE - THIS ROUTINE WILL CALL SADD TO DO THE ACTUAL MATRIX MULTI-
C     PLICATION, WHICH WILL AUTOMATICALLY ADJUST THE SCALE FACTORS
C     WHETHER THEY ARE S.P. OR D.P. (E.G. S.P. ALPHA AND BETA CAN BE
C     USED FOR D.P. A AND B MATRICES, AND VISE VERSA)
C
      CALL FNAME (IOUT1,FN(1))
      LCORE = KORSZ(CORE)
      DO 10 I = 1,7
      IA(I) = 0
      IB(I) = 0
      IC(I) = 0
   10 CONTINUE
      IA(1) = IN1
      IB(1) = IN2
      CALL RDTRL (IA)
      CALL RDTRL (IB)
      IF (IA(1) .LT. 0) IA(1) = 0
      IF (IB(1) .LT. 0) IB(1) = 0
      IF (IA(1)+IB(1) .EQ. 0) GO TO 100
C
C     SET DEFAULT VALUES FOR THE SCALE FACTORS
C
C     WHEN AN ITEM IS .LT. X OR XX, THAT ITEM HAS INPUT FROM USER
C
      DBLEA = .TRUE.
      DBLEB = .TRUE.
      IF (ALPHA(1).LT.X .OR. ALPHA(2).LT.X .OR. DALPHA(1).LT.XX .OR.
     1   DALPHA(2).LT.XX) GO TO 20
      ALP(1)   = 1.0
      ALP(2)   = 0.0
      ALPHA(1) = 1.0
      ALPHA(2) = 0.0
      DBLEA  = .FALSE.
   20 IF (BETA(1).LT.X .OR. BETA(2).LT.X .OR. DBETA(1).LT.XX .OR.
     1   DBETA(2).LT.XX) GO TO 25
      BTA(1)  = 1.0
      BTA(2)  = 0.0
      BETA(1) = 1.0
      BETA(2) = 0.0
      DBLEB  = .FALSE.
      IF (.NOT.DBLEA) GO TO 40
C
   25 IF ((ALPHA(1).LT.X .OR. ALPHA(2).LT.X) .AND. (DALPHA(1).LT.XX .OR.
     1    DALPHA(2).LT.XX)) GO TO 120
      IF (( BETA(1).LT.X .OR.  BETA(2).LT.X) .AND. ( DBETA(1).LT.XX .OR.
     1     DBETA(2).LT.XX)) GO TO 120
C
      IF (DALPHA(1).GT.XX .AND. DALPHA(2).GT.XX) DBLEA = .FALSE.
      IF ( DBETA(1).GT.XX .AND.  DBETA(2).GT.XX) DBLEB = .FALSE.
C
      DO 30 I = 1,2
      IF ( ALPHA(I) .GT.  X)  ALPHA(I) = 0.0
      IF (DALPHA(I) .GT. XX) DALPHA(I) = ZERO
      IF (  BETA(I) .GT.  X)   BETA(I) = 0.0
      IF ( DBETA(I) .GT. XX)  DBETA(I) = ZERO
   30 CONTINUE
C
C     MOVE ALPHA, BETA, DALPHA AND DBETA INTO ALP AND BTA ARRAYS FOR
C     MATRIX MULTIPLICATION TO BE PERFORMED IN SADD.
C
      DO 35 I = 1,2
      IF (.NOT.DBLEA)  ALP(I) =  ALPHA(I)
      IF (.NOT.DBLEB)  BTA(I) =   BETA(I)
      IF (     DBLEA) DALP(I) = DALPHA(I)
      IF (     DBLEB) DBTA(I) =  DBETA(I)
   35 CONTINUE
C
   40 IF (ECHO .EQ. 0) GO TO 55
      WRITE  (NOUT,45) UIM,FN
   45 FORMAT (A29,', SCALE FACTORS FOR THE OUTOUT DATA BLOCK ',2A4,
     1       ', IN ADD MODULE ARE -')
      IF (.NOT.DBLEA) WRITE (NOUT,50) ALP(1) ,ALP(2)
      IF (     DBLEA) WRITE (NOUT,51) DALP(1),DALP(2)
      IF (.NOT.DBLEB) WRITE (NOUT,52) BTA(1) ,BTA(2)
      IF (     DBLEB) WRITE (NOUT,53) DBTA(1),DBTA(2)
   50 FORMAT (5X,'1ST S.F. = (',E12.5,1H,,E12.5,1H))
   51 FORMAT (5X,'3RD S.F. = (',D12.5,1H,,D12.5,1H))
   52 FORMAT (1H+,48X,'2ND S.F. = (',E12.5,1H,,E12.5,1H))
   53 FORMAT (1H+,48X,'4TH S.F. = (',D12.5,1H,,D12.5,1H))
C
C     ENSURE THAT THE MATRICES BEING ADDED ARE OF THE SAME ORDER
C
   55 IF (IA(1).EQ.0 .OR. IB(1).EQ.0) GO TO 70
      IF (IA(2).EQ.IB(2) .AND. IA(3).EQ.IB(3)) GO TO 70
      CALL FNAME (IA(1),AA)
      CALL FNAME (IB(1),BB)
      WRITE  (NOUT,60) UFM,AA,BB,FN,IA(2),IA(3),IB(2),IB(3)
   60 FORMAT (A23,' 4149, ATTEMPT TO ADD MATRICES OF UNEQUAL ORDER IN',
     1       ' MODULE ADD, ',2A4,' TO ',2A4, /5X,'INTENDED OUTOUT DATA',
     2       ' BLOCK NAME =',2A4,I7,' BY',I6,' TO',I7,' BY',I6)
      GO TO 160
   70 IC(1) = IOUT1
      IC(2) = IA(2)
      IC(3) = IA(3)
      IF (IA(4) .EQ. 3) IC(2) = IA(3)
      IF (IA(1) .NE. 0) GO TO 80
      IC(2) = IB(2)
      IC(3) = IB(3)
C
C     DETERMINE TYPE
C
   80 ITA = 3
      ITB = 3
      IF (ALP(2).EQ.0.0 .AND. ALP(4).EQ.0.0) ITA = 1
      IF (BTA(2).EQ.0.0 .AND. BTA(4).EQ.0.0) ITB = 1
      IC(5) = MAX0(IA(5),IB(5),ITA,ITB)
      IF (IC(5).EQ.3 .AND. (IA(5).EQ.2 .OR. IB(5).EQ.2)) IC(5) = 4
C
C     DETERMINE FORM
C
      IC(4) = IA(4)
      IF (IA(1) .EQ. 0) IC(4) = IB(4)
      IF (IC(4).NE.1 .OR. IC(4).NE.6) GO TO 90
      IC(4) = 6
      IF (IA(1).NE.0 .AND. IA(4).NE.6) IC(4) = 1
      IF (IB(1).NE.0 .AND. IB(4).NE.6) IC(4) = 1
      IF (IC(2) .NE. IC(3)) IC(4) = 2
   90 IF (IA(4).EQ.3 .AND. IB(1).NE.0) IC(4) = IB(4)
      IF (IA(4).EQ.3 .AND. IB(1).EQ.0) IC(4) = IA(4)
C
      NOMAT = 2
      CALL SADD (CORE,CORE)
      CALL WRTTRL (IC)
      GO TO 170
C
  100 WRITE  (NOUT,110) UFM,FN
  110 FORMAT (A23,', INPUT MATRICES NOT SPECIFIED IN ADD MODULE.',
     1       ' INTENDED OUTPUT DATA BLOCK NAME =',2A4)
      GO TO 160
C
  120 DO 130 I=1,2
      IF ( ALPHA(I) .GT.  X) ALPHA(I)  = 0.0
      IF (DALPHA(I) .GT. XX) DALPHA(I) = ZERO
      IF (  BETA(I) .GT.  X)   BETA(I) = 0.0
      IF ( DBETA(I) .GT. XX)  DBETA(I) = ZERO
  130 CONTINUE
      WRITE  (NOUT,150) UFM,FN,ALPHA,BETA,DALPHA,DBETA
  150 FORMAT (A23,' IN ADD MODULE. INTENDED OUTPUT DATA BLOCK =',2A4,
     1       /5X,'SCALE FACTORS ARE ERRONEOUS =',4E9.2,2X,4D10.3)
  160 CALL MESAGE (-61,0,0)
C
  170 RETURN
      END
