      SUBROUTINE DETDET(DETA,IPOWR1,P,SML1,OLDD,IOLD)
C
      DIMENSION IPOWR1(1)
C
      DOUBLE PRECISION DETA(1),P(1),PROD,DET,CORE,SML2,OLDD,DET1,SML21
C
      DOUBLE PRECISION MINDD
C
      INTEGER FA,FL,FC,SR1,SR2,SR3,FA1,FL1,FC1,SR11,SR21
      INTEGER OPTION,SDET
      INTEGER OTPE,B,C,R,SR31
C
      COMMON /MACHIN/MACH
      COMMON /REGEAN/IM(21),IA,IN(4),LC,IN1(2),MZ,IN2(2),RMINR,IN3(2),
     1               NEVM,IL1,IL2,NFOUND,LAMA,IBUCK,NSYM
      COMMON /DETMX /ISEW(33),IPAAV,ISEW1(5),NDCMP,ISEW2(20),NPOLE
     1              ,ISING
      COMMON /DCOMPX/FA(7),FL(7),FC(7),SR1,SR2,SR3,DET,IPOWR,NZ
     1              ,SML2
      COMMON /ZZZZZZ /CORE(1)
      COMMON /REIGKR/OPTION
      COMMON /SFACT /FA1(7)   ,FC1(7)   ,FL1(7)   ,SR11     ,SR21
     1              ,NZ1      ,DET1(2)  ,IPOWRA   ,SR31     ,MINDD
     3              ,ICHOL    ,B        ,C        ,R        ,SML21
      COMMON /SYSTEM/KSYSTM(65)
C
      EQUIVALENCE ( KSYSTM( 2) , OTPE   )
C
      DATA SDET /4HSDET/
C
C ----------------------------------------------------------------------
C
      CALL SSWTCH (7, IPRT)
      ISAVE = IN(4)
      IN(4) = IL2
      IL2 = ISAVE
      NZZ = (KORSZ(CORE)/2)*2 -LC
      NDCMP = NDCMP+1
      IF(OPTION .EQ. SDET) GO TO 5
      FA(1)=IA
      CALL RDTRL(FA)
C
C     SET UP FOR UNSYMMETRIC
C
      NZ = NZZ
C
C
C     PUT IN TO PREVENT REWRITE
C
C     FA1(1) = -FA1(1)
C
CWKBD 10/94 SPR94011 FA(1) = -FA(1)
      FL(1)=IN(1)
      FC(1)=IN(2)
      DO 10 I=2,5
      FL(I)=FA(I)
   10 FC(I)=FA(I)
      SR1 = IN(3)
      SR2 = IN(4)
      SR3 = IL1
      CALL DECOMP(*60,CORE,CORE,CORE)
CWKBD 10/94 SPR94011     FC(1) = SR2
      CALL WRTTRL(FC)
      GO TO 14
C
C     SET UP FOR SYMMETRIC DECOMPOSITION
C
    5 FA1(1) = IA
      CALL RDTRL(FA1)
      FL1(1) = IN(1)
      FC1(1) = IN(4)
      ICHOL = 0
      IF(NDCMP .EQ. 1) B=0
      NZ1 = NZZ
      DO 6 I = 2,5
      FL1(I) = FA1(I)
      FC1(I) = FA1(I)
    6 CONTINUE
      SR11= IN(3)
      SR21 = IN(2)
      SR31 = IL1
      IF(MACH.EQ.4 .OR. MACH.EQ.12) FL1(5) = 1
      CALL SDCOMP(*60,CORE,CORE,CORE)
      FC1(5) = FL1(5)
      CALL WRTTRL(FC1)
      IPOWR=IPOWRA
      DET = DET1(1)
      SML1= SML21
   14 PROD = 1.0D0
      IF(IPRT .EQ. 0) GO TO 15
      WRITE(OTPE,99) P(1),DET,IPOWR
   99 FORMAT(2D16.7,I8)
   15 CONTINUE
      IPROD = 0
      IF( MZ  .EQ. 0) GO TO 12
      II =  IABS(MZ)
      DO 11  I=1,II
      PROD = PROD* P(1)
      CALL DETM6(PROD,IPROD)
   11 CONTINUE
C
C     TAKE OUT  POLE AT  RMINR
C
   12 IF (NPOLE .EQ. 0) GO TO 20
      DO  13  I = 1,NPOLE
      PROD = PROD*(P(1)- RMINR)
      CALL DETM6( PROD,IPROD)
   13 CONTINUE
   20 IF(NFOUND .EQ. 0) GO TO 40
      DO 30 I=1,NFOUND
      II = IPAAV +I
      IF(P(1).EQ. CORE(II)) GO TO 70
      PROD = PROD*(P(1)-CORE(II))
      CALL DETM6(PROD,IPROD)
   30 CONTINUE
   40 DETA(1) = DET/PROD
      SML1= SML2
      IPOWR1(1)= IPOWR-IPROD
      CALL DETM6(DETA(1),IPOWR1(1))
   50 IF(IPRT .EQ. 0) GO TO 51
      WRITE(OTPE,99) P(1),DETA(1),IPOWR1(1)
   51 RETURN
   60 DETA(1) = 0.0D0
      IPOWR1(1)=1
      SML1 = 1.0E-8
      ISING = ISING+1
      ISAVE = IN(4)
      IN(4) = IL2
      IL2 = ISAVE
      GO TO 50
C
C     SET DK = DK-1
C
   70 DETA(1) = OLDD
      SML1 = SML2
      IPOWR1(1) = IOLD
      GO TO 50
      END
