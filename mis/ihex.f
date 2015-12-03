      SUBROUTINE IHEX(TEMPS,PG,TYPE)
C
C     ELEMENT THERMAL LOAD GENERATOR FOR ISOPARAMETRIC SOLID ELEMENTS
C
C     TYPE = 1     CIHEX1
C     TYPE = 2     CIHEX2
C     TYPE = 3     CIHEX3
C
C***********************************************************************
C           THE EST ENTRIES ARE
C
C     NAME  ---------INDEX---------   DESCRIPTION
C            IHEX1   IHEX2   IHEX3
C
C     EID        1       1       1    ELEMENT ID NO.
C     SIL      2-9    2-21    2-33    SCALAR INDEX LIST
C     MID       10      22      34    MATERIAL ID NO.
C     CID       11      23      35    MATERIAL COORD. SYSTEM ID NO.
C     NIP       12      24      36    NO. INTEGRATION POINTS PER EDGE
C     MAXAR     13      25      37    MAX ASPECT RATIO
C     ALFA      14      26      38    MAX ANGLE FOR NORMALS
C     BETA      15      27      39    MAX ANGLE FOR MIDSIDE POINTS
C     BGPDT  16-47  28-107  40-167    BASIC GRID POINT DATA
C     GPT    48-55 108-127 168-199    GRID POINT TEMPERATURES
C***********************************************************************
C
      LOGICAL TDEP     ,MTDEP      ,ANIS       ,RECT
C
      INTEGER TYPE     ,OTPT       ,EID        ,IEST(1)    ,BGPDT      ,
     2        BCORD    ,GPT        ,JZ(32)     ,SIL        ,CID        ,
     3        UFM(6)
      INTEGER IB(46)
C
      DOUBLE PRECISION  SHP        ,DSHP       ,JACOB      ,DETJ
     1,     S          ,SFACT      ,A(6)      ,E1         ,E2
     2,     E3         ,PARG(96)   ,CN(3,32)   ,TEMP       ,ELTEMP
     3,     ALPVEC     ,GMAT(36)   ,GAUSS(8)   ,DALPHA(6)
C
      REAL  TEMPS(1)   ,PG(1)      ,PSGL(96)
C
      COMMON/TRIMEX/ EST(200)
      COMMON/MATIN/ MID      ,INFLAG     ,ELTEMP
      COMMON/MATOUT/   SE          ,G          ,SNU        ,RHO        ,
     2  TALPHA,TREF,CDAMP,SPACE(18),
     3                 MTDEP
      COMMON/MATISO/ BUFM6(46)
      COMMON/SYSTEM/ SYSBUF  ,OTPT       ,SYS1(7)          ,MTEMP      ,
     2               SYS2(45),HEAT
C
      COMMON/SSGWRK/    SHP(32)    ,DSHP(3,32) ,JACOB(3,3) ,S(4)
     1,     H(4)
C
      EQUIVALENCE (EID,EST(1),IEST(1)),(JZ(1),SHP(1))
      EQUIVALENCE (PSGL(1),PARG(1))
      EQUIVALENCE (IB(1),BUFM6(1))
C
      DATA  GAUSS/      0.577350269189626D0    ,0.555555555555556D0
     1,                 0.774596669241483D0    ,0.888888888888889D0
     2,                 0.347854845137454D0    ,0.861136311594053D0
     3,                 0.652145154862546D0    ,0.339981043584856D0/
      DATA UFM /4H0***,4H USE,4HR FA,4HTAL ,4HMESS,4HAGE /
C
C*****
C     COMPUTE EST POINTERS
C*****
      NGP = 12*TYPE - 4
      MID = 10 + 12*(TYPE - 1)
      CID=IEST(MID+1)
      NIP=IEST(MID+2)
      IF (NIP .LT. 2 .OR. NIP .GT. 4) NIP=TYPE/2+2
      BGPDT = MID + 6
      GPT=BGPDT+4*NGP
      DO 110 I=1,NGP
  110 JZ(I) = IEST(BGPDT + 4*I - 4)
      BCORD=GPT-3
      DO 120  I=2,NGP
      DO 120  J=1,3
      K = BGPDT + 4*(NGP - I) + 4 - J
      BCORD = BCORD - 1
      EST(BCORD) = EST(K)
  120 CONTINUE
      DO 130  I=2,NGP
  130 IEST(BGPDT+I-1) = JZ(I)
      MID=IEST(NGP+2)
C
C     ABSCISSAE AND WEIGHT COEFFICIENTS FOR GAUSSIAN QUADRATURE
C
      I=NIP-1
      GO TO (131,132,133),I
  131 H(1)=1.0
      S(1)=GAUSS(1)
      H(2)=H(1)
      S(2)=-S(1)
      GO TO 134
  132 H(1)=GAUSS(2)
      S(1)=GAUSS(3)
      H(2)=GAUSS(4)
      S(2)=0.0
      H(3)=H(1)
      S(3)=-S(1)
      GO TO 134
  133 H(1)=GAUSS(5)
      S(1)=GAUSS(6)
      H(2)=GAUSS(7)
      S(2)=GAUSS(8)
      H(3)=H(2)
      S(3)=-S(2)
      H(4)=H(1)
      S(4)=-S(1)
  134 CONTINUE
C
C=======================================================================
C     THIS SECTION OF CODE MUST BE UPDATED WHEN GENERAL ANISOTROPIC
C     MATERIAL IS ADDED
C
C     TEST FOR ANISOTROPIC MATERIAL
C
      ANIS = .FALSE.
      INFLAG=10
C
C     TEST FOR RECTANGULAR COORDINATE SYSTEM IN WHICH THE ANISOTROPIC
C     MATERIAL IS DEFINED
C
      RECT = .TRUE.
C=======================================================================
C
C     FETCH MATERIAL AND SET TEMPERATURE DEPENDENCE FLAG
C
      TDEP=.TRUE.
      DO 140 I=2,NGP
      IF (EST(GPT) .NE. EST(GPT+I-1)) GO TO 150
  140 CONTINUE
      TDEP=.FALSE.
  150 ELTEMP=EST(GPT)
      CALL MAT(EID)
      IF (.NOT. MTDEP) TDEP=.FALSE.
      IF (IB(46).EQ.6) ANIS=.TRUE.
      TREF=BUFM6(44)
C*****
C     IF ISOTROPIC TEMPERATURE INDEPENDENT MATERIAL, COMPUTE CONSTANTS
C*****
      IF (TDEP) GO TO 800
      IF (ANIS) GO TO 700
      IF (IB(46).NE.0) GO TO 640
      CALL PAGE2(2)
      WRITE(OTPT,7300) UFM,MID,EID
      NOGO = 1
      RETURN
  640 E1=BUFM6(1)
      E2=BUFM6(2)
      E3=BUFM6(22)
      TALPHA=BUFM6(38)
      GO TO 800
C
C=======================================================================
C     CODE TO TRANSFORM GENERAL ANISOTROPIC MATERIAL PROPERTIES TO
C     BASIC COORDINATE SYSTEM MUST BE ADDED HERE
C=======================================================================
C
  700 DO 710 IJK=1,36
  710 GMAT(IJK)=BUFM6(IJK)
  800 NTLP = 3*NGP
      DO 900 I=1,NTLP
  900 PARG(I) = 0.0
C*****
C     BEGIN INTEGRATION LOOP NOW
C*****
      DO 2000 I=1,NIP
      DO 2000 J=1,NIP
      DO 2000 K=1,NIP
C*****
C     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE
C*****
      CALL IHEXSD(TYPE,SHP,DSHP,JACOB,DETJ,EID,S(I),S(J),S(K),
     2            EST(BCORD))
      IF (DETJ .NE. 0.0D0) GO TO 1010
C
C     JACOBIAN MATRIX WAS SINGULAR
C
      CALL MESAGE(-61,0,0)
C*****
C     COMPUTE PARTIAL DERIVATIVE OF SHAPE FUNCTIONS WITH RESPECT
C     TO BASIC COORDINATES
C*****
 1010 CALL GMMATD(DSHP,NGP,3,0,JACOB,3,3,0,CN)
C*****
C     COMPUTE LOADING TEMPERATURE AT THIS INTEGRATION POINT
C*****
      TEMP=0.0D0
      DO 1012 L=1,NGP
 1012 TEMP=TEMP+SHP(L)*DBLE(TEMPS(L))
      TEMP=TEMP-DBLE(TREF)
C*****
C     IF MATERIAL IS TEMPERATURE DEPENDENT, COMPUTE TEMPERATURE AT THIS
C     INTEGRATION POINT AND FETCH MATERIAL PROPERTIES
C*****
      IF(.NOT.TDEP)  GO TO 1030
      ELTEMP=0.0D0
      DO 1020  L=1,NGP
 1020 ELTEMP=ELTEMP+SHP(L)*DBLE(EST(GPT+L-1))
      CALL MAT(EID)
      IF (ANIS) GO TO 1040
      IF (IB(46).NE.0) GO TO 1025
      CALL PAGE2(2)
      WRITE(OTPT,7300)  UFM,MID,EID
      NOGO = 1
      RETURN
 1025 E1=BUFM6(1)
      E2=BUFM6(2)
      E3=BUFM6(22)
      TALPHA=BUFM6(38)
      GO TO 1100
C*****
C     IF MATERIAL IS ANISOTROPIC AND NOT DEFINED IN RECTANGULAR COOR-
C     DINATE SYSTEM, MUST TRANSFORM TO BASIC COORDINATE SYSTEM AT THIS
C     INTEGRATION POINT
C*****
 1030 IF(.NOT. ANIS)  GO TO 1100
      IF (RECT) GO TO 1500
 1040 CONTINUE
      DO 1041 IJK=1,36
 1041 GMAT(IJK)=BUFM6(IJK)
C
C=======================================================================
C     INSERT GLOBAL TO BASIC TRANSFORMATION OPERATIONS HERE FOR
C     ANISOTROPIC MATERIAL MATRIX
      GO TO 1500
C=======================================================================
C*****
C     COMPUTE CONTRIBUTION TO THERMAL LOAD VECTOR FOR ISOTROPIC MATERIAL
C*****
 1100 ALPVEC=DBLE(TALPHA)*(E1+2.0*E2)
      SFACT=H(I)*H(J)*H(K)*DETJ*ALPVEC*TEMP
      L = 0
      DO 1400  II=1,NGP
      DO 1400 JJ=1,3
      L = L + 1
      PARG(L) = SFACT*CN(JJ,II)  + PARG(L)
 1400 CONTINUE
      GO TO 2000
C=======================================================================
 1500 CONTINUE
C     ADD LOAD COMPUTATIONS FOR ANISOTROPIC MATERIAL HERE
C=======================================================================
C
      SFACT=H(I)*H(J)*H(K)*DETJ*TEMP
      DO 1560 IJK=1,6
 1560 DALPHA(IJK)=BUFM6(IJK+37)
C
      CALL GMMATD(GMAT,6,6,0,DALPHA,6,1,0,A(1))
      L=0
      DO 1600 II=1,NGP
      L=L+1
      PARG(L)=PARG(L)+SFACT*(CN(1,II)*A(1)+CN(2,II)*A(4)+CN(3,II)*A(6))
      L=L+1
      PARG(L)=PARG(L)+SFACT*(CN(2,II)*A(2)+CN(1,II)*A(4)+CN(3,II)*A(5))
      L=L+1
      PARG(L)=PARG(L)+SFACT*(CN(3,II)*A(3)+CN(2,II)*A(5)+CN(1,II)*A(6))
 1600 CONTINUE
 2000 CONTINUE
      DO 2100 I=1,NTLP
 2100 PSGL(I)=PARG(I)
C*****
C     INSERT THERMAL LOAD INTO GLOBAL LOAD VECTOR  (PG ARRAY)
C*****
C
      DO 3000 I=1,NGP
      SIL = IEST(I+1)
      IBGP = BGPDT + I - 1
      IF (IEST(IBGP) .EQ. 0) GO TO 2500
      CALL BASGLB(PSGL(3*I-2),PSGL(3*I-2),EST(BCORD+3*I-3),IEST(IBGP))
 2500 DO 2600 J=1,3
      PG(SIL+J-1)=PG(SIL+J-1)+PSGL(3*I-3+J)
 2600 CONTINUE
 3000 CONTINUE
C
C
 7300 FORMAT(6A4,69H4005. AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED UN
     2DER MATERIAL ID =,I10,17H FOR ELEMENT ID =,I10)
      RETURN
      END
