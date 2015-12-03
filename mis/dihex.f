      SUBROUTINE DIHEX (TYPE)
C
C     THIS ROUTINE PROCESSES IHEX1, IHEX2, AND IHEX3 ELEMENT DATA TO
C     PRODUCE THE DIFFERENTIAL STIFFNESS MATRIX
C
C           TYPE = 1    IHEX1
C           TYPE = 2    IHEX2
C           TYPE = 3    IHEX3
C
C     THE EST ENTRIES ARE
C
C     NAME  ----------INDEX----------   DESCRIPTION
C            IHEX1    IHEX2    IHEX3
C
C     EID        1        1        1    ELEMENT ID NO.
C     SIL      2-9     2-21     2-33    SCALAR INDEX LIST
C     MID       10       22       34    MATERIAL ID NO.
C     CID       11       23       35    MATERIAL COORD. SYSTEM ID NO.
C     NIP       12       24       36    NO. INTEGRATION POINTS PER EDGE
C     MAXAR     13       25       37    MAX ASPECT RATIO
C     ALFA      14       26       38    MAX ANGLE FOR NORMALS
C     BETA      15       27       39    MAX ANGLE FOR MIDSIDE POINTS
C     BGPDT  16-47   28-107   40-167    BASIC GRID POINT DATA
C     GPT    48-55  108-127  168-199    GRID POINT TEMPERATURES
C     DEF       56      128      200    NOT USED
C     GPTLD  57-64  129-148  201-232    GRID POINT TEMPERATURE LOADS
C     UGV    65-88  149-208  233-328    GLOBAL DISPLACEMENT VECTOR
C
      LOGICAL          ANIS       ,RECT       ,TDEP       ,DIAG       ,
     1                 MTDEP
      INTEGER          HEAT       ,EID        ,SIL(1)     ,IB(46)     ,
     1                 TYPE       ,JZ(1)      ,CID        ,IEST(1)    ,
     2                 BCORD      ,BGPDT      ,GPT        ,NC(8)      ,
     4                 UFM(6)     ,ELNO(3)    ,IWORK(1)   ,OTPT
      REAL             NU         ,MAXAR      ,DMAXAR(3)  ,DALFA(3)   ,
     1                 DBETA(2)   ,EVEC(3,12) ,WORK(66)   ,VN(3,2)
      DOUBLE PRECISION SK         ,SV         ,Z(1)       ,JACOB(3,3) ,
     1                 DETJ       ,S(4)       ,H(4)       ,GAUSS(8)   ,
     2                 SFACT      ,PART(3,3)  ,E1         ,E2         ,
     3                 E3         ,TF(3,3)    ,TK(3,3)    ,SIG(6)     ,
     4                 SX         ,SY         ,SZ         ,SXY        ,
     5                 SYZ        ,SZX        ,STR(18)    ,C(3,3)
      DOUBLE PRECISION GMAT(36)    ,STORE(18)  ,DALPHA(6)
      DIMENSION        IGRID(128) ,GRID(128)
      COMMON /MATIN/   MID        ,INFLAG     ,TEMP
      COMMON /MATOUT/  E          ,G          ,NU         ,RHO        ,
     1                 TALPHA     ,TREF       ,CDAMP      ,SPACE(18)  ,
     3                 MTDEP
      COMMON /MATISO/  BUFM6(46)
      COMMON /DS1AAA/  NPVT       ,DUM6(6)    ,I6X6K      ,DUM12(12)  ,
     1                 JMAX       ,DUM2(2)    ,NROWSC
      COMMON /ZZZZZZ/  ZS(1)
      COMMON /DS1ADP/  ISIL(32)   ,SK(6,6)    ,WORK       ,STR
      COMMON /DS1AET/  EST(328)
      COMMON /SYSTEM/  SYSBUF,OTPT,NOGO,SYS(6),MTEMP
      EQUIVALENCE      (Z(1),JZ(1),ZS(1))    ,(EID,EST(1),IEST(1))   ,
     1                 (SIL(1),EST(2))       ,(WORK(1),IWORK(1))     ,
     2                 (SIG(1),SX)           ,(SIG(2),SY)            ,
     3                 (SIG(3),SZ)           ,(SIG(4),SXY)           ,
     4                 (SIG(5),SYZ)          ,(SIG(6),SZX)
      EQUIVALENCE      (WORK(1),EVEC(1,1))   ,(WORK(37),VN(1,1))     ,
     1                 (WORK(43),NC(1))
      EQUIVALENCE      (WORK(1),JACOB(1,1))  ,(WORK(19),H(1))        ,
     1                 (WORK(27),S(1))       ,(WORK(35),PART(1,1))   ,
     2                 (WORK(53),SIG(1))     ,(WORK(1),C(1,1))
      EQUIVALENCE      (WORK(1),TF(1,1))     ,(WORK(35),TK(1,1))
      EQUIVALENCE      (IB(1),BUFM6(1))      ,(GRID(1),IGRID(1))
      DATA    KGG   /  101 /, MGG   / -1     /
      DATA    DMAXAR,  DALFA, DBETA / 5.0    ,10.0      ,15.0        ,
     2                                45.0    ,45.0      ,45.0        ,
     3                                         45.0      ,45.0        /
      DATA    DTOR  ,  GAUSS  /    0.017453292519943E0,
     1                             0.577350269189626D0,
     2                             0.555555555555556D0,
     3                             0.774596669241483D0,
     4                             0.888888888888889D0,
     5                             0.347854845137454D0,
     6                             0.861136311594053D0,
     7                             0.652145154862546D0,
     8                             0.339981043584856D0/
      DATA    UFM          /4H0***,4H USE,4HR FA,4HTAL ,4HMESS,4HAGE /
      DATA    IHEX , ELNO  /4HIHEX,4H ELE,4HMENT,4H NO./
      DATA    NERR1        / 2141 /
C
      HEAT = 0
C
C     FOR DOUBLE PRECISION, OPEN CORE POINTERS MUST BE MODIFIED
C
      IZS = 1 + 2*(I6X6K + JMAX*NROWSC)
      NZS = IZS + 10655
      IZ  = IZS/2 + 1
      NZ  = NZS/2 + 1
      IPREC = 2
C
C     ALLOCATE LARGE ARRAYS IN OPEN CORE
C
      NGP = 12*TYPE - 4
      DO 5 I = 1,NGP
      IF (SIL(I) .EQ. NPVT) GO TO 7
    5 CONTINUE
      NOGO= 1
    7 IGP = I
      IF (HEAT .EQ. 1) GO TO 30
      NGG = 3*NGP
      IF (KGG .LE. 0) GO TO 10
      IK  = IZ + 3*NGG
      NK  = IK - 1 + (NGG+1)*NGG/2
      GO TO 20
   10 IK  = IZ
      NK  = IK + 3*NGG - 1
      IM  = NK + 1
      NM  =(NGP+1)*NGP/2 + NK
      GO TO 40
   20 NM  = NK
      IF (MGG .LE. 0) GO TO 40
      IM  = NK + 1
      NM  = NK + (NGP+1)*NGP/2
      GO TO 40
   30 IK  = IZ + 17
      NK  = IK - 1 + NGP**2
      IM  = NK + 1
      NM  = IM - 1 + NGP**2
      NGG = NGP
   40 IN  = NM + 1
      IG  = IN + NGP
      IX  = IG + 3*NGP
      ND  = NM + 9*NGP
      ID  = ND + 1
      ND  = ID + NGG - 1
      IF (ND .LE. NZ) GO TO 100
      WRITE (OTPT,7100) UFM,NERR1,IHEX,TYPE,ELNO,EID
      NOGO = 1
C
C     OPEN CORE MAP
C     =============
C
C     DOUBLE PRECISION  Z(1)
C     COMMON  /ZZZZZZ/  Z
C
C     NGG = ORDER OF ELEMENT MATRIX
C
C     INDEX      STIFFNESS             MASS                HEAT
C                AND MASS              ONLY              TRANSFER
C
C     IZ    NGG BY 3 PARTITION  NGG BY 3 PARTITION  FOUR WORD COORDINATE
C           OF MATRIX           OF MATRIX           VECTOR.  INPUT TO
C                                                   TRANSD
C
C     IZ+2                                          TRANSFORMED THERMAL
C                                                   CONDUCTANCE MATRIX
C
C     IT                                            MATERIAL TRANSFOR-
C                                                   MATION MATRIX
C
C     IK    SYMMETRIC HALF OF   SAME AS IZ          FULL CONDUCTANCE
C           STIFFNESS
C
C     IM    SYMMETRIC HALF OF   SYMMETRIC HALF OF   FULL CAPACITANCE
C           MASS                MASS
C
C     IN    --------------------SHAPE FUNCTIONS-------------------------
C
C     IG    --------------------D(SHAPE)/D(GREEK)-----------------------
C
C     IX    --------------------D(SHAPE)/D(BASIC XYZ)-------------------
C
C     ID    DISPLACEMENT
C           VECTOR IN BASIC
C           COORDINATES
C
C
C     CHECK GEOMETRY.  THE FOLLOWING CHECKS ARE MADE
C           1.  ASPECT RATIO
C           2.  ANGLES BETWEEN NORMALS OF SUB-TRIANGLES ON EACH FACE
C           3.  ANGLES BETWEEN VECTORS BETWEEN POINTS ALONG EACH EDGE
C           4.  REVERSE SEQUENCING
C           5.  DUPLICATE COORDINATE VALUES
C
C     FETCH EPT DATA, COMPUTE EST POINTERS
C
  100 MID  = 10 + 12*(TYPE-1)
      CID  = IEST(MID+1)
      NIP  = IEST(MID+2)
      MAXAR= EST(MID+3)
      ALFA = EST(MID+4)
      BETA = EST(MID+5)
      BGPDT= MID + 6
      GPT  = BGPDT + 4*NGP
      MID  = IEST(MID)
      IF (NIP.LT.2 .OR. NIP.GT.4) NIP = TYPE/2 + 2
      IF (MAXAR .LE. 0.0) MAXAR = DMAXAR(TYPE)
      IF (ALFA  .LT. 0.0) ALFA  = DALFA(TYPE)
      IF (BETA  .LT. 0.0) BETA  = DBETA(TYPE-1)
      ALFA = COS(DTOR*ALFA)
      BETA = COS(DTOR*BETA)
C
C     TRANSFORM DISPLACEMENT VECTOR TO BASIC COORDINATES
C
      DO 104 I = 1,NGP
      M  = BGPDT + 4*I - 4
      N  = ID + 3*I - 3
      J  = GPT + 2*NGP + 3*(I-1) + 1
      IF (IEST(M) .EQ. 0) GO TO 102
      CALL TRANSD (EST(M) ,Z(IZ))
      DO 101 L = 1,3
  101 Z(IK+L-1) = DBLE(EST(J+L-1)*0.25)
      CALL GMMATD (Z(IZ),3,3,0,Z(IK),3,1,0,Z(N))
      GO TO 104
  102 DO 103 L = 1,3
  103 Z(N+L-1) = DBLE(EST(J+L-1)*0.25)
  104 CONTINUE
C
C     REARRANGE BGPDT
C
      DO 110 I = 1,NGP
  110 IGRID(I) = IEST(BGPDT+4*I-4)
      BCORD = GPT - 3
      DO 120 I = 2,NGP
      DO 120 J = 1,3
      K = BGPDT + 4*(NGP-I) + 4 - J
      BCORD = BCORD - 1
      EST(BCORD) = EST(K)
  120 CONTINUE
      DO 130 I = 2,NGP
  130 IEST(BGPDT+I-1) = IGRID(I)
C
C     INITIALIZE FOR NUMERICAL INTEGRATION
C
C
C     ABSCISSAE AND WEIGHT COEFFICIENTS FOR GAUSSIAN QUADRATURE
C
      I = NIP - 1
      GO TO (510,520,530), I
  510 H(1) = 1.0
      S(1) = GAUSS(1)
      H(2) = 1.0
      S(2) =-GAUSS(1)
      GO TO 540
  520 H(1) = GAUSS(2)
      S(1) = GAUSS(3)
      H(2) = GAUSS(4)
      S(2) = 0.0
      H(3) = GAUSS(2)
      S(3) =-GAUSS(3)
      GO TO 540
  530 H(1) = GAUSS(5)
      S(1) = GAUSS(6)
      H(2) = GAUSS(7)
      S(2) = GAUSS(8)
      H(3) = GAUSS(7)
      S(3) =-GAUSS(8)
      H(4) = GAUSS(5)
      S(4) =-GAUSS(6)
C
C     GENERATE TABLE OF EQUIVALENTS IN SIL ARRAY SO MATRIX WILL BE
C     ORDERED ACCORDING TO INCREASING SIL NUMBERS
C
  540 DO 550 I = 1,NGP
      ISIL(I) = SIL(I)
  550 SIL(I) = I
C
C     NOW SIL(I) = PARTITION NUMBER OF ELEMENT GRID POINT I
C
C     ZERO OUT OPEN CORE FOR MATRIX SUMMATION
C
      DO 580 I = IK,NM
  580 Z(I) = 0.0
C
C     FETCH MATERIAL PROPERTIES
C
C     THIS SECTION OF CODE MUST BE UPDATED WHEN GENERAL ANISOTROPIC
C     MATERIAL IS ADDED.
C
C     TEST FOR ANISOTROPIC MATERIAL
C
      ANIS = .FALSE.
C
C     TEST FOR RECTANGULAR COORDINATE SYSTEM IN WHICH THE ANISOTROPIC
C     MATERIAL IS DEFINED
C
      RECT = .TRUE.
C
C     CHECK FOR TEMPERATURE DEPENDENCE
C
      TDEP = .TRUE.
      DO 610 I = 2,NGP
      IF (EST(GPT) .NE. EST(GPT+I-1)) GO TO 630
  610 CONTINUE
      TDEP = .FALSE.
  630 TEMP = EST(GPT)
      INFLAG = 10
      CALL MAT (EID)
      IF (.NOT.MTDEP) TDEP = .FALSE.
      IF (IB(46) .EQ. 6) ANIS = .TRUE.
      TREF = BUFM6(44)
C
      IF (KGG .LE. 0) GO TO 1000
C
C     IF ISOTROPIC, TEMPERATURE INDEPENDENT MATERIAL, COMPUTE CONSTANTS
C
      IF (TDEP) GO TO 1000
      IF (ANIS) GO TO 800
      IF (IB(46) .NE. 0) GO TO 640
      WRITE (OTPT,7300) UFM,MID,EID
      NOGO = 1
      RETURN
C
  640 E1 = BUFM6(1)
      E2 = BUFM6(2)
      E3 = BUFM6(22)
      TALPHA = BUFM6(38)
      GO TO 1000
C
C     IF MATERIAL IS ANISOTROPIC, DEFINED IN A RECTANGULAR COORDINATE
C     SYSTEM, AND NOT TEMPERATURE DEPENDENT, TRANSFORM IT TO BASIC
C     SYSTEM
C
  800 DO 810 IJK = 1,36
  810 GMAT(IJK) = BUFM6(IJK)
C
C     CODE TO TRANSFORM GENERAL ANISOTROPIC MATERIAL PROPERTIES TO
C     BASIC COORDINATE SYSTEM MUST BE ADDED HERE.
C
C     ALL SET TO BEGIN INTEGRATION LOOPS.  DO IT.
C
 1000 DO 2000 I = 1,NIP
      DO 2000 J = 1,NIP
      DO 2000 K = 1,NIP
C
C     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE
C
      CALL IHEXSD (TYPE,Z(IN),Z(IG),JACOB,DETJ,EID,S(I),S(J),S(K),
     1             EST(BCORD))
      IF (DETJ .NE. 0.0) GO TO 1010
C
C     BAD ELEMENT IF FALL HERE.  JACOBIAN MATRIX WAS SINGULAR.
C
      NOGO = 1
      RETURN
C
 1010 SFACT = H(I)*H(J)*H(K)*DETJ
      IF (KGG .LE. 0) GO TO 1015
C
C     STIFFNESS
C
C     COMPUTE STRAIN-DISPLACEMENT RELATIONS
C
C     MUST REVERSE CALLING ORDER SINCE MATRICES ARE STORED BY COLUMNS
C
      CALL GMMATD (Z(IG),NGP,3,0,JACOB,3,3,0,Z(IX))
C
C     IF MATERIAL IS TEMPERATURE DEPENDENT, MUST COMPUTE TEMPERATURE
C     AT THIS INTEGRATION POINT AND FETCH MATERIAL PROPERTIES AGAIN
C
 1015 IF (.NOT.TDEP) GO TO 1030
      TEMP = 0.0
      DO 1020 L = 1,NGP
 1020 TEMP = TEMP + Z(IN+L-1)*EST(GPT+L-1)
      CALL MAT (EID)
      IF (KGG .LE. 0) GO TO 1100
      IF (ANIS) GO TO 1040
      IF (IB(46) .NE. 0 ) GO TO 1025
      WRITE (OTPT,7300) UFM,MID,EID
      NOGO = 1
      RETURN
C
 1025 E1 = BUFM6(1)
      E2 = BUFM6(2)
      E3 = BUFM6(22)
      TALPHA = BUFM6(38)
      GO TO 1100
 1030 IF (KGG .LE. 0) GO TO 1100
C
C     IF MATERIAL IS ANISOTROPIC AND NOT DEFINED IN RECTANGULAR COOR-
C     DINATE SYSTEM, MUST TRANSFORM TO BASIC COORDINATE SYSTEM AT THIS
C     INTEGRATION POINT
C
C     THIS CODE MUST BE COMPLETED WHEN GENERAL ANISOTROPIC MATERIAL IS
C     ADDED
C
      IF (.NOT.ANIS) GO TO 1100
      IF (RECT) GO TO 1100
 1040 CONTINUE
C
C     INSERT GLOBAL TO BASIC TRANSFORMATION OPERATIONS HERE FOR
C     ANISOTROPIC MATERIAL MATRIX
C
      DO 1041 IJK = 1,36
 1041 GMAT(IJK) = BUFM6(IJK)
C
C     MATERIAL HAS BEEN EVALUATED FOR THIS INTEGRATION POINT WHEN
C     FALL HERE.
C
 1100 CONTINUE
C
C     COMPUTE STRESSES FOR DIFFERENTIAL STIFFNESS MATRIX
C
C     THERMAL EFFECTS
C
      IF (IEST(GPT+NGP+1) .EQ. -1) GO TO 1120
C
C     COMPUTE LOADING TEMPERATURE AT THIS POINT
C
      TEMP = 0.0
      DO 1110 L = 1,NGP
 1110 TEMP = TEMP + Z(IN+L-1)*EST(GPT+NGP+L)
      TEMP = 0.25*(TEMP-TREF)
      IF (ANIS) GO TO 1115
      SIG(1) =-TALPHA*(E1+2.0*E2)*TEMP
      SIG(2) = SIG(1)
      SIG(3) = SIG(1)
      SIG(4) = 0.0
      SIG(5) = 0.0
      SIG(6) = 0.0
      GO TO 1140
C
C     ANISOTROPIC
C
 1115 DO 1116 IJK = 1,6
 1116 DALPHA(IJK) = BUFM6(IJK+37)
C
      CALL GMMATD (GMAT(1),6,6,0,DALPHA(1),6,1,0,SIG)
      DO 1117 IJK = 1,6
 1117 SIG(IJK) = -SIG(IJK)*TEMP
      GO TO 1140
 1120 DO 1130 L = 1,6
 1130 SIG(L) = 0.0
C
C     DISPLACEMENT EFFECTS, COMPUTE STRESS MATRIX AND MULTIPLY BY DISPL.
C
 1140 STR(12) = 0.0
      STR(13) = 0.0
      STR(17) = 0.0
      DO 1150 L = 1,NGP
      II = IX + 3*L - 4
      IF (ANIS) GO TO 1141
      STR( 1) = E1*Z(II+1)
      STR( 2) = E2*Z(II+2)
      STR( 3) = E2*Z(II+3)
      STR( 4) = E2*Z(II+1)
      STR( 5) = E1*Z(II+2)
      STR( 6) = E2*Z(II+3)
      STR( 7) = E2*Z(II+1)
      STR( 8) = E2*Z(II+2)
      STR( 9) = E1*Z(II+3)
      STR(10) = E3*Z(II+2)
      STR(11) = E3*Z(II+1)
      STR(14) = E3*Z(II+3)
      STR(15) = E3*Z(II+2)
      STR(16) = E3*Z(II+3)
      STR(18) = E3*Z(II+1)
      GO TO 1145
C
C     ANISOTROPIC
C
 1141 DO 1142 IJK = 1,18
 1142 STORE(IJK) = 0.D0
      STORE( 1) = Z(II+1)
      STORE( 5) = Z(II+2)
      STORE( 9) = Z(II+3)
      STORE(10) = Z(II+2)
      STORE(11) = Z(II+1)
      STORE(14) = Z(II+3)
      STORE(15) = Z(II+2)
      STORE(16) = Z(II+3)
      STORE(18) = Z(II+1)
C
      CALL GMMATD (GMAT(1),6,6,0,STORE(1),6,3,0,STR(1))
C
 1145 CONTINUE
      CALL GMMATD (STR,6,3,-2,Z(ID+3*L-3),3,1,0,SIG)
 1150 CONTINUE
      SV = SX
      SX = SX + SY
      SY = SY + SZ
      SZ = SZ + SV
C
C     NOW BEGIN LOOPS OVER GRID POINTS ALONG ROWS AND COLUMNS
C
      DO 1400 N = 1,NGP
      DO 1400 M = N,NGP
      IF (N.EQ.IGP .OR. M.EQ.IGP) GO TO 1170
      GO TO 1400
 1170 CONTINUE
C
C     COMPUTE PARTITION FOR POINTWISE ROW M AND COLUMN N
C
      IF (KGG .LE. 0) GO TO 1300
      IF (.NOT.ANIS ) GO TO 1200
C
C     MUST ADD CODE TO COMPUTE THE CONTRIBUTION TO THE STIFFNESS MATRIX
C     FOR ANISOTROPIC MATERIAL HERE
C     =================================================================
C
 1200 IF (SIL(M) .GE. SIL(N)) GO TO 1210
C
C     MUST COMPUTE TRANSPOSE OF THIS PARTITION FOR SUMMATION IN ELEMENT
C     MATRIX
C
      MZ = IX + (N-1)*3
      NZ = IX + (M-1)*3
      GO TO 1220
 1210 MZ = IX + (M-1)*3
      NZ = IX + (N-1)*3
C
C     DIFFERENTIAL STIFFNESS
C
 1220 DO 1223 L = 1,3
      DO 1223 INC = 1,3
 1223 C(L,INC)  = Z(MZ+INC-1)*Z(NZ+L-1)
      PART(1,1) = SX*C(2,2) + SYZ*(C(2,3) + C(3,2)) + SZ*C(3,3)
      PART(2,2) = SY*C(3,3) + SZX*(C(3,1) + C(1,3)) + SX*C(1,1)
      PART(3,3) = SZ*C(1,1) + SXY*(C(1,2) + C(2,1)) + SY*C(2,2)
      PART(2,1) =-SX*C(2,1) + SXY*C(3,3) - SYZ*C(1,3) - SZX*C(2,3)
      PART(3,1) =-SZ*C(3,1) - SXY*C(3,2) - SYZ*C(2,1) + SZX*C(2,2)
      PART(1,2) =-SX*C(1,2) + SXY*C(3,3) - SYZ*C(3,1) - SZX*C(3,2)
      PART(3,2) =-SY*C(3,2) - SXY*C(3,1) + SYZ*C(1,1) - SZX*C(1,2)
      PART(1,3) =-SZ*C(1,3) - SXY*C(2,3) - SYZ*C(1,2) + SZX*C(2,2)
      PART(2,3) =-SY*C(2,3) - SXY*C(1,3) + SYZ*C(1,1) - SZX*C(2,1)
C
C     ADD STIFFNESS PARTITION TO ELEMENT MATRIX
C
C     COMPUTE INDEX INTO OPEN CORE WHERE PART(1,1) IS TO BE ADDED.
C
      IF (SIL(M)-SIL(N)) 1230,1240,1250
 1230 MZ = SIL(N)
      NZ = SIL(M)
      DIAG = .FALSE.
      GO TO 1260
 1240 MZ = SIL(M)
      NZ = SIL(N)
      DIAG = .TRUE.
      GO TO 1260
 1250 MZ = SIL(M)
      NZ = SIL(N)
      DIAG = .FALSE.
C
C     COLUMN NUMBER
C
 1260 L = (NZ-1)*3 + 1
C
C     INCREMENT BETWEEN COLUMNS
C
      INC = NGG - L
C
C     FIRST WORD OF COLUMN
C
      L = IK + ((L-1)*L)/2 + (INC+1)*(L-1)
C
C     WORD IN COLUMN FOR THIS ROW
C
      L = L + 3*(MZ-NZ)
C
C     ADD PARTITION
C
      DO 1280 NZ = 1,3
      DO 1270 MZ = 1,3
      IF (DIAG .AND. MZ.LT.NZ) GO TO 1270
      Z(L+MZ-1) = Z(L+MZ-1) + PART(MZ,NZ)*SFACT
 1270 CONTINUE
      L   = L + INC
      INC = INC - 1
 1280 CONTINUE
 1300 IF (MGG .LE. 0) GO TO 1400
 1400 CONTINUE
 2000 CONTINUE
C
C     END OF INTEGRATION LOOPS
C
C
C     LOOK FOR NON-BASIC COORDINATE SYSTEM
C
      DO 2003 I = 1,NGP
      IF (IEST(BGPDT+I-1) .NE. 0) GO TO 2005
 2003 CONTINUE
      GO TO 2061
C
C     RESTORE GRID POINT DATA TO ORIGINAL FORM FOR DOING TRANSFORM
C     TO GLOBAL COORDINATES
C
C     FIRST, TRANSFER IT TO OPEN CORE AT IN
C
 2005 K = (IN-1)*2 + 1
      J = NGP*4
      DO 2010 I = 1,J
 2010 GRID(I) = EST(BGPDT+I-1)
C
C     NOW MOVE IT BACK AND REARRANGE IT
C
      DO 2020 I = 1,NGP
      IEST(BGPDT+4*I-4) = IGRID(I)
      DO 2020 J = 1,3
      EST(BGPDT+4*I-4+J) = GRID(NGP+3*I+J-3)
 2020 CONTINUE
C
C     FETCH GLOBAL TO BASIC TRANSFORMATION MATRICES
C
      DO 2025 I = 1,NGP
      J = IN + (I-1)*9
      CALL TRANSD (EST(BGPDT+4*I-4),Z(J))
 2025 CONTINUE
C
C     TRANSFORM STIFFNESS TO GLOBAL COORDINATES
C
      DO 2060 I = 1,NGP
C
C     COLUMN INDICES
C
      K   = (I-1)*3 + 1
      INC = NGG - K + 1
      L   = IK + ((K-1)*K)/2 + INC*(K-1)
      M   = L + INC
      N   = M + INC - 1
C
C     TRANSFORMATION MATRIX INDEX
C
      NZ  = IN + (I-1)*9
C
C     TERMS ON DIAGONAL PARTITION
C
      CALL TKTZTK (TK,Z,NZ,L,M,N)
C
C     OFF-DIAGONAL PARTITIONS
C
      L   = L + 3
      M   = M + 2
      N   = N + 1
      IRP = I + 1
      IF (IRP .GT. NGP) GO TO 2060
      DO 2050 J = IRP,NGP
      NZ  = IN + 9*(J-1)
      DO 2030 K = 1,3
      TK(K,1) = 0.0
      TK(K,2) = 0.0
      TK(K,3) = 0.0
      DO 2030 MZ = 1,3
      TK(K,1) = TK(K,1) + Z(L+MZ-1)*Z(NZ+3*MZ+K-4)
      TK(K,2) = TK(K,2) + Z(M+MZ-1)*Z(NZ+3*MZ+K-4)
      TK(K,3) = TK(K,3) + Z(N+MZ-1)*Z(NZ+3*MZ+K-4)
 2030 CONTINUE
      MZ = IN + 9*(I-1)
      DO 2040 K = 1,3
      Z(L+K-1) = 0.0
      Z(M+K-1) = 0.0
      Z(N+K-1) = 0.0
      DO 2040 II = 1,3
      Z(L+K-1) = Z(L+K-1) + TK(K,II)*Z(MZ+3*II-3)
      Z(M+K-1) = Z(M+K-1) + TK(K,II)*Z(MZ+3*II-2)
      Z(N+K-1) = Z(N+K-1) + TK(K,II)*Z(MZ+3*II-1)
 2040 CONTINUE
      L = L + 3
      M = M + 3
      N = N + 3
 2050 CONTINUE
 2060 CONTINUE
C
C     BUILD STIFFNESS PARTITIONS AND PASS TO EMGOUT
C
 2061 DO 2065 I = 1,36
 2065 SK(I,1) = 0.0D0
      I = IGP
      DO 2090 J = 1,3
C
C     COLUMN NUMBER
C
      K = (I-1)*3 + J
C
C     NUMBER OF TERMS TO FETCH TO COMPLETE THIS COLUMN IN PARTITION
C
      L = K - 1
      IF (L .EQ. 0) GO TO 2075
C
C     FETCH TERMS AND LOAD INTO J-TH COLUMN OF PARTITION
C
      N   = IK + L
      INC = NGG - 1
      DO 2070 M = 1,L
      Z(IZ+NGG*J-NGG+M-1) = Z(N)
      N   = N + INC
      INC = INC - 1
 2070 CONTINUE
C
C     FILL OUT PARTITION WITH COLUMNS OF STIFFNESS MATRIX
C
C     COMPUTE INDEX IN OPEN CORE OF FIRST TERM OF COLUMN K
C
 2075 N = IK + ((K-1)*K)/2 + (NGG-K+1)*(K-1)
C
C     INSERT THIS COLUMN IN PARTITION
C
      DO 2080 M = K,NGG
      Z(IZ+NGG*J-NGG+M-1) = Z(N)
      N = N + 1
 2080 CONTINUE
 2090 CONTINUE
      DO 2100 I = 1,NGP
      J = (I-1)*3 + IZ - 1
      DO 2095 M = 1,3
      DO 2095 N = 1,3
      K = J + N + (M-1)*NGG
 2095 SK(N,M) = Z(K)
      CALL DS1B (SK,ISIL(I))
 2100 CONTINUE
C
C     ALL DONE, NO ERRORS
C
      RETURN
C
C
 7100 FORMAT (6A4,I4,2H, ,A4,I1,3A4,I9,' INSUFFICIENT CORE TO COMPUTE',
     1       ' ELEMENT MATRIX')
 7300 FORMAT (6A4,'4005. AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED ',
     1       'UNDER MATERIAL ID =',I10,17H FOR ELEMENT ID =,I10)
C
      END
