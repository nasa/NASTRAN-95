      SUBROUTINE ESTMAG (HEST,ESTFLD,MPT,DIT,GEOM1,IANY,KCOUNT)
C
C     CREATE SCRATCH FILE ESTFLD WHICH WILL BE USED TO COMPUTE TOTAL
C     MAGNETIC FIELD. READ EST AND CREATE SIMILAR RECORDS CONTAINING
C     ELTYPE,EID,NUMBER OF SILS,SILS,3 X 3 MATERIALS MATRIX, AND 3 X 3
C     TRANSFORMATION MATRIX TO BRING HM BACK TO BASIC COORD. SYSTEM
C     FROM ELEMENT SYSTEM,OUTPUT COORD. SYSTEM ID, AND BASIC COORDS.
C     OF STRESS POINT(USUALLY AVERAGE OF GRID COORDS.)
C
      INTEGER         HEST,ESTFLD,SYSBUF,ELTYPE,POINTR(6,20),MCB(7),
     1                BUF1,BUF2,IZ(1),NAM(2),ESTWDS,FRSTGD,OTPE,DIT,
     2                BFIELD(2),OLDEID,BUF3,FILE,GEOM1
      DIMENSION       DN(8),XM(32),COORD(3),KOUNT(2),NAME(2),V12(3),
     1                V13(3),XI(3),XJ(3),XK(3),ECPT(200),IECPT(200),
     2                E(9),G(9)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /GPTA1 / NELEMS,LAST,INCR,NE(1)
      COMMON /SYSTEM/ SYSBUF,OTPE
      COMMON /ZZZZZZ/ Z(1)
      COMMON /HMATDD/ IIHMAT,NNHMAT,MPTFIL,IDITFL
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /HMTOUT/ XMAT(6)
      EQUIVALENCE     (Z(1),IZ(1)),(ECPT(1),IECPT(1))
      DATA    NAM   / 4HESTM,4HAG  /
      DATA    BFIELD/ 3101,31      /
      DATA    DN    / 4*-.25, 4*.5 /
C
C     ITYPE ITH MID ISYS1 ITEMP DIM
C
      DATA POINTR/   1,   0,  4,   9,   17,   1,
     1               3,   0,  4,   8,   16,   1,
     2               6,   5,  6,  15,   27,   2,
     3               9,   5,  6,   9,   21,   2,
     4              10,   0,  4,   9,   17,   1,
     5              16,   6,  7,  10,   26,   2,
     6              17,   5,  6,   9,   21,   2,
     7              18,   6,  7,  10,   26,   2,
     8              19,   6,  7,  16,   32,   2,
     9              34,   0, 16,  34,   42,   1,
     1              36,   5,  6,   7,   19,   2,
     2              37,   6,  7,   8,   24,   2,
     3              39,   0,  2,   7,   23,   3,
     3              40,   0,  2,   9,   33,   3,
     4              41,   0,  2,  11,   43,   3,
     5              42,   0,  2,  11,   43,   3,
     6              65,   0, 10,  16,   48,   3,
     6              66,   0, 22,  28,  108,   3,
     7              67,   0, 34,  40,  168,   3,
     8              80,  11, 12,  14,   46,   2     /
C
      LCORE = KORSZ(Z)
      BUF1  = LCORE - SYSBUF + 1
      BUF2  = BUF1  - SYSBUF
      BUF3  = BUF2  - SYSBUF - 1
      LCORE = BUF3  - 1
      IF (LCORE .LE. 0) GO TO 1008
C
C     COUNT NUMBER OF TERMS IN A COLUMN OF HCCEN
C
      KCOUNT = 0
C
C     SET UP MATERIALS
C
      IIHMAT = 0
      NNHMAT = LCORE
      MPTFIL = MPT
      IDITFL = DIT
      CALL PREHMA (Z)
      NEXTZ  = NNHMAT + 1
C
      CALL GOPEN (HEST,Z(BUF1),0)
      CALL GOPEN (ESTFLD,Z(BUF2),1)
C
C     READ IN ANY BFIELD CARDS
C
      IANY   = 0
      IALL   = 1
      IFIELD = 0
      IDEFID = 0
      NFIELD = 0
      FILE   = GEOM1
      CALL PRELOC (*1001,Z(BUF3),GEOM1)
      CALL LOCATE (*3,Z(BUF3),BFIELD,IDEX)
      IANY   = 1
      CALL READ (*1002,*1,GEOM1,Z(NEXTZ+1),LCORE-NEXTZ,0,IWORDS)
      GO TO 1008
    1 NFIELD = IWORDS/2
      IF (NFIELD.NE.1 .OR. IZ(NEXTZ+2).NE.-1) GO TO 2
      IFIELD = IZ(NEXTZ+1)
      IDEFID = IFIELD
      GO TO 3
C
C     BFIELD ARE NOT THE SAME FOR EVERY ELEMENT
C
    2 IALL = 0
    3 CALL CLOSE (GEOM1,1)
C
C     CHECK FOR ALL SO THAT CSTM WONT BE OPENED
C
      IF (NFIELD .EQ. 0) GO TO 8
      DO 4 I = 1,IWORDS,2
      IF (IZ(NEXTZ+I) .NE. 0) GO TO 5
    4 CONTINUE
      IANY = 0
      IALL = 1
      IFIELD = 0
    5 CONTINUE
C
C     CHECK FOR A DEFAULT ID
C
      DO 6 I = 2,IWORDS,2
      IF (IZ(NEXTZ+I) .EQ. -1) GO TO 7
    6 CONTINUE
      GO TO 8
    7 IDEFID = IZ(NEXTZ+I-1)
    8 CONTINUE
      FILE = HEST
C
   10 CALL READ (*120,*1003,HEST,ELTYPE,1,0,IFLAG)
      CALL WRITE (ESTFLD,ELTYPE,1,0)
      OLDEID = 0
      ICOUNT = 0
      IDX    = (ELTYPE-1)*INCR
      ESTWDS = NE(IDX+12)
      NGRIDS = NE(IDX+10)
      FRSTGD = 2
      IF (ELTYPE.GE.39 .AND. ELTYPE.LE.42) FRSTGD = 3
      NAME(1) = NE(IDX+1)
      NAME(2) = NE(IDX+2)
C
C     PICK UP MATERIAL ID, START OF BGPDT DATA, AND DIMENSIONALITY OF
C     ELEMENT
C
      DO 20 I = 1,20
      JEL = I
      IF (ELTYPE-POINTR(1,I)) 500,30,20
   20 CONTINUE
      GO TO 500
C
   30 ITH   = POINTR(2,JEL)
      MID   = POINTR(3,JEL)
      ISYS1 = POINTR(4,JEL)
      ISYS2 = ISYS1 + 4
      ISYS3 = ISYS2 + 4
C
C     FOR IS2D8, USE 4TH POINT FOR GEOMETRY SINCE THAT IS WHAT WE USE
C     FOR IS2D8 ELSEWHERE
C
      IF (ELTYPE .EQ. 80) ISYS3 = ISYS3 + 4
      ITEMP = POINTR(5,JEL)
      IDIM  = POINTR(6,JEL)
C
   40 CALL READ (*1002,*110,HEST,ECPT,ESTWDS,0,IFLAG)
      IF (ELTYPE .LT. 65) KCOUNT = KCOUNT + 3
      IF (ELTYPE .EQ. 65) KCOUNT = KCOUNT + 27
      IF (ELTYPE.EQ.66 .OR. ELTYPE.EQ.67) KCOUNT = KCOUNT + 63
      IF (ELTYPE .EQ. 80) KCOUNT = KCOUNT + 27
C
C     FIND BFIELD FOR THIS ELEMENT
C
      IF (IALL .EQ. 1) GO TO 47
      DO 45 I = 2,IWORDS,2
      IF (IECPT(1) .EQ. IZ(NEXTZ+I)) GO TO 46
   45 CONTINUE
      IFIELD = IDEFID
      GO TO 47
   46 IFIELD = IZ(NEXTZ+I-1)
   47 CONTINUE
C
C     WRITE EID, SILS
C
      CALL WRITE (ESTFLD,IECPT(1),1,0)
      CALL WRITE (ESTFLD,NGRIDS,1,0)
      CALL WRITE (ESTFLD,IECPT(FRSTGD),NGRIDS,0)
C
C     FETCH MATERIALS
C
      MATID = IECPT(MID)
      SINTH = 0.
      COSTH = 0.
C***
C    ASSUME HERE THAT FOR ISOPARAMETRICS WE HAVE TEMPERATURE-INDEPENDENT
C    MATERIALS IN THIS MAGNETICS PROBLEM
C***
      ELTEMP = ECPT(ITEMP)
      INFLAG = 3
      CALL HMAT (IECPT(1))
      G(1) = XMAT(1)
      G(2) = XMAT(2)
      G(3) = XMAT(3)
      G(4) = XMAT(2)
      G(5) = XMAT(4)
      G(6) = XMAT(5)
      G(7) = XMAT(3)
      G(8) = XMAT(5)
      G(9) = XMAT(6)
C
C     NOW CREATE TRANSFORMATION MATRIX FROM LOACL COORDS TO BASIC
C     DETERMINE DIMENSIONALITY OF ELEMENT
C
      GO TO (50,70,90), IDIM
C
C     ONE-DIMENSIONAL-DETERMINE THE LOCAL X-AXIS(IN BASIC COORDS)
C
   50 DO 55 I = 1,3
   55 V12(I) = ECPT(ISYS2+I) - ECPT(ISYS1+I)
      XLEN   = SQRT(V12(1)**2 + V12(2)**2 + V12(3)**2)
      DO 60 I = 1,3
   60 V12(I) = V12(I)/XLEN
C
      DO 65 I = 1,9
   65 E(I) = 0.
      E(1) = V12(1)
      E(4) = V12(2)
      E(7) = V12(3)
      GO TO 100
C
C     TWO-DIMENSIONAL  WE WILL USE ONLY GRIDS 1,2,3 ASSUMING A PLANAR
C     OR NEARLY PLANAR ELEMENT FOR QUADS
C
   70 DO 75 I = 1,3
      V12(I) = ECPT(ISYS2+I) - ECPT(ISYS1+I)
      V13(I) = ECPT(ISYS3+I) - ECPT(ISYS1+I)
   75 CONTINUE
      XLEN  = SQRT(V12(1)**2 + V12(2)**2 + V12(3)**2)
      DO 78 I = 1,3
   78 XI(I) = V12(I)/XLEN
C
      XK(1) = XI(2)*V13(3) - XI(3)*V13(2)
      XK(2) = XI(3)*V13(1) - XI(1)*V13(3)
      XK(3) = XI(1)*V13(2) - XI(2)*V13(1)
      XLEN  = SQRT(XK(1)**2 + XK(2)**2 + XK(3)**2)
      DO 80 I = 1,3
   80 XK(I) = XK(I)/XLEN
C
      XJ(1) = XK(2)*XI(3) - XK(3)*XI(2)
      XJ(2) = XK(3)*XI(1) - XK(1)*XI(3)
      XJ(3) = XK(1)*XI(2) - XK(2)*XI(1)
      XLEN  = SQRT(XJ(1)**2 + XJ(2)**2 + XJ(3)**2)
      DO 85 I = 1,3
   85 XJ(I) = XJ(I)/XLEN
C
      DO 86 I = 1,3
      E(3*I-2) = XI(I)
      E(3*I-1) = XJ(I)
      E(3*I  ) = XK(I)
   86 CONTINUE
C
C     CHECK ON MATERIALS AS IN EMRING
C
      ANGLE = ECPT(ITH)*0.017453293
      IF (XMAT(3).EQ.0. .AND. XMAT(5).EQ.0.) GO TO 87
      GO TO 100
   87 IF (ABS(ANGLE) .LE. .0001) GO TO 100
      DO 88 I = 1,9
   88 G(I) = 0.
      S    = SIN(ANGLE)
      C    = COS(ANGLE)
      CSQ  = C*C
      SSQ  = S*S
      CS   = C*S
      X2   = 2.*CS*XMAT(2)
      G(1) = CSQ*XMAT(1) - X2 + SSQ*XMAT(4)
      G(2) = CS*(XMAT(1) - XMAT(4)) + (CSQ-SSQ)*XMAT(2)
      G(5) = SSQ*XMAT(1) + X2 + CSQ*XMAT(4)
      G(4) = G(2)
      G(9) = XMAT(6)
C
      IF (ELTYPE.NE.36 .AND. ELTYPE.NE.37) GO TO 100
C
C     SINCE MAT5 INFO FOR TRAPRG,TRIARG MUST BE GIVEN IN X-Y TERMS,
C     RE-ORDER THE 3 X 3 , INTERCHANGING Y AND Z
C
      TEMP = G(5)
      G(5) = G(9)
      G(9) = TEMP
      TEMP = G(2)
      G(2) = G(3)
      G(3) = TEMP
      G(4) = G(2)
      G(7) = G(3)
      GO TO 100
C
C     THREE-DIMENSIONAL-NO ELEMENT COORDINATE SYSTEM-EVERYTHING IS
C     OUTPUT IN BASIC- SO E IS IDENTITY
C
   90 DO 95 I = 1,9
   95 E(I) = 0.
      E(1) = 1.
      E(5) = 1.
      E(9) = 1.
C
  100 CALL WRITE (ESTFLD,G,9,0)
      CALL WRITE (ESTFLD,E,9,0)
      IF (ELTYPE.GE.65 .AND. ELTYPE.LE.67) GO TO 104
C
C     COMPUTE THE AVERAGE COORDINATES OF THE GRID POINTS OF THE ELEMENT
C     FOR USE IN NON-RECTANGULAR COORDIANTE SYSTEMS. THIS POINT IS NOT
C     NECESSARILY THE CENTROID,BUT ANY POINT WILL DO FOR CONSTANT STRAIN
C     ELEMENTS AND THIS IS CONVENIENT
C
      IF (ELTYPE .NE. 80) GO TO 1013
C
C     FOR IS2D8 USE SHAPE FUNCTION
C
      DO 1012 I = 1,3
      COORD(I) = 0.
      DO 1012 J = 1,8
      JSUB = ISYS1 + 4*(J-1)
      COORD(I) = COORD(I) + DN(J)*ECPT(JSUB+I)
 1012 CONTINUE
      GO TO 108
 1013 CONTINUE
      DO 102 I = 1,3
      COORD(I) = 0.
      DO 102 J = 1,NGRIDS
      JSUB = ISYS1 + 4*(J-1)
      COORD(I) = COORD(I) + ECPT(JSUB+I)
  102 CONTINUE
      DO 103 I = 1,3
  103 COORD(I) = COORD(I)/FLOAT(NGRIDS)
      GO TO 108
C
C     ISOPARAMETRICS-PICK UP COORDS. OF APPLICABLE POINT. FOR THE LAST
C     POINT, GO TO THE PREVIOUS METHOD
C
  104 IF (IECPT(1) .EQ. OLDEID) GO TO 105
      OLDEID = IECPT(1)
  105 ICOUNT = ICOUNT + 1
      IF (ELTYPE.EQ.65 .AND. ICOUNT.LT. 9) GO TO 106
      IF (ELTYPE.GT.65 .AND. ICOUNT.LT.21) GO TO 106
C
C     CENTROIDAL POINT-COMPUTE COORDS BASED ON XI=ETA=ZETA=0
C
      ICOUNT = 0
      OLDEID = 0
      IF (ELTYPE .NE. 65) GO TO 1051
      DO 1050 I = 1,8
 1050 XM(I) = .125
      GO TO 1057
 1051 IF (ELTYPE .NE. 66) GO TO 1054
      DO 1052 I = 1,20
 1052 XM(I) = .25
      DO 1053 I = 1,7,2
      XM(I) =-.25
 1053 XM(I+12) =-.25
      GO TO 1057
 1054 CON1 =  9./64.
      CON2 =-19./64.
      DO 1055 I = 1,32
 1055 XM(I) = CON1
      DO 1056 I = 1,10,3
      XM(I) = CON2
 1056 XM(I+20) = CON2
C
 1057 DO 1058 I = 1,3
      COORD(I) = 0.
      DO 1058 J = 1,NGRIDS
      JSUB = ISYS1 + 4*(J-1)
      COORD(I) = COORD(I) + ECPT(JSUB+I)*XM(J)
 1058 CONTINUE
      GO TO 109
  106 IF (ELTYPE.EQ.67 .AND. ICOUNT.LT.21) GO TO 1071
      JSUB = ISYS1 + 4*(ICOUNT-1)
      DO 107 I = 1,3
  107 COORD(I) = ECPT(JSUB+I)
      IF (ICOUNT .GT. 1) GO TO 109
      GO TO 108
C
C     FOR IHEX3, MUST GET PROPER COORDINATES IF NOT THE LAST POINT
C
 1071 IF (ICOUNT.GE.9 .AND. ICOUNT.LE.12) GO TO 1072
      IF ((ICOUNT/2)*2 .EQ. ICOUNT) GO TO 1073
C
C     CORNERS
C
      IF (ICOUNT.EQ.1 .OR. ICOUNT.EQ.13) JCOUNT =-1
      IADD = 0
      IF (ICOUNT .GE. 13) IADD = 8
      JCOUNT = JCOUNT + 1
      NUM = 1
      KOUNT(1) = ICOUNT + JCOUNT + IADD
      GO TO 1075
C
C     MIDSIDES
C
 1072 KADD = 4
      JCO  = 3
      GO TO 1074
 1073 KADD = 1
      IF (ICOUNT.EQ.2 .OR. ICOUNT.EQ.14) JCO = -1
 1074 IADD = 0
      IF (ICOUNT .GE. 14) IADD = 8
      JCO  = JCO + 1
      NUM  = 2
      KOUNT(1) = ICOUNT + JCO + IADD
      KOUNT(2) = KOUNT(1) + KADD
 1075 DO 1077 I = 1,3
      COORD(I) = 0.
      DO 1076 J = 1,NUM
      JSUB = ISYS1 + 4*(KOUNT(J)-1)
 1076 COORD(I) = COORD(I) + ECPT(JSUB+I)
 1077 CONTINUE
      DO 1078 I = 1,3
 1078 COORD(I) = COORD(I)/FLOAT(NUM)
      IF (ICOUNT .GT. 1) GO TO 109
C
C     WRITE OUT CID AND COORDINATES
C
  108 CALL WRITE (ESTFLD,IFIELD,1,0)
  109 CALL WRITE (ESTFLD,COORD,3,0)
C
C     FOR ISOPARAMETRICS, GET COORDS OF NEXT POINT, OTHERWISE,
C     GO BACK FOR ANOTHER ELEMENT OF THIS TYPE
C
      IF (OLDEID .EQ. 0) GO TO 40
      GO TO 105
C
C
C     GET ANOTHER ELEMENT TYPE
C
  110 CALL WRITE (ESTFLD,0,0,1)
      GO TO 10
C
C     DONE
C
  120 CALL CLOSE (ESTFLD,1)
      CALL CLOSE (HEST,1)
      MCB(1) = HEST
      CALL RDTRL (MCB)
      MCB(1) = ESTFLD
      CALL WRTTRL (MCB)
      RETURN
C
C     FATAL ERRORS
C
  500 WRITE  (OTPE,501) UFM,NAME
  501 FORMAT (A23,', ELEMENT TYPE ',2A4,' NOT ALLOWED IN ESTMAG')
      CALL MESAGE (-61,0,0)
C
 1001 N = -1
      GO TO 1010
 1002 N = -2
      GO TO 1010
 1003 N = -3
      GO TO 1010
 1008 N = -8
      FILE = 0
 1010 CALL MESAGE (N,FILE,NAM)
      RETURN
      END
