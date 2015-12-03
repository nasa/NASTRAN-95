      SUBROUTINE CRIGGP (N23)
C
C     ******************************************************************
C                                                                      *
C     THIS SUBROUTINE GENERATES COEFFICIENTS FOR RIGID ELEMENTS        *
C     FOR USE BY SUBROUTINE GP4.  THE DATA SO GENERATED IS             *
C     COMPATIBLE WITH MPC SET DATA.                                    *
C                                                                      *
C     (MODIFIED BY G.CHAN/SPERRY TO REDUCE EXCESSIVE OPENINGS,         *
C     CLOSINGS, AND READINGS OF THE BGPDT FILE (IN 2ND METHOD).        *
C     WITHOUT THIS MODIFICATION, A PROBLEM OF 2000 RIGID ELEMENTS,     *
C     FOR EXAMPLE, WOULD REQUIRE MORE THAN 10,000 OPENS AND 10,000     *
C     CLOSES AND OVER 10 MILLION CALLS TO SUBROUTINE READ   10/86)     *
C                                                                      *
C     (MODIFIED AGAIN BY G.CHAN/UNISYS TO INCLUDE CRROD, CRBAR, CRBE1, *
C     CRBE2, CRBE3, CRTRPLT, AND CRSPLINE RIGID ELEMENTS    11/88)     *
C                                                                      *
C     ******************************************************************
C
C     EXTERNAL          ORF    ,LSHIFT
      LOGICAL           AGAIN  ,GENRE  ,L38    ,DEBUG
C     INTEGER           ORF    ,LSHIFT
      INTEGER           GEOMP  ,BGPDT  ,CSTM   ,RGT    ,SCR1       ,
     1                  BUF(20),MASK16 ,GPOINT ,Z      ,FLAG       ,
     2                  FILE   ,RET    ,RET1   ,IC(1)  ,MCODE(2)   ,
     3                  BUF1   ,BUF2   ,BUF3   ,BUF4
      INTEGER           CRIGDR(2),CRIGD1(2)    ,CRIGD2(2),CRIGD3(2),
     1                  CRTRPT(2),CRSPLI(2)    ,CRROD(2) ,CRBAR(2) ,
     2                  CRBE1(2) ,CRBE2(2)     ,CRBE3(2)
CWKBR 8/94 SUN  INTEGER           RDREW    ,CLRSEW
      INTEGER           RDREW
      DOUBLE PRECISION  DZ(1)
      DIMENSION         RZ(1)  ,NAME(2),INDCMP(6)
      DIMENSION         A(36)  ,B(6)   ,IB(6)  ,C(18)
      CHARACTER         UFM*23 ,UWM*25 ,UIM*29
      COMMON /XMSSG /   UFM    ,UWM    ,UIM
      COMMON /MACHIN/   MACH   ,IHALF  ,JHALF
      COMMON /ZZZZZZ/   Z(1)
      COMMON /GP4FIL/   GEOMP  ,BGPDT  ,CSTM   ,RGT    ,SCR1
      COMMON /GP4PRM/   BUF    ,BUF1   ,BUF2   ,BUF3   ,BUF4   ,KNKL1  ,
     1                  MASK16 ,NOGO   ,GPOINT ,KN
      COMMON /NAMES /   RD     ,RDREW  ,WRT    ,WRTREW ,CLSREW
      COMMON /SYSTEM/   KSYSTM(55)
      EQUIVALENCE      (Z(1),RZ(1),DZ(1))
      EQUIVALENCE      (IC(1)     ,C(1) )
      EQUIVALENCE      (KSYSTM(2) , NOUT)
      EQUIVALENCE      (KSYSTM(55),IPREC)
      DATA    CRIGD1/  5310,   53/,
     1        CRIGD2/  5410,   54/,
     2        CRIGD3/  8310,   83/,
     3        CRIGDR/  8210,   82/,
     4        CRROD /  6510,   65/,
     5        CRBAR /  6610,   66/,
     6        CRTRPT/  6710,   67/,
     7        CRBE1 /  6810,   68/,
     8        CRBE2 /  6910,   69/,
     9        CRBE3 /  7010,   70/,
     O        CRSPLI/  7110,   71/
      DATA    NAME  /  4HCRIG,2HGP/
      DATA    MSET  /  4HMSET    /
      DATA    A     /  36*0.     /
      DATA    DEBUG /  .FALSE.   /
      DATA    L38   /  .FALSE.   /
C
C
C     ****************************************************************
C      OPEN CORE -
C                                        ALLOCATED BY
C     !<--- ALLOCATED BY GP4 --->!<------- CRIGGP -   --------->!
C     +--------+--------+------+-+----+-----+------   ----+-----+-----+
C     ! EQEXIN ! EQEXIN !SORTED!U!CSTM!BGPDT!   ...       ! DEP ! GINO!
C     !1ST REC !2ND REC ! SIL  !S!    !     !             ! SIL !BFFRS!
C     +--------+--------+------+-+----+-----+------   ----+-----+-----+
C      1      KN       KM        /    /    / \_KNKL1      /     /
C                            KNKL2  KNKL3 KNKL4         MU   BUF4
C
C      OPEN CORE FORMAT, STARTS WITH Z(KNKL2)
C      (KNKL2 = INITIAL VALUE OF KNKL1)
C
C      NUMBER OF WORDS                 CONTENTS
C
C           NCSTM    ***     COORDINATE SYSTEM TRANSFORMATION TABLE
C           NBGPDT   ***     BASIC GRID POINT DEFINITION TABLE
C                    *       (ONLY IF ENOUGH OPEN CORE SPACE AVAILABLE)
C                    ***     SIL 1   ***
C                    *       SIL 2     *
C             6      *       SIL 3     *  INDEPENDENT GRID POINT
C                    *       SIL 4     *
C                    *       SIL 5     *
C                    ***     SIL 6   ***
C                    ***     SIL 1          ***             ***
C                    *       DEGREE OF FREEDOM*               *
C                    *       INTERNAL INDEX   *               *
C                    *       SIL 2            *               *
C                    *       DEGREE OF FREEDOM*  FIRST DEPEND.*  ALL
C                    *       INTERNAL INDEX   *  GRID POINT   *  DEPEND.
C           3*MDEP   *           .            *               *  GRID
C                    *           .            *               *  POINTS
C                    *       SIL 6            *               *
C                    *       DEGREE OF FREEDOM*               *
C                    ***     INTERNAL INDEX ***             ***
C                    ***     INDEPENDENT GRID POINT BGPDT TABLE
C             4      *          WORD 1     COORDINATE SYSTEM ID-INTEGER
C                    ***        WORD 2-4 = X, Y, Z, IN BASIC SYSTEM-REAL
C          4*MDBGP   ***     DEPENDENT GRID POINT BGPDT TABLE
C                    ***
C         36*MDBGP   *       ROW STORED GG MATRIX (SINGLE PRECISION)
C                    ***     36 ELEMENTS * NO. DEPEND. GRID PT.
C          9*IPREC   ***     INDEPEND. GRID PT TRANSFORMATION MAT.-REAL
C          9*IPREC   ***     DEPEND. GRID PT TRANSFORMATION MATRIX-REAL
C         36*IPREC   ***     GG MATRIX  -  REAL  36 ELEMENTS
C                    ***          .        ***
C                    *            .          *
C                    *  AVAILABLE OPEN CORE  *
C                    *            .          *
C                    *            .          *
C                    ***          .        ***
C                    ***
C           MDEP     *       DEPENDENT SILS
C                    ***
C                    ***
C          BUFFERS   *       GINO BUFFERS
C                    ***
C
C     *************************************************************
C     NOTE  IPREC = 1   SINGLE PRECISION
C           IPREC = 2   DOUBLE PRECISION
C           MDEP  =     NUMBER DEPENDENT SILS
C           MDBGP =     NUMBER DEPENDENT GRID POINTS
C     *************************************************************
C
      MASK15= JHALF/2
      KN2   = KN/2
      NCSTM = 0
      KNKL2 = KNKL1
      KIOLD = 0
      IBUF1 =-99
      AGAIN = .FALSE.
      CALL SSWTCH (20,J)
      IF (J .EQ. 1) DEBUG =.TRUE.
      CALL SSWTCH (38,J)
      IF (J .EQ. 1) L38 =.TRUE.
      CALL PAGE2(-4)
      WRITE  (NOUT,200) UIM
  200 FORMAT (A29,' 3113, RIGID ELEMENTS ARE BEING PROCESSED IN GP4',/)
C
C     OPEN CSTM AND READ INTO CORE, FROM Z(KNKL2) THRU Z(KNKL3)
C
      LEFT = BUF4 - KNKL1
      FILE = CSTM
      CALL OPEN (*300,CSTM,Z(BUF2),RDREW)
      CALL SKPREC (CSTM,1)
      CALL READ (*1230,*270,CSTM,Z(KNKL2),LEFT,1,NCSTM)
      GO TO 1280
C
C     IF CORE WAS FILLED WITHOUT HITTING AN EOR, CALL MESAGE
C
  270 IF (IPREC .EQ. 1) CALL PRETRS (Z(KNKL1),NCSTM)
      IF (IPREC .EQ. 2) CALL PRETRD (Z(KNKL1),NCSTM)
      CALL CLOSE (CSTM,CLSREW)
      GO TO 300
C
C     IF THERE IS ENOUGH CORE AVAILABLE, OPEN AND READ BGPDT INTO OPEN
C     CORE, FROM Z(KNKL3+1) THRU Z(KNKL4), CLOSE BGPDT FILE, AND RESET
C     VARIOUS POINTERS FOR BUILDING UP RGT DATA. (AGAIN=.FALSE.)
C     THIS METHOD USES ONLY ONE OPEN, ONE CLOSE, AND ONE READ.
C
C     HOWEVER, IF THERE IS NOT ENOUGH CORE FOR BGPDT DATA AND THE NEEDED
C     SPACE FOR BUILDING UP RGT DATA, SET AGAIN TO .TRUE., AND REPEAT
C     DATA PROCESSING BY READING DATA DIRECTLY OFF THE BGPDT FILE EACH
C     TIME WHEN THE BGPDT DATA IS NEEDED.   THIS SECOND METHOD USES ONLY
C     ONE OPEN, ONE CLOSE, AND MULTIPLE READS.
C
C     IN THE SECOND METHOD, TWO POINTERS, KIOLD AND KINEW, ARE USED TO
C     COMPUTE PRECISELY WHERE TO READ DATA OFF THE BGPDT FILE
C
  290 AGAIN = .TRUE.
      CALL WRITE  (RGT,0,0,1)
      CALL BCKREC (RGT)
      KNKL3 = 0
      KNKL1 = KNKL2
      NBGPDT= KNKL1 + NCSTM
      CALL CLOSE (BGPDT,CLSREW)
  300 FILE  = BGPDT
      CALL OPEN (*1210,BGPDT,Z(BUF2),RDREW)
      CALL FWDREC (*1240,BGPDT)
      KIOLD = 0
C
C     CALCULATE STARTING POINT
C     AND READ BGPDT INTO OPEN CORE
C
      KNKL1 = KNKL1 + NCSTM
      IF (AGAIN) GO TO 310
      KNKL3 = KNKL1
      CALL READ (*1230,*310,BGPDT,Z(KNKL3+1),BUF4-KNKL3,1,NBGPDT)
      IMHERE = 305
      IF (DEBUG) WRITE (NOUT,1255) IMHERE
      KNKL3 = 0
      NBGPDT= KNKL1
      AGAIN = .TRUE.
      CALL BCKREC (BGPDT)
  310 IF (.NOT.AGAIN) CALL CLOSE (BGPDT,CLSREW)
      KNKL4 = KNKL3 + NBGPDT
      KNKL1 = KNKL4 + 1
      MU    = BUF4  - 1
      IRDG  = 0
      ITYPE = 0
      GENRE = .FALSE.
C
C     *************************************************************
C
C     CRIGD1, CRIDG2, AND CRBE2 RIGID ELEMENTS ARE PROCESSED HERE
C
C     *************************************************************
C
C     LOCATE CRIGD1 DATA IN THE INPUT FILE
C
      FILE = GEOMP
      CALL LOCATE (*500,Z(BUF1),CRIGD1,FLAG)
      IRDG = 1
      GO TO 1000
C
C     LOCATE CRIGD2 DATA ON INPUT FILE
C
  500 FILE = GEOMP
      CALL LOCATE (*600,Z(BUF1),CRIGD2,FLAG)
      IRDG = 2
      IMHERE = 500
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
      GO TO 1000
C
C     LOCATE CRBE2 DATA ON INPUT FILE
C
  600 FILE = GEOMP
      CALL LOCATE (*4000,Z(BUF1),CRBE2,FLAG)
      IRDG = 3
      IMHERE = 600
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
C
 1000 CONTINUE
      IF (DEBUG) WRITE (NOUT,1005) IRDG
 1005 FORMAT ('0 IRDG/CRIGGP =',I6)
C
C     READ ELEMENT ID AND INDEPENDENT GRID POINT NUMBER
C
 1730 IFILE = GEOMP
      NWDS  = 2
      GO TO 1734
 1732 IFILE = SCR1
      NWDS  = 9
 1734 FILE  = IFILE
      CALL READ (*1230,*1240,IFILE,BUF,NWDS,0,FLAG)
      IF ((DEBUG.OR.L38) .AND. BUF(1).NE.IBUF1) WRITE (NOUT,1735) BUF(1)
 1735 FORMAT (5X,'ELEMENT',I8,' IS BEING PROCESSED')
      IF (.NOT.GENRE) GO TO 1739
      IBUF1 = BUF(1)
C
C     SET UP INDEPENDENT D.O.F. FOR THE GENERAL RIGID ELEMENTS,
C     CRIGID3 AND CRBE1, AND ALSO THE CRBAR AND CRTRPLT ELEMENTS
C     WHICH WERE CONVERTED TO CRIGID3 FORMAT BY IFS3P
C
      DO 1736 I = 1,6
      INDCMP(I) = BUF(I+2)
 1736 CONTINUE
      ITYPE = BUF(9)
      IF (ITYPE .NE. 0) GO TO 1739
      DO 1737 I = 1,36
 1737 A(I) = 0.0
      INDEX = 0
      ILAST = 0
      DO 1738 I = 1,6
      IF (INDCMP(I) .NE. I) GO TO 1738
      J    = 6*ILAST + I
      A(J) = 1.0
      ILAST= ILAST + 1
 1738 CONTINUE
      NIND = ILAST
C
 1739 ASSIGN 1740 TO RET
      ASSIGN 1743 TO RET1
      IDR   = BUF(1)
      GPOINT= BUF(2)
      NTYPE = 1
      GO TO 7060
C
C     STORE SIL FOR INDEPENDENT DEGREES OF FREEDOM
C
 1740 DO 1742 I=1,6
      Z(KNKL1+I-1) = GPOINT + I - 1
 1742 CONTINUE
 1743 KINEW = K - 2*KN
      ASSIGN 1750 TO RET
      ASSIGN 1745 TO RET1
C
C     READ DEPENDENT GRID POINTS
C
      J = KNKL1 + 3
      MDBGP = 0
      MDEP  = 0
 1745 CALL READ (*1230,*1240,IFILE,BUF,7,0,FLAG)
      IF (BUF(1) .EQ. -1) GO TO 1760
      MDBGP = MDBGP + 1
      GPOINT= BUF(1)
      NTYPE = 2
      GO TO 7060
 1750 CONTINUE
      IF (NOGO .NE. 0) GO TO 1745
C
C     STORE DEPENDENT GRID POINT SIL, DOF, AND INTERNAL INDEX
C
      DO 1756 I = 1,6
      IF (BUF(I+1) .EQ. 0) GO TO 1756
      J = J + 3
      L = J
      Z(L) = GPOINT + I - 1
      Z(L+1) = I
      Z(L+2) = K - 2*KN
      MDEP   = MDEP + 1
 1756 CONTINUE
      GO TO 1745
C
C     HERE WHEN ALL DEPENDENT GRID POINTS FOR AN ELEMENT HAVE BEEN READ
C
 1760 MORE = 0
      I = KNKL1 + 6 + 3*MDEP + 4 + 4*MDBGP + (9+9+36*MDBGP+36)*IPREC
C
C     CHECK FOR OPEN CORE AVAILABILITY
C
      IMHERE = 176
      IF (I      .GE. MU) GO TO 1250
      IF (BUF(2) .EQ.  0) MORE = 1
      IF (NOGO   .NE.  0) GO TO 3645
C
C     LOCATE DATA IN BGPDT FOR INDEPENDENT GRID POINT
C
      IOPEN = KNKL1 + 6 + 3*MDEP
      IF (AGAIN) GO TO 1761
      KI4 = KNKL3 + KINEW*4
      IF (KI4 .GT. KNKL4) GO TO 1290
      Z(IOPEN    ) = Z(KI4 -3)
      Z(IOPEN + 1) = Z(KI4 -2)
      Z(IOPEN + 2) = Z(KI4 -1)
      Z(IOPEN + 3) = Z(KI4   )
      GO TO 1763
 1761 FILE = BGPDT
      IF (KINEW .GT. KIOLD) GO TO 1762
      CALL BCKREC (BGPDT)
      KIOLD = 0
 1762 KI4 = (KINEW-KIOLD-1) * 4
      IF (KI4 .GT. 0) CALL READ (*1230,*1240,BGPDT,BUF,-KI4,0,FLAG)
      CALL READ (*1230,*1240,BGPDT,BUF,4,0,FLAG)
      Z(IOPEN    ) = BUF(1)
      Z(IOPEN + 1) = BUF(2)
      Z(IOPEN + 2) = BUF(3)
      Z(IOPEN + 3) = BUF(4)
 1763 KIOLD = KINEW
C
C     SORT DEPENDENT DEGREE OF FREEDOM LIST ON BGPDT REFERENCE NUMBER
C
      I = MDEP*3
      CALL SORT (0,0,3,3,Z(KNKL1+6),I)
C
      J = 0
      M = 0
      INDX  = KNKL1 + 5
      INDXX = KNKL1 + 6 + 3*MDEP + 4
      DO 1768 I = 1,MDEP
      K = INDX + 3*I
      KINEW = Z(K)
      IF (KIOLD .EQ. KINEW) GO TO 1767
      J = J + 1
C
C     READ GRID POINT INFORMATION
C
      M = M + 1
      N = INDXX + (M-1)*4
      IF (AGAIN) GO TO 1764
      KI4 = KNKL3 + KINEW*4
      IF (KI4 .GT. KNKL4) GO TO 1290
      Z(N    ) = Z(KI4 -3)
      Z(N + 1) = Z(KI4 -2)
      Z(N + 2) = Z(KI4 -1)
      Z(N + 3) = Z(KI4   )
      GO TO 1766
 1764 FILE = BGPDT
      IF (KINEW .GT. KIOLD) GO TO 1765
      CALL BCKREC (BGPDT)
      KIOLD = 0
 1765 KI4 = (KINEW-KIOLD-1)*4
      IF (KI4 .GT. 0) CALL READ (*1230,*1240,BGPDT,BUF,-KI4,0,FLAG)
      CALL READ (*1230,*1240,BGPDT,BUF,4,0,FLAG)
      Z(N    ) = BUF(1)
      Z(N + 1) = BUF(2)
      Z(N + 2) = BUF(3)
      Z(N + 3) = BUF(4)
 1766 KIOLD = KINEW
 1767 Z(K)  = J
 1768 CONTINUE
C
      IF (IPREC .EQ. 2) GO TO  3200
C
C     FORM REFERENCE GRID POINT TRANSFORMATION MATRIX
C
      IBA = KNKL1 + 6 + 3*MDEP
      ITA = IBA + 4 + 4*MDBGP + 36*MDBGP
      IF (Z(IBA) .NE. 0) CALL TRANSS (RZ(IBA),RZ(ITA))
C
C     PREPARE POINTERS USED TO FORM THE G MATRIX
C
      ITB = ITA + 9
      ITC = ITB - 1
C
C     SET INDEXES FOR TRANSFORMATION MATRIXES AND GG MATRIXES TO
C     FIRST ELEMENT - 1 FOR SUBROUTINE FORMGG
C
      ITA = ITA - 1
      IG  = INDXX + 4*MDBGP - 1
      IGG = IG + (36*MDBGP) + 9 + 9
      INDX= KNKL1 + 3
      M   = -1
C
C     BEGIN LOOP TO FORM THE G MATRIX
C
      DO 3050 I = 1,MDEP
      K  = INDX + I*3
      MM = Z(K+2)
      IF (MM .EQ. M) GO TO 3030
      IBB = INDXX + (MM-1)*4
C
C     FORM DEPENDENT DEGREE OF FREEDOM TRANSFORMATION MATRIX
C
      IF (Z(IBB) .NE. 0) CALL TRANSS (RZ(IBB),RZ(ITB))
C
C     FORM THE GG MATRIX
C
      CALL FORMGG (IGG,ITA,ITC,IBA,IBB)
 3030 CONTINUE
C
C     SELECT PROPER ROW BASED ON COMPONENT NUMBER AND STORE IN G
C     ACCORDING TO PARTITIONING VECTOR OF REFERENCE GRID POINT.
C
      M  = MM
      MM = Z(K+1)
      DO 3040 IJ = 1,6
      INDXXX = IGG + (MM-1)*6 + IJ
      RZ(IG+IJ) = RZ(INDXXX)
 3040 CONTINUE
      IG = IG + 6
 3050 CONTINUE
      GO TO 3300
C
C     FORM REFERENCE GRID POINT TRANSFORMATION MATRIX (DOUBLE PREC.)
C
 3200 IBASE = (KNKL1 + 6 + 3*MDEP + 4 + 4*MDBGP + 36*MDBGP) / 2 + 1
      IBA = KNKL1 + 6 + 3*MDEP
      ITA = IBASE
      IF (Z(IBA) .NE. 0) CALL TRANSD (RZ(IBA),DZ(ITA))
C
C     PREPARE POINTERS USED TO FORM THE G MATRIX
C
      ITB = ITA + 9
      ITC = ITB - 1
C
C     SET INDEXES FOR TRANSFORMATION MATRIXES AND GG MATRIXES TO
C     FIRST ELEMENT - 1 FOR SUBROUTINE FORMGG
C
      ITA = ITA - 1
      IG  = INDXX + 4*MDBGP - 1
      IGG = IBASE + 9 + 9 - 1
      INDX= KNKL1 + 3
      M   = -1
C
C     BEGIN LOOP TO FORM THE G MATRIX
C
      DO 3250 I = 1, MDEP
      K  = INDX  + I*3
      MM = Z(K+2)
      IF (MM .EQ. M) GO TO 3230
      IBB = INDXX + (MM-1)*4
C
C     FORM DEPENDENT DEGREE OF FREEDOM TRANSFORMATION MATRIX
C
      IF (Z(IBB) .NE. 0) CALL TRANSD (RZ(IBB),DZ(ITB))
C
C     FORM THE GG MATRIX
C
      CALL FORMG2 (IGG,ITA,ITC,IBA,IBB)
 3230 CONTINUE
C
C     SELECT PROPER ROW BASED ON COMPONENT NUMBER AND STORE IN G
C
      M  = MM
      MM = Z(K+1)
      DO 3240 IJ = 1,6
      INDXXX = IGG + (MM-1)*6 + IJ
      RZ(IG+IJ) = DZ (INDXXX)
 3240 CONTINUE
      IG = IG + 6
 3250 CONTINUE
 3300 IG = INDXX + 4*MDBGP - 1
C
C     WRITE THE CODED COLUMN-ROW NUMBERS AND ELEMENTS OF THE GM
C     MATRIX ON RGT FILE SO AS TO MAKE RIGID ELEMENT DATA
C     COMPATIBLE WITH MPC SET DATA
C     (REVISED 7/86, CODED COLUMN-ROW NUMBERS ARE NOT USED HERE.
C     THEY WILL BE RE-CODED IN GP4 IF NEEDED)
C
      K  = 0
      IF (GENRE .AND. ITYPE.EQ.0) GO TO 3380
      MU = MU - MDEP
C
C     TEST FOR OPEN CORE AVAILABILITY
C
      IMHERE = 3380
      IF (IOPEN .GE. MU) GO TO 1250
 3380 CONTINUE
      INDX = KNKL1 + 3
      DO 3640 I = 1, MDEP
      IF (GENRE .AND. ITYPE.EQ.0) GO TO 3390
      Z(MU+I) = Z(INDX + I*3)
 3390 KROW    = Z(INDX + I*3)
      MCODE(2)= KROW
      IF (KROW .GT. MASK15) N23 = 3
      DO 3620 J = 1,6
      K    = K+1
      KCOL = Z(KNKL1+J-1)
      MCODE(1) = KCOL
      IF (KCOL .GT. MASK15) N23 = 3
      IF (GENRE .AND. ITYPE.EQ.0) GO TO 3440
      RZ(IG+K) = -RZ(IG+K)
      IF (GENRE .AND. ITYPE.EQ.1) GO TO 3400
      CALL WRITE (RGT,MCODE,2,0)
      CALL WRITE (RGT,RZ(IG+K),1,0)
      GO TO 3620
 3400 IC(J) = IB(J)
      IF (IC(J) .GT. MASK15) N23 = 3
      GO TO 3620
 3440 IF (INDEX  .GE. NIND) GO TO 3460
      IF (INDCMP(J) .NE. J) GO TO 3460
      INDEX = INDEX + 1
      IB(INDEX) = KCOL
 3460 A(6*ILAST+J) = RZ(IG+K)
 3620 CONTINUE
      IF (   .NOT.GENRE) GO TO 3635
      IF (ITYPE .EQ. -1) GO TO 3635
      IF (ITYPE .EQ.  1) GO TO 3625
      INDEX = INDEX + 1
      IB(INDEX) = KROW
      ILAST = ILAST + 1
      GO TO 3640
 3625 CALL GMMATS (RZ(IG+K-5),1,6,0,A,6,6,0,B)
      DO 3630 J = 1, 6
      CALL WRITE (RGT,IC(J),1,0)
      CALL WRITE (RGT,KROW ,1,0)
      CALL WRITE (RGT,B(J) ,1,0)
 3630 CONTINUE
 3635 MCODE(1) = KROW
      COEFF = 1.0
      CALL WRITE (RGT,MCODE,2,0)
      CALL WRITE (RGT,COEFF,1,0)
 3640 CONTINUE
      IF (.NOT.GENRE .OR. ITYPE.NE.0) GO TO 3645
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ISING = -1
      CALL INVERS (6,A,6,B,0,DET,ISING,C)
C
C     CHECK TO SEE IF GENERAL RIGID ELEMENTS (CRIGD3, CRBE1, CRBAR, AND
C     CRTRPLT) ARE PROPERLY DEFINED
C
      IF (ISING .NE. 2) GO TO 3645
      WRITE  (NOUT,6130) UFM,IDR
      NOGO = 1
 3645 IF (MORE .EQ.0) GO TO 3650
      IF (.NOT.GENRE) GO TO 1730
      GO TO 1732
C
 3650 IF (GENRE) CALL CLOSE (SCR1,1)
      IF (IRDG .LE. 8) GO TO (500,600,4000,4100,4200,4300,5200), IRDG
      CALL ERRTRC ('CRIGGP  ',3655)
C
C     ******************************************************************
C
C     CRBAR, CRTRPLT, CRIGD3, AND CRBE1 ELEMENTS ARE PROCESSED HERE.
C     THE CRBAR AND CRTRPLT HAVE THE SAME DATA FORMAT AS THAT OF THE
C     GENERAL RIGID ELEMENT CRIGD3.
C     CRBE1 WAS MADE EXACTLY SAME AS CRIGD3 IN IFS3P ROUTINE.
C
C     ******************************************************************
C
C     LOCATE CRBAR DATA ON INPUT FILE
C
 4000 FILE = GEOMP
      IMHERE = 4000
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
      CALL LOCATE (*4100,Z(BUF1),CRBAR,FLAG)
      IRDG = 4
      GO TO 5000
C
C     LOCATE CRTRPLT DATA ON INPUT FILE
C
 4100 FILE = GEOMP
      IMHERE = 4100
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
      CALL LOCATE (*4200,Z(BUF1),CRTRPT,FLAG)
      IRDG = 5
      GO TO 5000
C
C     LOCATE CRIGD3 DATA ON INPUT FILE
C
 4200 CALL LOCATE (*4300,Z(BUF1),CRIGD3,FLAG)
      IMHERE = 4200
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
      IRDG = 6
      GO TO 5000
C
C     LOCATE CRBE1 DATA ON INPUT FILE
C
 4300 CALL LOCATE (*5200,Z(BUF1),CRBE1,FLAG)
      IMHERE = 4300
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
 4400 FORMAT ('0  I AM HERE/CRIGGP =',I6)
      IRDG  = 7
C
 5000 GENRE = .TRUE.
      MORE  = 1
      IF (DEBUG) WRITE (NOUT,1005) IRDG
C
C     OPEN SCR1 FILE TO WRITE
C
      CALL OPEN (*1210,SCR1,Z(BUF4),1)
C
C     READ ELEMENT ID
C
 5010 FILE = GEOMP
      CALL READ (*1230,*1240,GEOMP,BUF,1,0,FLAG)
      IDR = BUF(1)
C
C     READ INDEPENDENT GRID POINTS AND THEIR COMPONENT NUMBERS
C
      N = 0
      J = KNKL1
 5020 CALL READ (*1230,*1240,GEOMP,BUF,1,0,FLAG)
      IF (BUF(1) .EQ. MSET) GO TO 5040
      N = N + 7
      Z(J) = BUF(1)
      CALL READ (*1230,*1240,GEOMP,Z(J+1),6,0,FLAG)
      J = J + 7
      GO TO 5020
 5040 NIND = N/7
C
C     CHECK TO SEE IF THE NUMBER OF INDEPENDENT GRID POINTS
C     IS MORE THAN ONE AND SET TYPE FLAG
C
      ITYPE = -1
      IF (NIND .EQ. 1) GO TO 5050
      ITYPE = 0
      J = KNKL1
C
C     WRITE THE INDEPENDENT GRID POINTS AS A PSEUDO CRIGD2 ELEMENT
C
C
C     WRITE THE ELEMENT ID
C
      CALL WRITE (SCR1,IDR,1,0)
C
C     WRITE THE FIRST INDEPENDENT GRID POINT AND ITS COMPONENT NUMBERS
C
      CALL WRITE (SCR1,Z(J),7,0)
C
C     WRITE THE TYPE FLAG
C
      CALL WRITE (SCR1,ITYPE,1,0)
C
C     WRITE THE REMAINING INDEPENDENT GRID POINTS AND THEIR
C     COMPONENT NUMBERS
C
      J = J + 7
      N = N - 7
      CALL WRITE (SCR1,Z(J),N,0)
      DO 5045 L =1,7
 5045 BUF(L) = -1
      BUF(2) =  0
      CALL WRITE (SCR1,BUF,7,0)
      ITYPE = 1
C
C     WRITE THE FIRST INDEPENDENT GRID POINT AND ALL THE
C     DEPENDENT GRID POINTS AS A PSEUDO CRIGD2 ELEMENT
C
 5050 J = KNKL1
C
C     WRITE THE ELEMENT ID
C
      CALL WRITE (SCR1,IDR,1,0)
C
C     WRITE THE FIRST INDEPENDENT GRID POINT AND ITS COMPONENT NUMBERS
C
      CALL WRITE (SCR1,Z(J),7,0)
C
C     WRITE THE TYPE FLAG
C
      CALL WRITE (SCR1,ITYPE,1,0)
C
C     PROCESS THE DEPENDENT GRID POINTS AND THEIR COMPONENT NUMBERS
C
 5060 CALL READ (*1230,*1240,GEOMP,BUF,7,0,FLAG)
      IF (BUF(1) .EQ. -1) GO TO 5070
      CALL WRITE (SCR1,BUF,7,0)
      GO TO 5060
 5070 IF (BUF(2) .EQ. -1) MORE = 0
      DO 5080 L = 1,7
 5080 BUF(L) = -1
      BUF(2) =  0
      IF (MORE .EQ. 0) BUF(2) = -1
      CALL WRITE (SCR1,BUF,7,0)
      IF (MORE .EQ. 1) GO TO 5010
      CALL WRITE (SCR1,0,0,1)
C
C     CLOSE SCR1, AND OPEN IT FOR READ
C
      CALL CLOSE (SCR1,1)
      CALL OPEN (*1210,SCR1,Z(BUF4),0)
      IMHERE = 5085
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
      GO TO 1732
C
C     *********************************************************
C
C     CRBE3 AND CRSPLINE ELEMENTS ARE PROCESSED HERE
C
C     *********************************************************
C
C     LOCATE CRBE3 DATA ON INPUT FILE
C
 5200 FILE = GEOMP
      IRDG = 8
      CALL LOCATE (*5300,Z(BUF1),CRBE3,FLAG)
      IMHERE = 5200
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
      GO TO 5400
C
C     LOCATE CRSPLINE DATA ON INPUT FILE
C
 5300 FILE = GEOMP
      IRDG = 9
      CALL LOCATE (*5800,Z(BUF1),CRSPLI,FLAG)
      IMHERE = 530
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
C
 5400 J = IRDG-7
      IF (DEBUG) WRITE (NOUT,1005) IRDG
      IF (IPREC .EQ. 1) CALL CRSPLS (*5600,J,MU,KNKL3+1,Z(KNKL1),AGAIN,
     1    N23)
      IF (IPREC .EQ. 2) CALL CRSPLD (*5600,J,MU,KNKL3+1,Z(KNKL1),AGAIN,
     1    N23)
      GO TO (5300,5800), J
 5600 WRITE  (NOUT,5610) UFM
 5610 FORMAT (A23,' 8, INSUFFICIENT CORE FOR CRBE3 OR CRSPLINE RIGID ',
     1       'ELEMENT COMPUTATION')
      NOGO = 1
C
C     *********************************************************
C
C     CRIGDR AND CRROD (RIGID ROD ELEMENTS) ARE PROCESSED HERE
C     (CRROD DATA FORMAT WAS CONVERTED TO CRIGDR FORMAT IN IFS3P)
C
C     *********************************************************
C
C     LOCATE CRIGDR AND CRROD DATA ON INPUT FILE
C
 5800 GENRE= .FALSE.
      NWDS = 4
      FILE = GEOMP
      CALL LOCATE (*5900,Z(BUF1),CRIGDR,FLAG)
      IRDG = 10
      IMHERE = 5800
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
      GO TO 6000
 5900 FILE = GEOMP
      IRDG = 11
      CALL LOCATE (*7000,Z(BUF1),CRROD,FLAG)
      IMHERE = 5900
      IF (DEBUG) WRITE (NOUT,4400) IMHERE
C
C     ***************************************************************
C
C                  OPEN CORE FORMAT FOR RIGID ROD
C
C      NUMBER OF WORDS                 CONTENTS
C
C           NCSTM    ***     COORDINATE SYSTEM TRANSFORMATION TABLE
C           NBGPDT   ***     BASIC GRIP POINT DEFINITION TABLE
C                    ***     SIL 1 ***
C             3      *       SIL 2   * INDEPENDENT GRID POINT
C                    ***     SIL 3 ***
C                    ***     INDEPENDENT GRID POINT BGPDT TABLE
C             4      *          WORD 1     COORDINATE SYSTEM ID-INTEGER
C                    ***        WORD 2-4   X, Y, Z, IN BASIC SYSTEM-REAL
C                    ***     SIL 1 ***
C             3      *       SIL 2   * DEPENDENT GRID POINT
C                    ***     SIL 3 ***
C             4      ***     DEPENDENT GRID POINT BGPDT TABLE
C                    ***                   ***
C                    *                       *
C                    *  AVAILABLE OPEN CORE  *
C                    *                       *
C                    *                       *
C                    ***                   ***
C                    ***
C           MDEP     *       DEPENDENT SILS
C                    ***
C                    ***
C          BUFFERS   *       GINO BUFFERS
C                    ***
C
C     **************************************************************
C
C
C     CHECK AVAILABILITY OF CORE
C
 6000 CONTINUE
      IF (DEBUG) WRITE (NOUT,1005) IRDG
      ITEST  = KNKL1 + 14 + 27*IPREC + 2
      IF (ITEST .GE. MU) GO TO 1250
C
C     READ ELEMENT DATA
C
 6010 CALL READ (*1230,*7000,GEOMP,BUF,NWDS,0,FLAG)
      IDR   = BUF(1)
      IDEPGP= BUF(3)
      ICOMP = BUF(4)
C
C     PROCESS THE INDEPENDENT GRID POINT
C
      FILE = BGPDT
      J = KNKL1
      GPOINT = BUF(2)
      ASSIGN 6020 TO RET
      ASSIGN 6050 TO RET1
      GO TO 7060
C
C     STORE SIL VALUES
C
 6020 IF (NOGO  .EQ. 0) GO TO 6030
      IF (J .EQ. KNKL1) GO TO 6050
      GO TO 6010
 6030 Z(J  ) = GPOINT
      Z(J+1) = GPOINT + 1
      Z(J+2) = GPOINT + 2
      KINEW  = K - 2*KN
C
C     LOCATE DATA IN BGPDT
C
      IF (AGAIN) GO TO 6035
      KI4 = KNKL3 + KINEW*4
      IF (KI4 .GT. KNKL4) GO TO 1290
      Z(J+3) = Z(KI4-3)
      Z(J+4) = Z(KI4-2)
      Z(J+5) = Z(KI4-1)
      Z(J+6) = Z(KI4  )
      GO TO 6045
 6035 IF (KINEW .GT. KIOLD) GO TO 6040
      CALL BCKREC (BGPDT)
      KIOLD = 0
 6040 KI4 = (KINEW-KIOLD-1) * 4
      IF (KI4 .GT. 0) CALL READ (*1230,*1240,BGPDT,BUF,-KI4,0,FLAG)
      CALL READ (*1230,*1240,BGPDT,BUF,4,0,FLAG)
C
C     STORE BASIC GRID POINT DATA
C
      Z(J+3) = BUF(1)
      Z(J+4) = BUF(2)
      Z(J+5) = BUF(3)
      Z(J+6) = BUF(4)
 6045 KIOLD  = KINEW
      IF (J .NE. KNKL1) GO TO 6060
C
C     PROCESS THE DEPENDENT GRID POINT
C
 6050 J = J + 7
      GPOINT = IDEPGP
      ASSIGN 6010 TO RET1
      GO TO 7060
 6060 IF (IPREC .EQ. 1) CALL CRDRD  (*6065,*6125,MU,ICOMP,N23)
      IF (IPREC .EQ. 2) CALL CRDRD2 (*6065,*6125,MU,ICOMP,N23)
      GO TO 6010
 6065 WRITE  (NOUT,6070) UFM,IDR
 6070 FORMAT (A23,' 3133, RIGID ELEMENT',I9,' HAS ZERO LENGTH')
      NOGO = 1
      GO TO 6010
 6125 WRITE  (NOUT,6130) UFM,IDR
 6130 FORMAT (A23,' 3134, RIGID ELEMENT',I9,' IS NOT PROPERLY DEFINED')
      NOGO = 1
      GO TO 6010
C
 7000 IF (IRDG .EQ. 10) GO TO 5900
C
      IF (AGAIN) CALL CLOSE (BGPDT,CLSREW)
      IF (NOGO .NE. 0) CALL MESAGE (-61,0,NAME)
      CALL WRITE (RGT,0,0,1)
C
C     WRITE A LIST OF DEPENDENT SIL VALUES FOR RIGID ELEMENTS ONTO THE
C     RGT IN SORTED FORM
C
      JRIGID = MU + 1
      M = BUF4 - JRIGID
      CALL SORT  (0,0,1,1,Z(JRIGID),M)
      CALL WRITE (RGT,Z(JRIGID),M,1)
      J = BUF4-1
      IF (DEBUG) WRITE (NOUT,7010) (Z(I),I=JRIGID,J)
 7010 FORMAT (/,'  CRIGGP/@7010  DEPEND.SIL LIST:',/,(5X,10I7))
      KNKL1 = KNKL2
C
C     CLOSE RGT FILE AND RETURN
C
      CALL CLOSE (RGT,CLSREW)
      RETURN
C
C     **********************************************************
C
C     INTERNAL SUBROUTINE TO PERFORM BINARY SEARCH IN EQEXIN
C     AND CONVERT THE EXTERNAL NUMBER TO A SIL VALUE
C
 7060 KLO = 0
      KHI = KN2
      LASTK = 0
 7070 K= (KLO+KHI+1)/2
      IF (LASTK .EQ. K) GO TO 1350
      LASTK = K
      IF (GPOINT-Z(2*K-1)) 7090,7150,7100
 7090 KHI= K
      GO TO 7070
 7100 KLO= K
      GO TO 7070
 7150 K = Z(2*K) + 2*KN
      GPOINT= Z(K)
      GO TO RET, (1740,1750,6020)
C
C     **********************************************************
C
C     FATAL ERROR MESSAGES
C
 1210 J= -1
      GO TO 1260
 1230 J= -2
      GO TO 1260
 1240 J= -3
      GO TO 1260
 1250 IF (AGAIN) GO TO 1280
      CALL CLOSE (SCR1,CLSREW)
      WRITE  (NOUT,1255) IMHERE
 1255 FORMAT (///,' *** CRIGGP/GP4 NEEDS MORE OPEN CORE.',
     1        /5X,' CRIGGP REVERTED TO USE SLOW METHOD',I9,//)
      GO TO 290
 1260 CALL MESAGE (J,FILE,NAME)
 1280 J= -8
      GO TO 1260
 1290 WRITE  (NOUT,1300) KNKL1,KNKL3,KNKL4,KI4
 1300 FORMAT (//,' *** SYSTEM FATAL ERROR IN CRIGGP',4I10)
      J =-61
      GO TO 1260
 1330 NOGO= 1
      CALL MESAGE (30,N,BUF)
      GO TO RET1, (1743,1745,6010,6050)
 1350 IF (GENRE .AND. ITYPE.EQ.1 .AND. NTYPE.EQ.1) GO TO 1743
      BUF(1) = GPOINT
      BUF(2) = IRDG*100000000 + IDR
      N = 151
      GO TO 1330
      END
