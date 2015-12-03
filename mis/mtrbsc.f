      SUBROUTINE MTRBSC
C
COMMENT.  ALL WRITE STATEMENTS WHICH HAVE BEEN COMMENTED OUT, HAVE BEEN
C         LEFT IN THE PROGRAMMING FOR ANY FUTURE DEBUGGING USE.
C
C
C      ************* BASIC BENDING TRIANGLE   ELEMENT ROUTINE **********
C
C     CALLS FROM THIS ROUTINE ARE MADE TO. . .
C
C          MAT    - MATERIAL DATA ROUTINE
C          SMA2B  - INSERTION ROUTINE
C          TRANSD - DOUBLE PRECISION TRANSFORMATION SUPPLIER
C          INVERD - DOUBLE PRECISION INVERSE ROUTINE
C          GMMATD - DOUBLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
C          MESAGE - ERROR MESSAGE WRITER
C
C
C     ******************************************************************
C
      DOUBLE PRECISION        A        ,E        ,XSUBB    ,TEMP
     1                       ,XSUBC    ,D        ,YSUBC    ,XCYC
     2                       ,XCSQ     ,DETERM   ,YCSQ     ,XBSQ
     3                       ,G2X2     ,J2X2     ,HYQ      ,AIJ
     4                       ,BIJ      ,SIIJ     ,SIZERO   ,MBARAA
     5                       ,MAR      ,MRR      ,S
     6                       ,PROD9    ,TEMP9    ,G
     7                       ,YPRODJ   ,XPRODI
     8                       ,FJ       ,FJ2      ,FI       ,FIJ
C
C
      DIMENSION D(9)      ,G(9)      ,G2X2(4)   ,J2X2(4)   , S(18)
     1         ,ECPT(1)   ,HYQ(6)    ,SIIJ(7,7) ,MBARAA(9) , MAR(18)
     2         ,MRR(36)
C     DIMENSION MNAME(9)
C     DIMENSION NASTER(130)
C     DATA (MNAME(I), I = 1,9) /6H1(MAA),6H (MAB),6H (MAC),6H (MBA),
C    $6H (MBB),6H (MBC),6H (MCA),6H (MCB),6H (MCC) /
C     DATA NASTER /130*1H*/
C
      COMMON /SMA2IO/ DUM1(10), IFMGG, DUM2(25)
      COMMON /SMA2CL/  DUM3(2), NPVT
     2,                  DUMCL(7)
     3,                  LINK(10)           ,NOGO
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALP12,
     1                T SUB 0, G SUB E, SIGTEN, SIGCOM, SIGSHE,
     2                G2X211, G2X212, G2X222, SPACE(2)
C
C     ECPT BLOCK
      COMMON /SMA2ET/
     1                   NECPT(1)      ,NGRID(3)
     2                  ,ANGLE         ,MATID1
     3                  ,EYE           ,MATID2
     4                  ,T2            ,FMU
     5                  ,Z11           ,Z22
     6                  ,DUMMY1        ,X1
     7                  ,Y1            ,Z1
     8                  ,DUMMY2        ,X2
     9                  ,Y2            ,Z2
     1                  ,DUMMY3        ,X3
     2                  ,Y3            ,Z3            ,DUMB(76)
C
      COMMON /SMA2DP/    A(225)        ,PROD9(9)
     1                  ,TEMP9(9)      ,XSUBB
     2                  ,XSUBC         ,YSUBC
     3                  ,E(9)          ,TEMP
     4                  ,XCSQ          ,XBSQ
     5                  ,YCSQ          ,XCYC
     6                  ,AIJ           ,DETERM
     7                  ,BIJ           ,SIZERO
     8                  ,FJ            ,FJ2
     9                  ,FI            ,FIJ
     T                  ,YPRODJ        ,XPRODI
     1
     2                  ,ISING         ,DUMMY(59)
C
      EQUIVALENCE
     1          (D(1),G(1),SIIJ(1,1),A(1))  ,(ECPT(1),NECPT(1))
     2         ,(G2X2(1),A(10))             ,(J2X2(1),A(14))
     3         ,(HYQ(1),A(50))              ,(MBARAA(1),A(136))
     4         ,(MAR(1),A(145))             ,(MRR(1),A(163))
     5         ,(S(1),A(82))
C
C     ECPT LIST FOR BASIC BENDING TRIANGLE             NAME IN
C                                                      THIS
C     ECPT                                             ROUTINE   TYPE
C     ******************************************************************
C     ECPT( 1) = ELEMENT ID                            NECPT(1)  INTEGER
C     ECPT( 2) = GRID POINT A                          NGRID(1)  INTEGER
C     ECPT( 3) = GRID POINT B                          NGRID(2)  INTEGER
C     ECPT( 4) = GRID POINT C                          NGRID(3)  INTEGER
C     ECPT( 5) = THETA = ANGLE OF MATERIAL             ANGLE     REAL
C     ECPT( 6) = MATERIAL ID 1                         MATID1    INTEGER
C     ECPT( 7) = I = MOMENT OF INERTIA                 EYE       REAL
C     ECPT( 8) = MATERIAL ID 2                         MATID2    INTEGER
C     ECPT( 9) = T2                                    T2        REAL
C     ECPT(10) = NON-STRUCTURAL-MASS                   FMU       REAL
C     ECPT(11) = Z1                                    Z11       REAL
C     ECPT(12) = Z2                                    Z22       REAL
C     ECPT(13) = COORD. SYSTEM ID 1                    NECPT(13) INTEGER
C     ECPT(14) = X1                                    X1        REAL
C     ECPT(15) = Y1                                    Y1        REAL
C     ECPT(16) = Z1                                    Z1        REAL
C     ECPT(17) = COORD. SYSTEM ID 2                    NECPT(17) INTEGER
C     ECPT(18) = X2                                    X2        REAL
C     ECPT(19) = Y2                                    Y2        REAL
C     ECPT(20) = Z2                                    Z2        REAL
C     ECPT(21) = COORD. SYSTEM ID 3                    NECPT(21) INTEGER
C     ECPT(22) = X3                                    X3        REAL
C     ECPT(23) = Y3                                    Y3        REAL
C     ECPT(24) = Z3                                    Z3        REAL
C     ECPT(25) = ELEMENT TEMPERATURE                   ELTEMP    REAL
C     ******************************************************************
C
C     SETTING UP G MATRIX
C
      INFLAG = 2
      MATID = MATID1
      CALL MAT( ECPT(1) )
C
C     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
C
      G(1) = G11
      G(2) = G12
      G(3) = G13
      G(4) = G12
      G(5) = G22
      G(6) = G23
      G(7) = G13
      G(8) = G23
      G(9) = G33
C
C     COMPUTATION OF D = I.G-MATRIX (EYE IS INPUT FROM THE ECPT)
C
      DO 50 I = 1,9
   50 D(I) = G(I) * DBLE(EYE)
C
C     F1LL  (HBAR) MATRIX STORING AT A(100). . .A(135)
      XCSQ = XSUBC ** 2
      YCSQ = YSUBC ** 2
      XBSQ = XSUBB ** 2
      XCYC = XSUBC * YSUBC
C
      DO 80 I = 100,135
   80 A(I) = 0.0D0
C
      A(100) = XBSQ
      A(103) = XBSQ * XSUBB
      A(107) = XSUBB
      A(112) = -2.0D0 * XSUBB
      A(115) = -3.0D0 * XBSQ
      A(118) = XCSQ
      A(119) = XCYC
      A(120) = YCSQ
      A(121) = XCSQ * XSUBC
      A(122) = YCSQ * XSUBC
      A(123) = YCSQ * YSUBC
      A(125) = XSUBC
      A(126) = YSUBC * 2.0D0
      A(128) = XCYC  * 2.0D0
      A(129) = YCSQ  * 3.0D0
      A(130) =-2.0D0 * XSUBC
      A(131) =-YSUBC
      A(133) =-3.0D0 * XCSQ
      A(134) =-YCSQ
C
C
C     ******************************************************************
C
      IF( T2 .EQ. 0.0E0 ) GO TO 110
C
C     ALL OF THE FOLLOWING OPERATIONS THROUGH STATEMENT LABEL 110
C     ARE NECESSARY IF T2 IS NON-ZERO.
C
C
C     GET THE G2X2 MATRIX
C
      MATID = MATID2
      INFLAG = 3
      CALL MAT( ECPT(1) )
      IF(G2X211.EQ.0.0E0 .AND. G2X212.EQ.0.0E0 .AND. G2X222.EQ.0.0E0)
     1  GO TO 110
      G2X2(1) = G2X211 * T2
      G2X2(2) = G2X212 * T2
      G2X2(3) = G2X2(2)
      G2X2(4) = G2X222 * T2
C
      DETERM = G2X2(1) * G2X2(4)  -  G2X2(3) * G2X2(2)
      J2X2(1) = G2X2(4) / DETERM
      J2X2(2) =-G2X2(2) / DETERM
      J2X2(3) = J2X2(2)
      J2X2(4) = G2X2(1) / DETERM
C
C
C     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
C       YQ  RIGHT PORTION IS COMPUTED AND USED AS A  (2X3). THE LEFT
C           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
C           A(50)...A(55) UNTIL NOT NEEDED ANY FURTHER.
C
C
C
      TEMP  = 2.0D0 * D(2) + 4.0D0 * D(9)
      HYQ(1) = -6.0D0 * (J2X2(1) * D(1) + J2X2(2) * D(3))
      HYQ(2) = -J2X2(1) * TEMP - 6.0D0 * J2X2(2) * D(6)
      HYQ(3) = -6.0D0 * (J2X2(1) * D(6) + J2X2(2) * D(5))
      HYQ(4) = -6.0D0 * (J2X2(2) * D(1) + J2X2(4) * D(3))
      HYQ(5) = -J2X2(2) * TEMP - 6.0D0 * J2X2(4) * D(6)
      HYQ(6) = -6.0D0 * (J2X2(2) * D(6) + J2X2(4) * D(5))
C
C     ADD TO 6 OF THE (HBAR) ELEMENTS THE RESULT OF (H  )(H  )
C                                                    UY   YQ
C     THE PRODUCT IS FORMED DIRECTLY IN THE ADDITION PROCESS BELOW.
C     NO (H  ) MATRIX IS ACTUALLY COMPUTED DIRECTLY.
C          UY
C
C     THE FOLLOWING IS THEN STEP 6 PAGE 8, FMMS-66
C
      DO 100 I = 1,3
      A(I + 102) = A(I + 102) + XSUBB * HYQ(I)
  100 A(I + 120) = A(I + 120) + XSUBC * HYQ(I)    + YSUBC * HYQ(I + 3)
C
C     THIS ENDS ADDED COMPUTATION FOR CASE OF T2 NOT ZERO
C
C     ******************************************************************
C
  110 CONTINUE
C
C     AT THIS POINT INVERT  (H) WHICH IS STORED AT A(100). . .A(135)
C     STORE INVERSE BACK IN A(100). . A(135)
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ISING = -1
      CALL INVERD(6,A(100),6,A(136),0,DETERM,ISING,A(142))
C
C     CHECK TO SEE IF H WAS SINGULAR
      IF( ISING .NE. 2 ) GO TO 120
C
C
C     ISING = 2 IMPLIES SINGULAR MATRIX THUS ERROR CONDITION.
      CALL MESAGE(30,33,ECPT(1))
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
      NOGO=1
      RETURN
C
C
CHURN OUT INTEGRAL VALUES I   USED IN REFERENCED M MATRICES
C                          IJ                           SEE P.9, FMMS-66
C
C     THE CALCULATION FOR  (I  ) ARE AS FOLLOWS
C                            IJ
C                                                      ***
C         A1  = XSUBB * YSUBC**(J+1) / ((J+1)*(J+2))      *
C           0J                                            *
C                                                         *
C         B   = XSUBC * YSUBC**(J+1) / (J+2)              *
C           0J                                            ** J=0,6
C                                                         *
C         A   = A1   + B                                  *
C           0J    0J    0J                                *
C                                                         *
C         I   = MU * A1                                   *
C           0J         0J                              ***
C
C                                                            ***
C         A1  = I * XSUBB * A      /(I+J+2)                     *
C           IJ               I-1,J                              *
C                                                               *
C         B   = XSUBC**(I+1) * YSUBC**(J+1) /((I+1)*(I+J+2))    *  I=1,6
C           IJ                                                  ** J=0,6
C                                                               *
C         A   = A1   + B                                        *
C           IJ    IJ    IJ                                      *
C                                                               *
C         I     MU * A1                                         *
C           IJ=        IJ                                       *
C                                                            ***
C      NOTE.. LOOPS FOR PROGRAM BEGIN AT 1 INSTEAD OF 0
C                                      I.E.  I = 1,7
C                                            J = 1,7
C
  120 DO 140 J=1,7
      YPRODJ = YSUBC **J
      FJ  = J
      FJ2 = J+1
      AIJ       = XSUBB * YPRODJ /(FJ * FJ2)
      BIJ       = XSUBC * YPRODJ / FJ2
      SIIJ(1,J) = FMU   * AIJ
      AIJ       = AIJ   + BIJ
      IF(J .EQ. 7) GO TO 140
      K = 8 - J
      DO 130 I = 2,K
          XPRODI = XSUBC **I
          FI     = I
          FIJ    = I + J
          AIJ       = (FI-1.0D0) * XSUBB * AIJ / FIJ
          BIJ       = XPRODI * YPRODJ /(FI * FIJ)
          SIIJ(I,J) = FMU * AIJ
  130     AIJ       = AIJ + BIJ
C
  140 CONTINUE
      SIZERO = SIIJ(1,1) / 3.0D0
C
CHUNK IN NUMBERS FOR (M-BAR-AA)    3X3 MATRIX AS PER MS-48, PAGES 6-10
C
C                    (M  )         3X6 MATRIX
C                      AR
C
C                    (M  )         6X6 MATRIX
C                      RR
C
C     (M-BAR-AA) MATRIX
C
      MBARAA(1) =  SIIJ(1,1)
      MBARAA(2) =  SIIJ(1,2)
      MBARAA(3) = -SIIJ(2,1)
      MBARAA(4) =  SIIJ(1,2)
      MBARAA(5) =  SIIJ(1,3)
      MBARAA(6) = -SIIJ(2,2)
      MBARAA(7) = -SIIJ(2,1)
      MBARAA(8) = -SIIJ(2,2)
      MBARAA(9) =  SIIJ(3,1)
C
C     (M  ) MATRIX
C       AR
      MAR( 1) = SIIJ(3,1)
      MAR( 2) = SIIJ(2,2)
      MAR( 3) = SIIJ(1,3)
      MAR( 4) = SIIJ(4,1)
      MAR( 5) = SIIJ(2,3)
      MAR( 6) = SIIJ(1,4)
      MAR( 7) = SIIJ(3,2)
      MAR( 8) = SIIJ(2,3)
      MAR( 9) = SIIJ(1,4)
      MAR(10) = SIIJ(4,2)
      MAR(11) = SIIJ(2,4)
      MAR(12) = SIIJ(1,5)
      MAR(13) =-SIIJ(4,1)
      MAR(14) =-SIIJ(3,2)
      MAR(15) =-SIIJ(2,3)
      MAR(16) =-SIIJ(5,1)
      MAR(17) =-SIIJ(3,3)
      MAR(18) =-SIIJ(2,4)
C
C     (M  ) MATRIX  A 6X6 SYMMETRIC MATRIX
C       RR
      MRR( 1) = SIIJ(5,1)
      MRR( 2) = SIIJ(4,2)
      MRR( 3) = SIIJ(3,3)
      MRR( 4) = SIIJ(6,1)
      MRR( 5) = SIIJ(4,3)
      MRR( 6) = SIIJ(3,4)
      MRR( 7) = MRR(2)
      MRR( 8) = SIIJ(3,3)
      MRR( 9) = SIIJ(2,4)
      MRR(10) = SIIJ(5,2)
      MRR(11) = SIIJ(3,4)
      MRR(12) = SIIJ(2,5)
      MRR(13) = MRR(3)
      MRR(14) = MRR(9)
      MRR(15) = SIIJ(1,5)
      MRR(16) = SIIJ(4,3)
      MRR(17) = SIIJ(2,5)
      MRR(18) = SIIJ(1,6)
      MRR(19) = MRR( 4)
      MRR(20) = MRR(10)
      MRR(21) = MRR(16)
      MRR(22) = SIIJ(7,1)
      MRR(23) = SIIJ(5,3)
      MRR(24) = SIIJ(4,4)
      MRR(25) = MRR( 5)
      MRR(26) = MRR(11)
      MRR(27) = MRR(17)
      MRR(28) = MRR(23)
      MRR(29) = SIIJ(3,5)
      MRR(30) = SIIJ(2,6)
      MRR(31) = MRR( 6)
      MRR(32) = MRR(12)
      MRR(33) = MRR(18)
      MRR(34) = MRR(24)
      MRR(35) = MRR(30)
      MRR(36) = SIIJ(1,7)
C
      IF(T2 .EQ. 0.0) GO TO 146
      IF(G2X211.EQ.0.0E0 .AND. G2X212.EQ.0.0E0 .AND. G2X222.EQ.0.0E0)
     1   GO TO 146
C
      MAR( 4) = MAR( 4)   + HYQ(1) * SIIJ(2,1) + HYQ(4) * SIIJ(1,2)
      MAR( 5) = MAR( 5)   + HYQ(2) * SIIJ(2,1) + HYQ(5) * SIIJ(1,2)
      MAR( 6) = MAR( 6)   + HYQ(3) * SIIJ(2,1) + HYQ(6) * SIIJ(1,2)
      MAR(10) = MAR(10)   + HYQ(1) * SIIJ(2,2) + HYQ(4) * SIIJ(1,3)
      MAR(11) = MAR(11)   + HYQ(2) * SIIJ(2,2) + HYQ(5) * SIIJ(1,3)
      MAR(12) = MAR(12)   + HYQ(3) * SIIJ(2,2) + HYQ(6) * SIIJ(1,3)
      MAR(16) = MAR(16)   - HYQ(1) * SIIJ(3,1) - HYQ(4) * SIIJ(2,2)
      MAR(17) = MAR(17)   - HYQ(2) * SIIJ(3,1) - HYQ(5) * SIIJ(2,2)
      MAR(18) = MAR(18)   - HYQ(3) * SIIJ(3,1) - HYQ(6) * SIIJ(2,2)
      MRR( 4) = MRR( 4)   + HYQ(1) * SIIJ(4,1) + HYQ(4) * SIIJ(3,2)
      MRR( 5) = MRR( 5)   + HYQ(2) * SIIJ(4,1) + HYQ(5) * SIIJ(3,2)
      MRR( 6) = MRR( 6)   + HYQ(3) * SIIJ(4,1) + HYQ(6) * SIIJ(3,2)
      MRR(10) = MRR(10)   + HYQ(1) * SIIJ(3,2) + HYQ(4) * SIIJ(2,3)
      MRR(11) = MRR(11)   + HYQ(2) * SIIJ(3,2) + HYQ(5) * SIIJ(2,3)
      MRR(12) = MRR(12)   + HYQ(3) * SIIJ(3,2) + HYQ(6) * SIIJ(2,3)
      MRR(16) = MRR(16)   + HYQ(1) * SIIJ(2,3) + HYQ(4) * SIIJ(1,4)
      MRR(17) = MRR(17)   + HYQ(2) * SIIJ(2,3) + HYQ(5) * SIIJ(1,4)
      MRR(18) = MRR(18)   + HYQ(3) * SIIJ(2,3) + HYQ(6) * SIIJ(1,4)
      MRR(19) = MRR( 4)
      MRR(20) = MRR(10)
      MRR(21) = MRR(16)
      MRR(22) = MRR(22)   + HYQ(1) * (HYQ(1) * SIIJ(3,1) + 2.0D0
     1 * (SIIJ(5,1) + HYQ(4) * SIIJ(2,2))) + HYQ(4) * (2.0D0 * SIIJ(4,2)
     2 + HYQ(4) * SIIJ(1,3))
      MRR(23) = MRR(23)   + HYQ(2) * SIIJ(5,1) + HYQ(5) * SIIJ(4,2)
     1 + HYQ(1) * (SIIJ(3,3) + HYQ(2) * SIIJ(3,1) + HYQ(5) * SIIJ(2,2))
     2 + HYQ(4) * (SIIJ(2,4) + HYQ(2) * SIIJ(2,2) + HYQ(5) * SIIJ(1,3))
      MRR(24) = MRR(24)   + HYQ(3) * SIIJ(5,1) + HYQ(6) * SIIJ(4,2)
     1 + HYQ(1) * (SIIJ(2,4) + HYQ(3) * SIIJ(3,1) + HYQ(6) * SIIJ(2,2))
     2 + HYQ(4) * (SIIJ(1,5) + HYQ(3) * SIIJ(2,2) + HYQ(6) * SIIJ(1,3))
      MRR(25) = MRR( 5)
      MRR(26) = MRR(11)
      MRR(27) = MRR(17)
      MRR(28) = MRR(23)
      MRR(29) = MRR(29)   + HYQ(2) * (HYQ(2) * SIIJ(3,1) + 2.0D0
     1 * (SIIJ(3,3) + HYQ(5) * SIIJ(2,2))) + HYQ(5) * (2.0D0 * SIIJ(2,4)
     2 + HYQ(5) * SIIJ(1,3))
      MRR(30) = MRR(30)   + HYQ(3) * SIIJ(3,3) + HYQ(6) * SIIJ(2,4)
     1 + HYQ(2) * (SIIJ(2,4) + HYQ(3) * SIIJ(3,1) + HYQ(6) * SIIJ(2,2))
     2 + HYQ(5) * (SIIJ(1,5) + HYQ(3) * SIIJ(2,2) + HYQ(6) * SIIJ(1,3))
      MRR(31) = MRR( 6)
      MRR(32) = MRR(12)
      MRR(33) = MRR(18)
      MRR(34) = MRR(24)
      MRR(35) = MRR(30)
      MRR(36) = MRR(36)   + HYQ(3) * (HYQ(3) * SIIJ(3,1) + 2.0D0
     1 * (SIIJ(2,4) + HYQ(6) * SIIJ(2,2))) + HYQ(6) * (2.0D0 * SIIJ(1,5)
     2 + HYQ(6) * SIIJ(1,3))
C
  146 CONTINUE
C
C
C     FILL S-MATRIX EQUIVALENCED TO A(82)  (S IS  6X3 )
C
      S( 1) = 1.0D0
      S( 2) = 0.0D0
      S( 3) =-XSUBB
      S( 4) = 0.0D0
      S( 5) = 1.0D0
      S( 6) = 0.0D0
      S( 7) = 0.0D0
      S( 8) = 0.0D0
      S( 9) = 1.0D0
      S(10) = 1.0D0
      S(11) = YSUBC
      S(12) =-XSUBC
      S(13) = 0.0D0
      S(14) = 1.0D0
      S(15) = 0.0D0
      S(16) = 0.0D0
      S(17) = 0.0D0
      S(18) = 1.0D0
C
CAN NOW COMPUTE 9 (3X3) MASS MATRICES (FMMS-66, PAGES 10-11)
C
C
C                -1 T           -1
C     ( M ) = ( H  )  ( M  ) ( H  )
C                        RR
C
C              PARTITION (M)
C                                           ///       ///
C                                           /     *     /
C                                           / MBB * MBC /
C                                           /     *     /
C                                ( M )  =   / ********* /
C                                           /     *     /
C                                           / MCB * MCC /
C                                           /     *     /
C                                           ///       ///
C                                                       4 (3X3) MATRICES
C                        -1
C     ( M  ) = ( M  ) ( H  )
C        AI       AR
C
C              PARTITION (M  )              ///                 ///
C                          AI               /          *          /
C                               ( M  )  =   / M-BAR-AB * M-BAR-AC /
C                                  AI       /          *          /
C                                           ///                 ///
C                                                       2 (3X3) MATRICES
C                               T            T
C     ( MAB )  = (M-BAR-AB) - (S ) (MBB) - (S ) (MCB)
C                               B            C
C
C                               T            T
C     ( MAC )  = (M-BAR-AC) - (S ) (MBC) - (S ) (MCC)
C                               B            C
C
C                               T     T      T      T
C     ( MAA )  = (M-BAR-AA) - (S ) (M  ) - (S ) (MAC )
C                               B    AB      C
C
C                           - (M-BAR-AB) (S ) - (M-BAR-AC) (S )
C                                          B                 C
C
C                    T
C     ( MBA )  = (MAB )
C
C                    T
C     ( MCA )  = (MAC )
C
CHOOSE APPROPRIATE BLOCK OF A-ARRAY FOR STORAGE
C
C     (3X3)    STORED IN      (3X3)     STORED IN     (3X3)    STORED IN
C     (MAA)   A( 1... 9)      (MAB)   A(10)...8)      (MAC)   A(19...27)
C     (MBA)   A(28...36)      (MBB)   A(37)...45)     (MBC)   A(46...54)
C     (MCA)   A(55...63)      (MCB)   A(64...72)      (MCC)   A(73...81)
C
C       -1
C     (H  ) IS STORED AT A(100...135)
C     (S)   EQUIVALENCED A( 81... 99)
C     WORKING STORAGE IS A(181...216)
C     (M-BAR-AB) STORED UNTIL NO LONGER NEEDED IN A(163...171)
C     (M-BAR-AC) STORED UNTIL NO LONGER NEEDED IN A(172...180)
C
C               -1 T          -1
COMPUTE (M) = (H  )  ((M  ) (H  ))
C                       RR
C
      CALL GMMATD(MRR(1),6,6,0,A(100),6,6,0,A(37))
      CALL GMMATD(A(100),6,6,1,A( 37),6,6,0,A( 1))
C
CREATE PARTITION OF 4 (3X3)
      DO 150 I=1,3
        A(I+36) = A(I   )
        A(I+39) = A(I+ 6)
        A(I+42) = A(I+12)
C
        A(I+45) = A(I+ 3)
        A(I+48) = A(I+ 9)
        A(I+51) = A(I+15)
C
        A(I+63) = A(I+18)
        A(I+66) = A(I+24)
        A(I+69) = A(I+30)
C
        A(I+72) = A(I+21)
        A(I+75) = A(I+27)
  150   A(I+78) = A(I+33)
C
COMPUTE                 -1
C       (M  ) = (M  ) (H  )    AND  PARTITION INTO 2 (3X3)  (M-BAR-AB)
C         AI      AR                                    AND (M-BAR-AC)
C
      CALL GMMATD(MAR(1),3,6,0,A(100),6,6,0,A(181))
      DO 160 I=1,3
        A(I+162) = A(I+180)
        A(I+165) = A(I+186)
        A(I+168) = A(I+192)
C
        A(I+171) = A(I+183)
        A(I+174) = A(I+189)
  160   A(I+177) = A(I+195)
COMPUTE (MAB)
      CALL GMMATD(S( 1),3,3,1,A(37),3,3,0,A(181))
      CALL GMMATD(S(10),3,3,1,A(64),3,3,0,A(190))
      DO 170 I=1,9
  170   A(I+9)=A(I+162) - A(I+180) - A(I+189)
COMPUTE (MAC)
      CALL GMMATD(S( 1),3,3,1,A(46),3,3,0,A(181))
      CALL GMMATD(S(10),3,3,1,A(73),3,3,0,A(190))
      DO 180 I=1,9
  180   A(I+18) = A(I+171) - A(I+180) - A(I+189)
COMPUTE (MAA)
      CALL GMMATD(S(  1),3,3,1,A(10),3,3,1,A(181))
      CALL GMMATD(S( 10),3,3,1,A(19),3,3,1,A(190))
      CALL GMMATD(A(163),3,3,0,S( 1),3,3,0,A(199))
      CALL GMMATD(A(172),3,3,0,S(10),3,3,0,A(208))
      DO 190 I=1,9
  190   A(I) = MBARAA(I) - A(I+180) - A(I+189) - A(I+198) - A(I+207)
COMPUTE (MBA) AND (MCA)
      DO 200 I=1,3
        NPT = 3 * I + 7
        A(I+27) = A(NPT)
        A(I+30) = A(NPT + 1)
        A(I+33) = A(NPT + 2)
C
        A(I+54) = A(NPT +  9)
        A(I+57) = A(NPT + 10)
  200   A(I+60) = A(NPT + 11)
C
      RETURN
C
      END
