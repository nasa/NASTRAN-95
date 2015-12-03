      SUBROUTINE HBDYS
C
C     THIS IS THE BOUNDARY CONDITION (HEAT) ELEMENT ROUTINE
C     IT PRODUCES THE STIFFNESS AND OR DAMPING ELEMENT MATRICES.
C
      LOGICAL         HEAT     ,NOGO
      INTEGER         NGRIDS(7),NECPT(53),OUTPT    ,SILTAB(8)  ,
     1                SET1(8)  ,SET2(4)  ,SILS     ,DICT(13)   ,
     2                ELID     ,ESTID
      REAL            C(16)    ,CC(4,4)  ,PI       ,MASTER(8,8),
     1                MAST(64) ,KE       ,ME       ,ITEMP      ,
     2                A1(5)    ,A2(3)    ,A3(3)    ,A4(3)
      REAL            ECPT(53)
      CHARACTER       UFM*23   ,UWM*25   ,UIM*29   ,SFM*25
      COMMON /XMSSG / UFM      ,UWM      ,UIM      ,SFM
      COMMON /SYSTEM/ KSYSTM(100)
      COMMON /EMGEST/ ECPT1    ,IFLAG    ,SILS(8)  ,V(3)       ,
     1                ECPT14   ,MATFLG   ,AF       ,EMISS      ,
     2                ABSORP   ,R1       ,R2       ,CSID(4,8)  ,
     3                AVGTMP
      COMMON /HMTOUT/ HX       ,CPX
      COMMON /MATIN / MATID    ,INFLAG   ,ELTEMP
      COMMON /EMGPRM/ D15(15)  ,KMBGG(3) ,IPREC    ,NOGO       ,
     1                HEAT     ,ICMBAR
      COMMON /EMGDIC/ DMMM(2)  ,NLOCS    ,ELID     ,ESTID
      COMMON /CONDAS/ PI
      EQUIVALENCE     (NECPT(1), ECPT(1)), (SET2(1), SET1(5))  ,
     1                (ECPT1   , ECPT(1)), (DICT5  , DICT(5))  ,
     2                (KSYSTM(2),OUTPT  ), (CC(1,1), C(1)   )  ,
     3                (MASTER(1,1),MAST(1))
      DATA    NGRIDS/ 1, 2, 2, 3, 4, 2 ,2/
C
C     EST ENTRY FOR -CHBDY- ELEMENT
C     ======================================================
C     ECPT( 1)  = EL-ID       ELEMENT ID
C     ECPT( 2)  = IFLAG       ELEM. TYPE FLAG = (1,2,3,4,5,6,7)
C     ECPT( 3)  = SIL-1       SCALER INDICES
C     ECPT( 4)  = SIL-2
C     ECPT( 5)  = SIL-3
C     ECPT( 6)  = SIL-4
C     ECPT( 7)  = SIL-5
C     ECPT( 8)  = SIL-6
C     ECPT( 9)  = SIL-7
C     ECPT(10)  = SIL-8
C     ECPT(11)  = V1          ORIENTATION VECTOR
C     ECPT(12)  = V2
C     ECPT(13)  = V3
C     ECPT(14)  = ECPT14
C     ECPT(15)  = MATFLG      MAT ID FOR MAT4, MAT5 DATA
C     ECPT(16)  = AF          AREA FACTOR
C     ECPT(17)  = EMISS       EMISSIVITY COEFF
C     ECPT(18)  = ABSORP      ABSORPTIVITY COEFF
C     ECPT(19)  = R1          RADII OF ELIPTICAL CYLINDER
C     ECPT(20)  = R2
C     ECPT(21)  = CSID-1      COORDINATE SYSTEM ID AND
C     ECPT(22)  = X1          COORDINATE GRID POINTS
C     ECPT(23)  = Y1          (1-4 ARE ELEMENT POINTS,
C     ECPT(24)  = Z1
C     ECPT(25)  = CSID-2
C     ECPT(26)  = X2
C     ECPT(27)  = Y2
C     ECPT(28)  = Z2
C     ECPT(29)  = CSID-3
C     ECPT(30)  = X3
C     ECPT(31)  = Y3
C     ECPT(32)  = Z3
C     ECPT(33)  = CSID-4
C     ECPT(34)  = X4
C     ECPT(35)  = Y4
C     ECPT(36)  = Z4
C     ECPT(37)  = CSID-5       5-8 ARE POINTS IN THE FLUID)
C       -ETC-     -ETC-
C     ECPT(53)  = AVGTMP      AVERAGE ELEM. TEMPERATURE
C
C
C     GENERAL INITIALIZATION
C
      IF (.NOT. HEAT) RETURN
      IMHERE = 0
      IF (IFLAG.LT.1 .OR. IFLAG.GT.7) GO TO 470
      IF (IFLAG .EQ. 7) AF = PI*(R1+R2)
      N = NGRIDS(IFLAG)
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(4) = 1
      DICT5   = 0.0
C
C     MASTER OUTPUT MATRIX OF SIZE UP TO 8 X 8 IS FORMED.  DUPLICATE
C     SILS ARE SUPERIMPOSED RESULTING IN A POSSIBLY SMALLER OUTPUT MATRX
C
C     FOR A GIVEN ELEMENT-ID THE MATRIX OUTPUT WILL BE OF ORDER EQUAL
C     TO THE NUMBER OF UNIQUE SILS PRESENT.
C
C     IFLAG = 1 WILL BE 1X1 OR 2X2    *
C     IFLAG = 2 WILL BE 2X2 UP TO 4X4  *
C     IFLAG = 3 WILL BE 2X2 UP TO 4X4   * (DEPENDING ON GROUDING AND
C     IFLAG = 4 WILL BE 3X3 UP TO 6X6  *   DUPLICATE SILS.)
C     IFLAG = 5 WILL BE 4X4 UP TO 8X8 *
C
C     -SET1- WILL BE A MAP OF OUTPUT POSITIONS FOR SILS 1 THRU 4
C     -SET2- WILL BE A MAP OF OUTPUT POSITIONS FOR SILS 5 THRU 8
C
C
C     FIRST FORM THE TABLE OF UNIQUE SILS.
C
      ISIZE = 0
      DO 50 I = 1,8
      IF (SILS(I) .LE. 0) GO TO 50
      IF (ISIZE   .LE. 0) GO TO 40
      DO 30 J = 1,ISIZE
      IF (SILS(I) .EQ. SILTAB(J)) GO TO 50
   30 CONTINUE
   40 ISIZE = ISIZE + 1
      SILTAB(ISIZE) = SILS(I)
   50 CONTINUE
      CALL SORT (0,0,1,1,SILTAB(1),ISIZE)
      IMHERE = 50
      IF (ISIZE .LE. 0) GO TO 470
C
C     BUILD -SET1- AND -SET2- MAPS OF WHERE OUTPUTS GO IN MASTER OUTPUT.
C
      DO 100 I = 1,8
      J = 8
      IF (SILS(I) .LE. 0) GO TO 90
      DO 80 J = 1,ISIZE
      IF (SILS(I) .EQ. SILTAB(J)) GO TO 90
   80 CONTINUE
      IMHERE = 80
      GO TO 470
   90 SET1(I) = J
  100 CONTINUE
      DICT(3) = ISIZE
C
C     FORM STIFFNESS -HEAT- IF REQUESTED.
C
      IF (KMBGG(1) .EQ. 0) GO TO 360
      INFLAG = 1
      ELTEMP = AVGTMP
      MATID  = MATFLG
      IF (MATID .EQ. 0) GO TO 360
      CALL HMAT (NECPT)
      CP = CPX
      H  = HX
      IF (H .EQ. 0.0) GO TO 360
      GO TO (120,130,140,210,240,130,130), IFLAG
C
C     IFLAG = 1, (POINT), 1 GRID-POINT.  (1 X 1)  C = H * AF
C
  120 C(1) = H
      C(2) = AF
      C(1) = C(1)*C(2)
      GO TO 300
C
C     IFLAG = 2, (LINE OR ELLIPTIC CYL. )    **    **
C             2 GRID POINTS           H*AF*L * 2  1 *
C                           (2X2)  C =------ *      *
C                                       6    * 1  2 *
C                                            **    **
C
  130 C(1) = H
      C(2) = AF
      C(3) = ECPT(26) - ECPT(22)
      C(4) = ECPT(27) - ECPT(23)
      C(5) = ECPT(28) - ECPT(24)
      C(1) = C(1)*C(2)*SQRT(C(3)**2 + C(4)**2 + C(5)**2)/3.0
      C(2) = C(1)/2.0
      C(5) = C(2)
      C(6) = C(1)
      GO TO 300
C
C     IFLAG = 3, (REVOLUTION), 2 GRID-POINTS     **                **
C                                                *(3X +X )  (X + X )*
C                                        H*2PI*L *   1  2     1   2 *
C                             (2X2)  C = ------- *                  *
C                                          12    *(X + X )  (X +3X )*
C                                                *  1   2     1   2 *
C                                                **                **
C
  140 IF (ECPT(22).LE.0.0 .OR. ECPT(26).LE.0.0) GO TO 180
      IF (ECPT(23).NE.0.0 .OR. ECPT(27).NE.0.0) GO TO 180
      GO TO 200
  180 WRITE  (OUTPT,190) UFM,NECPT(1)
  190 FORMAT (A23,' 3088, ILLEGAL GEOMETRY FOR REVOLUTION ELEMENT',I14)
      NOGO = .TRUE.
      GO TO 490
C
C     FILL CONDUCTIVITIY MATRIX
C
  200 C(1) = H
      C(2) = PI
      C(3) = ECPT(26) - ECPT(22)
      C(4) = ECPT(28) - ECPT(24)
C
C     NOTE Y2 AND Y1 ARE 0 FOR REVOLUTION ELEMENT.
C
      C(1) = C(1)*C(2)*SQRT(C(3)**2 + C(4)**2)/6.0
      C(2) = C(1)*(ECPT(22) + ECPT(26))
      C(5) = C(2)
      C(6) = C(1)*(ECPT(22) + 3.0*ECPT(26))
      C(1) = C(1)*(3.0*ECPT(22) + ECPT(26))
      GO TO 300
C
C     IFLAG = 4, (TRIANGLE), 3 GRID-POINTS.       **       **
C                                                 * 2  1  1 *
C                                           H * A *         *
C                                (3X3) C =  ----- * 1  2  1 *
C                                            24   *         *
C                                                 * 1  1  2 *
C                                                 **       **
C
C
C     COMPUTE AREA -A- OF TRIANGLE   GET R2-R1 AND R3-R2
C
  210 C(1) = ECPT(26) - ECPT(22)
      C(2) = ECPT(27) - ECPT(23)
      C(3) = ECPT(28) - ECPT(24)
      C(4) = ECPT(30) - ECPT(26)
      C(5) = ECPT(31) - ECPT(27)
      C(6) = ECPT(32) - ECPT(28)
C
C     (R2-R1) X (R3-R2)  INTO  C(1),C(2),C(3)
C
      CALL SAXB (C(1),C(4),C(1))
      C(7) = SQRT(C(1)**2 + C(2)**2 + C(3)**2)
      IF (C(7) .LE. 0.0) GO TO 220
      C(2) = C(7)*H/24.0
      C(1) = 2.0 *C(2)
      C(3) = C(2)
      C(5) = C(2)
      C(6) = C(1)
      C(7) = C(2)
      C(9) = C(2)
      C(10)= C(2)
      C(11)= C(1)
      GO TO 300
  220 WRITE  (OUTPT,230) UFM,NECPT(1)
  230 FORMAT (A23,' 3089, ILLEGAL GEOMETRY FOR TRIANGLE ELEMENT',I14)
      NOGO = .TRUE.
      GO TO 490
C
C     IFLAG = 5, (QUADRILATERAL), 4 GRID-POINTS.
C
C               ***                                              ***
C               * 2(A2+A3+A4)  (A3+A4)     (A2+A4)      (A2+A3)    *
C               *                                                  *
C               *              2(A1+A3+A4) (A1+A4)      (A1+A3)    *
C   (4X4)  C  = *                                                  *
C               *                           2(A1+A2+A4) (A1+A2)    *
C               *     -SYM-                                        *
C               *                                       2(A1+A2+A3)*
C               ***                                              ***
C
C     R  =  XI, YI, ZI
C      I
C
C     A1 = MAG((R3-R2) X (R4-R3))
C     A2 = MAG((R4-R3) X (R1-R4))
C     A3 = MAG((R1-R4) X (R2-R1))
C     A4 = MAG((R2-R1) X (R3-R2))
C
C
C     R3-R2
C
  240 C( 1) = ECPT(30) - ECPT(26)
      C( 2) = ECPT(31) - ECPT(27)
      C( 3) = ECPT(32) - ECPT(28)
C
C     R4-R3
C
      C( 4) = ECPT(34) - ECPT(30)
      C( 5) = ECPT(35) - ECPT(31)
      C( 6) = ECPT(36) - ECPT(32)
C
C     R1-R4
C
      C( 7) = ECPT(22) - ECPT(34)
      C( 8) = ECPT(23) - ECPT(35)
      C( 9) = ECPT(24) - ECPT(36)
C
C     R2-R1
C
      C(10) = ECPT(26) - ECPT(22)
      C(11) = ECPT(27) - ECPT(23)
      C(12) = ECPT(28) - ECPT(24)
C
C
      CALL SAXB (C( 1),C( 4),A1(1))
      CALL SAXB (C( 4),C( 7),A2(1))
      CALL SAXB (C( 7),C(10),A3(1))
      CALL SAXB (C(10),C( 1),A4(1))
C
      C(1) = A1(1)*A2(1) + A1(2)*A2(2) + A1(3)*A2(3)
      C(2) = A1(1)*A3(1) + A1(2)*A3(2) + A1(3)*A3(3)
      C(3) = A1(1)*A4(1) + A1(2)*A4(2) + A1(3)*A4(3)
      IF (C(1)*C(2)*C(3) .LE. 0.0) GO TO 280
      A1(1) = SQRT(A1(1)**2 + A1(2)**2 + A1(3)**2)
      A1(2) = SQRT(A2(1)**2 + A2(2)**2 + A2(3)**2)
      A1(3) = SQRT(A3(1)**2 + A3(2)**2 + A3(3)**2)
      A1(4) = SQRT(A4(1)**2 + A4(2)**2 + A4(3)**2)
      A1(5) = A1(1) + A1(2) + A1(3) + A1(4)
      ITEMP = H/48.0
      DO 270 I = 1,4
      IC = 4*(I-1)
      DO 270 J = 1,4
      IJ = IC + J
      IF (I .EQ. J) GO TO 260
      C(IJ) = ITEMP*(A1(5) - A1(I) - A1(J))
      GO TO 270
  260 C(IJ) = ITEMP*(2.0*(A1(5) - A1(I)))
  270 CONTINUE
      GO TO 300
  280 WRITE  (OUTPT,290) UFM,NECPT(1)
  290 FORMAT (A23,' 3090, ILLEGAL GEOMETRY FOR QUAD. ELEMENT',I14)
      NOGO =.TRUE.
      GO TO 490
C
C     HERE WHEN -C- MATRIX OF SIZE N X N IS READY FOR INSERTION (MAPING)
C     INTO MASTER OUTPUT MATRIX OF SIZE ISIZE X ISIZE.
C
  300 DO 310 I = 1,64
      MAST(I) = 0.0
  310 CONTINUE
C
      DO 330 I = 1,N
      I1 = SET1(I)
      I2 = SET2(I)
      DO 320 J = 1,N
      J1 = SET1(J)
      J2 = SET2(J)
      KE = CC(I,J)
      MASTER(I1,J1) = MASTER(I1,J1) + KE
      MASTER(I1,J2) = MASTER(I1,J2) - KE
      MASTER(I2,J1) = MASTER(I2,J1) - KE
      MASTER(I2,J2) = MASTER(I2,J2) + KE
  320 CONTINUE
  330 CONTINUE
C
C     CONDENSE (ISIZE X ISIZE) MATRIX IN (8 X 8) MASTER ARRAY INTO A
C     SINGLE STRAND FOR OUTPUT TO EMGOUT
C
      K = 0
      DO 350 JCOL = 1,ISIZE
      DO 340 IROW = 1,ISIZE
      K = K + 1
      MAST(K) = MASTER(IROW,JCOL)
  340 CONTINUE
  350 CONTINUE
C
C     OUTPUT VIA EMGOUT THE TRIANGLE IN GLOBAL FOR STIFFNESS MATRIX
C
      CALL EMGOUT (MAST(1),MAST(1),K,1,DICT,1,IPREC)
C
C     FORM DAMPING -HEAT- IF REQUESTED.
C
  360 IF (KMBGG(3) .EQ. 0) GO TO 490
      INFLAG = 4
      ELTEMP = AVGTMP
      MATID  = MATFLG
      IF (MATID .EQ. 0) GO TO 490
      CALL HMAT (NECPT)
      CP = HX
      IF (CP .EQ. 0.0) GO TO 490
      GO TO (380,390,400,410,420,390,390), IFLAG
C
C     IFLAG = 1, (POINT), 1 GRID-POINT.  (1 X 1)  C = CP* AF
C
  380 C(1) = CP
      C(2) = AF
      C(1) = C(1)*C(2)
      GO TO 440
C
C     IFLAG = 2, (LINE OR ELLIPTIC CYL. )
C             2 GRID POINTS           CP*AF*L*      *
C                                  C = ------*1 , 1 *
C                                        2   *      *
C
  390 C(1) = CP
      C(2) = AF
      C(3) = ECPT(26) - ECPT(22)
      C(4) = ECPT(27) - ECPT(23)
      C(5) = ECPT(28) - ECPT(24)
      C(1) = C(1)*C(2)*SQRT(C(3)**2 + C(4)**2 + C(5)**2)/2.0
      C(2) = C(1)
      GO TO 440
C
C     IFLAG = 3, (REVOLUTION), 2 GRID-POINTS
C                                               CP*PI*L *              *
C                                           C = ------- *2X +X , 2X +X *
C                                                  3    *  1  2    2  1*
C
  400 C(1) = CP
      C(2) = PI
      C(3) = ECPT(26) - ECPT(22)
      C(4) = ECPT(28) - ECPT(24)
C
C     NOTE Y2 AND Y1 ARE 0 FOR REVOLUTION ELEMENT.
C
      C(1) = C(1)*C(2)*SQRT(C(3)**2 + C(4)**2)/3.0
      C(2) = C(1)*(ECPT(22) + 2.0*ECPT(26))
      C(1) = C(1)*(2.0*ECPT(22) + ECPT(26))
      GO TO 440
C
C     IFLAG = 4, (TRIANGLE), 3 GRID-POINTS.
C                                          CP*A *         *
C                                      C = ---- * 1, 1, 1 *
C                                           3   *         *
C
C
C     COMPUTE AREA -A- OF TRIANGLE   GET R2-R1 AND R3-R2
C
  410 C(1) = ECPT(26) - ECPT(22)
      C(2) = ECPT(27) - ECPT(23)
      C(3) = ECPT(28) - ECPT(24)
      C(4) = ECPT(30) - ECPT(26)
      C(5) = ECPT(31) - ECPT(27)
      C(6) = ECPT(32) - ECPT(28)
C
C     (R2-R1) X (R3-R2)  INTO  C(1),C(2),C(3)
C
      CALL SAXB (C(1),C(4),C(1))
      C(7) = SQRT(C(1)**2 + C(2)**2 + C(3)**2)
      C(1) = C(7)*CP/6.0
      C(2) = C(1)
      C(3) = C(1)
      GO TO 440
C
C     IFLAG = 5, (QUADRILATERAL), 4 GRID-POINTS.
C
C                                CP *                                  *
C                            C = -- * A +A +A , A +A +A , A +A +A , ETC*
C                                6  *  2  3  4   3  4  1   4  1  2     *
C
C     R  =  XI, YI, ZI
C      I
C
C     A1 = MAG((R3-R2) X (R4-R3))
C     A2 = MAG((R4-R3) X (R1-R4))
C     A3 = MAG((R1-R4) X (R2-R1))
C     A4 = MAG((R2-R1) X (R3-R2))
C
C
C     R3-R2
C
  420 C( 1) = ECPT(30) - ECPT(26)
      C( 2) = ECPT(31) - ECPT(27)
      C( 3) = ECPT(32) - ECPT(28)
C
C     R4-R3
C
      C( 4) = ECPT(34) - ECPT(30)
      C( 5) = ECPT(35) - ECPT(31)
      C( 6) = ECPT(36) - ECPT(32)
C
C     R1-R4
C
      C( 7) = ECPT(22) - ECPT(34)
      C( 8) = ECPT(23) - ECPT(35)
      C( 9) = ECPT(24) - ECPT(36)
C
C     R2-R1
C
      C(10) = ECPT(26) - ECPT(22)
      C(11) = ECPT(27) - ECPT(23)
      C(12) = ECPT(28) - ECPT(24)
C
C
      CALL SAXB (C( 1),C( 4),A1(1))
      CALL SAXB (C( 4),C( 7),A2(1))
      CALL SAXB (C( 7),C(10),A3(1))
      CALL SAXB (C(10),C( 1),A4(1))
C
      A1(1) = SQRT(A1(1)**2 + A1(2)**2 + A1(3)**2)
      A1(2) = SQRT(A2(1)**2 + A2(2)**2 + A2(3)**2)
      A1(3) = SQRT(A3(1)**2 + A3(2)**2 + A3(3)**2)
      A1(4) = SQRT(A4(1)**2 + A4(2)**2 + A4(3)**2)
      A1(5) = A1(1) + A1(2) + A1(3) + A1(4)
      ITEMP = CP/12.0
      DO 430 I = 1,4
      C(I) = ITEMP*(A1(5) - A1(I))
  430 CONTINUE
      GO TO 440
C
C     HERE WHEN DIAGONAL C MATRIX OF SIZE 1 X N IS READY FOR INSERTION
C     (MAPING) INTO MASTER DIAGONAL OUTPUT MATRIX OF SIZE 1 X ISIZE.
C
  440 DO 450 I = 1,8
      MAST(I) = 0.0
  450 CONTINUE
C
      DO 460 I = 1,N
      I1 = SET1(I)
      I2 = SET2(I)
      ME = C(I)
      MAST(I1) = MAST(I1) + ME
      MAST(I2) = MAST(I2) + ME
  460 CONTINUE
C
C     OUTPUT VIA EMGOUT THE DIAGONAL MATRIX IN GLOBAL
C
      DICT(2) = 2
      CALL EMGOUT (MAST(1),MAST(1),ISIZE,1,DICT,3,IPREC)
      GO TO 490
C
C     LOGIC ERROR
C
  470 WRITE  (OUTPT,480) SFM,IMHERE,NECPT(1),SILS
  480 FORMAT (A25,' 3037 FROM HBDYS.', /5X,
     1       'LOGIC ERROR,  IMHERE =',I5,'  ELEMENT ID =',I10, /5X,
     2       'SILS =',8I10)
      NOGO = .TRUE.
  490 RETURN
      END
