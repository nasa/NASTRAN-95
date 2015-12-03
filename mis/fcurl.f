      SUBROUTINE FCURL (FMEO, FME1, FFEO, FFE1, YI, S, LAM1)
C     ------------------------------------------------------------------
      DIMENSION  FMEO (10,2), FME1 (10,2), FFEO (10,2), FFE1 (10,2)
      DIMENSION    YI (6, 7)
      REAL LAM1
      FMEO( 1,1) = 0.0
      FMEO( 2,1) = YI(1,1)
      FMEO(3,1)  =  YI(1,2)  *  2.0
      FMEO(4,1)  =  YI(1,3)  *  3.0
      FMEO( 5,1) = YI(1,1) * LAM1
      FMEO( 6,1) = YI(1,2) * LAM1
      FMEO( 7,1) = YI(1,3) * LAM1
      FMEO( 8,1) = YI(1,4) * LAM1
      FMEO( 9,1) = YI(1,5) * LAM1
      FMEO(10,1) = YI(1,6) * LAM1
C
      FMEO( 1,2) = YI(4,1)
      FMEO( 2,2) = YI(4,2)
      FMEO( 3,2) = YI(4,3)
      FMEO( 4,2) = YI(4,4)
      FMEO( 5,2) = YI(2,1)
      FMEO( 6,2) = YI(2,2)
      FMEO( 7,2) = YI(2,3)
      FMEO( 8,2) = YI(2,4)
      FMEO( 9,2) = YI(2,5)
      FMEO(10,2) = YI(2,6)
C
      S1 = 1.0 / S
      FME1( 1,1) = 0.0
      FME1( 2,1) = S1 * YI(1,2)
      FME1(3,1)  =  S1  *  YI(1,3) * 2.0
      FME1(4,1)  =  S1  *  YI(1,4) * 3.0
      FME1( 5,1) = S1 * YI(1,2) * LAM1
      FME1( 6,1) = S1 * YI(1,3) * LAM1
      FME1( 7,1) = S1 * YI(1,4) * LAM1
      FME1( 8,1) = S1 * YI(1,5) * LAM1
      FME1( 9,1) = S1 * YI(1,6) * LAM1
      FME1(10,1) = S1 * YI(1,7) * LAM1
C
      FME1( 1,2) = S1 * YI(4,2)
      FME1( 2,2) = S1 * YI(4,3)
      FME1( 3,2) = S1 * YI(4,4)
      FME1( 4,2) = S1 * YI(4,5)
      FME1( 5,2) = S1 * YI(2,2)
      FME1( 6,2) = S1 * YI(2,3)
      FME1( 7,2) = S1 * YI(2,4)
      FME1( 8,2) = S1 * YI(2,5)
      FME1( 9,2) = S1 * YI(2,6)
      FME1(10,2) = S1 * YI(2,7)
C
      FFEO( 1,1) = 0.0
      FFEO (2,1) = 0.0
      FFEO (3,1) = 0.0
      FFEO (4,1) = 0.0
      FFEO( 5,1) = 0.0
      FFEO( 6,1) = 0.0
      FFEO( 7,1) = - 2.0 * YI(1,1)
      FFEO( 8,1) = - 6.0 * YI(1,2)
      FFEO( 9,1) = -12.0 * YI(1,3)
      FFEO(10,1) = -20.0 * YI(1,4)
C
      FFEO (1,2) = 0.0
      FFEO (2,2) = 0.0
      FFEO (3,2) = 0.0
      FFEO (4,2) = 0.0
      FFEO( 5,2) = 0.0
      FFEO( 6,2) = -YI(4,1)
      FFEO( 7,2) = -2.0 * YI(4,2)
      FFEO( 8,2) = -3.0 * YI(4,3)
      FFEO( 9,2) = -4.0 * YI(4,4)
      FFEO(10,2) = -5.0 * YI(4,5)
C
      FFE1( 1,1) = 0.0
      FFE1 (2,1) = 0.0
      FFE1 (3,1) = 0.0
      FFE1 (4,1) = 0.0
      FFE1( 5,1) = 0.0
      FFE1( 6,1) = 0.0
      FFE1( 7,1) = -S1 *  2.0 * YI(1,2)
      FFE1( 8,1) = -S1 *  6.0 * YI(1,3)
      FFE1( 9,1) = -S1 * 12.0 * YI(1,4)
      FFE1(10,1) = -S1 * 20.0 * YI(1,5)
C
      FFE1 (1,2) = 0.0
      FFE1 (2,2) = 0.0
      FFE1 (3,2) = 0.0
      FFE1 (4,2) = 0.0
      FFE1( 5,2) = 0.0
      FFE1( 6,2) = -S1 * YI(4,2)
      FFE1( 7,2) = -S1 * 2.0 * YI(4,3)
      FFE1( 8,2) = -S1 * 3.0 * YI(4,4)
      FFE1( 9,2) = -S1 * 4.0 * YI(4,5)
      FFE1(10,2) = -S1 * 5.0 * YI(4,6)
      RETURN
      END
