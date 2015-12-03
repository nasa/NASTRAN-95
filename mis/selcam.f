      SUBROUTINE SELCAM (CAMERA,PLTNUM,OPT)
C
      REAL    EDGE(2)  ,ORIGIN(2),XYMAX(2) ,SAVE(2,3)
      INTEGER A(17)    ,CAMERA   ,CAMNUM   ,OPT      ,PLOTER   ,PLTNUM
     2,       CON10(2) ,CAM10(3)
C
      COMMON /PLTDAT/  PD(20,2)
C
      EQUIVALENCE (   MODEL,PD( 1,1)) , (  PLOTER,PD( 2,1))
     1,           (XYMAX(1),PD( 7,1)) , ( EDGE(1),PD( 9,1))
     2,           (  CAMNUM,PD(11,1)) ,(ORIGIN(1),PD( 8,2))
      DATA CON10,CAM10 / 1,2, 1,2,3 /
C
       DO 101 I = 1,2
      SAVE(I,1) = EDGE(I)
      EDGE(I) = 0.
      SAVE(I,2) = ORIGIN(I)
      ORIGIN(I) = 0.
      SAVE(I,3) = XYMAX(I)
      XYMAX(I) = 0.
      A(I) = IABS(PLTNUM)
  101 CONTINUE
      CAMNUM = MIN0(MAX0(CAMERA,1),3)
      IF(OPT) 165,160,165
C
C     PLOTTER 1, 2
C
  160 A(3) = A(1)
      A(1) = CON10(1)
      A(2) = 0
      A(4) = SAVE(1,3) + 2.*SAVE(1,1) + .1
      A(5) = SAVE(2,3) + 2.*SAVE(2,1) + .1
      A(6) = 0
      CALL WPLT10 (A,0)
  165 A(1) = CON10(2)
      A(2) = CAM10(CAMNUM)
      DO 166 I = 3,6
      A(I) = 0
  166 CONTINUE
      CALL WPLT10 (A,0)
C
      DO 301 I=1,2
      EDGE(I) = SAVE(I,1)
      ORIGIN(I) = SAVE(I,2)
      XYMAX(I) = SAVE(I,3)
  301 CONTINUE
      RETURN
      END
