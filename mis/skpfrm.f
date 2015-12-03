      SUBROUTINE SKPFRM (BFRAMS)
C
      INTEGER         BFRAMS,BFRMS,PLOTER,CAMERA,A(10),ADV10(3),CON10
      REAL            SAVE(2,4),XYMAX(2)
      COMMON /PLTDAT/ MODEL,PLOTER,REG(2,2),AXYMAX(2),EDGE(2),CAMERA,
     1                SKPPLT(9),PXYMAX(7),ORIGIN(2)
      DATA    ADV10 , CON10 / 1,2,3, 3 /
C
      DO 101 I = 1,2
      SAVE(I,1) = REG(I,1)
      REG(I,1)  = 0.
      SAVE(I,2) = REG(I,2)
      REG(I,2)  = AXYMAX(I)+2.*EDGE(I)
      SAVE(I,3) = ORIGIN(I)
      ORIGIN(I) = 0.
      SAVE(I,4) = EDGE(I)
      EDGE(I)   = 0.
  101 CONTINUE
      XYMAX(1) = AMAX1(REG(1,2),REG(2,2))
      XYMAX(2) = AMIN1(REG(1,2),REG(2,2))
      REG(1,2) = XYMAX(1)
      REG(2,2) = XYMAX(2)
      BFRMS    = MIN0(MAX0(BFRAMS,1),5)
C
C     PLOTTER 1, 2
C
      A(1) = CON10
      A(2) = ADV10(CAMERA)
      DO 141 I = 3,6
      A(I) = 0
  141 CONTINUE
      DO 142 I = 1,BFRMS
      CALL WPLT10 (A,0)
  142 CONTINUE
      GO TO 300
C
  300 DO 301 I = 1,2
      REG(I,1) = SAVE(I,1)
      REG(I,2) = SAVE(I,2)
      ORIGIN(I)= SAVE(I,3)
      EDGE(I)  = SAVE(I,4)
  301 CONTINUE
C
      RETURN
      END
