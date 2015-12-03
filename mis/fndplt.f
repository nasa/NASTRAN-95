      SUBROUTINE FNDPLT (PLOTER, MODEL, PMODEL)
C
C     PLOTER = PLOTTER INDEX.
C     MODEL = MODEL INDEX.
C     PMODEL = PLOTTER MODEL ID.
C
C...  DATA FOR PLOTTER + MODEL RECOGNITION.
C
      INTEGER PLOTER, PMODEL(2), PLTTER(2,6), PLTMDL(2,6)
C
      DATA PLTMDL /
C       NASTRAN GENERAL PURPOSE PLOTTER
     1   1HM,1, 1HT,1, 1HD,1,
     2   1HM,0, 1HT,0, 1HD,0/
      DATA PLTTER /
     1   1,-1,  2,-2,  2,-3,
     2   1,+1,  2,+2,  2,+3/
C
C     FIND THE MODEL ID.
C
      N = -1
      N1 = PMODEL(2)
      DO 120 I = 1, 6
      IF (PMODEL(1).NE.PLTMDL(1,I))  GO TO 120
      IF (N.LE.0)  N=I
      IF (N1.EQ.PLTMDL(2,I)) N = I
  120 CONTINUE
C
C     SETUP THE PLOTTER + MODEL INDICES.
C
      I2 = PMODEL(2)
      IF (N.LT.0) I2 = 0
      N = IABS (N)
      DO 130 I = 1,2
      IF (PLTMDL(I,N).NE.0)  PMODEL(I)=PLTMDL(I,N)
  130 CONTINUE
      PLOTER = PLTTER(1,N)
      MODEL  = PLTTER(2,N)
C
      RETURN
      END
