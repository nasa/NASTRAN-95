      SUBROUTINE FLUNAM (LU,FILNAM)
C
C     THIS ROUTINE FORMULATES A FORTRAN LOGICAL UNIT NAME FROM A
C     LOGICAL UNIT NUMBER       =       =       =    ===
C
C     INPUT  LU          e.g. LU = 8
C     OUTPUT FILNAM           FILNAM = 'fort.08'  NOTE - IS .08 NOT .8
c
      CHARACTER   FILNAM*7,FOR7*7,FOR5*5,FORT*5
      EQUIVALENCE (FOR5,FOR7)
      DATA  FORT/ 'fort.' /
C
      J = LU + 100
      WRITE  (FOR7,10) J
   10 FORMAT (4X,I3)
      FOR5   = FORT
      FILNAM = FOR7
      RETURN
      END
