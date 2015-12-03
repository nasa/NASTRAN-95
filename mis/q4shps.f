      SUBROUTINE Q4SHPS (XI,ETA, SHP, SSHP)
C     &    ENTRY Q4SHPD (DI,ETD,DSHP,DSSHP)
C
C*****
C     COMPUTES SHAPE FUNCTIONS AND THEIR DERIVATIVES
C     FOR THE QUAD4 ELEMENT
C*****
C
      REAL             XI,ETA, SHP(4), SSHP(8),CLC(2,4)
      DOUBLE PRECISION DI,ETD,DSHP(4),DSSHP(8),DLD(2,4)
      DATA   CLC /-1.0  ,-1.0  ,1.0  ,-1.0  ,1.0  ,1.0  ,-1.0  ,1.0  /
      DATA   DLD /-1.0D0,-1.0D0,1.0D0,-1.0D0,1.0D0,1.0D0,-1.0D0,1.0D0/
C
C     SINGLE PRECISION -
C
      DO 10 I=1,4
      SHP (I  ) = 0.25 * (1.0+XI *CLC(1,I)) * (1.0+ETA*CLC(2,I))
      SSHP(I  ) = 0.25 * (1.0+ETA*CLC(2,I)) * CLC(1,I)
      SSHP(I+4) = 0.25 * (1.0+XI *CLC(1,I)) * CLC(2,I)
   10 CONTINUE
      RETURN
C
      ENTRY Q4SHPD (DI,ETD,DSHP,DSSHP)
C     ================================
C
C     DOUBLE PRECISION -
C
      DO 20 I=1,4
      DSHP (I  ) = .25D0 * (1.D0+DI *DLD(1,I)) * (1.D0+ETD*DLD(2,I))
      DSSHP(I  ) = .25D0 * (1.D0+ETD*DLD(2,I)) * DLD(1,I)
      DSSHP(I+4) = .25D0 * (1.D0+DI *DLD(1,I)) * DLD(2,I)
   20 CONTINUE
      RETURN
      END
