      SUBROUTINE SPLT10( ICOMP, COMPS, NC)
      INTEGER COMPS(9)
      IF( ICOMP .EQ. 0) ICOMP= 1
      IC = ICOMP
      NC = 0
      DO 10  I=1,9
      IX = IC/10
      JX = IC - 10*IX
      IC = IX
      IF ( JX .EQ. 0) GO TO 5
      NC =NC+1
      COMPS(NC)= JX
    5 IF( IC .EQ. 0) GO TO 15
   10 CONTINUE
   15 IF (NC .EQ. 1) RETURN
      CALL SORT (0,0,1,1, COMPS,NC )
C
C     REMOVE DUPLICATES
      IX= 1
      DO 20 I=2,NC
      IF ( COMPS(I) .EQ. COMPS(I-1)) GO TO 20
      IX = IX+1
      COMPS(IX) = COMPS(I)
   20 CONTINUE
      NC = IX
      RETURN
      END
