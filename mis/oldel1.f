      SUBROUTINE OLDEL1
C
C     ANY ELEMENT (NEW OR OLD) WHICH HAS NOT BEEN CONVERTED TO USE
C     EMGPRO SHOULD HAVE AN ENTRY POINT IN OLDEL1, OLDEL2, OR OLDEL3
C     ***************************************************************
C
      ENTRY AXIF2S
      GO TO 10
      ENTRY AXIF2D
      GO TO 10
      ENTRY AXIF3S
      GO TO 10
      ENTRY AXIF3D
      GO TO 10
      ENTRY AXIF4S
      GO TO 10
      ENTRY AXIF4D
      GO TO 10
      ENTRY CONES
      GO TO 10
      ENTRY CONED
      GO TO 10
      ENTRY ELBOWS
      GO TO 10
      ENTRY ELBOWD
      GO TO 10
      ENTRY FLMASS
      GO TO 10
      ENTRY FLMASD
      GO TO 10
      ENTRY FLUD2S
      GO TO 10
      ENTRY FLUD2D
      GO TO 10
      ENTRY FLUD3S
      GO TO 10
      ENTRY FLUD3D
      GO TO 10
      ENTRY FLUD4S
      GO TO 10
      ENTRY FLUD4D
C
 10   CALL EMGOLD
      RETURN
      END
