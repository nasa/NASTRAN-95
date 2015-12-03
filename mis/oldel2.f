      SUBROUTINE OLDEL2
C
C     ANY ELEMENT (NEW OR OLD) WHICH HAS NOT BEEN CONVERTED TO USE
C     EMGPRO SHOULD HAVE AN ENTRY POINT IN OLDEL1, OLDEL2, OR OLDEL3
C     ***************************************************************
C
      ENTRY HEXA1S
      GO TO 10
      ENTRY HEXA1D
      GO TO 10
      ENTRY HEXA2S
      GO TO 10
      ENTRY HEXA2D
      GO TO 10
      ENTRY PLOTLS
      GO TO 10
      ENTRY PLOTLD
      GO TO 10
      ENTRY QDMEMS
      GO TO 10
      ENTRY QDMEMD
      GO TO 10
      ENTRY QDPLTS
      GO TO 10
      ENTRY QDPLTD
      GO TO 10
      ENTRY QUAD1S
      GO TO 10
      ENTRY QUAD1D
      GO TO 10
      ENTRY QUAD2S
      GO TO 10
      ENTRY QUAD2D
      GO TO 10
      ENTRY SLOT3S
      GO TO 10
      ENTRY SLOT3D
      GO TO 10
      ENTRY SLOT4S
      GO TO 10
      ENTRY SLOT4D
C
 10   CALL EMGOLD
      RETURN
      END
