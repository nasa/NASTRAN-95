      SUBROUTINE OLDEL3
C
C     ANY ELEMENT (NEW OR OLD) WHICH HAS NOT BEEN CONVERTED TO USE
C     EMGPRO SHOULD HAVE AN ENTRY POINT IN OLDEL1, OLDEL2, OR OLDEL3
C     ***************************************************************
C
      ENTRY TETRAS
      GO TO 10
      ENTRY TETRAD
      GO TO 10
      ENTRY TRAPRS
      GO TO 10
      ENTRY TRAPRD
      GO TO 10
      ENTRY TRIARS
      GO TO 10
      ENTRY TRIARD
      GO TO 10
      ENTRY TRIA1S
      GO TO 10
      ENTRY TRIA1D
      GO TO 10
      ENTRY TRIA2S
      GO TO 10
      ENTRY TRIA2D
      GO TO 10
      ENTRY TRPLTS
      GO TO 10
      ENTRY TRPLTD
      GO TO 10
      ENTRY WEDGES
      GO TO 10
      ENTRY WEDGED
C
 10   CALL EMGOLD
      RETURN
      END
