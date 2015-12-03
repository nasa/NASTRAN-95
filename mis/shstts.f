      SUBROUTINE SHSTTS (TAB,UAB,VAB)
C
C     TO CREATE STRESS TENSOR TRANSFORMATION MATRICES FROM AN ORTHOGONAL
C     TRANSFORMATION FOR SHELL ELEMENTS.
C
C     INPUT :
C           TAB    - ORTHOGONAL INPLANE ROTATION TRANSFORMATION
C     OUTPUT:
C           UAB    - TENSOR TRANSFORMATION FOR NORMAL AND INPLANE SHEAR
C                    COMPONENTS
C           VAB    - TENSOR TRANSFORMATION FOR OUT-OF-PLANE SHEAR
C
C     USAGE:
C           THE INPUT IS ASSUMED TO BE ROW-LOADED.
C           OUTPUTS ARE CREATED ROW-LOADED.
C           DEFINING:
C           [S]      AS A 2-D STRESS VECTOR;
C           [E]      AS A 2-D STRAIN VECTOR;
C           [Q]      AS A 2-D SHEAR FORCE VECTOR;
C           [G]      AS A 2-D STRESS/FORCE-STRAIN RELATION; AND
C           [ALPHA]  AS A VECTOR OF THERMAL EXPANSION COEFFICIENTS,
C
C           THEN THE FOLLOWING RELATIONSHIPS ARE TRUE:
C
C                       T                        T
C           [S]  = [UAB] [S]         [G]  = [UAB] [G] [UAB]
C              A            B           A            B
C
C                       T
C           [Q]  = [VAB] [Q]
C              A            B
C
C           IF [TBA] IS INPUT, THE OUTPUT WILL BE:
C
C                -1                      -1       T
C           [UAB]  = [UBA],   AND   [VAB]  = [VAB] = [VBA]
C
C           WHICH MAY BE USED IN THE FOLLOWING:
C
C           [E]  = [UBA] [E]         [ALPHA]  = [UBA] [ALPHA]
C              A            B               A                B
C
C           [Q]  = [VBA][Q]
C              A           B
C
C
      REAL     TAB(9),UAB(9),VAB(4)
C
      UAB(1) = TAB(1)*TAB(1)
      UAB(2) = TAB(4)*TAB(4)
      UAB(3) = TAB(1)*TAB(4)
      UAB(4) = TAB(2)*TAB(2)
      UAB(5) = TAB(5)*TAB(5)
      UAB(6) = TAB(2)*TAB(5)
      UAB(7) = TAB(1)*TAB(2)*2.0
      UAB(8) = TAB(4)*TAB(5)*2.0
      UAB(9) = TAB(1)*TAB(5) + TAB(2)*TAB(4)
C
      VAB(1) = TAB(5)*TAB(9)
      VAB(2) = TAB(2)*TAB(9)
      VAB(3) = TAB(4)*TAB(9)
      VAB(4) = TAB(1)*TAB(9)
C
      RETURN
      END
