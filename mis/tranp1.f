      SUBROUTINE TRANP1 (IN,IOUT,NSCRTH,IS1,IS2,IS3,IS4,IS5,IS6,IS7,IS8)
C
C     DRIVER OF THE OUT-OF-CORE MATRIX TRANSPOSE ROUTINE TRNSP
C     (DTRANP IS THE TRNSP MODULE DRIVER)
C
C
      INTEGER         SCR,NAM(2)
      COMMON /ZZZZZZ/ CORE(1)
      COMMON /TRNSPX/ IA(7),IAT(7),LCORE,NSCRH,SCR(8)
      DATA    NAM   / 4HTRNS,4HP1  /
C
      IF (NSCRTH .GT. 8) CALL MESAGE (-37,0,NAM)
      IA(1)  = IN
      CALL RDTRL (IA)
      IAT(1) = IOUT
      IAT(2) = IA(3)
      IAT(3) = IA(2)
      IAT(5) = IA(5)
      IAT(4) = IA(4)
C
C     REVERSE THE FORM OF THE LOWER AND UPPER TRIANGULAR MATRIX
C
      IF (IA(4) .EQ. 4) IAT(4) = 5
      IF (IA(4) .EQ. 5) IAT(4) = 4
      LCORE  = KORSZ(CORE)
      NSCRH  = NSCRTH
      SCR(1) = IS1
      SCR(2) = IS2
      SCR(3) = IS3
      SCR(4) = IS4
      SCR(5) = IS5
      SCR(6) = IS6
      SCR(7) = IS7
      SCR(8) = IS8
      CALL TRNSP  (CORE)
      CALL WRTTRL (IAT)
      RETURN
      END
