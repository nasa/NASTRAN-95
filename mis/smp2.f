      SUBROUTINE SMP2
C*****
C THIS MODULE, WHICH IS CALLED ONLY FOR DIFFERENTIAL STIFFNESS, PARTI-
C TIONS KDFF AND THEN COMPUTES KDAA AS FOLLOWS ....
C                       I
C               -D      I     D
C               K       I    K
C   D            AA     I     AO
C  K   =     ----------------------
C   FF                  I
C                D  T   I     D
C              (K  )    I    K
C                AO     I     OO
C                       I
C
C   D     -D         D                 D         T             D
C  K   =  K     +   K    X  G    +   (K    X  G )    +   G  X K    X  G
C   AA     AA        AO      O         AO      O          O    OO      O
C
C*****
C
C DMAP CALL ...
C
C     SMP2     USET,GO,KDFF/KDAA/
C
      INTEGER
     1                   USET               ,GO
     2,                  SCR1               ,SCR2
     3,                  UF                 ,UA
     4,                  UO                 ,MCB(7)
C
C
C
      COMMON /BLANK/ ICOM
C
C INPUT FILES
C
      DATA     USET,GO,KDFF /101,102,103/
C
C OUTPUT FILE
C
      DATA     KDAA /201/
C
C SCRATCH FILES
C
      DATA     SCR1,SCR2,KDAAB,KDAO,KDOO /301,302,303,304,305/
C
C USET BIT POSITIONS
C
      DATA     UF,UA,UO/26,25,30/
C
C  TEST FOR PRESENCE OF KDFF
C
      MCB(1)=KDFF
      CALL RDTRL(MCB)
      IF(MCB(1).LT.0) RETURN
C
C PARTITION KFF
C
      CALL UPART (USET,SCR1,UF,UA,UO)
      CALL MPART (KDFF,KDAAB,KDAO,O,KDOO)
C
C COMPUTE KDAA
C
      CALL ELIM(KDAAB,KDAO,KDOO,GO,KDAA,SCR1,SCR2,306)
      RETURN
      END
