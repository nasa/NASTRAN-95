      SUBROUTINE ONLINS (*,LX)
C
C     ON-LINE SCAN ROUTINE, CALLED ONLY BY SCAN
C
C     WRITTEN FEBY G.CHAN/SPERRY,  FEB. 1986
C
      IMPLICIT INTEGER          (A-Z)
      EXTERNAL        LSHIFT,   RSHIFT,   ANDF,     ORF,      COMPLF
      LOGICAL         DEBUG
      INTEGER         NAME(2),  CARD(20), IZ(1)
      REAL            R(2),     Z(30)
      COMMON /MACHIN/ MACH
      COMMON /BLANK / IELT(2),  ICOMP,    NTOP,     AMAX,     AMIN,
     1                IBEG,     IEND,     ICOMPX
      COMMON /SYSTEM/ IBUF,     OUTTAP,   NOGO,     IN,       DUM(74),
     1                SWTCH1,   JDUM(6),  INTRA
      COMMON /XSCANX/ SKIP(2),  LCORE,    LBEG,     LEND,     DUMM(2),
     1                IEL,      IOPT,     ISET,     ISORT,    IDUM(4),
     2                DEBUG
      COMMON /IFP1A / SCR1,     CASECC,   IS,       NWPC,     NCPW,
     1                NMODES,   ICC,      NSET,     DUMMY(3), ISUB,
     2                LENCC,    IBLNK,    IEQUAL,   IEOR
C
C            /ZZIFP1/ IS THE OPEN CORE FOR SCAN
      COMMON /ZZZZZZ/ LCSE(400),CORE(1)
      EQUIVALENCE     (IZ(1),LCSE(1))
      EQUIVALENCE     (IMAX,AMAX),        (IMIN,AMIN),
     1                (IDUPL,IBEG),       (INC,IEND),
     2                (CARD(1),CORE(1)),  (IZ(1),Z(1))
      DATA    BLANK , EQUAL ,   STOP  ,   ALL   ,   NAME             /
     1        4H    , 4H=   ,   4HSTOP,   4HALL ,   4HONLI,   4HNS   /
      DATA    LU    , DEBUG1,   DEBUG2,   DEBUG3,   I0               /
     1        1     , 4HDEBU,   4HG ON,   4HG OF,   0                /
C
C     INITIALIZE /IFP1A/
C
      SCR1   = 301
      CASECC = 101
      IS     = 0
      NWPC   = 20
      NCPW   = 4
      NMODES = 0
      ICC    = 0
      ISUB   = 1
      IBLNK  = BLANK
      IEQUAL = EQUAL
      IEOR   = COMPLF(0)
      IEOR   = RSHIFT(IEOR,1)
C
C     SET INTERACTIVE FLAG TO POSITIVE, A SIGNAL TO SCAN, TOTAPE, IFP1C
C
      INTRA  = IABS(INTRA)
      IF (INTRA .EQ. 0) INTRA = 1
C
      ICOMP = LX
      NWPC1 = NWPC + 1
      NOUT  = OUTTAP
      WRITE  (NOUT,10)
 10   FORMAT (///1X,'*** SCAN INTERACTIVE INPUT ***')
C
C     READ CASECC FILE AND SAVE DATA IN LCSE, ONE SUBCASE AT A TIME
C     SAVE SET DATA IN CORE BEGIN AT CORE(BGN)
C
 15   LCSE(166) = 200
      LCSE(199) = 0
      LCSE(200) = 0
      NZ    = KORSZ(CORE(1)) - 3*IBUF - 1
      NZ    = MIN0(NZ,LCORE)
      ISCAN = 0
      NSET  = 0
      I81   = NWPC1
      SUBID =-1
      LX    = 0
      IF (ICOMP .EQ. -2) GO TO 30
C
C     NO QUESTION ASKED IF SORT2 DATA TYPE IS USED.
C
      LX = 1
 20   WRITE  (NOUT,25)
 25   FORMAT (//,' ENTER SUBCASE ID (DEFAULT=FIRST SUBCASE)')
      READ   (IN,26) R
 26   FORMAT (2A4)
      CALL A82INT (*20,R,8,SUBID,I)
      IF (SUBID .EQ.  0) SUBID = -1
      IF (INTRA .GT. 10) WRITE (LU,27) SUBID
 27   FORMAT (///3X,'SUBCASE ID',I8)
 30   JJ = 1
      CALL REWIND (CASECC)
      CALL FWDREC (*110,CASECC)
 32   JJ = JJ + 1
      CALL READ (*110,*110,CASECC,LCSE(JJ),1,0,I)
      IF (SUBID .EQ. -1) SUBID = LCSE(JJ)
      IF (LCSE(JJ) .EQ. SUBID) GO TO 35
      CALL FWDREC (*110,CASECC)
      GO TO 32
 35   LCSE(1) = LCSE(JJ)
      CALL READ (*110,*125,CASECC,LCSE(2),199,0,I)
      LENCC = LCSE(166)
      LSEM  = LCSE(LENCC)
      NSET  = LCSE(LENCC-1)
      IF (LSEM .GT. 0) CALL READ (*110,*125,CASECC,CORE(I81),LSEM,0,I)
      I81   = I81 + LSEM
      BGN   = I81
      END   = I81
 37   CALL READ (*40,*40,CASECC,CORE(I81),2,0,I)
      JMP   = CORE(I81+1)
      CORE(I81+2) = JJ
      I81  = I81 + 3
      CALL READ (*110,*125,CASECC,CORE(I81),JMP,0,I)
      NSET = NSET + 1
      I81  = I81 + JMP
      GO TO 37
C
C     SET CARD
C
 40   WRITE  (NOUT,43)
 43   FORMAT (//,' ENTER A BLANK, OR A SET CARD (SEE USER MANUAL P. ',
     1       '2.3-44)', /,' E.G.  SET 101 = 1, 5 THRU 20')
 45   CORE(I81) = IEOR
      NOGO  = 0
      CALL XREAD (*40,CARD)
      IF (CARD(1).EQ.BLANK .AND. CARD(2).EQ.BLANK) GO TO 60
      WRITE (LU,77) CARD
      IF (CARD(1) .NE. DEBUG1) GO TO 46
      J  = LSHIFT(1,20)
      IF (CARD(2) .EQ. DEBUG2) SWTCH1 = ORF(J,SWTCH1)
      J  = COMPLF(J)
      IF (CARD(2) .EQ. DEBUG3) SWTCH1 = ANDF(J,SWTCH1)
      DEBUG = .FALSE.
      IF (CARD(2) .EQ. DEBUG2) DEBUG = .TRUE.
      GO TO 40
 46   IB  = I81
      NZZ = NZ - I81
      CALL XRCARD (CORE(I81),NZZ,CARD(1))
      IF (CORE(I81+8) .NE. ALL) GO TO 47
      CORE(I81  ) = CORE(I81+4)
      CORE(I81+1) = 1
      CORE(I81+2) = JJ
      CORE(I81+3) =-1
      I81 = I81 + 4
      GO TO 50
 47   ICC = 1
      CALL IFP1C (I81,NZZ)
C
C     CONTINUATION CARDS FOR SET ARE READ IN BY IFP1C
C
      IF (NOGO .EQ. 0) GO TO 50
      I81  = IB
      GO TO 40
 50   NSET = NSET + 1
      WRITE  (NOUT,52) CORE(IB)
 52   FORMAT (/,' THIS NEW SET',I6,' IS DEFINED FOR LOCAL USE ONLY',
     1       //,' ENTER A BLANK, OR ANOTHER SET CARD')
      KK = 55
      IF (DEBUG) WRITE (6,55) KK,I81
 55   FORMAT ('   ONLINS/',I2,4X,'I81 =',I7)
      GO TO 45
C
C     SET DATA - FROM CORE(BGN) THRU CORE(END)
C
 60   END = I81 - 1
      NZZ = NZ - I81
C
C     SCAN CARD
C
 70   WRITE  (NOUT,72)
 72   FORMAT (//,' ENTER A BLANK, OR A SCAN CARD (SEE USER MANUAL P.2.3-
     141A',   /,'  E.G. SCAN (STRESS,CBAR,AXIAL,SA/MAX) = 15, SET 102',
     2        /,'       SCAN (FORCE,3,ROD,2) = +2000.,-1500.',
     3        /,'       SCAN (HELP)' )
C
 75   JUMPH = 0
      CALL XREAD (*70,CARD)
      IF (CARD(1).EQ.STOP  .AND. CARD(2).EQ.BLANK) GO TO 135
      IF (CARD(1).EQ.BLANK .AND. CARD(2).EQ.BLANK) GO TO 90
      WRITE  (LU,77) CARD
 77   FORMAT (20A4)
      IB = I81
      CALL XRCARD (CORE(I81),NZZ,CARD(1))
      CALL IFP1H (I81,NZZ,JUMPH)
      IF (NOGO  .NE. 0) GO TO 80
      IF (JUMPH .EQ. 0) GO TO 82
      CALL IFP1H (0,0,2)
 80   I81 = IB
      IF (NOGO) 70,75,70
C
 82   J = CORE(IB)
      IF (ISCAN .EQ. 0) ISCAN = J
      IF (ISCAN .EQ. J) ISCAN = 30000000
      WRITE  (NOUT,85)
 85   FORMAT (/,' ENTER A BLANK, OR ANOTHER SCAN CARD')
      KK = 87
      IF (DEBUG) WRITE (6,55) KK,I81
      GO TO 75
C
C     MOVE SET AND SCAN DATA TO THE END OF CASECC ARRAY IN /ZZIFP1/
C     THEN, MOVE THE ENTIRE CASECC DATA (SET AND SCAN INCLUDED) TO
C     THE END OF THE OPEN CORE. FINALLY, MOVE THE SAME DATA BLOCK
C     TO THE BEGINNING OF THE OPEN CORE SPACE IN /ZZSCAN/ FOR SCAN
C     OPERATION
C
 90   L   = LENCC
      IF (I81 .LE. NWPC1) GO TO 100
      J   = BGN + 2
      I81 = I81 - 1
      DO 95 I = NWPC1,I81
      IF (I .NE. J) GO TO 92
      J   = J + CORE(J-1) + 3
      GO TO 95
 92   L   = L + 1
      LCSE(L) = CORE(I)
 95   CONTINUE
      J   = LCORE
      DO 96 I = 1,L
      LCSE(J) = LCSE(I)
 96   J   = J - 1
      IF (I .GT. J) CALL MESAGE (+8,0,NAME)
      J   = LCORE
      DO 97 I = 1,L
      Z(I) = LCSE(J)
 97   J   = J - 1
      IF (DEBUG) WRITE (6,99) (Z(I),I=1,L)
 99   FORMAT (//,' Z(1...200+) =', (/4X,10I7))
 100  IF (LX .GT. 0) LX = L
C
      IF (ISCAN .EQ. 20000000) GO TO 103
      IF (Z(25) .EQ. 0) GO TO 140
C
C     STRESS SCAN
C
      Z(24) =-1
      Z(25) = 1
      Z(26) = 1
 103  IF (ISCAN .NE. 20000000) GO TO 105
      IF (Z(28) .EQ. 0) GO TO 150
C
C     FORCE SCAN
C
      Z(27) =-1
      Z(28) = 1
      Z(29) = 1
 105  IF (INTRA .GT. 10) OUTTAP = LU
      RETURN
C
 110  JJ = JJ - 1
      WRITE  (NOUT,115) SUBID,(Z(I),I=1,JJ)
 115  FORMAT (//,' SUBCASE',I5,' NOT FOUND',
     1        //,' EXISTING SUBCASES ARE -', (/5X,10I7))
      GO TO 15
C
 125  CALL MESAGE (+2,CASECC,NAME)
      GO TO 105
 135  RETURN 1
C
 140  WRITE  (NOUT,145)
 145  FORMAT (//,' STRESS OUTPUT FILE NOT AVAILABLE FOR SCAN',//)
      GO TO 75
 150  WRITE  (NOUT,155)
 155  FORMAT (//,' FORCE  OUTPUT FILE NOT AVAILABLE FOR SCAN',//)
      GO TO 75
      END
