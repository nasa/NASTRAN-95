      SUBROUTINE OUTPT3
C
C     PUNCH UP TO 5 MATRIX DATA BLOCK ONTO DMI CARDS
C
C     CALL TO THIS MODULE IS
C
C     OUTPUT3   M1,M2,M3,M4,M5//C,N,PO/C,Y,N1=AB/C,Y,N2=CD/C,Y,N3=EF/
C                                      C,Y,N4=GH/C,Y,N5=IJ   $
C
C               PO = FORTRAN OUTPUT FILE UNIT NO. (DEFAULT = 0)
C                    .GE.0 MEANS NO LISTING OF  CARD IMAGES WILL BE MADE
C                    .LT.0 MEANS LISTING OF DMI CARD IMAGES WILL BE MADE
C                          ON FORTRAN UNIT = IABS(PO).
C
C
C
      LOGICAL         FIRST
      INTEGER         IN(5),SUBNAM(2),NAME(2),TRL(7),ERNO,PARAM,
     1                TRL1,TRL2,TRL3,TRL4,TRL5,TRL6,TRL7,EOL,EOR
      CHARACTER       UFM*23,UWM*25,UIM*29
      COMMON /XMSSG / UFM,UWM,UIM
      COMMON /BLANK / JO,PARAM(2,5)
     1       /SYSTEM/ NB,NO,JUNK(6),NLPP
     2       /ZZZZZZ/ X(1)
     3       /ZNTPKX/ Z(4),IZ,EOL,EOR
     4       /PHDMIX/ NAMEX(2),NAM,IFO,ITIN,ITOUT,IR,IC,NOUTPT,KPP,NLP,
     5                ERNO,ICOL,IRO,XX,ICARD1
      EQUIVALENCE     (TRL(1),TRL1), (TRL(2),TRL2), (TRL(3),TRL3),
     1                (TRL(4),TRL4), (TRL(5),TRL5), (TRL(6),TRL6),
     2                (TRL(7),TRL7)
      DATA    SUBNAM/ 4HOUTP,4HUT3 /,  IN/ 101,102,103,104,105 /
      DATA    ITYP  / 1 /
C
C
      LCOR = KORSZ(X) - NB
      IF (LCOR .LE. 0) CALL MESAGE (-8,LCOR,SUBNAM)
      IBUF = LCOR+1
      JONO = 0
      IF (JO .LT. 0) JONO = IABS(JO)
      NOUTPT = JONO
      ITIN = 1
      KPP  = 2
      NLP  = NLPP
C
      DO 1000 II = 1,5
      TRL1 = IN(II)
      CALL RDTRL (TRL)
      IF (TRL1 .LE. 0) GO TO 1000
      CALL FNAME (IN(II),NAME)
      CALL GOPEN (IN(II),X(IBUF),0)
      NAMEX(1) = NAME(1)
      NAMEX(2) = NAME(2)
      NAM  = PARAM(1,II)
      IFO  = TRL4
      ITOUT= 0
      IR   = TRL3
      IC   = TRL2
      CALL PHDMIA
      IF (ERNO .NE. 0) GO TO 9400
C
      DO 900 J = 1,TRL2
      CALL INTPK (*900,IN(II),0,ITYP,0)
      FIRST = .FALSE.
      ICOL  = J
C
      DO 800 I = 1,TRL3
      IF (EOL .NE. 0) GO TO 850
      CALL ZNTPKI
      IRO = IZ
      XX  = Z(1)
C
C     VAX MAY HAVE A FEW IMBEDED ZEROS
C
      IF (XX .EQ. 0.0) GO TO 800
      IF (FIRST) GO TO 100
      FIRST = .TRUE.
      CALL PHDMIB
      IF (ERNO) 9400,200,9400
  100 CALL PHDMIC
      IF (ERNO) 9400,200,9400
  200 CONTINUE
  800 CONTINUE
C
  850 CALL PHDMID
      IF (ERNO .NE. 0) GO TO 9400
  900 CONTINUE
C
      NCARDS = ICARD1 + 1
      CALL PAGE2 (-2)
      WRITE  (NO,1) UIM,NAME,NCARDS
    1 FORMAT (A29,' 4103, OUTPUT3 HAS PUNCHED MATRIX DATA BLOCK ',2A4,
     1       ' ONTO ',I5,' DMI CARDS.')
      CALL CLOSE (IN(II),1)
 1000 CONTINUE
      RETURN
C
C     ERROR MESSAGE
C
 9400 CALL PAGE2 (-2)
      WRITE  (NO,9450) UFM
 9450 FORMAT (A23,' 4104, ATTEMPT TO PUNCH MORE THAN 99999 DMI CARDS ',
     1       'FOR A SINGLE MATRIX.')
      CALL MESAGE (-61,0,0)
      RETURN
C
      END
