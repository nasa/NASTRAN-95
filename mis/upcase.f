      SUBROUTINE UPCASE (BYTE,N)
C
C     THIS ROUTINE CHANGES ALL LOWER CASE CHARACTERS INTO UPPER CASE.
C     IT ALSO CONVERTS BCD INPUT CODE TO EBCDIC FOR IBM MACHINE
C
      LOGICAL         FLAG
      INTEGER         TAB(20),   FFFLAG
      CHARACTER*1     BYTE(1),  BK1,      LA,       LZ,       IL,
     1                IC,       IP,       LC(256)
      CHARACTER*56    KC(5)
      COMMON /MACHIN/ MACHX
      COMMON /UPCASX/ FLAG,     ID,       IA,       IZ
      COMMON /XECHOX/ FFFLAG
      EQUIVALENCE     (KC(1),LC(1))
C
C                     TAB = UPPER CASE 'A' TO LOWER CASE 'a' SPAN
C
      DATA            TAB / +32, -64, +32, +3968, +32, +32, +32, +32 ,
     1                      +32, +32, +32, +32,   +32, +32, +32, +32 ,
     2                      +32, +32, +32, +32     /
      DATA            BK1,      LA,       LZ,       IL,       IC     /
     1                ' ',      'A',      'Z',      '(',      ','    /
      DATA            IP /      '%'       /
C
C     TAB IS DECIMAL VALUE BETWEEN UPPER CASE 'A' AND LOWER CASE 'a'
C     TAB IS POSITIVE IF LOWER CASE 'a' COMES AFTER UPPER CASE 'A' IN
C     MACHINE ASCII CHARACTER SET; OTHERWISE TAB IS NEGATIVE.
C
C     THE FOLLOWING KC TABLE MUST BE PUNCHED IN EBCDIC CODE (FOR IBM
C     ONLY)                          =======    ===========
C
      DATA            KC /
     1   '                                                        ',
     2   '                   .)(+ +          $*)  -/         ,(%  ',
     3 '           =''''=  ABCDEFGHI       JKLMNOPQR        STUVWX',
     4   'YZ                       ABCDEFGHI       JKLMNOPQR      ',
     5   '  STUVWXYZ      0123456789      WRITTEN BY G.CHAN/UNISYS'/
C
      IF (MACHX .EQ. 2) GO TO 30
      IF (FLAG) GO TO 10
      FLAG =.TRUE.
      ID = TAB(MACHX)
      IA = ICHAR(LA) + ID
      IZ = ICHAR(LZ) + ID
C
 10   DO 20 I = 1,N
      IF (BYTE(I) .EQ. BK1) GO TO 20
      J = ICHAR(BYTE(I))
      IF (J.LT.IA .OR. J.GT.IZ) GO TO 20
      BYTE(I) = CHAR(J-ID)
 20   CONTINUE
      RETURN
C
C     IBM MACHINE ONLY, WHICH USES EBCDIC CODE
C
 30   DO 40 I = 1,N
      J = ICHAR(BYTE(I))
 40   BYTE(I) = LC(J+1)
C
C     THE % SIGN MAY BE CHANGED TO ( IN BCD-EBCDIC CONVERSION,
C     CHANGE IT BACK TO %
C
      IF (FFFLAG.NE.1234 .OR. N.LT.5) RETURN
      DO 50 I = 5,N
      IF (BYTE(I).EQ.IL .AND. BYTE(I+1).EQ.IL .AND. (BYTE(I-1).EQ.IC
     1   .OR. BYTE(I-1).EQ.BK1)) BYTE(I) = IP
 50   CONTINUE
      RETURN
      END
