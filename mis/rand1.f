      SUBROUTINE RAND1(FILE,MID,TYPE,ID,COMP,Q)
C
C     PUTS ID RECORD ON RANDOM OUTPUT FILES
C
      INTEGER FILE,TYPE,COMP,IDR(50)
      INTEGER Q(2)
      INTEGER MID1(2,7)
      COMMON /OUTPUT/ HEAD(1)
      DATA MID1/2001,4HDISP,
     1          2010,4HVELO,
     2          2011,4HACCE,
     3          2002,4HLOAD,
     4          2003,4HSPCF,
     5          2004,4HELFO,
     6          2005,4HSTRE/
      DATA IDR /50*0/
      IDR(1)= 50
      IDR(3) = MID
      DO 10 I = 1,7
      IF(TYPE .EQ. MID1(2,I)) GO TO 20
   10 CONTINUE
   20 ITYPE = MID1(1,I)
      IDR(2) = ITYPE
      IDR(5) = ID*10
      IDR(6) = COMP
      IDR(8) = Q(1)
      IDR(9) = Q(2)
      IDR(10) = 2
      CALL WRITE(FILE,IDR(1),50,0)
      CALL WRITE(FILE,HEAD(1),96,1)
      RETURN
      END
