      SUBROUTINE SMA2B (KE,J,II,IFILE,DUMDP)
C ******
C SUBROUTINE SMA2B  ADDS A N X N DOUBLE PRECISION MATRIX, KE, TO THE
C SUBMATRIX OF ORDER NROWSC X JMAX, WHICH IS IN CORE.  N IS 1 IF EITHER
C NPVT, THE PIVOT POINT, IS A  SCALAR POINT, OR J, THE SECOND SUBSCRIPT
C OF KE CORRESPONDS TO A SCALAR POINT, OR J .NE. TO ANY ENTRY IN THE
C GPCT.  OTHERWISE N IS 6.
C ******
C
C
C
C
      DOUBLE PRECISION
     1                   DZ(1)              ,KE(36)
     2,                  DUMDP
C
C
C
      INTEGER
     1                   IZ(1)              ,EOR
     2,                  CLSRW              ,CLSNRW
     3,                  FROWIC
     4,                  TNROWS             ,OUTRW
      INTEGER ECPT
C
      COMMON /BLANK/ NOBGG
C
C
C
      COMMON   /SYSTEM/
     1                   ISYS(21), LINKNO
      COMMON   /SEM   /  MASK(3) , LNKNOS(15)
C
C SMA2 I/O PARAMETERS
C
      COMMON   /SMA2IO/
     1                   IFCSTM             ,IFMPT
     2,                  IFDIT              ,IDUM1
     3,                  IFECPT             ,IGECPT
     4,                  IFGPCT             ,IGGPCT
     5,                  IDUM2              ,IDUM3
     6,                  IFMGG              ,IGMGG
     7,                  IFBGG              ,IGBGG
     8,                  IDUM4              ,IDUM5
     9,                  INRW               ,OUTRW
     T,                  CLSNRW             ,CLSRW
     1,                  NEOR               ,EOR
     2,                  MCBMGG(7)          ,MCBBGG(7)
C
C SMA2 VARIABLE CORE
C
      COMMON   /ZZZZZZ /  Z(1)
C
C SMA2 VARIABLE CORE BOOKKEEPING PARAMETERS
C
      COMMON   /SMA2BK/
     1                   ICSTM              ,NCSTM
     2,                  IGPCT              ,NGPCT
     3,                  IPOINT             ,NPOINT
     4,                  I6X6M              ,N6X6M
     5,                  I6X6B              ,N6X6B
C
C SMA2 PROGRAM CONTROL PARAMETERS
C
      COMMON   /SMA2CL/
     1                   IOPTB              ,BGGIND
     2,                  NPVT               ,LEFT
     3,                  FROWIC             ,LROWIC
     4,                  NROWSC             ,TNROWS
     5,                  JMAX               ,NLINKS
     6,                  LINK(10)           ,NOGO
C
C ECPT COMMON BLOCK
C
      COMMON   /SMA2ET/
     1                   ECPT(100)
C
C
C
      EQUIVALENCE
     1                   (Z(1),IZ(1),DZ(1))
C
C
C     CALL EMG1B AND THEN RETURN IF THIS IS LINK 8.
C     PROCEED NORMALLY FOR OTHER LINKS.
C
      IF (LINKNO.NE.LNKNOS(8)) GO TO 100
      CALL EMG1B (KE, J, II, IFILE, DUMDP)
      RETURN
C
C DETERMINE WHICH MATRIX IS BEING COMPUTED.
C
  100 IBASE = I6X6M
      IF (IFILE .EQ. IFMGG) GO TO 5
      IF (IOPTB .LT. 0) RETURN
      IBASE = I6X6B
C
C SEARCH THE GPCT AND FIND AN INDEX M SUCH THAT
C IABS(GPCT(M)) .LE. J .LT. IABS(GPCT(M+1))
C
    5 LOW = IGPCT + 1
      LIM = NGPCT + LOW - 2
      IF (LOW .GT. LIM) GO TO 15
      DO 10 I = LOW,LIM
      ISAVE = I
      IF (J .GE. IABS(IZ(I+1)) ) GO TO 10
      IF (J .GE. IABS(IZ(I)) ) GO TO 20
   10 CONTINUE
      IF ( J .GE. IABS(IZ(ISAVE+1)) )  ISAVE = ISAVE + 1
      GO TO 20
C
C IF II .GT. 0, WE ARE DEALING WITH A SCALAR POINT.
C
   15 ISAVE = LOW
   20 IF (II .GT. 0) GO TO 60
C
C AT THIS POINT IT HAS BEEN DETERMINED THAT J IS A SCALAR INDEX NUMBER
C WHICH CORRESPONDS TO A GRID POINT.  HENCE THE DOUBLE PRECISION 6 X 6
C MATRIX, KE, WILL BE ADDED TO THE MATRIX.
C
      L1 = FROWIC - 1
      JJ = IPOINT + ISAVE - IGPCT
      J2 = IZ(JJ) - 1
      I1 = 0
      LIM = NROWSC - 1
   30 IF (I1 .GT. LIM) RETURN
      K1 = IBASE + I1*JMAX + J2
      J1 = 0
      L = 6*L1
      K = K1
   40 J1 = J1 + 1
      IF (J1 .GT. 6) GO TO 50
      L = L + 1
      K = K + 1
      DZ(K) = DZ(K) + KE(L)
      GO TO 40
   50 I1 = I1 + 1
      L1 = L1 + 1
      GO TO 30
C
C AT THIS POINT WE ARE DEALING WITH A 1 X 1.
C FIRST COMPUTE THE ROW NUMBER, NROW
C
   60 NROW = II - NPVT + 1
C
C THE FOLLOWING 2 FORTRAN STATEMENTS ARE MERELY TO CHECK THE PROGRAM
C LOGIC.  EVENTUALLY THEY CAN BE DELETED.
C
      IF (NROW .GE. 1  .AND.  NROW .LE. TNROWS)  GO TO 70
      CALL MESAGE (-30,22,ECPT(1))
   70 LROWIC = FROWIC + NROWSC - 1
C
C IF NROW, THE ROW INTO WHICH THE NUMBER KE(1) IS TO BE ADDED IS NOT
C IN CORE IT CANNOT BE ADDED AT THIS TIME.
C
      IF (NROW .LT. FROWIC  .OR.  NROW .GT. LROWIC) RETURN
      J2 = ISAVE
      J3 = IPOINT + ISAVE - IGPCT
      INDEX = IBASE + (NROW-1)*JMAX + IZ(J3) + J - IABS(IZ(J2))
      DZ(INDEX) = DZ(INDEX) + KE(1)
      RETURN
C
      END
