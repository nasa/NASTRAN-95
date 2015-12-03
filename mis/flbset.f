      SUBROUTINE FLBSET
C
C     CONSTRUCTS THE HYDROELASTIC USET VECTOR AND WRITES THE CONECT
C     FILE FOR USE IN CORE ALLOCATION DURING MATRIX ASSEMBLY
C
      EXTERNAL      COMPLF   ,RSHIFT   ,ANDF     ,ORF
C
      LOGICAL       ERROR
C
      INTEGER       GEOM2    ,ECT      ,BGPDT    ,SIL      ,MPT
     1             ,GEOM3    ,CSTM     ,USET     ,EQEXIN   ,USETF
     2             ,USETS    ,AF       ,DKGG     ,FBELM    ,FRELM
     3             ,CONECT   ,AFMAT    ,AFDICT   ,KGMAT    ,KGDICT
     4             ,Z        ,GROUP(3) ,TWO      ,UX       ,UY
     5             ,UFR      ,UZ       ,UAB      ,UI       ,UA
     6             ,MCB(7)   ,NAME(2)  ,FILE     ,TOTAL    ,NAM(2)
C
      INTEGER       ANDF     ,ORF      ,COMPLF   ,RSHIFT
C
C     MACHINE AND HALF WORD
C
      COMMON / MACHIN /       MACH     ,IHALF    ,JHALF
C
C     GINO FILES
C
      COMMON / FLBFIL /       GEOM2    ,ECT      ,BGPDT    ,SIL
     1                       ,MPT      ,GEOM3    ,CSTM     ,USET
     2                       ,EQEXIN   ,USETF    ,USETS    ,AF
     3                       ,DKGG     ,FBELM    ,FRELM    ,CONECT
     4                       ,AFMAT    ,AFDICT   ,KGMAT    ,KGDICT
C
C     CORE POINTERS
C
      COMMON / FLBPTR /       ERROR    ,ICORE    ,LCORE    ,IBGPDT
     1                       ,NBGPDT   ,ISIL     ,NSIL     ,IGRAV
     2                       ,NGRAV    ,IGRID    ,NGRID    ,IBUF1
     3                       ,IBUF2    ,IBUF3    ,IBUF4    ,IBUF5
C
C     OPEN CORE
C
      COMMON / ZZZZZZ /       Z(1)
C
C     POWERS OF TWO
C
      COMMON /TWO    /        TWO(32)
C
C     USET PIT POSITIONS
C
      COMMON /BITPOS /        BIT1(6)  ,UA       ,BIT2(16) ,UX
     1                       ,UY       ,UFR      ,UZ       ,UAB
     2                       ,UI
C
      DATA    NAME   /        4HFLBS,4HET   /
      DATA    NAM    /        4HCONE,4HCT   /
      DATA    MCB    /        7*0           /
C
C***********************************************************************
C
C     READ SIL INTO CORE
C
      FILE = SIL
      ISIL = ICORE
      NZ   = IGRID - ISIL - 1
      CALL GOPEN (SIL,Z(IBUF1),0)
      CALL READ (*1002,*10,SIL,Z(ISIL),NZ,0,NSIL)
      GO TO 1008
   10 CALL CLOSE (SIL,1)
C
C     WRITE OUT CONECT FILE
C
C     FILE 1 - FOR USE IN ASSEMBLING AF MATRIX, CONTAINS SILS WHICH
C              CONNECT FLUID POINTS TO STRUCTURE POINTS ALONG THE
C              BOUNDARY AND SILS WHICH CONNECT FLUID POINTS ALONG THE
C              FREE SURFACE
C     FILE 2 - FOR USE IN ASSEMBLING THE DKGG MATRIX, CONTAINS SILS
C              WHICH CONNECT STRUCTURE POINTS ALONG THE BOUNDARY AND
C              SILS WHICH CONNECT FLUID POINTS ALONG THE FREE SURFACE
C
C     EACH FILE IS COMPOSED OF A 3 WORD RECORD FOR EACH SIL
C
C              WORD      DESCRIPTION
C
C               1        SIL NUMBER
C               2        MAXIMUN GRID POINTS CONNECTED
C               3        MAXIMUM SILS CONNECTED
C
      FILE = CONECT
      CALL OPEN (*1001,CONECT,Z(IBUF1),1)
C
C     FILE 1
C
      CALL WRITE (CONECT,NAM,2,1)
      DO 20 I = 1,NGRID
      J = IGRID + I - 1
      IF (Z(J) .LE. 0) GO TO 20
      NFR = Z(J) / 1000000
      NFL = Z(J) - NFR*1000000
      GROUP(1) = Z(ISIL+I-1)
      GROUP(2) = NFR+NFL
      GROUP(3) = NFR + 3*NFL
      CALL WRITE (CONECT,GROUP,3,1)
   20 CONTINUE
      CALL EOF (CONECT)
C
C     FILE 2
C
      CALL WRITE (CONECT,NAM,2,1)
      DO 60 I = 1,NGRID
      J = IGRID + I - 1
      IF (Z(J) .GE. 0 .AND. Z(J) .LT. 1000000) GO TO 60
      IF (Z(J) .GT. 0) GO TO 30
      NGROUP = 3
      NNGRID = IABS(Z(J))
      NNSIL  = NNGRID*3
      GO TO 40
   30 NGROUP = 1
      NNGRID = Z(J) / 1000000
      NNSIL  = NNGRID
   40 JSIL   = Z(ISIL+I-1)
      DO 50 J = 1,NGROUP
      GROUP(1) = JSIL
      GROUP(2) = NNGRID
      GROUP(3) = NNSIL
      CALL WRITE (CONECT,GROUP,3,1)
   50 JSIL = JSIL + 1
   60 CONTINUE
C
      CALL CLOSE (CONECT,1)
      MCB(1) = CONECT
      MCB(2) = NGRID
      CALL WRTTRL (MCB)
C
C     READ USET TABLE INTO CORE
C
      FILE  = USET
      IUSET = ISIL  + NSIL  + 1
      NZ    = IGRID - IUSET - 1
      CALL GOPEN (USET,Z(IBUF1),0)
      CALL READ  (*1002,*70,USET,Z(IUSET),NZ,0,NUSET)
      GO TO 1008
   70 CALL CLOSE (USET,1)
C
C     CONSTRUCT A LIST OF FREE SURFACE GRID POINTS BY PASSING THROUGH
C     THE GRID POINT CONNECTIVITY TABLE.
C
      ICORE = IUSET + NUSET
      IFREE = ICORE
      DO 80 I = 1,NGRID
      IF (Z(IGRID+I-1) .LT. 1000000) GO TO 80
      Z(ICORE) = I
      ICORE = ICORE + 1
      IF (ICORE .GE. IGRID) GO TO 1008
   80 CONTINUE
      NFREE = ICORE - IFREE
C
C     PASS THROUGH SIL AND PROCESS EACH GRID POINT TO SET THE
C     APPROPRIATE BIT POSITIONS IN THE NEW USET
C
C     *** NOTE.
C     THE UW BIT IS NO LONGER USED.  INSTEAD THE UA BIT WILL REFLECT
C     THE SOLUTION SET  (UAB + UFR)
C
      Z(ISIL+NSIL) = NUSET + 1
      NSTR  = 0
      TOTAL = 0
      MASKA = COMPLF(TWO(UA))
      JUSET = IUSET
      DO 110 IGRD = 1,NSIL
      K = IBGPDT + 4*(IGRD-1)
      ICSTM = Z(K)
      IF (ICSTM .EQ. -1) GO TO 82
      IF (Z(ISIL+IGRD) .EQ. Z(ISIL+IGRD-1)+1) GO TO 100
C
C     STURCTURE POINT - SET UX AND UZ. ALSO SET UAB IF UA IS SET
C
      NNSIL = 6
      GO TO 84
   82 NNSIL = 1
   84 NSTR  = NSTR + NNSIL
      DO 90 J = 1,NNSIL
      Z(JUSET) = ORF(Z(JUSET),TWO(UX))
      Z(JUSET) = ORF(Z(JUSET),TWO(UZ))
      IF (ANDF(Z(JUSET),TWO(UA)) .EQ. 0) GO TO 85
      Z(JUSET) = ORF(Z(JUSET),TWO(UAB))
   85 CONTINUE
      TOTAL = ORF(TOTAL,Z(JUSET))
   90 JUSET = JUSET + 1
      GO TO 110
C
C     FLUID POINT - SET Y BIT.
C
  100 Z(JUSET) = ORF(Z(JUSET),TWO(UY))
      CALL BISLOC (*102,IGRD,Z(IFREE),1,NFREE,JLOC)
C
C     FREE SURFACE FLUID POINT - SET UFR, UA AND UZ BITS
C
      Z(JUSET) = ORF(Z(JUSET),TWO(UFR))
      Z(JUSET) = ORF(Z(JUSET),TWO(UA))
      Z(JUSET) = ORF(Z(JUSET),TWO(UZ))
      GO TO 106
C
C     INTERIOR FLUID POINT - SET UI BIT AND TURN OF UA BIT
C
  102 Z(JUSET) = ORF(Z(JUSET),TWO(UI))
      Z(JUSET) = ANDF(Z(JUSET),MASKA)
C
  106 CONTINUE
      TOTAL = ORF(TOTAL,Z(JUSET))
      JUSET = JUSET + 1
  110 CONTINUE
C
C     WRITE OUT NEW USETF VECTOR
C
      CALL GOPEN (USETF,Z(IBUF1),1)
      CALL WRITE (USETF,Z(IUSET),NUSET,1)
      CALL CLOSE (USETF,1)
      MCB(1) = USETF
      MCB(2) = 0
      MCB(3) = NUSET
      MCB(4) = RSHIFT(TOTAL,IHALF)
      MCB(5) = ANDF(TOTAL,JHALF)
      CALL WRTTRL (MCB)
C
C     WRITE OUT NEW USETS VECTOR
C
      CALL GOPEN (USETS,Z(IBUF1),1)
      LUSET = IUSET + NUSET - 1
      DO 120 I = IUSET,LUSET
      IF (ANDF(Z(I),TWO(UX)) .EQ. 0) GO TO 120
      CALL WRITE (USETS,Z(I),1,0)
  120 CONTINUE
      CALL CLOSE (USETS,1)
      MASK   = COMPLF(ORF(TWO(UY),TWO(UFR)))
      TOTAL  = ANDF(TOTAL,MASK)
      MCB(1) = USETS
      MCB(3) = NSTR
      MCB(4) = RSHIFT(TOTAL,IHALF)
      MCB(5) = ANDF(TOTAL,JHALF)
      CALL WRTTRL (MCB)
C
C     PRINT NEW USET VECTOR IF USER REQUESTS
C
      ICORE = IUSET + NUSET
      CALL FLBPRT (IUSET,ICORE,IBUF1)
C
C     USET PROCESSING COMPLETED
C
      ICORE = IUSET
      RETURN
C
C     ERROR CONDITIONS
C
 1001 N = -1
      GO TO 1100
 1002 N = -2
      GO TO 1100
 1008 N = -8
 1100 CALL MESAGE (N,FILE,NAME)
      RETURN
      END
