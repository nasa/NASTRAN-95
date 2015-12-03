      SUBROUTINE VALVEC
C
C     LARGE ORDER REAL SYMMETRIC EIGENVALUE-EIGENVECTOR PROBLEM
C
      INTEGER         QR,MCB(8),TRI(3),QRX(3),WIL(3),VAL(3)
      DIMENSION       VCOM(30)
      COMMON /ZZZZZZ/ A(1)
      COMMON /GIVN  / TITLE(150)
CWKBR 2/94 SPR93027 COMMON /SYSTEM/ ISYS
      COMMON /SYSTEM/ ISYS, IDUMM(53), IPREC
      EQUIVALENCE     (MD   ,TITLE(3)), (VCOM(1),TITLE(101)),
     1                (N    ,VCOM( 1)), (NV     ,VCOM(   7)),
     2                (OEIGS,VCOM(11)), (NVER   ,VCOM(  13)),
     3                (NEVER,VCOM(14)), (ITERM  ,VCOM(  16))
      DATA    TRI   / 4HTRID, 4HI   , 4H    /
      DATA    QRX   / 4HQRIT, 4HER  , 4H    /
      DATA    WIL   / 4HWILV, 4HEC  , 4H    /
      DATA    VAL   / 4HVALV, 4HEC  , 4H    /
      DATA    IBEGN , IEND  / 4HBEGN, 4HEND /
C
C     DEFINITION OF VARIABLES AND DATA FORMATS
C
C     MD       INPUT MATRIX
C     N        SIZE OF MATRIX
C     NV       NUMBER OF EIGENVECTORS DESIRED
C     OEIGS    EIGENVALUE SUMMARY FILE
C     A        OPEN CORE
C     ID       POINTER TO DIAGONALS      -- N OF THEM (D.P.)
C     IO       POINTER TO OFF-DIAGONALS  -- N OF THEM (D.P.)
C     IV       POINTER TO EIGENVALUES    -- N OF THEM (D.P.)
C     IL       POINTER TO ORDER FOUND ARRAY N OF THEM (S.P.)
C     I1 - I6  POINTS TO SCRATCH ARRAYS  -- 2XN LONG
C     NVER     NUMBER OF VECTORS    ERRORS
C     NEVER    NUMBER OF EIGENVALUE ERRORS
C     ITERM    REASON FOR TERMINATION
C
C     INITIALIZATION FOR VALVEC IN BLOCKDATA ROUTINE READBD
C
C     DATA
C    1 MO, MD,MR1, M1, M2, M3, M4,LGAMA,OEIGS,PHIA,ORDER,RSTRT,NCOL,MAX/
C    *301,304,202,303,307,308,309,  201,  204, 305,   -2,   0 ,   0,253/
C
C
      VAL(3) = IBEGN
      CALL CONMSG (VAL,3,0)
      ITERM  = 1
      MCB(1) = MD
      CALL RDTRL (MCB(1))
      N  = MCB(2)
      N2 = N*IPREC
      ID = 1
      IO = ID + N2
      IV = IO + N2
      IL = IV + N2
      I1 = IL + N
      IF ((I1 + 1)/2  .EQ. I1/2) I1 = I1 + 1
      I2 = I1 + N2
      I3 = I2 + N2
      I4 = I3 + N2
      I5 = I4 + N2
      I6 = I5 + N2
C
C     TRIDIAGONALIZATION.
C
      IF (N .GT. 2) GO TO 101
CWKBD 2/94 SPR93027 CALL SMLEIG (A(ID),A(IO),A(IV)) 
CWKBNB 2/94 SPR93027
      IF ( IPREC .EQ. 2 ) CALL SMLEIG (A(ID),A(IO),A(IV)) 
      IF ( IPREC .EQ. 1 ) CALL SMLEIG1(A(ID),A(IO),A(IV)) 
CWKBNE 2/94 SPR93027
      
      IF (N-2) 300,200,300
  101 TRI(3) = IBEGN
      CALL CONMSG (TRI,3,0)
CWKBD 2/94 SPR93027 CALL TRIDI (A(ID),A(IO),A(IV),A(IL),A(I1),A(IL))
CWKBNB 2/94 SPR93027
      IF ( IPREC .EQ. 2 )
     &CALL TRIDI (A(ID),A(IO),A(IV),A(IL),A(I1),A(IL))
C                   D      O    V     A     B    
      IF ( IPREC .EQ. 1 )
     &CALL TRIDI1(A(ID),A(IO),A(IV),A(IL),A(I1),A(IL))
C                   D      O    V     A     B   
CWKBNE 2/94 SPR93027
      TRI(3) = IEND
      CALL CONMSG (TRI,3,0)
C
C     EIGENVALUES
C
  200 QR =  0
      IF (N .LE. 2) QR = 1
      QRX(3) = IBEGN
      CALL CONMSG (QRX,3,0)
CWKBD 2/94 SPR93027 CALL QRITER (A(IV),A(I1),A(IL),QR) 
CWKBNB 2/94 SPR93027
      IF ( IPREC .EQ. 2 ) CALL QRITER (A(IV),A(I1),A(IL),QR) 
      IF ( IPREC .EQ. 1 ) CALL QRITER1(A(IV),A(I1),A(IL),QR) 
CWKBNE 2/94 SPR93027
      
      QRX(3) = IEND
      CALL CONMSG (QRX,3,0)
      RSTRT  = 0
      WIL(3) = IBEGN
      CALL CONMSG (WIL,3,0)
C
C     EIGENVECTORS
C
CWKBDB 2/94 SPR93027
C     CALL WILVEC (A(ID),A(IO),A(IV),A(IL),A(I1),A(I2),A(I3),A(I4),
C     1             A(I5),A(I6),N,A(I6))
CWKBDE 2/94 SPR93027
CWKBNB 2/94 SPR93027
      IF ( IPREC .EQ. 1 )
C                    D      0    C    A      B
     &CALL WILVEC1(A(ID),A(IO),A(IV),A(IL),A(I1),A(I2),A(I3),A(I4),
     &             A(I5),A(I6),N,A(I6))
      IF ( IPREC .EQ. 2 )
C                    D      0    C    A      B
     &CALL WILVEC (A(ID),A(IO),A(IV),A(IL),A(I1),A(I2),A(I3),A(I4),
     &             A(I5),A(I6),N,A(I6))
CWKBNE 2/94 SPR93027
      WIL(3) = IEND
      CALL CONMSG (WIL,3,0)
  300 CONTINUE
      CALL GOPEN (OEIGS,A(1),1)
      MCB(1) = 4
      MCB(2) = N
      MCB(3) = NV
      MCB(4) = NEVER
      MCB(5) = NVER
      MCB(8) = ITERM
      CALL WRITE (OEIGS,MCB,8,1)
      CALL CLOSE (OEIGS,1)
      VAL(3) = IEND
      CALL CONMSG (VAL,3,0)
      RETURN
      END
