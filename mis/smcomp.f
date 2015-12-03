      SUBROUTINE SMCOMP (*,ZI,ZR,ZD)
C      
C DRIVER PROGRAM FOR SYMMETRIC DECOMPOSITION.  SUBROUTINE SMCPH1 READS
C THE INPUT MATRIX AND STORES THE DATA EITHER IN MEMORY OR ON THE
C SPILL FILE.  SUBROUTINE SMCPH2 IS THEN CALLED TO PERFORM THE
C MATRIX DECOMPOSITION.
C
      REAL              ZR(4)
      INTEGER           ZI(4)
      INTEGER           MODULE(5), BEGN, END
      DOUBLE PRECISION  ZD(4)
      INCLUDE          'SMCOMX.COM'
C  mcb   - matrix control block for input matrix
C  lll   - matrix control block for lower triangular matrix
C  dbc   - dbc(1) = available scratch file, dbc(2-7) are not used
C  scr1, scr2, scr3 - three available scratch files
C  lcore - amount of open core available for use
C  ddr   - d.p. values of (real, imaginary) for scaled value of determinant
C  power - scale factor to apply to determinant, determinant=det * 10**power
C  mindd - d.p. value for minimum value of diagonal elements
C  chlsky - cholesky option when =1, i.e., form c matrix
C     
      DATA  MODULE / 4HSMCO, 4HMP  , 3*4H    /
      DATA  BEGN   / 4HBEGN /
      DATA  END    / 4HEND  /
      IERROR = 0 
      NCOL   = MCB(2)
      MODULE( 3 ) = BEGN
      STURM  = 0
      CALL CONMSG ( MODULE, 5, 0 )
      CALL SMCPH1 ( ZI, ZR, ZD )
      IF ( IERROR .EQ. 1 ) GO TO 701
      IF ( IERROR .NE. 0 ) GO TO 700
      CALL SMCPH2 ( ZI, ZR, ZD )
      IF ( IERROR .EQ. 1 ) GO TO 701
C
C print roots information if this is an eigenvalue problem, and keep
C two largest shift point data if several shift point movings are involved.
C      
      IF ( SHFTPT .GT. 0. ) WRITE ( NOUT, 901 ) STURM, SHFTPT
901   FORMAT( 20X, I5, ' ROOTS BELOW ', 1P,E14.6 )
      IF ( STURM .NE. 0 ) GO TO 100
      IF ( KEEP  .LE. 0 ) GO TO 700
      STURM  = KEEP
      SHFTPT = PTSHFT
      GO TO 700
100   IF ( KEEP .GT. STURM ) GO TO 700
      JJ     = KEEP
      RS     = PTSHFT
      KEEP   = STURM
      PTSHFT = JJ
      SHFTPT = RS
700   MODULE( 3 ) = END
      CALL CONMSG ( MODULE, 5, 0 )
      IF ( IERROR .NE. 0 ) RETURN 1
      GO TO 777
701   CONTINUE
      MODULE( 3 ) = END
      CALL CONMSG ( MODULE, 5, 0 )
777   CONTINUE
      RETURN
      END
