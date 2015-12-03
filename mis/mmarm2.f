      SUBROUTINE MMARM2 ( ZI, ZD, MEMPCOL )
C
C  MMARM2 - This routine will store matrix columns in memory in compact
C           form and in real double precision.  The input matrix is 
C           assumed to be stored as either real single or double precision.
C           The column is stored in memory according to the following scheme:
C
C  MEMPCOL  = Input, extra memory needed for each column that is stored
C             in memory in compact form.  This is needed for methods 40
C             and 41 where for each column of "B" stored in compact form
C             in memory, there needs to be space available for a column
C             of the "D" matrix.
C
C
C  1st word = column number (negative)
C  2nd word = index to next column within this array
C  3st word = row position of first element in following string 
C  4nd word = number of terms in string (ntms)
C  5rd word           }
C     |               }
C     |               } = actual
C     |               }   matrix
C     |               }   string
C     |               }   data
C     |               }
C     |               }
C  5+(ntms*prec)      } (where prec=1 for s.p.;  =2 for d.p. )
C     n               } Last value of last string for this column
C
C  Words 3 through 5+(ntms*prec) above data repeat for all strings 
C  within a column.  Words 1 through n repeat for all columns that are
C  read into memory.

C
C  Argument list :
C     ZI  - Memory for storage of data (integer)
C     ZD  - Same location as ZI but real double reference 
C
      INTEGER          ZI(1)
      INTEGER          IBLK(15), MODULE(2) 
      REAL             RXL(1)       
      DOUBLE PRECISION ZD(1), DXL     
      INCLUDE          'MMACOM.COM'          
      COMMON  /ZZZZZZ/ DXL(1)
      COMMON  /SYSTEM/ IBFSIZ, IWR
      EQUIVALENCE      ( DXL,RXL )
      DATA             MODULE / 4HMMAR, 4HM2   /
      MEM       = 1
      DO 10 I   = 1,15
10    IBLK(I)   = 0
      IBLK(1)   = IRFILE
C
C IRCOL1, FIRST COLUMN EXPECTED FOR THIS PASS
C IRCOLN, ON INPUT, THIS IS THE LAST COLUMN THAT IS NEEDED 
C         ON OUTPUT, THIS IS THE LAST COLUMN READ
C LASMEM, LAST AVAILABLE MEMORY INDEX TO THE "ZI" ARRAY
C      
      ICOL      = IRCOL1
100   CONTINUE                   
      IBLK(8)   = -1
      LASINDM   = MEM - 1        
      CALL DSCPOS ( IRFILE, ICBLK, ICLR, ICBP )
      CALL GETSTR ( *900, IBLK )
C      IF ( ICOL .NE. IBLK( 12 ) ) GO TO 7001
      ZI( MEM ) = -ICOL
      MEM1      = MEM + 1
      MEM       = MEM + 2
105   CONTINUE
      NTMS     = IBLK( 6 )
      IF ( ( MEM + 2 + NTMS*2 ) .GT. LASMEM ) GO TO 2000
      ITYPE    = IBLK( 2 )
      JROW     = IBLK( 4 )      
      INDEX    = IBLK( 5 )
      ZI(MEM)  = JROW
      ZI(MEM+1)= NTMS
      GO TO ( 110, 120 ), ITYPE
  110 CONTINUE
      MINDEX   = MEM/2 + 1
      DO 115 II = 1,NTMS
      ZD( MINDEX+II ) = SIGN*RXL( INDEX+II-1 )
  115 CONTINUE
      GO TO 180
  120 CONTINUE
      MINDEX     = MEM/2+1
      DO 125 II  = 1,NTMS
      ZD( MINDEX+II ) = SIGN*DXL( INDEX+II-1 )
  125 CONTINUE
  180 CONTINUE
      MEM        = MEM + 2 + NTMS*2  
      CALL ENDGET ( IBLK )
      CALL GETSTR ( *1000, IBLK )
      GO TO 105
900   CONTINUE
      ZI( MEM )   = -ICOL
      MEM1        = MEM + 1
      MEM         = MEM + 2
1000  CONTINUE
C
C CHECK IF SPACE IS AVAILABLE FOR A FULL COLUMN OF "D" MATRIX, IF NECESSARY
C
      IF ( MEM .GT. ( LASMEM-MEMPCOL ) ) GO TO 2000
      LASMEM      = LASMEM - MEMPCOL
      ZI( MEM1 )  = MEM 
      ICOL        = ICOL + 1
      IF ( ICOL .GT. IRCOLN ) GO TO 7000
      GO TO 100
2000  LASINDM    = MEM1 - 2
C 
C SAVE I/O LOCATION OF LAST COLUMN FOR NEXT PASS
C
      IRPOS( 1 ) = ICBLK
      IRPOS( 2 ) = ICLR
      IRPOS( 3 ) = ICBP
      IRCOLN     = ICOL - 1
      IF ( IRCOLN .LT. IRCOL1 ) CALL MESAGE ( -8, MEM+MEMPCOL, MODULE )
      GO TO 7777
7000  CONTINUE  
      LASINDM    = MEM - 1        
C      GO TO 7777
C7001  WRITE( IWR, 9001 ) ICOL, IBLK(12), IRFILE
C9001  FORMAT(' ERROR OCCURRED IN MMARM2, EXPECTED COLUMN =',I10
C     &,/,    ' BUT READ COLUMN =',I10,' FROM FILE =',I5 )
C      PRINT *,' IBLK=',IBLK
C      CALL DSMSG ( 777 )
C      CALL MESAGE ( -61, 0, 0 )
7777  RETURN        
      END
