      SUBROUTINE MMARC1 ( ZI, ZR )
C  MMARC1 - This routine will store a matrix column in memory in compact
C           form and in real single precision.  The input matrix is 
C           assumed to be stored as real single precision.
C           The column is stored in memory according to the following scheme:
C
C
C  1st word = row position of first element in following string 
C  2nd word = number of terms in string (ntms)
C  3rd word           }
C     |               }
C     |               } = actual
C     |               }   matrix
C     |               }   string
C     |               }   data
C     |               }
C     |               }
C  3+(ntms*prec)      } (where prec=1 for s.p.;  =2 for d.p. )
C
C  The above data repeats for all strings within a column
C
C  Argument list :
C     ZI  - Memory for storage of data (integer)
C     ZR  - Same location as ZI but real single reference 
C
      REAL             ZR(1)
      INTEGER          ZI(1)
      INTEGER          IBLK(15) 
      INCLUDE          'MMACOM.COM'         
      COMMON  /ZZZZZZ/ RXL(1)
      MEM       = 1
      DO 10 I   = 1,15
10    IBLK(I)   = 0
      IBLK(1)   = IRFILE
      IBLK(8)   = -1
      LASIND    = MEM - 1        
      ZI( MEM ) = 0
100   CALL GETSTR ( *7000, IBLK )
      JROW      = IBLK( 4 )   
      INDEX     = IBLK( 5 )
      NTMS      = IBLK( 6 )
      ZI(MEM)   = JROW
      ZI(MEM+1) = NTMS
      MEM       = MEM + 1
      DO 200 II = 1,NTMS
      ZR(MEM+II)= SIGN*RXL(INDEX+II-1)
  200 CONTINUE
      MEM       = MEM + 1 + NTMS  
      CALL ENDGET ( IBLK )
      GO TO 100
 7000 CONTINUE  
      LASIND    = MEM - 1        
      RETURN        
      END
