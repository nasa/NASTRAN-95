      SUBROUTINE MMARC3 ( ZI, ZR )
C
C  MARRC3 - This routine will store a matrix column in memory in compact
C           form and in complex single precision.  The input matrix is
C           assumed to be stored as either real or complex single precision.
C           The column is stored in memory according to the following scheme:
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
      INTEGER          ZI(1)
      INTEGER          IBLK(15) 
      REAL             ZR(1)       
      INCLUDE          'MMACOM.COM'        
      COMMON  /ZZZZZZ/ RXL(1)
      COMMON  /SYSTEM/ IBFSIZ, IWR
      MEM      = 1
      DO 10 I  = 1,15
 10   IBLK(I)  = 0
      IBLK(1)  = IRFILE
      IBLK(8)  = -1     
      LASIND   = MEM - 1        
      ZI( MEM )= 0
  100 CALL GETSTR ( *1000, IBLK )
      ITYPE    = IBLK( 2 )
      JROW     = IBLK( 4 )      
      INDEX    = IBLK( 5 )
      NTMS     = IBLK( 6 )
      ZI(MEM)  = JROW
      ZI(MEM+1)= NTMS
      GO TO ( 110, 120, 130 ), ITYPE
  110 CONTINUE
      MINDEX   = MEM + 2
      DO 115 II = 1,NTMS
      ZR( MINDEX   ) = SIGN*RXL( INDEX+II-1 )
      ZR( MINDEX+1 ) = 0.
      MINDEX = MINDEX+2
  115 CONTINUE
      GO TO 180
C
C THE FOLLOWING LINE SHOULD NEVER BE REFERENCED
C
  120 CONTINUE
      WRITE( IWR, * ) ' ERROR IN MMARC3'
      STOP
  130 CONTINUE
      MINDEX   = MEM + 1
      NTMS2    = NTMS*2
      DO 135 II = 1,NTMS2
      ZR( MINDEX+II ) = SIGN*RXL( INDEX+II-1 )
  135 CONTINUE
  180 CONTINUE
      MEM        = MEM + 2 + NTMS*2  
      CALL ENDGET ( IBLK )
      GO TO 100
 1000 CONTINUE  
      LASIND     = MEM - 1        
      RETURN        
      END
