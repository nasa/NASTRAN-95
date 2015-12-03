      SUBROUTINE MMARC4 ( ZI, ZD )
C  MMARC4 - This routine will store a matrix column in memory in compact
C           form and in complex double precision.  The input matrix can
C           be stored in any precision or type.
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
C     ZD  - Same location as ZI but real double reference   
C
      INTEGER          ZI(1)
      INTEGER          IBLK(15) 
      DOUBLE PRECISION ZD(1), DXL(1)
      INCLUDE          'MMACOM.COM'           
      COMMON  /ZZZZZZ/ RXL(1)
      EQUIVALENCE      ( RXL, DXL )
      MEM      = 1
      DO 10 I  = 1,15
 10   IBLK(I)  = 0
      IBLK(1)  = IRFILE
      IBLK(8)  = -1     
      LASIND   = MEM - 1        
      ZI( MEM) = 0
  100 CALL GETSTR ( *1000, IBLK )
      ITYPE    = IBLK( 2 )
      JROW     = IBLK( 4 )      
      INDEX    = IBLK( 5 )
      NTMS     = IBLK( 6 )
      ZI(MEM)  = JROW
      ZI(MEM+1)= NTMS
      GO TO ( 110, 120, 130, 140 ), ITYPE
  110 CONTINUE
      MINDEX   = MEM/2 + 1
      DO 115 II = 1,NTMS
      ZD( MINDEX+1 ) = SIGN*RXL( INDEX+II-1 )
      ZD( MINDEX+2 ) = 0.D0
      MINDEX = MINDEX+2
  115 CONTINUE
      GO TO 180
  120 CONTINUE
      MINDEX = MEM/2 + 1
      DO 125 II = 1,NTMS
      ZD( MINDEX+1 ) = SIGN*DXL( INDEX+II-1 )
      ZD( MINDEX+2 ) = 0.D0
      MINDEX  = MINDEX + 2
  125 CONTINUE
      GO TO 180
  130 CONTINUE
      MINDEX  = MEM/2 + 1 
      NTMS2   = NTMS*2
      DO 135 II = 1,NTMS2
      ZD( MINDEX+II ) = SIGN*RXL( INDEX+II-1 )
  135 CONTINUE
      GO TO 180
  140 CONTINUE
      MINDEX  = MEM/2 + 1 
      NTMS2   = NTMS*2
      DO 145 II = 1,NTMS2
      ZD( MINDEX+II ) = SIGN*DXL( INDEX+II-1 )
  145 CONTINUE
  180 CONTINUE
      MEM     = MEM + 2 + NTMS*4  
      CALL ENDGET ( IBLK )
      GO TO 100
 1000 CONTINUE  
      LASIND  = MEM - 1        
      RETURN        
      END
