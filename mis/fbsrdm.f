      SUBROUTINE FBSRDM ( MCB   , ICORE , RCORE , DCORE 
     &,                   MEMTOT, BUFF, LASIND, IPOS )  
C
C  FBSRDM - This routine will store an entire matrix in memory
C           if sufficient memory exists.  The matrix 
C           is stored in memory according to the following scheme:
C           (Subroutine FERRDM is very similiar to this subroutine)
C
C  1st word = current column number
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
C  3+(ntms*prec)+1 = row position of first element in above string
C  3+(ntms*prec)+2 = number of terms in ABOVE string (ntms)
C
C  The above data repeats for all strings within a column and then
C  for all columns in the matrix.
C
C  Argument list :
C     MCB    - Matrix control block for input matrix
C     ICORE  - Memory for storage of data (integer)
C     RCORE  - Same location as ICORE but real single reference
C     DCORE  - Same location as ICORE but real double reference
C     MEMTOT - Total amount of memory available for this data
C     BUFF   - Buffer allocation for input matrix
C     LASIND - Memory index of last string stored in memory 
C     IPOS   - 6 word array with the following information
C              (1) = last column read into memory
C              (2) = block number of following column not read into memory
C              (3) = current logical record pointer for following column
C                    not read into memory
C              (4) = current buffer pointer for following record not read
C                    into memory
C              (5) = last block number in file
C              (6) = current logical record pointer for last record in file
C              (7) = current buffer pointer for last record in file
C
      DOUBLE PRECISION DCORE(1), DXL
      REAL             RCORE(1), RXL(1)
      INTEGER          RD, RDREW, WRT, WRTREW, REW, BUFF(2)
      INTEGER          IPOS(7)  , ICORE(1)
      INTEGER          IBLK(20),MCB(7)
      COMMON  /ZZZZZZ/ DXL(1)
      COMMON  /SYSTEM/ KSYSTM(65)
      COMMON  /NAMES / RD, RDREW, WRT, WRTREW, REW
      EQUIVALENCE      ( KSYSTM( 2), NOUT  )
      EQUIVALENCE      ( DXL,RXL )
      MEM          = 1
      NCOL         = MCB( 2 )
      NTYPE        = MCB( 5 )
      INCR         = 1
      IF ( NTYPE .EQ. 2 .OR. NTYPE .EQ. 3 ) INCR = 2
      IF ( NTYPE .EQ. 4 ) INCR = 4
      NTWDS        = 0
      IPOS( 1 )    = NCOL
      DO  5 I  = 2,7
      IPOS( I ) = 0
  5   CONTINUE
      DO 10 I  = 1,20
 10   IBLK(I)      = 0
      IBLK(1)      = MCB( 1 )
      IBLK(9)      = 1
      IBLK(10)     = 1
      CALL GOPEN  ( MCB, BUFF, RDREW )
      CALL REWIND ( MCB)
      CALL SKPREC ( MCB, 1 )
      DO 1000 JCOL = 1,NCOL
      IBLK(8)      = -1
      LASIND       = MEM - 1        
      CALL DSCPOS  ( MCB, IBLOCK, ICLR, ICBP )     
  100 CALL GETSTR(*1000,IBLK(1))
      INDEX        = IBLK( 5 )
      NTMS         = IBLK( 6 )
      JROW         = IBLK( 4 )
      NTWDS        = NTWDS + 4 + NTMS*INCR
      IF ( NTWDS .GT. MEMTOT ) GO TO 2000
      ICORE(MEM)   = JCOL
      ICORE(MEM+1) = NTMS
      GO TO ( 110, 120, 130, 140 ), NTYPE
  110 CONTINUE
      MINDEX     = MEM + 1
      DO 115 II  = 1,NTMS
      RCORE(MINDEX+II) = RXL(INDEX+II-1)
  115 CONTINUE
      MEM        = MEM + 2 + NTMS  
      GO TO 180
  120 CONTINUE
      MINDEX     = MEM/2+1
      DO 125 II  = 1,NTMS
      DCORE(MINDEX+II) = DXL(INDEX+II-1)
  125 CONTINUE
      MEM        = MEM + 2 + NTMS*2  
      GO TO 180
  130 CONTINUE
      MINDEX     = MEM + 1
      NTMS2      = NTMS*2
      DO 135 II  = 1,NTMS2
      RCORE(MINDEX+II) = RXL(INDEX+II-1)
  135 CONTINUE
      MEM        = MEM + 2 + NTMS2  
      GO TO 180
  140 CONTINUE
      MINDEX     = MEM/2+1
      NTMS2      = NTMS*2
      DO 145 II = 1,NTMS2
      DCORE(MINDEX+II) = DXL(INDEX+II-1)
  145 CONTINUE
      MEM        = MEM + 2 + NTMS*4  
      GO TO 180
  180 CONTINUE
      ICORE(MEM  ) = JROW
      ICORE(MEM+1) = NTMS
      MEM          = MEM + 2
  185 CALL ENDGET (IBLK( 1 ) )
      GO TO 100
 1000 CONTINUE  
      LASIND    = MEM - 1        
      GO TO 7000
 2000 IPOS( 1 ) = JCOL - 1
      IPOS( 2 ) = IBLOCK
      IPOS( 3 ) = ICLR
      IPOS( 4 ) = ICBP
      CALL SKPREC ( MCB, NCOL-JCOL+1 )
      CALL DSCPOS ( MCB, IBLOCK, ICLR, ICBP )
      IPOS( 5 ) = IBLOCK
      IPOS( 6 ) = ICLR
      IPOS( 7 ) = ICBP
 7000 CONTINUE
      CALL CLOSE ( MCB , REW )
      RETURN        
      END
