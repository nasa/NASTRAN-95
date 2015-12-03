      SUBROUTINE FERRDM ( MCB,NIDX,MEMTOT,IBUFFI,LASIND,IPOS )  
C
C  FERRDM - This routine will store an entire matrix in memory
C           if sufficient memory exists.  The matrix    
C           is stored in memory according to the following scheme:
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
C  3+(ntms*prec)      } (where prec=1 for s.p.; =2 for d.p. )
C  3+(ntms*prec)+1 = row position of first element in above string
C  3+(ntms*prec)+2 = number of terms in ABOVE string (ntms)
C
C  The above data repeats for all strings within a column and then
C  for all columns in the matrix.
C
C  Argument list :
C     MCB    - Matrix control block for input matrix
C     NIDX   - Memory index for storing matrix data
C     MEMTOT - Total amount of memory available for this data
C     IBUFFI - Buffer allocation for input matrix
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
      DOUBLE PRECISION DCORE(1), DXL(1)
      REAL             RCORE(1), RXL(1)
      INTEGER          RD, RDREW, WRT, WRTREW, REW, IXL(1)
      INTEGER          IPOS(7)
      DIMENSION        IBLK(20),MCB(7)
      COMMON  /SYSTEM/ KSYSTM(65)
      COMMON  /ZZZZZZ/ ICORE(1)
      COMMON  /NAMES / RD, RDREW, WRT, WRTREW, REW
      EQUIVALENCE      ( KSYSTM( 2), NOUT              )
      EQUIVALENCE      ( KSYSTM(55), IPREC             )
      EQUIVALENCE      ( ICORE,DCORE,RCORE,DXL,RXL,IXL )
      MEM          = NIDX
      NCOL         = MCB( 2 )
      NTWDS        = 0
      IPOS( 1 )    = NCOL
      DO 10 i  = 1,20
 10   IBLK(i)      = 0
      IBLK(1)      = MCB( 1 )
      IBLK(9)      = 1
      IBLK(10)     = 1
      CALL GOPEN  ( MCB, ICORE( IBUFFI ), RDREW )
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
      NTWDS        = NTWDS + 4 + NTMS*IPREC
      IF ( NTWDS .GT. MEMTOT ) GO TO 2000
      ICORE(MEM)   = JCOL
      ICORE(MEM+1) = NTMS
      IF ( IPREC .EQ. 1 ) GO TO 160
      MINDEX         = MEM/2+1
      DO 150 II = 1,NTMS
      DCORE(MINDEX+II) = DXL(INDEX+II-1)
  150 CONTINUE
      GO TO 180
  160 MINDEX     = MEM + 1
      DO 170 II  = 1,NTMS
      RCORE(MINDEX+II) = RXL(INDEX+II-1)
  170 CONTINUE
  180 CONTINUE
      MEM          = MEM + 2 + NTMS*IPREC
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
