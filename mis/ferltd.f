      SUBROUTINE FERLTD (IFILE,DZ,DY,ZM)
C
C  FERLTD was originally subroutine FRMLTD.  FERLTD allows for 
C  reading the input matrix from core and after the core data is
C  exhausted, then reading the remaining data from the file.
C  See subroutine FERRDM for how data is stored within memory for the 
C  matrix and for the contents of SMAPOS.
C
C   FEER MATRIX TRANSPOSE MULTIPLY  (DOUBLE PREC)
C
      DOUBLE PRECISION  DZ(1)     ,DY(1)     ,DSUM      ,ZM(1)
      DOUBLE PRECISION  DCORE(1)   
      INTEGER           IFILE(7)  ,SMAPOS
      COMMON  /UNPAKX/  ITYP      ,IP        ,NP        ,INCR
      COMMON  /ZZZZZZ/  ICORE(1)
      COMMON  /FEERIM/  NIDSMA    ,NIDLT     ,NIDORV    ,NLTLI
     1,                 NSMALI    ,IBFSMA    ,IBFLT
     2,                 IBFORV    ,SMAPOS(7) ,LTPOS(7)
      EQUIVALENCE       ( DCORE(1),ICORE(1) )
      N     = IFILE(2) 
      ICCOL = 1
      IF ( NIDSMA .EQ. 0 ) GO TO 1005
      MEM   = NIDSMA
      ILCOL = SMAPOS( 1 )
      DO 20 I = 1,N
      ICCOL = I
C CHECK TO SEE IF REMAINING DATA IS ON THE FILE AND NOT IN MEMORY
      IF ( ICCOL .GT. ILCOL ) GO TO 1000
      DY(I) = 0.D0
      DSUM  = 0.D0
    5 ICOL  = ICORE(MEM)
      IF( ICOL .NE. I ) GO TO 20
      NTMS  = ICORE(MEM+1)
      IP    = ICORE(MEM+2+2*NTMS)
      NP    = IP+NTMS-1
      INDX  = MEM/2+1
      II    = 0
      DO 10 J = IP,NP
      II    = II +1
   10 DSUM  = DSUM + DCORE(INDX+II) * DZ(J)
      DY(I) = DSUM
      MEM   = MEM+4+2*NTMS
      GO TO 5
   20 CONTINUE
      GO TO 7000
 1000 CONTINUE
      CALL DSSPOS ( IFILE, SMAPOS(2), SMAPOS(3), SMAPOS(4) )
      GO TO 1008
 1005 CALL REWIND ( IFILE )
      CALL SKPREC ( IFILE, 1 )
 1008 CONTINUE
      INCR  = 1
      ITYP  = IFILE(5)
      DO 1020 I = ICCOL, N
      DY(I) = 0.D0
      IP    = 0
      CALL UNPACK(*1020,IFILE,ZM(1))
      II   = 0
      DSUM = 0.D0   
      DO 1010 J = IP,NP
      II = II +1
 1010 DSUM  = DSUM + ZM(II) * DZ(J)
      DY(I) = DSUM
 1020 CONTINUE
 7000 CONTINUE     
      RETURN
      END
