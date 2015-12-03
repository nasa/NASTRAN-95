      SUBROUTINE FERFBS(V1,V2,V3,VB)
C
C  FERFBS is a modification of the old FRBK subroutine.  It has been
C  modified to read matrix data from memory until that data is exhausted
C  and then to read the remaining data from the file.
C
      REAL              DCORE(1)
      REAL              V1(1)     ,V2(1)     ,V3(1)     ,VB(1)  ,
     1                  XL(1)     ,XLJJ      ,V3J       ,V2J
      INTEGER           IBLK(20)  ,SMAPOS
      COMMON / ZZZZZZ / ICORE(1)   
      COMMON / OPINV  / MCBLT(7)  ,MCBSMA(7)
      COMMON / SYSTEM / KSYSTM(65)
      COMMON / FEERIM / NIDSMA    ,NIDLT     ,NIDORV    ,NLTLI   
     1,                 NSMALI    ,IBFSMA    ,IBFLT
     2,                 IBFORV    ,SMAPOS(7) ,LTPOS(7)
      EQUIVALENCE       ( KSYSTM(02),NOUT)
      EQUIVALENCE       ( DCORE(1)  ,ICORE(1), XL )  
C
      NROW    = MCBLT(2)
      DO 10 I = 1,NROW
   10 V2(I) = V1(I)
      ILROW = LTPOS( 1 )
      ICROW = NROW
      IF ( ILROW .EQ. 0 .AND. NIDLT .NE. 0 ) GO TO 1000
C
C     BACKWARD SUBSTITUTION
C
C     POSITION FILE TO LAST COLUMN
C
      IF ( NIDLT .EQ. 0 ) GO TO 12
      CALL DSSPOS ( MCBLT, LTPOS(5), LTPOS(6), LTPOS(7) )
      GO TO 16
  12  CALL REWIND ( MCBLT )
      CALL SKPREC ( MCBLT, NROW+1 )
  16  CONTINUE
      IBLK( 1 ) = MCBLT( 1 )
      J       = NROW
  15  IBLK(8) = -1
      ICROW   = J
      IF ( J .LE. ILROW ) GO TO 1000
  20  CALL GETSTB(*50,IBLK(1))
      NTMS    = IBLK(6)
      JI      = IBLK(5)
      IK      = IBLK(4)
      IF( IK - NTMS + 1 .NE. J) GO TO 25
      NTMS    = NTMS - 1
      XLJJ    = XL(JI-NTMS)
      IF(NTMS .EQ. 0) GO TO 40
   25 V2J     = V2(J)
      DO 30 II= 1,NTMS
      V2J     = V2J + XL(JI) * V2(IK)
      JI      = JI - 1
      IK      = IK - 1
   30 CONTINUE
      V2(J)   = V2J
   40 CALL ENDGTB(IBLK(1))
      GO TO 20
   50 V2(J)   = V2(J) / XLJJ
      IF(J .EQ. 1) GO TO 2000
      J       = J -1
      GO TO 15
C
C     CONTINUE BACKWARD SUBSTITUTION WITH DATA IN MEMORY
C
1000  CONTINUE
      MEM     = NLTLI
      NTMS    = ICORE(MEM)
      MEM     = MEM - NTMS - 3
      J       = ICROW
 1015 ICOL    = ICORE(MEM)
      IF ( ICOL .NE. J ) GO TO 1050
      NTMS    = ICORE(MEM+1)
      NTMSS   = NTMS
      JI      = MEM + 1 + NTMS
      IK      = ICORE( MEM + 2 + NTMS ) + NTMS - 1
      IF( IK-NTMS+1 .NE. J) GO TO 1025
      NTMS    = NTMS - 1
      XLJJ    = DCORE(JI-NTMS)
      IF(NTMS .EQ. 0) GO TO 1040
 1025 V2J     = V2(J)
      DO 1030 II= 1,NTMS
      V2J     = V2J + DCORE(JI) * V2(IK)
      JI      = JI - 1
      IK      = IK - 1
 1030 CONTINUE
      V2(J)   = V2J
 1040 IF ( MEM .EQ. NIDLT ) GO TO 1050
      NTMSNX  = ICORE( MEM-1 )
      MEM     = MEM - NTMSNX - 4
      GO TO 15
 1050 V2(J)   = V2(J) / XLJJ
      IF(J .EQ. 1) GO TO 2000
      J       = J -1
      GO TO 1015
2000  CALL FERLTS(MCBSMA(1),V2(1),V3(1),VB(1))
C
C BEGIN FORWARD SWEEP DIRECTLY ON V3
C
      ICROW = 1
      IF ( NIDLT .EQ. 0 ) GO TO 3005
      MEM   = NIDLT
      DO 2120 J = 1, NROW
      ICROW = J
      IF ( J .GT. ILROW ) GO TO 3000
 2080 ICOL  = ICORE(MEM)
      IF( ICOL .NE. J ) GO TO 2120
      JI    = MEM + 2
      NTMS  = ICORE( MEM+1 )
      NTMSS = NTMS
      IK    = ICORE(MEM + 2 + NTMS)
      IF ( IK .NE. J ) GO TO 2085
      NTMS  = NTMS - 1
      V3(J) = V3(J) / DCORE(JI)
      JI    = JI + 1
      IK    = IK + 1
 2085 IF(NTMS .EQ. 0) GO TO 2100
      V3J   = V3(J)
      DO 2090 II = 1,NTMS
      V3(IK)= V3(IK) + DCORE(JI) * V3J
      IK    = IK + 1
      JI    = JI + 1
 2090 CONTINUE
 2100 MEM   = MEM + NTMSS + 4
      GO TO 2080
 2120 CONTINUE
      GO TO 7000
 3000 CONTINUE
C
C     CONTINUE FORWARD SWEEP DIRECTLY ON V3
C
C     POSITION FILE TO CONTINUE READING COLUMN DATA NOT IN MEMORY
C
      CALL DSSPOS ( MCBLT, LTPOS(2), LTPOS(3), LTPOS(4) )
      GO TO 3008
 3005 CALL REWIND ( MCBLT )
      CALL SKPREC ( MCBLT, 1 )
 3008 CONTINUE
      DO 3120 J = ICROW, NROW
      IBLK( 8 ) = -1
 3080 CALL GETSTR( *3120, IBLK )
      IK    = IBLK( 4 )
      JI    = IBLK( 5 )
      NTMS  = IBLK( 6 )
      IF ( IK .NE. J) GO TO 3085
      NTMS  = NTMS - 1
      V3(J) = V3(J) / XL(JI)
      JI    = JI + 1
      IK    = IK + 1
 3085 IF(NTMS .EQ. 0) GO TO 3100
      V3J   = V3(J)
      DO 3090 II = 1,NTMS
      V3(IK)= V3(IK) + XL(JI) * V3J
      IK    = IK + 1
      JI    = JI + 1
 3090 CONTINUE
 3100 CALL ENDGET(IBLK(1))
      GO TO 3080
 3120 CONTINUE
      GO TO 7000
 7000 CONTINUE
      RETURN
      END
