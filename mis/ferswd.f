      SUBROUTINE FERSWD(V1,V3,VB)         
C
C  The original to this subroutine was FRSW2.  It has been modified
C  to read the matrix data from memory and after this data is exhausted
C  then to read the remaining data from the file.
C
      DOUBLE PRECISION  V1(1)     ,V3(1)     ,VB(1)  
     1,                 XL(1)     ,XLJJ      ,V3J
     2,                 ZERO      ,SUM       ,DCORE(1)
      INTEGER           IBLK(20)
      COMMON / ZZZZZZ / ICORE(1)  
      COMMON  /OPINV /  MCBLT(7) ,MCBSMA(7)
      COMMON  /SYSTEM/  KSYSTM(65)
      COMMON  /FEERIM/  NIDSMA, NIDLT    , NIDORV  , NLTLI
     1,                 NSMALI, IBFSMA   , IBFLT
     2,                 IBFORV, SMAPOS(7), LTPOS(7)
      EQUIVALENCE       (KSYSTM(02),IO)
      EQUIVALENCE       ( DCORE(1),ICORE(1), XL(1) )   
      DATA              ZERO / 0.0D0 /
C
      NROW    = MCBLT(2)
      CALL FERLTD(MCBSMA(1),V1(1),V3(1),VB(1))
C   FORWARD SWEEP DIRECTLY ON V3
      ICROW = 1
      IF ( NIDLT .EQ. 0 ) GO TO 1005
      ILROW = LTPOS( 1 )
      MEM   = NIDLT
      DO 190 J = 1,NROW
      ICROW = J
      IF ( ICROW .GT. ILROW ) GO TO 1000
  140 ICOL  = ICORE(MEM)
      IF ( ICOL .NE. J ) GO TO 180
      JI    = MEM/2+2
      NTMS  = ICORE(MEM+1)
      NTMSS = NTMS
      IK    = ICORE(MEM+2+2*NTMS)
      IF(IK .NE. J) GO TO 150
      NTMS  = NTMS - 1
      XLJJ  = DCORE(JI)
      JI    = JI + 1
      IK    = IK + 1
  150 IF(NTMS .EQ. 0) GO TO 170
      V3J   = V3(J)
      DO 160 II = 1,NTMS
      V3(IK)= V3(IK) + DCORE(JI) * V3J
      IK    = IK + 1
      JI    = JI + 1
  160 CONTINUE
  170 MEM   = MEM + NTMSS*2 + 4
      GO TO 140
  180 V3(J) = V3(J) / XLJJ
  190 CONTINUE
      GO TO 3000
 1000 CONTINUE
C POSITION FILE TO APPROPRIATE COLUMN 
      CALL DSSPOS ( MCBLT, LTPOS(2), LTPOS(3), LTPOS(4) )
      GO TO 1008
 1005 CONTINUE
      CALL REWIND ( MCBLT )
      CALL SKPREC ( MCBLT, 1 )
 1008 CONTINUE
      IBLK( 1 ) = MCBLT( 1 )
C
C CONTINUE WITH FORWARD SWEEP
C   
      DO 1090 J = ICROW, NROW
      IBLK( 8 ) = -1
 1030 CALL GETSTR ( *1070, IBLK )
      IK   = IBLK( 4 )
      JI   = IBLK( 5 )
      NTMS = IBLK( 6 )
      IF ( IK .NE. J ) GO TO 1040
      NTMS = NTMS - 1
      XLJJ = XL( JI )
      JI   = JI + 1
      IK   = IK + 1
 1040 IF ( NTMS .EQ. 0 ) GO TO 1060
      V3J  = V3( J )
      IF ( V3J .EQ. ZERO ) GO TO 1060
      DO 1050 II = 1, NTMS
      V3( IK ) = V3( IK ) + XL(JI)*V3J
      IK   = IK + 1
      JI   = JI + 1
 1050 CONTINUE
 1060 CALL ENDGET ( IBLK )
      GO TO 1030
 1070 CONTINUE
      V3( J ) = V3( J ) / XLJJ
 1090 CONTINUE
 2000 CONTINUE
C
C     BACKWARD SUBSTITUTION OMIT DIAGONAL
C
      ICROW = NROW
      IF ( J .EQ. 1 ) RETURN
      IF ( ILROW .EQ. NROW .AND. NIDLT .NE. 0 ) GO TO 3000
      J     = NROW
 2090 IBLK( 8 ) = -1
 2100 CALL GETSTB ( *2130, IBLK )
      IK    = IBLK( 4 )
      JI    = IBLK( 5 )
      NTMS  = IBLK( 6 )
      IF ( IK-NTMS+1 .EQ. J ) NTMS = NTMS - 1
      IF ( NTMS .EQ. 0 ) GO TO 2120
      SUM   = ZERO
      DO 2110 II = 1, NTMS
      SUM   = SUM + XL(JI) * V3(IK)
      JI    = JI - 1
      IK    = IK - 1
 2110 CONTINUE
      V3( J ) = V3( J ) + SUM
 2120 CALL ENDGTB ( IBLK )
      GO TO 2100
 2130 IF ( J .EQ. 1 ) GO TO 7000
      J = J - 1
      IF ( J .LE. ILROW ) GO TO 3000
      GO TO 2090
C  CONTINUE BACKWARD SUBSTITUTION USING DATA FROM MEMORY
 3000 CONTINUE
      MEM = MEM - NTMSS*2 - 4
 3200 CONTINUE
 3210 ICOL = ICORE(MEM)
      IF ( ICOL .NE. J ) GO TO 3240
      NTMS  = ICORE(MEM+1)
      NTMSS = NTMS
      JI    = MEM/2+1+NTMS
      IK    = ICORE(MEM+2+2*NTMS)+NTMS-1
      IF( IK-NTMS+1 .EQ. J) NTMS = NTMS - 1
      IF( NTMS .EQ. 0 ) GO TO 3230
      V3J   = V3( J )
      DO 3220 II = 1,NTMS
      V3J   = V3J + DCORE(JI) * V3(IK)
      JI    = JI-1
      IK    = IK-1
 3220 CONTINUE
      V3(J) = V3J
 3230 IF ( MEM .EQ. NIDLT ) GO TO 3250
      NTMSNX= ICORE(MEM-1)
      MEM   = MEM - NTMSNX*2 - 4
      GO TO 3210
 3240 IF ( J .EQ. 1 )  GO TO 3250
      J     = J-1
      GO TO 3200
 3250 CONTINUE
 7000 CONTINUE 
      RETURN
      END
