      SUBROUTINE SDR2
C
C     SDR2 IS THE EXECUTIVE CONTROL PROGRAM FOR THE SDR2 MODULE.
C
      INTEGER         ANY   ,LOADS ,DISPL ,VEL   ,ACC   ,SPCF  ,PLOTS ,
     1                CASECC
      COMMON /SDR2X4/ NAM(2),END   ,MSET  ,ICB(7),OCB(7),MCB(7),DTYPE(8)
     1,               ICSTM ,NCSTM ,IVEC  ,IVECN ,TEMP  ,DEFORM,FILE  ,
     2                BUF1  ,BUF2  ,BUF3  ,BUF4  ,BUF5  ,ANY   ,ALL   ,
     3                TLOADS,ELDEF ,SYMFLG,BRANCH,KTYPE ,LOADS ,SPCF  ,
     4                DISPL ,VEL   ,ACC   ,STRESS,FORCE ,KWDEST,KWDEDT,
     5                KWDGPT,KWDCC ,NRIGDS,STA(2),REI(2),DS0(2),DS1(2),
     6                FRQ(2),TRN(2),BK0(2),BK1(2),CEI(2),PLA(22)      ,
     7                NRINGS,NHARMS,AXIC  ,KNSET ,ISOPL ,STRSPT,DDRMM
      COMMON /SDR2X2/ CASECC
      COMMON /SYSTEM/ SYSBUF,OPTE  ,NOGO  ,INTAP ,MPCN  ,SPCN  ,METHOD,
     1                LOADNN,SYMM  ,STFTMP,PAGE  ,LINE  ,TLINE ,MAXLIN,
     2                DATE(3),TIME  ,ECHO  ,PLOTS
C
C     EXECUTE THE PHASES OF SDR2.
C
      CASECC = 101
      CALL SDR2AA
      CALL SDR2A
      IF (ANY .NE. 0) CALL SDR2B
      K = LOADS + SPCF + DISPL + VEL + ACC + PLOTS
      IF (K   .NE. 0) CALL SDR2C
      IF (ANY .NE. 0) CALL SDR2D
      RETURN
      END
