      SUBROUTINE ASCM02 (NAME,IPHASE,ISOL,NOGO)
C
C     RUN COMMAND DATA
C
      INTEGER        COMND(6,2),SUBNAM(2),RDMAP(18,6),PTBS(7,1)
      COMMON /ASDBD/ IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1               IPH,NPH,IDAT(117)
      DATA    COMND/
     1               4HRUN     ,  1    ,  0    ,  0    ,  1    ,  0  ,
     2               4HENDD    ,  6    ,  0    ,  0    ,  0    ,  0  /
      DATA    RDMAP/
     1 4HPARA,4HM   ,4H  //,4H*ADD,4H*/DR,4HY/-1,4H /0 ,4H$   ,4H    ,
     * 4H    , 8*4H    ,
     2 4HLABE,4HL   ,4H  LB,4HSEND,4H $  ,13*4H    ,
     3 4HPARA,4HM   ,4H  //,4H*ADD,4H*/DR,4HY/DR,4HY/1 ,4H$   ,4H    ,
     * 4H    , 8*4H    ,
     4 4HCOND,4H    ,4H  FI,4HNIS,,4HDRY ,4H$   ,12*4H    ,
     5 4HREPT,4H    ,4H  LB,4HSBEG,4H,1 $,13*4H    ,
     6 4HJUMP,4H    ,4H  FI,4HNIS ,4H$   ,13*4H           /
      DATA    PTBS /
     1               1,  22, 23, 3,  4HRUN ,  0,  0       /
C
      DATA   SUBNAM/ 4HASCM,2H02  /
C
C     VALIDATE COMMAND AND SET POINTERS
C
      DO 10 I = 1, 2
      IF (NAME .EQ. COMND(1,I)) GO TO 20
   10 CONTINUE
      GO TO 70
   20 ICOMND = I
      IRDM   = 1
      NRDM   = COMND(2,ICOMND)
      IXTRA  = IRDM  + 18*NRDM
      NXTRA  = COMND(3,ICOMND)
      IOCT   = IXTRA + NXTRA
      NOCT   = COMND(4,ICOMND)
      IPTBS  = IOCT  + 3*NOCT
      NPTBS  = COMND(5,ICOMND)
      IPH    = IPTBS + 7*NPTBS
      NPH    = COMND(6,ICOMND)
C
C     MOVE RDMAP DATA
C
      K = 0
      IF (NRDM .EQ. 0) GO TO 40
      DO 30 J = 1,NRDM
      DO 30 I = 1,18
      K = K + 1
   30 IDAT(K) = RDMAP(I,J)
   40 CONTINUE
C
C     MOVE PTBS DATA
C
      IF (NPTBS .EQ. 0) GO TO 60
      DO 50 J = 1,NPTBS
      DO 50 I = 1,7
      K = K + 1
   50 IDAT(K) = PTBS(I,J)
   60 CONTINUE
C
      RETURN
C
C     INPUT ERROR
C
   70 CALL MESAGE (7,0,SUBNAM)
      NOGO = 1
      RETURN
C
      END
