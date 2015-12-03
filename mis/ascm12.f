      SUBROUTINE ASCM12 (NAME,IPHASE,ISOL,NOGO)
C
C     PLOT COMMAND DMAP DATA
C
      INTEGER        COMND(6,1),SUBNAM(2),RDMAP(18,6),PTBS(7,15)
      COMMON /ASDBD/ IRDM,NRDM,IXTRA,NXTRA,IOCT,NOCT,IPTBS,NPTBS,
     1               IPH,NPH,IDAT(213)
      DATA    COMND/ 4HPLOT,  6,  0,  0, 15,  0   /
      DATA    RDMAP/
     1 4HPLTM,4HRG  ,4H  CA,4HSECC,4H,PCD,4HB/PL,4HTSTP,4H,GPS,4HTP,E,
     * 4HLSTP,4H,BGS,4HTP,C,4HASST,4HP,EQ,4HSTP/,4H*NAM,4HE   ,4H */ ,
     2 4H    ,4H    ,4H  S,,4HN,NG,4HP/S,,4HN,LS,4HIL/S,4H,N,N,4HPSET,
     * 4H $  , 8*4H    ,
     3 4HSETV,4HAL  ,4H  //,4HS,N,,4HPLTF,4HLG/1,4H/S,N,4H,PFI,4HL/0 ,
     * 4H$   , 8*4H    ,
     4 4HPLOT,4H    ,4H  PL,4HTSTP,4H,GPS,4HTP,E,4HLSTP,4H,CAS,4HSTP,,
     * 4HBGST,4HP,EQ,4HSTP,,4H,,,,,4H,,/P,4HMSTP,4H/NGP,4H/LSI,4HL/  ,
     5 4H    ,4H    ,4H  S,,4HN,NP,4HSET/,4HS,N,,4HPLTF,4HLG/S,4H,N,P,
     * 4HFIL ,4H$   , 7*4H    ,
     6 4HPRTM,4HSG  ,4H  PM,4HSTP/,4H/ $ ,13*4H        /
      DATA    PTBS /
     1           1  , 26  , 26  ,  3  ,4HSTEP  ,         0  ,  0  ,
     2           1  , 32  , 32  ,  3  ,4HSTEP  ,         0  ,  0  ,
     3           1  , 38  , 38  ,  3  ,4HSTEP  ,         0  ,  0  ,
     4           1  , 44  , 44  ,  3  ,4HSTEP  ,         0  ,  0  ,
     5           1  , 51  , 51  ,  3  ,4HSTEP  ,         0  ,  0  ,
     6           1  , 57  , 57  ,  3  ,4HSTEP  ,         0  ,  0  ,
     7           1  , 62  , 62  ,  8  ,4HNAME  ,         0  ,  0  ,
     8           4  , 14  , 14  ,  3  ,4HSTEP  ,         0  ,  0  ,
     9           4  , 20  , 20  ,  3  ,4HSTEP  ,         0  ,  0  ,
     O           4  , 26  , 26  ,  3  ,4HSTEP  ,         0  ,  0  ,
     1           4  , 33  , 33  ,  3  ,4HSTEP  ,         0  ,  0  ,
     2           4  , 39  , 39  ,  3  ,4HSTEP  ,         0  ,  0  ,
     3           4  , 45  , 45  ,  3  ,4HSTEP  ,         0  ,  0  ,
     4           4  , 58  , 58  ,  3  ,4HSTEP  ,         0  ,  0  ,
     5           6  , 13  , 13  ,  3  ,4HSTEP  ,         0  ,  0  /
      DATA  SUBNAM  / 4HASCM,2H12  /
C
C     VALIDATE COMMAND AND SET POINTERS
C
      IF (NAME .NE. COMND(1,1)) GO TO 70
      ICOMND = 1
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
      IF (NRDM .EQ. 0) GO TO 35
      DO 30 J = 1,NRDM
      DO 30 I = 1,18
      K = K + 1
   30 IDAT(K) = RDMAP(I,J)
   35 CONTINUE
C
C     MOVE PTBS DATA
C
      IF (NPTBS .EQ. 0) GO TO 65
      DO 60 J = 1,NPTBS
      DO 60 I = 1,7
      K = K + 1
   60 IDAT(K) = PTBS(I,J)
   65 CONTINUE
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
