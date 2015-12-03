      SUBROUTINE XPOLCK (DBN1,DBN2,FN,L)
C
      IMPLICIT INTEGER (A-Z)
      EXTERNAL        LSHIFT   ,  ANDF    ,   ORF
      DIMENSION       NPOLCK(2),  DDBN( 1),   DFNU( 1),  FCUM( 1),
     1                FCUS(  1),  FDBN( 1),   FEQU( 1),  FILE( 1),
     2                FKND(  1),  FMAT( 1),   FNTU( 1),  FPUN( 1),
     3                FON (  1),  FORD( 1),   MINP( 1),  MLSN( 1),
     4                MOUT(  1),  MSCR( 1),   SAL ( 1),  SDBN( 1),
     5                SNTU(  1),  SORD(  1)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /XFIAT / FIAT(7)
      COMMON /XFIST / FIST
      COMMON /XDPL  / DPD(6)
      COMMON /XSFA1 / MD(401),SOS(1501),COMM(20),XF1AT(5)
      EQUIVALENCE    (KSYSTM(2),OUTTAP  )
      EQUIVALENCE               (DPD  (1),DNAF    ),(DPD  (2),DMXLG   ),
     1      (DPD  (3),DCULG   ),(DPD  (4),DDBN (1)),(DPD  (6),DFNU (1)),
     2      (FIAT (1),FUNLG   ),(FIAT (2),FMXLG   ),(FIAT (3),FCULG   ),
     3      (FIAT (4),FEQU (1)),(FIAT (4),FILE (1)),(FIAT (4),FORD (1)),
     4      (FIAT (5),FDBN (1)),(FIAT (7),FMAT (1)),(MD   (1),MLGN    ),
     5      (MD   (2),MLSN (1)),(MD   (3),MINP (1)),(MD   (4),MOUT (1)),
     6      (MD   (5),MSCR (1)),(SOS  (1),SLGN    ),(SOS  (2),SDBN (1)),
     7      (SOS  (4),SAL  (1)),(SOS  (4),SNTU (1)),(SOS  (4),SORD (1)),
     8      (XF1AT(1),FNTU (1)),(XF1AT(1),FON  (1)),(XF1AT(2),FPUN (1)),
     9      (XF1AT(3),FCUM (1)),(XF1AT(4),FCUS (1)),(XF1AT(5),FKND (1))
      EQUIVALENCE               (COMM (1),ALMSK   ),(COMM (2),APNDMK  ),
     1      (COMM (3),CURSNO  ),(COMM (4),ENTN1   ),(COMM (5),ENTN2   ),
     2      (COMM (6),ENTN3   ),(COMM (7),ENTN4   ),(COMM (8),FLAG    ),
     3      (COMM (9),FNX     ),(COMM(10),LMSK    ),(COMM(11),LXMSK   ),
     4      (COMM(12),MACSFT  ),(COMM(13),RMSK    ),(COMM(14),RXMSK   ),
     5      (COMM(15),S       ),(COMM(16),SCORNT  ),(COMM(17),TAPMSK  ),
     6      (COMM(18),THCRMK  ),(COMM(19),ZAP     )
      DATA   POOL    /4HPOOL       /
      DATA   NPOLCK  /4HXPOL,4HCK  /
C
C
C     XPOLCK CHECKS THE DATA POOL DICT FOR A DATA BLOCK NAME
C
      LMT1 = DCULG*ENTN4
      DO 1 I = 1,LMT1,ENTN4
      IF (DBN1.NE.DDBN(I) .OR. DBN2.NE.DDBN(I+1)) GO TO 1
      FN = ANDF(RMSK,DFNU(I))
      L  = I
      RETURN
C
    1 CONTINUE
      FN = 0
      RETURN
C
C
      ENTRY XFILPS (NEW)
C     ==================
C
C     XFILPS POSITIONS THE POOL TAPE FORWARD OR BACKWARD
C
C     NEW IS DESIRED POSITION
C     FNX IS CURRENT POSITION
C
      FDIF = NEW - FNX
      IF (FDIF) 10,30,20
   10 FDIF = FDIF - 1
   20 CALL SKPFIL (POOL,FDIF)
      IF (FDIF.LT.0 .AND. NEW.NE.1) CALL SKPFIL (POOL,+1)
   30 RETURN
C
C
      ENTRY XPLEQK (NX,NY)
C     ====================
C
C     XPLEQK MOVES SECONDARY EQUIVALENCED DATA BLOCK NAMES FROM THE
C     POOL DICT. TO FIAT.   NTU-LTU DATA ARE ALSO STORED IN FIAT FOR THE
C     EQUIV. D.B.   NTU-LTU DATA IS EXTRACTED FROM SOS IF FOUND, IF NOT,
C     IT IS COPIED FROM THE CALLING PRIMARY D.B.
C
C     NX IS THE POOL DICT. INDEX
C     NY IS THE FIAT INDEX FOR PRIMARY D.B.
C
      FEQU(NY) = ORF(S,FEQU(NY))
      KFIL = ANDF(RMSK,DFNU(NX))
      LMT1 = DCULG*ENTN4
      LMT2 = SLGN *ENTN2
      LMT3 = FCULG*ENTN1
      NFCULG = LMT3 + 1
C
C     SEARCH FOR EQUIV FILES IN DICT
C
      DO 150 I = 1,LMT1,ENTN4
      IF (DDBN(I).EQ.0 .AND. DDBN(I+1).EQ.0) GO TO 150
      IF (KFIL .NE. ANDF(RMSK,DFNU(I))) GO TO 150
      IF (I .EQ. NX) GO TO 150
C
C     SEE IF NAME IS IN FIAT
C
      DO 100 J = 1,LMT3,ENTN1
      IF (DDBN(I).EQ.FDBN(J) .AND. DDBN(I+1).EQ.FDBN(J+1)) GO TO 115
  100 CONTINUE
      FDBN(NFCULG  ) = DDBN(I  )
      FDBN(NFCULG+1) = DDBN(I+1)
      FILE(NFCULG) = FILE(NY)
      FNTU(NFCULG) = FNTU(NY)
      FORD(NFCULG) = ORF(LSHIFT(1000,16),ANDF(RMSK,FILE(NFCULG)))
      FEQU(NFCULG) = ORF(S,FEQU(NFCULG))
      DO 110 J = 1,LMT2,ENTN2
      IF (DDBN(I).EQ.SDBN(J) .AND. DDBN(I+1).EQ.SDBN(J+1)) GO TO 120
  110 CONTINUE
      GO TO 140
C
C     FILE ALREADY ALLOCATED  BE SURE EQUIVED
C
  115 FILE(J) = ORF(ANDF(RMSK,FILE(NY)),ANDF(LMSK,FILE(J)))
      FEQU(J) = ORF(S,FEQU(J))
      GO TO 150
  120 FORD(NFCULG) = ORF(ANDF(LMSK,SORD(J)),ANDF(RMSK,FILE(NFCULG)))
      FEQU(NFCULG) = ORF(S,FEQU(NFCULG))
      FNTU(NFCULG) = SNTU(J)
  140 NFCULG = NFCULG+ ENTN1
      FCULG  = FCULG + 1
C
C     FLAG INDICATES D.B. S HAVE BEEN ADDED TO FIAT
C
      FLAG = -1
      IF (FCULG .GT. FMXLG) GO TO 900
  150 CONTINUE
      RETURN
C
  900 WRITE  (OUTTAP,901) SFM
  901 FORMAT (A25,' 1051, FIAT OVERFLOW')
      CALL MESAGE (-37,0,NPOLCK)
      RETURN
      END
