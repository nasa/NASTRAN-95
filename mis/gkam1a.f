      SUBROUTINE GKAM1A (MI,PHIDH,SDT,SCR1,SCR2,IOPT,IOUT,NOPP,W,NW,
     1                   NOSDT,LHSET,I2DD,IWS,SCR3)
C
      INTEGER          PHIDH,SDT,SCR1,SCR2,SYSBUF,MCB(7),SCR3,FILE,
     1                 NAME(2),IHH(3)
      DOUBLE PRECISION MD,MC,MA(2),ZERO(2)
      DIMENSION        W(1),ITAB(2),ITABT(13)
      CHARACTER        UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG /  UFM,UWM,UIM,SFM
      COMMON /SYSTEM/  KSYSTM(65)
      COMMON /BLANK /  XX(9), KDAMP
      COMMON /CONDAS/  PI,TWOPHI,RADEG,DEGRA,S4PISQ
      COMMON /PACKX /  IT1,IT2,II,JJ,INCR
      COMMON /UNPAKX/  IT11,III,JJJ,INCR1
      EQUIVALENCE      (KSYSTM(1),SYSBUF), (KSYSTM(2),NOUT),
     1                 (KSYSTM(55),IPREC), (MD,MA(1)), (MC,MA(2))
      DATA    ZERO  /  0.0D0,0.0D0  /
      DATA    NAME  /  4HGKAM,4H1A  /
      DATA    IHH   /  4HMHH ,4HBHH ,4HKHH  /
      DATA    G     /  0.0          /
      DATA    ITABT ,  ITAB(1) / 4,15,21,1,25,22,2,35,23,3,45,24,4,0 /
C
C
      MC = 0.0
      IF (NOPP .LT. 0) GO TO 10
C
C     COMPUTE PHIDH(T)*I2DD*PHIDH ONTO SCR2
C
      CALL SSG2B (I2DD,PHIDH,0,SCR1,0,2,1,IOUT)
      CALL SSG2B (PHIDH,SCR1,0,SCR2,1,2,1,IOUT)
      MCB(1) = I2DD
      CALL RDTRL (MCB)
      IF (MCB(4) .NE. 6) GO TO 11
      MCB(1) = SCR2
      CALL RDTRL (MCB)
      MCB(4) = 6
      CALL WRTTRL (MCB)
   11 CONTINUE
      MII = SCR1
   10 IF (NOPP .LT. 0) MII = IOUT
C
C     BUILD  MII  DATA  BLOCK  = MIXF(W)
C
      LC = KORSZ(W(NW+1))
      NZ = LC - SYSBUF
C
C     RESTORE MODES
C     FILE = SCR3
C
      CALL OPEN (*130,SCR3,W(NZ+1),0)
      CALL FREAD (SCR3,W,NW,1)
      CALL CLOSE (SCR3,1)
      FILE = MI
      CALL OPEN (*170,MI,W(NZ+1),0)
      IMI  = 0
      CALL SKPREC (MI,IWS)
   21 CONTINUE
      NZ   = NZ - SYSBUF
      IBUF = NZ - SYSBUF
      ICRQ = -IBUF
      IF (ICRQ .GT. 0) GO TO 150
      CALL GOPEN (MII,W(NZ+1),1)
      CALL MAKMCB (MCB,MII,LHSET,6,IPREC)
      IF (KDAMP .EQ. 1) MCB(5) = MCB(5) + 2
C
C     SET UP FOR  PACK  AND  UNPACK
C
      IT1  = 2
      IF (KDAMP .EQ. 1) IT1 = 4
      IT2  = MCB(5)
      INCR = 1
      IT11 = 2
      INCR1= 1
      DO 90 I = 1,NW
      MC   = 0.0
      K    = IWS + I - 1
      II   = I
      JJ   = I
      III  = K
      JJJ  = K
      IF (IMI .NE. 0) GO TO 85
      CALL UNPACK (*160,MI,MD)
   22 CONTINUE
      GO TO (30,50,40), IOPT
C
C     BUILDING  MHH
C
   30 CALL PACK (MD,MII,MCB)
      GO TO 90
C
C     BUILDING  KHH
C
   40 MD = MD*W(I)*W(I)
      IF (KDAMP .NE. 1) GO TO 30
      ASSIGN 45 TO IRET
      IF (NOSDT .GT. 0) GO TO 70
   45 MC = G*MD
      GO TO 30
C
C     BUILDING  BHH
C
   50 CONTINUE
      IF (KDAMP .EQ. 1) GO TO 61
      ASSIGN 60 TO IRET
      IF (NOSDT .GT. 0) GO TO 70
   60 MD = MD*W(I)*G
      GO TO 30
   61 MD = 0.0
      GO TO 30
C
C     LOOK UP G(W)  IN  SDT
C
   70 IF (ITAB(1) .GT. 0) GO TO 80
      ITAB(1) = 1
      ITAB(2) = NOSDT
      CALL PRETAB (SDT,W(NW+1),W(NW+1),W(IBUF),IBUF-1,IZ,ITAB(1),ITABT)
   80 CALL TAB (ITAB(2),W(I)/TWOPHI,G)
      GO TO IRET, (60,45)
C
C     PICK UP MODAL MASS FROM LAMA
C
   85 CALL FREAD (MI+1,0,-5,0)
      CALL FREAD (MI+1,XMASS,1,0)
      CALL FREAD (MI+1,0,-1,0)
      MD = XMASS
      GO TO 22
C
C     ADD  INTERPOLATION HERE
C
   90 CONTINUE
      CALL CLOSE (MI  ,1)
      CALL CLOSE (MI+1,1)
      NE = LHSET - NW
      IF (NE .LE. 0) GO TO 110
      DO 100 I = 1,NE
      CALL PACK (ZERO,MII,MCB)
  100 CONTINUE
  110 CALL  WRTTRL (MCB)
      CALL  CLOSE (MII,1)
      RETURN
C
C     ERROR MESAGES
C
  130 IP1  = -1
  140 CALL MESAGE (IP1,FILE,NAME)
      RETURN
  150 IP1  = -8
      FILE = ICRQ
      GO TO 140
  160 WRITE  (NOUT,9001) SFM,IHH(IOPT)
 9001 FORMAT (A25,' 2203, NULL COLUMN FOUND IN MI FILE DURING ASSEMBLY',
     1       ' OF ',A4,' MATRIX BY GKAM MODULE.')
      IP1  = -37
      GO TO 140
C
C     USE LAMA RATHER THAN MI
C
  170 CONTINUE
      CALL GOPEN (MI+1,W(NZ+1),0)
      CALL SKPREC (MI+1,1)
      CALL FREAD (MI+1,MCB,-7*(IWS-1),0)
      IMI = 1
      GO TO 21
      END
