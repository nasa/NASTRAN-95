      SUBROUTINE CIFSDD
C
C     THIS SUBROUTINE INITIALIZES THE CIFS1P, CIFS2P, CIFS3P,
C     CIFS4P, AND CIFS5P COMMON BLOCKS.
C
      INTEGER B1, BARDF2, BARDF5, BARDF6, BARDF7, BARDF8
      INTEGER G1
C
      LOGICAL SLOT
      LOGICAL FPHYS, FPHYS1, DMIFLG, FTHRU, FPHYS2
      LOGICAL GRDMSG, LH, IDFREQ
      LOGICAL LFLSYM, FFPHYS
C
      COMMON /CIFS1P/ B1, BARDF2, BARDF5, BARDF6, BARDF7, BARDF8,
     1                KM1, SLOT, IDRDL
      COMMON /CIFS2P/ FPHYS, FPHYS1, KM2, DMIFLG, IBCDS, FTHRU, FPHYS2
      COMMON /CIFS3P/ GRDMSG, LA1, L7, KM3, L0, G1, LH,
     1                IGDST2, IGDST6, IGDST7, IGDST8, IDDSF,
     2                IDFREQ, IDRAD, NVAR, IDS, JMS, KMS, LPLF
      COMMON /CIFS4P/ J(20), KM4, LFLSYM, FFPHYS
      COMMON /CIFS5P/ KM5, IC, IP, ICONT, IAERO, IPOPT
C
      DATA ICC/1HC/, IPP/1HP/
C
      B1 = 1
      BARDF2 = 0
      BARDF5 = 0
      BARDF6 = 0
      BARDF7 = 0
      BARDF8 = 0
      KM1 = 0
      SLOT = .FALSE.
      IDRDL = 0
C
      FPHYS  = .TRUE.
      FPHYS1 = .TRUE.
      KM2 = 0
      DMIFLG = .FALSE.
      IBCDS = 0
      FTHRU  = .FALSE.
      FPHYS2 = .TRUE.
C
      GRDMSG = .FALSE.
      LA1 = 0
      L7  = 0
      KM3 = 0
      L0 = 1
      G1 = 1
      LH     = .TRUE.
      IGDST2 = 0
      IGDST6 = 0
      IGDST7 = 0
      IGDST8 = 0
      IDDSF = 0
      IDFREQ = .TRUE.
      IDRAD = 0
      NVAR  = 0
      IDS = 0
      JMS = 0
      KMS = 0
      LPLF = 0
C
      DO 100 I=3,20
      J(I)  = 0
  100 CONTINUE
      J(1) = 20
      J(2) = 2
      KM4 = 0
      LFLSYM = .FALSE.
      FFPHYS = .TRUE.
C
      KM5 = 0
      IC = ICC
      IP = IPP
      ICONT = 0
      IAERO = 0
      IPOPT = 0
C
      RETURN
      END
