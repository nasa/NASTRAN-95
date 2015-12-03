      SUBROUTINE READ4 (LAMA,PHI,SCR1,EPS,MASS)
C
C     READ4 WILL TEST FOR CLOSE AND EQUAL ROOTS AND MAKE SURE THE
C     CORRESPONDING VECTORS ARE ORTHOGONAL
C
      INTEGER          NAME(2)   ,PHI(7)   ,RSP      ,PHI1(7)
      INTEGER          RDREW     ,WRTREW
CWKBI ALPHA-OSF 9/94      
      INTEGER          SCR1
      DOUBLE PRECISION DZ(1)
      CHARACTER        UFM*23    ,UWM*25
      COMMON /XMSSG /  UFM       ,UWM
      COMMON /ZZZZZZ/  Z(1)
      COMMON /SYSTEM/  KSYSTM(65)
      COMMON /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                 REW       ,NOREW    ,EOFNRW   ,RSP
      COMMON /UNPAKX/  ITYPE     ,IUNPAK   ,JUNPAK   ,INCR
      COMMON /PACKX /  ITYPA     ,ITYPB    ,IPAK     ,JPAK     ,
     1                 INCRX
      EQUIVALENCE      (KSYSTM(1),ISYS)    ,(KSYSTM(2),IOUT)   ,
     1                 (DZ(1),Z(1))
      DATA    NAME  /  4HREAD,4H4   /
C
      NCOL  = PHI(2)
      NROW  = PHI(3)
      NZ    = KORSZ(Z)
      IBUF  = NZ - ISYS
      IBUF1 = IBUF - ISYS
      IBUF2 = IBUF1 - ISYS
      ICLOS = 0
      IDID  = 0
      IPR   = PHI(5)
      RMULT = .01
      ITYPE = RSP
      IUNPAK= 1
      JUNPAK= NROW
      INCR  = 1
      ITYPA = RSP
      ITYPB = RSP
      IPAK  = 1
      JPAK  = NROW
      INCRX = 1
      EPSI  = EPS
      IF (EPS .LE. 0.) EPSI = .0001
      NZ = NZ - ISYS - ISYS - 1 - ISYS
      CALL MAKMCB (PHI1,SCR1,NROW,2,RSP)
      IFILE = LAMA
      CALL GOPEN (LAMA,Z(IBUF),0)
      CALL READ (*170,*10,LAMA,Z(1),NZ,1,N)
      GO TO 180
   10 CALL CLOSE (LAMA,REW)
C
C     REJECT ALL BUT VALUES FOR WHICH VECTORS EXIST
C
      N  = PHI(2)
      NZ = NZ -N
      IF (NZ .LT. NROW) GO TO 180
      IFILE = PHI(1)
      CALL GOPEN (PHI,Z(IBUF),0)
      IPOS = 1
      I    = 1
      EPS1 = RMULT
   20 CONTINUE
      IF (ABS(Z(I))+ABS(Z(I+1)) .LT. EPS1) GO TO 1111
      IF (Z(I+1) .EQ. 0.0) GO TO 110
      IF (ABS(1.0-Z(I)/Z(I+1)) .GT. EPS1) GO TO 100
 1111 IF (ICLOS .NE. 0) GO TO 110
      ICLOS = I
      GO TO 110
   30 NUM  = I - ICLOS + 1
      EPS1 = RMULT
C
C     NUM   = NUMBER OF CLOSE ROOTS IN THIS GROUP
C     ICLOS = THE INDEX OF THE FIRST CLOSE ROOT
C
      IF (IDID .EQ. 1) GO TO 40
      IDID  = 1
      IFILE = SCR1
      CALL GOPEN (SCR1,Z(IBUF1),WRTREW)
   40 II = N + 1
   50 IF (IPOS .EQ. ICLOS) GO TO 70
      IFILE = PHI(1)
      CALL UNPACK (*190,PHI,Z(II))
      CALL PACK (Z(II),SCR1,PHI1)
      IPOS = IPOS + 1
      GO TO 50
   70 CONTINUE
C
C     CHECK FOR CORE OVERFLOW
C     EIGENVALUES + EIGENVECTORS + GEN. MASS + ACCUM.
C
      KORE = II + NUM*NROW + NUM*NUM + N + N + 3
      IF (KORE .GT. NZ) GO TO 160
      DO 80 J = 1,NUM
      CALL UNPACK (*190,PHI,Z(II))
      IPOS = IPOS + 1
      II   = II + NROW
      IF (II+NROW .GE. NZ) GO TO 180
   80 CONTINUE
      IJ = II + N + N + 3
      II = II/2 + 1
      CALL ORTCK (Z(N+1),MASS,Z(IBUF2),NUM,NROW,Z(IJ),DZ(II),EPSI)
      II = N + 1
      DO 90 J = 1,NUM
      CALL PACK (Z(II),SCR1,PHI1)
   90 II = II + NROW
      ICLOS = 0
  100 IF (ICLOS .NE. 0) GO TO 30
  110 I = I + 1
      IF (I     .LT.   N) GO TO 20
      IF (ICLOS .NE.   0) GO TO 30
      IF (IDID  .EQ.   0) GO TO 150
      IF (IPOS .GT. NCOL) GO TO 121
      DO 120 I = IPOS,NCOL
      CALL UNPACK (*190,PHI,Z)
      CALL PACK (Z(1),SCR1,PHI1)
  120 CONTINUE
  121 CALL WRTTRL (PHI1)
C
C     COPY VECTORS FROM SCR1 TO PHI
C
      CALL CLOSE (PHI,REW)
      CALL CLOSE (SCR1,REW)
      CALL GOPEN (PHI,Z(IBUF),1)
      CALL GOPEN (SCR1,Z(IBUF1),RDREW )
      CALL MAKMCB (PHI,PHI,NROW,2,IPR)
      ITYPB = IPR
      DO 140 I = 1,N
      CALL UNPACK (*190,SCR1,Z)
      CALL PACK (Z,PHI,PHI)
  140 CONTINUE
      CALL WRTTRL (PHI)
      CALL CLOSE (SCR1,REW)
  150 CALL CLOSE (PHI,REW)
      RETURN
C
  160 EPS2 = EPS1/10.
      WRITE  (IOUT,165) UWM,NUM,I,EPS1,EPS2
  165 FORMAT (A25,' 3142, INSUFFICIENT CORE STORAGE FOR EIGENVECTORS ',
     1       'ASSOCIATED WITH',I4,' MULTIPLE EIGENVALUES STARTING WITH',
     2       /28X,'MODE NUMBER',I4,' USING CURRENT MULTIPLE ROOT ',
     3       'CRITERIA. CRITERIA REDUCED FROM ',1P,E12.5,' TO ',E12.5)
      EPS1 = EPS2
      I = ICLOS
      GO TO 20
  170 NO = -2
      GO TO 200
  180 NO = -8
      GO TO 200
  190 NO = -7
  200 CALL MESAGE (NO,IFILE,NAME)
      RETURN
      END
