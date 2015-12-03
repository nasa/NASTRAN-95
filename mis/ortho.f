      SUBROUTINE ORTHO (U,V,X1,X2,X3,X4,X5,NZ,IBUF1,IBUF2,IBUF3,IBUF4)
C
C     ORTHO WILL ORTHOGONALIZE THE CURRENT ITERANT WITH RESPECT TO
C     THE PREVIOUSLY EXTRACTED EIGENVECTORS
C
      INTEGER          FILEM     ,FILEK    ,FILEB    ,FILELM   ,
     1                 FILEVC    ,SR0FIL   ,SR5FIL   ,REAL     ,
     2                 SUB(2)    ,IBUF1(1) ,IBUF2(1) ,IBUF3(1) ,
     3                 SQR       ,CDP      ,IBUF4(1)
      DOUBLE PRECISION U(1)      ,V(1)     ,X1(1)    ,X2(1)    ,
     1                 X3(1)     ,X4(1)    ,X5(1)    ,PJ(2)    ,
     2                 CONST1(2) ,CONST2(2),ALPHA(2),BETA(2)
      COMMON /CINVPX/  FILEK(7)  ,FILEM(7) ,FILEB(7),FILELM(7) ,
     1                 FILEVC(7),DMPFIL    ,SCRFIL(10)
      COMMON /CINVXX/  DUM(17)   ,REAL     ,XXXX     ,NORTHO
      COMMON /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                 REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                 RDP       ,CSP      ,CDP      ,SQR
      EQUIVALENCE      (SR0FIL,SCRFIL(10)) ,(SR5FIL,SCRFIL(5)) ,
     1                 (NCOL,FILEK(2))
      DATA    SUB   /  4HORTH ,4HO   /
C
      NCOL2 = NCOL  + NCOL
      NCOL4 = NCOL2 + NCOL2
      IF (FILEB(1) .EQ. 0) GO TO 5
      CALL CMTIMU (V,X1,0,IBUF4)
      CALL CMTIMU (U,X2,FILEB,IBUF4)
      GO TO 7
    5 DO 6 I = 1,NCOL2
    6 X2(I) = 0.D0
    7 CONTINUE
      CALL CMTIMU (U,X3,0,IBUF4)
      CALL SSWTCH (12,L7)
      CONST1(1) = 1.0D0
      CONST1(2) = 0.
      CONST2(1) =-1.0D0
      CONST2(2) = 0.
      CALL CSUB (X1,X2,X2,CONST1,CONST2)
C
C     REWIND EIGENVALUE AND EIGENVECTOR FILES
C
      IFILE = FILELM(1)
      CALL OPEN (*1000,IFILE,IBUF1,RDREW)
      IFILE = FILEVC(1)
      CALL OPEN (*1000,IFILE,IBUF2,RDREW)
      IFILE = SR0FIL
      CALL OPEN (*1000,IFILE,IBUF3,RDREW)
      DO 100 K = 1,NORTHO
C
C     READ AN EIGENVALUE
C
      IFILE = FILELM(1)
      CALL READ (*1010,*1020,IFILE,PJ(1),4,1,FLAG)
      CONST1(1) = -1.D0
      CONST1(2) = 0.
      CALL CSUB (X3,X2,X5,PJ,CONST1)
C
C     READ THE RIGHT EIGENVECTOR
C
      IFILE = FILEVC(1)
      CALL READ (*1010,*1020,IFILE,X1(1),NCOL4,1,FLAG)
C
C     READ THE LEFT EIGENVECTOR
C
      IFILE = SR0FIL
      CALL READ (*1010,*1020,IFILE,X4(1),NCOL4,1,FLAG)
C
      IF (FILEB(1) .NE. 0) GO TO 40
C
C    COMPUTE ALPHA USING REAL FORMULA
C
      CALL CX TRN Y (X4,X3,CONST1)
      GO TO 55
   40 CALL CX TRN Y (X4(1),X5(1),CONST1(1))
   55 ALPHA(1) = CONST1(1)
      ALPHA(2) = CONST1(2)
      BETA(1)  = ALPHA(1)*PJ(1) - ALPHA(2)*PJ(2)
      BETA(2)  = ALPHA(1)*PJ(2) + ALPHA(2)*PJ(1)
      IF (L7 .EQ. 0) GO TO 1901
      WRITE  (6,500) CONST1,CONST2,ALPHA
  500 FORMAT (4H NUM ,2D12.5,6H DENOM ,2D12.5,6H ALPHA ,2D12.5 )
 1901 CONTINUE
      DO 60 I = 1,NCOL2,2
      U(I  ) = U(I  ) - ALPHA(1)*X1(I) + ALPHA(2)*X1(I+1)
      U(I+1) = U(I+1) - ALPHA(2)*X1(I) - ALPHA(1)*X1(I+1)
      IF (FILEB(1) .EQ. 0) GO TO 60
      V(I  ) = V(I  ) - BETA(1)*X1(I  ) + BETA(2)*X1(I+1)
      V(I+1) = V(I+1) - BETA(1)*X1(I+1) - BETA(2)*X1(I  )
   60 CONTINUE
  100 CONTINUE
      CALL CLOSE (FILELM,NOREW)
      CALL CLOSE (FILEVC,NOREW)
      CALL CLOSE (SR0FIL,NOREW)
      RETURN
C
 1000 NO = -1
      GO TO 1500
 1010 NO = -2
      GO TO 1500
 1020 NO = -3
 1500 CALL MESAGE (NO,IFILE,SUB)
      RETURN
      END
