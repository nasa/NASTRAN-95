      SUBROUTINE REDU (CDATA,NX,IX,NAS,IAS,NVAR,VAR,IPRE,IER)
C
      INTEGER         CDATA(5),VAR(3,6),BLANK,EQS
      DIMENSION       IX(3,1),IAS(2,1),KEYS(6)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ IBUF,IOUT
      DATA    KEYS  / 4HNAMA,4HNAMB,4HNONA,4HNONB,4HPREC,4HBOUN /
      DATA    NAME  / 4HNAME/, EQS /4H=   /,BLANK/4H            /
C
C     INITIALLIZE
C
      DO 10 I = 1,6
   10 VAR(1,I) = KEYS(I)
C
      NVAR = 18
      DO 20 I = 1,2
      VAR(2,I) = BLANK
   20 VAR(3,I) = BLANK
      DO 30 I = 3,6
      VAR(2,I) = -1
   30 VAR(3,I) = 0
C
C     DECODE COMMAND
C
      I2 = 4
      IF (CDATA(5) .EQ.  EQS) I2 = 6
      IF (CDATA(1)*2 .LT. I2) GO TO 100
C
      VAR(2,1) = CDATA(I2  )
      VAR(3,1) = CDATA(I2+1)
C
      NVX = 6
C
C     FIND NAME
C
      DO 40 I = 1,NX
      IF (IX(1,I) .NE. NAME) GO TO 35
      VAR(2,2) = IX(2,I)
      VAR(3,2) = IX(3,I)
      GO TO 40
  35  IF (IX(1,I) .NE. KEYS(6)) GO TO 37
      VAR(2,6) = IX(2,I)
      VAR(3,6) = IX(3,I)
      GO TO 40
   37 NVX = NVX + 1
      DO 38 J = 1,3
   38 VAR(J,NVX) = IX(J,I)
   40 CONTINUE
      IF (VAR(2,2) .EQ. BLANK) GO TO 100
      IF (VAR(3,6) .LE.     0) GO TO 120
      IF (IPRE.LE.0 .OR. IPRE.GT.2) IPRE = 1
C
      VAR(3,5) = IPRE
C
C     FIND STRUCTURE NUMBERS, B MAY NOT PRE-EXIST
C
      IF (NAS .EQ. 0)  GO TO  80
      DO 70 I = 1,NAS
      IF (VAR(2,1).NE.IAS(1,I) .OR. VAR(3,1).NE.IAS(2,I)) GO TO 55
      VAR(3,3) = I
      GO TO 70
   55 IF (VAR(2,2).EQ.IAS(1,I) .AND. VAR(3,2).EQ.IAS(2,I)) GO TO 100
   70 CONTINUE
   80 NAS = NAS + 1
      VAR(3,4) = NAS
      IAS(1,NAS) = VAR(2,2)
      IAS(2,NAS) = VAR(3,2)
      IF (VAR(3,3) .NE. 0) GO TO 90
      NAS = NAS + 1
      VAR(3,3) = NAS
      IAS(1,NAS) = VAR(2,1)
      IAS(2,NAS) = VAR(3,1)
   90 IER  = 0
      NVAR = NVX*3
      RETURN
C
  100 WRITE  (IOUT,101) UFM
  101 FORMAT (A23,' 6614, ILLEGAL OR NON-EXISTANT STRUCTURE NAME USED ',
     1        'ABOVE')
      GO TO 130
  120 WRITE  (IOUT,121) UFM
  121 FORMAT (A23,' 6615, ILLEGAL BOUNDARY SET IDENTIFICATION NUMBER')
  130 IER = 1
      RETURN
      END
