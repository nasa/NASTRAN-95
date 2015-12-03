      SUBROUTINE PEXIT
C
      INTEGER         HH,SS,DATE(3)
      COMMON /OUTPUT/ LE(17)
      COMMON /MACHIN/ MACH
      COMMON /MSGX  / NMSG
      COMMON /RESDIC/ IRDICT
      COMMON /SYSTEM/ ISYSTM(100)
      EQUIVALENCE     (ISYSTM( 2),NOUT  ), (ISYSTM(76),NOSBE),
     1                (ISYSTM(82),ICPFLG), 
     2                (ISYSTM(15),DATE  )
C
C     SEE IF ANY MESSAGES ARE IN THE QUEUE
C
      IF (NMSG   .GT. 0) CALL MSGWRT
      IF (ICPFLG .NE. 0) WRITE (IRDICT,10)
   10 FORMAT ('$ END OF CHECKPOINT DICTIONARY')
C
C     JOB DONE. PRINT LAST 4 MESSAGE LINES
C
      CALL WALTIM (I)
      HH = I/3600
      MM = (I-HH*3600)/60
      SS = I - HH*3600 - MM*60
      CALL CPUTIM (I,T,0)
      IF (MACH .EQ. 4) I = T
      IF (LE(1).EQ.-1 .AND. LE(2).EQ.-1) GO TO 70
      WRITE  (NOUT,20) LE,DATE,HH,MM,SS
   20 FORMAT (////40X,'* * * END OF JOB * * *', /1H1, /,' JOB TITLE = ',
     1       17A4, /,' DATE:',I3,1H/,I2,1H/,I2, /,' END TIME:',I3,1H:,
     2       I2,1H:,I2)
C
C     CDC TOTAL CPU TIME IS A BIG NUMBER. DON'T PRINT IT
C
      IF (MACH.EQ.4 .OR. LE(1).EQ.-1) GO TO 50
      IF (MACH .LE. 5) WRITE (NOUT,30) I
      IF (MACH .GT. 5) WRITE (NOUT,40) I
   30 FORMAT (' TOTAL CPU TIME',I6,' SEC.')
   40 FORMAT (' TOTAL WALL CLOCK TIME',I7,' SEC.')
C
C     FLUSH O/P BUFFERS
C
   50 WRITE  (NOUT,60)
   60 FORMAT (1H )
C
      IF (MACH.EQ.4 .AND. NOSBE.GT.0) CALL LINK (-1,NOSBE,1)
      GO TO 90
C
   70 J = 5
      IF (LE(9) .GE. 0) J = 3
      WRITE  (NOUT,80) (LE(I),I=J,8)
   80 FORMAT (//1X,6A4)
C
   90 CONTINUE
      CALL DBMSTF 
      DO 100 I = 1,4
      CLOSE ( I )
100   CONTINUE
      DO 200 I = 7,22
      CLOSE ( I )
200   CONTINUE
CWKBR 8/94 SUN  CALL EXIT
      CALL EXIT( 0 )
      END
