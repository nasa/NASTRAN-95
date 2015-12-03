      SUBROUTINE CFEER2 (IRET)
C
C     CFEER2 INITIALIZES AND CALLS CDCOMP FOR CFCNTL
C
      LOGICAL            QPR
      INTEGER            FILEA    ,FILEL    ,FILEU    ,SCR1     ,
     1                   SCR2     ,SCR3     ,SCR4     ,SCR5     ,
     2                   SCR6     ,SCR7     ,SCR8     ,SCR9     ,
     3                   SR1FIL   ,SR2FIL   ,SR3FIL   ,DUMM     ,
     4                   TYPOUT   ,BBBBAR
      DOUBLE PRECISION   DET      ,MINDIA   ,DZ(1)
      COMMON  /CDCMPX/   FILEA(7) ,FILEL(7) ,FILEU(7) ,SR1FIL   ,
     1                   SR2FIL   ,SR3FIL   ,DET(2)   ,POWER    ,
     2                   NZ       ,MINDIA   ,BBBBAR(5)
      COMMON  /FEERAA/   DUMM(36) ,SCR1     ,SCR2     ,SCR3     ,
     1                   SCR4     ,SCR5     ,SCR6     ,SCR7     ,
     2                   SCR8     ,SCR9     ,DUMQ(72) ,MCBLT(7) ,
     3                   MCBUT(7)
      COMMON  /FEERXC/   DUMXC(21),QPR
      COMMON  /ZZZZZZ/   Z(1)
      COMMON  /UNPAKX/   TYPOUT   ,IROW     ,NLAST    ,INCR
      COMMON  /SYSTEM/   KSYSTM(65)
      EQUIVALENCE        (Z(1), DZ(1)     ) ,(NOUT,KSYSTM(2))   ,
     1                   (IPREC,KSYSTM(55))
C
      ITYPE    = IPREC + 2
      IRET     = 0
      FILEA(1) = SCR1
      FILEL(1) = SCR3
      FILEU(1) = SCR4
      SR1FIL   = SCR5
      SR2FIL   = SCR6
      SR3FIL   = SCR7
      FILEA(2) = DUMM(3)
      FILEA(3) = DUMM(3)
      FILEA(4) = DUMM(4)
      FILEA(5) = ITYPE
      FILEA(6) = 0
      FILEA(7) = 0
      FILEL(5) = ITYPE
      NZ       = KORSZ(Z)
      BBBBAR(1)= 0
      CALL CDCOMP (*110,Z,Z,Z)
C
C     ---------- SPECIAL PRINT -------------------------------
C
      IF (.NOT.QPR) GO TO 80
      WRITE  (NOUT,10)
   10 FORMAT (//,7H CFEER2,//)
      WRITE  (NOUT,20)
   20 FORMAT (1H ,13(10H----------))
      TYPOUT = ITYPE
      IROW   = 1
      NLAST  = DUMM(2)
      LIMIT  = 2*NLAST
      INCR   = 1
      IBUF   = NZ - KSYSTM(1) - 2
      IFILXX = SCR3
   30 CALL GOPEN (IFILXX,Z(IBUF),0)
      DO 50 I = 1,NLAST
      WRITE  (NOUT,40) I
   40 FORMAT (1H ,6HCOLUMN,I4)
      CALL UNPACK (*50,IFILXX,Z)
      IF (IPREC .EQ. 2) WRITE (NOUT,60) (DZ(J),J=1,LIMIT)
      IF (IPREC .NE. 2) WRITE (NOUT,70) ( Z(J),J=1,LIMIT)
   50 CONTINUE
      CALL CLOSE (IFILXX,1)
      WRITE (NOUT,20)
      IF (IFILXX .EQ. SCR4) GO TO 80
      IFILXX = SCR4
      GO TO 30
   60 FORMAT (1H ,13(10H----------)/(1H ,4D25.16))
   70 FORMAT (1H ,13(10H----------)/(1H ,4E25.16))
   80 CONTINUE
C
C     --------------------------------------------------------
C
   90 DO 100 I = 1,7
      MCBUT(I) = FILEU(I)
  100 MCBLT(I) = FILEL(I)
      RETURN
C
  110 IRET = 1
      GO TO 90
      END
