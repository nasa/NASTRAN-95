      SUBROUTINE DSSEND ( FILE )
C
C DSSEND (Dataset Set to End) will position a file to the end
C to allow for closing a file for read and opening it for write
C append.  This eliminates having to read sequentially to the end 
C of the file before closing for read.
C
      INCLUDE 'DSIOF.COM'
      INCLUDE 'XNSTRN.COM'
      INTEGER    FILE
      NAME  = FILE
      CALL DSGEFL
C
C GET LAST BLOCK NUMBER IN THIS FILE FROM FCB
C      
      NBLOCK = FCB( 6, IFILEX )
C
C GET CURRENT BLOCK NUMBER IN THIS FILE FROM FCB
C
      ICBLK  = FCB( 4, IFILEX )
      IF ( ICBLK .EQ. NBLOCK ) GO TO 10
      CALL DBMMGR( 6 )
10    CONTINUE
      INDCLR = IBASE( INDBAS+4) + INDBAS - 1
      INDCBP = INDCLR
      CALL DSSDCB
      RETURN
      END
