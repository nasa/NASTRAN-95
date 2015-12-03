      SUBROUTINE DSREAD ( IUNIT, BUFF, LEN, IREC )                              
      INTEGER           BUFF( LEN )                                             
      COMMON / SYSTEM / SYSBUF, IWR                                             
      INCLUDE          'DSIOF.COM'                                              
      IF ( IREC  .LT. 0 ) GO TO 701                                             
c      PRINT *,' DSREAD,LEN,IREC,IUNIT=',LEN,IREC,IUNIT                         
      ISTAT=0                                                                   
      READ ( IUNIT, REC=IREC, ERR=702, IOSTAT=ISTAT )                  
     &       BUFF                                                               
      IF ( ISTAT .EQ. 0 ) GO TO 777                                             
      IOERR = ISTAT                                                             
      CALL DSMSG  ( 101 )                                                       
      CALL MESAGE ( -61, 0, 0 )                                                 
701   WRITE ( IWR, 901 ) IUNIT, IREC, MDSNAM( IUNIT )                           
901   FORMAT(//' ERROR IN DSREAD-BAD REC NO., UNIT=',I4,' REC=',I4              
     &,      /,' FILE NAME=',A72)                                               
      ICCERR = 0                                                                
      CALL DSMSG  ( 101 )                                                       
      CALL MESAGE ( -61, 0, 0 )                                                 
      GO TO 777                                                                 
702   WRITE( IWR, 902 ) IUNIT, IREC, ISTAT, MDSNAM( IUNIT )                     
902   FORMAT(//', ERROR ENCOUNTERED IN DSREAD, UNIT=',I5,' RECORD='             
     &, I5,' STATUS=',I9,/' DSNAME=',A72 )                                      
      ICCERR = ISTAT                                                            
      CALL DSMSG( 101 )                                                         
      CALL MESAGE( -61, 0, 0 )                                                  
      GO TO 777                                                                 
777   CONTINUE                                                                  
      NUMREA = NUMREA + 1                                                       
      RETURN                                                                    
      END                                                                       
