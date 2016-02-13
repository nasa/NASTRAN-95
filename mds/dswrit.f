      SUBROUTINE DSWRIT ( IUNIT, BUFF, LEN, IREC, ICCERR )                      
      INTEGER           BUFF( LEN )                                             
      COMMON / SYSTEM / SYSBUF, IWR                                             
      INCLUDE          'DSIOF.COM'                                              
c      print *,' dswrit,len,IREC,UNIT=',len,irec,iunit                          
      IF ( IREC .LE. 0 ) GO TO 701                                              
      WRITE ( IUNIT, REC=IREC, IOSTAT=ISTAT, ERR=702 ) BUFF                     
      ICCERR = 0                                                                
      GO TO 777                                                                 
701   WRITE( IWR, 901 ) IUNIT, IREC, MDSNAM( IUNIT )                            
901   FORMAT(//' ERROR IN DSWRIT, BAD RECORD NO., UNIT=',I4,' REC=',I5          
     &,      /,' FILE NAME=',A72 )                                              
      ICCERR = ISTAT                                                            
      CALL DSMSG  ( 101 )                                                       
      CALL MESAGE ( -61, 0, 0 )                                                 
702   WRITE( IWR, 902 ) IUNIT, IREC, ISTAT, MDSNAM( IUNIT )                     
902   FORMAT(//', ERROR ENCOUNTERED IN DSWRCC, UNIT=',I5,' RECORD='             
     &, I5,' STATUS=',I9,/' DSNAME=',A72 )                                      
      ICCERR = ISTAT                                                            
      CALL DSMSG  ( 101 )                                                       
      CALL MESAGE ( -61, 0, 0 )                                                 
777   CONTINUE                                                                  
      NUMWRI = NUMWRI + 1                                                       
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
