      SUBROUTINE DSOCFF ( DSNAME, IUNIT, ISTATUS )                              
      CHARACTER*72      DSNAME                                                  
      COMMON / SYSTEM / SYSBUF, IWR                                             
      COMMON / MACHIN / MAC(3), LQRO
      INCLUDE  'DSIOF.COM'                                                      
C  OPEN AND CLOSE FILE IN ORDER TO DELETE SPACE                                 
      OPEN  ( UNIT=IUNIT, FILE=DSNAME    , IOSTAT=ISTATUS, ERR=100 
     &,       STATUS='UNKNOWN' )            
100   CLOSE ( UNIT=IUNIT, STATUS='DELETE', IOSTAT=ISTATUS, ERR=701 )            
C NOW, OPEN FILE AS NEW FOR NASTRAN                                             
c      print *,' dsocff,nbuff=',nbuff                                           
      nbuff4 = nbuff * ( MOD(LQRO,100) / 10 )                                  
      OPEN  ( UNIT=IUNIT, FILE=DSNAME, RECL=NBUFF4, STATUS='NEW'                
     &,       access='direct', form='unformatted',IOSTAT=ISTATUS                
     &,       ERR=702 )                                                         
      GO TO 777                                                                 
701   WRITE ( IWR, 901 ) IUNIT, ISTATUS, DSNAME                                 
901   FORMAT(//,' FATAL ERROR IN DSOCFF, UNABLE TO CLOSE UNIT=',I4              
     &,         ' STATUS='I4                                                    
     &,       /,' FILE NAME=',A72 )                                             
      ICCERR = ISTATUS                                                          
      CALL DSMSG  ( 101 )                                                       
      CALL MESAGE ( -61, 0, 0 )                                                 
702   WRITE ( IWR, 902 ) IUNIT, ISTATUS, DSNAME                                 
902   FORMAT(//,' FATAL ERROR IN DSOCFF, UNABLE TO OPEN UNIT=',I4               
     &,         ' STATUS=',I4                                                   
     &,       /,' FILE NAME=',A72 )                                             
      ICCERR = ISTATUS                                                          
      CALL DSMSG  ( 101 )                                                       
      CALL MESAGE ( -61, 0, 0 )                                                 
777   RETURN                                                                    
      END                                                    
