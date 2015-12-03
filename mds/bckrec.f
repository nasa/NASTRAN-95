        SUBROUTINE BCKREC ( FILE )                                              
      INCLUDE 'DSIOF.COM'                                                       
        INTEGER FILE                                                            
        NAME = FILE                                                             
        CALL DSGEFL                                                             
        CALL DSBRC1                                                             
        CALL DSSDCB                                                             
        RETURN                                                                  
        END                                                                     
