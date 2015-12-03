        SUBROUTINE EOF ( FILE )                                                 
      INCLUDE 'DSIOF.COM'                                                       
        INTEGER FILE                                                            
        NAME   = FILE                                                           
        IRETRN = 0                                                              
        CALL DSGEFL                                                             
        CALL DSEFWR                                                             
        CALL DSSDCB                                                             
        RETURN                                                                  
        END                                                                     
