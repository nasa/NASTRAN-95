        SUBROUTINE DSGNOP                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'GINOX.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        CHARACTER*4     CBUFF( 3 )                                              
        EQUIVALENCE     (CBUFF,IBASE)                                           
        IDSN    = IFILEX                                                        
        IF ( IOCODE .GE. 2 ) GO TO 10                                           
        IF ( IOCODE .NE. 1 ) GO TO 30                                           
        CALL DSRLSE                                                             
        GO TO 30                                                                
   10   INEXT   = IAND( MDSFCB( 3, IDSN ), MASKH2 )                             
        IF ( INEXT .EQ. 0 ) GO TO 30                                            
        ITEST =  FCB( 6, IDSN )                                                 
        IF ( NBLOCK .LE. ITEST ) GO TO 30                                       
        IDSN    = INEXT                                                         
        GO TO 10                                                                
   30   IOP = MOD ( IOCODE,2 )                                                  
        MDSFCB( 2,IFILEX ) = IDSN                                               
        MDSFCB( 1,IDSN )   = IOR( MDSFCB( 1,IDSN ), MASKH2 )                    
   40   CONTINUE                                                                
        CALL DSOPEN( MDSNAM( IDSN ), IDSN, IOCODE)                              
        RETURN                                                                  
        END                                                                     
