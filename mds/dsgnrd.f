        SUBROUTINE DSGNRD                                                       
      INCLUDE 'XNSTRN.COM'                                                      
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'GINOX.COM'                                                       
        CHARACTER*4     CBUFF(2)                                                
        EQUIVALENCE     (CBUFF,IBASE)                                           
        IDSN   =  MDSFCB( 2, IFILEX )                                           
        IDSNR  = IDSN                                                           
   10   ISTRB  = FCB( 5, IDSNR )                                                
        IF ( NBLOCK .GE. ISTRB ) GO TO 20                                       
        IDSNR  = MDSFCB( 3, IDSNR ) / MULQ2                                     
        GO TO 30                                                                
   20   IEND   = FCB( 6, IDSNR )                                                
        IF ( NBLOCK .LE. IEND ) GO TO 40                                        
        IDSNR  = IAND( MDSFCB( 3, IDSNR  ), MASKH2 )                            
   30   IF ( IDSNR .GE. 1 .AND. IDSNR .LE. MAXDSN ) GO TO 10                    
        CALL DSMSG( 121 )                                                       
   40   IF ( IDSN .EQ. IDSNR ) GO TO 50                                         
        CALL DSCLOS( IDSN )                                                     
        MDSFCB( 1,IDSN ) = IAND( MDSFCB( 1,IDSN ), MASKH1 )                     
        IDSN   = IDSNR                                                          
        MDSFCB( 1, IDSN )   = IOR ( MDSFCB( 1,IDSN ), MASKH2 )                  
        MDSFCB( 2, IFILEX ) = IDSN                                              
        ISAVE = IOP                                                             
        IOP = 0                                                                 
        CALL DSOPEN( MDSNAM(IDSN), IDSN, IOP )                                  
        IOP = ISAVE                                                             
        CBUFF( INDBAS ) = MDSNAM( IDSN )                                        
   50   IOBLK  = NBLOCK - ISTRB + 1                                             
        CALL DSREAD( IDSN, IBASE(INDBAS+3), NBUFF, IOBLK )                      
        RETURN                                                                  
        END                                                                     
