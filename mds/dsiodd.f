      SUBROUTINE DSIODD                                                         
      INCLUDE 'GINOX.COM'                                                       
      INCLUDE 'DSIOF.COM'                                                       
      LGINOX = 5*NUMFCB + NUMSOF + 2                                            
      LHALF  = 16                                                               
      LENDSP = 0                                                                
      LENWPB = 0                                                                
      MASKH1 = 'FFFF0000'X                                                     
      MASKH2 = '0000FFFF'X                                                     
      MASKE1 = 'FF000000'X                                                     
      MASKE2 = '00FF0000'X                                                     
      MASKE3 = '0000FF00'X                                                      
      MASKE4 = '000000FF'X                                                      
      MCBMAS = '40000000'X                                                      
      MAXDSN = NUMFCB                                                           
      MASKQ1 = MASKE1                                                           
      MASKQ2 = MASKE2                                                           
      MASKQ3 = MASKE3                                                           
      MASKQ4 = MASKE4                                                           
      MULQ1  = 2**24                                                            
      MULQ2  = 2**16                                                            
      MULQ3  = 2**8                                                             
      IDSX   = '00EE0000'X                                                      
      IDSP   = '000E0000'X                                                      
      IDSC   = '000C0000'X                                                      
      IDSRH  = '11000000'X                                                      
      IDSRT  = '77000000'X                                                      
      IDSSB  = '22000000'X                                                      
      IDSSE  = '7F000000'X                                                      
      IDSCH  = '3B000000'X                                                      
      IDSCT  = '3F000000'X                                                      
      IDSSH  = '4B000000'X                                                      
      IDSST  = '4F000000'X                                                      
      IDSSD  = 'DD000000'X                                                      
      IDSEB  = 'EB000000'X                                                      
      IDSEF  = 'EF000000'X                                                      
      NWRDEL( 1 ) = 1                                                           
      NWRDEL( 2 ) = 2                                                           
      NWRDEL( 3 ) = 2                                                           
      NWRDEL( 4 ) = 4                                                           
      RETURN                                                                    
      END                                                                       
