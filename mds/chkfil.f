       program chkfil                                                   
       character*80 card                                                
       read(5,901) card                                                 
901    format(a80)                                                      
       inptp = index( card, 'NPTP' )                                    
       iplt2 = index( card, 'PLT'  )                                    
       iexit = 0                                                        
       if ( inptp .ne. 0 ) iexit = 1                                    
       if ( iplt2 .ne. 0 ) iexit = iexit + 10                           
       if ( iexit .eq. 0 ) go to 700
       if ( iexit .eq. 1 ) open ( 77, file='nogood1', status='unknown' )
       if ( iexit .eq. 10) open ( 77, file='nogood2', status='unknown' )
       if ( iexit .eq. 11) open ( 77, file='nogood3', status='unknown' )
       write ( 77, * ) ' iexit=',iexit
       close ( 77 )
700    call exit( iexit )                                               
       end                                                              
