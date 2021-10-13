# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
%matplotlib qt5



import os
import pandas as pd
import matplotlib.pyplot as plt 



loc = 'loc_of_simulation_result'


shift_list = [ 'first', 'second', 'third' , 'four', 'all']   
shift_2_list =  [ 'First' , 'Second', 'Third', 'Fourth' ,'All']


subfig_title_list = [ '(a) When shift occur in the first dimension', '(b) When shift occur in the second dimension', '(c) When shift occur in the third dimension' , '(d) When shift occur in the four dimension', '(e) When shift occur in all dimension']   

for sig_type in ['negative', 'none',  'positive']: #

    for n in [5 ,30 ]:#
        plt.figure(figsize=(50,70))
        
        plt.subplot(231)
        plt.ylabel('$ARL_{1}$',size=16 , weight="bold")
        
        plt.subplot(234)
        plt.ylabel('$ARL_{1}$',size=12 , weight="bold")
        
        
        
        for cur_shift in range( len(shift_list) ):
        
            shift = shift_list[  cur_shift  ]
            shift_2 = shift_2_list[  cur_shift  ]
            
            #########################################################
            ###### read the corresponding file 
            
            ######### hotelling T2 ARL
            os.chdir( loc + '//hotellings_T2' )
            #'''
            tmp_csv_name_1 = 'arl_' + sig_type + '_n=' + str(n) +'_en.csv'
            
            tmp_df111 = pd.read_csv( tmp_csv_name_1, sep = ',')
            tmp_df1 = tmp_df111.loc[1:,:]
            

            
            ####### proposed method ARL
            os.chdir( loc + '//Proposed_Method' )
            
            tmp_csv_name_3 = 'arl_' + sig_type + '_n=' + str(n) + '_' + shift +'shift.csv'
            
            tmp_df333 = pd.read_csv( tmp_csv_name_3, sep = ',')
            
            
            tmp_df3 = tmp_df333.loc[1:,:]
            
            tmp_df3.index = tmp_df3['epsilon']
            
            
            #### 
            epsilon_select_list = [ '0.1','0.2', '0.3','0.4', '0.5', '0.6','0.7','0.8','0.9','5']
            
            #### combine results and produce a dataframe
            df = tmp_df3[ ['epsilon']+ epsilon_select_list ]
            df['Hotelling T^{2}'] = list(tmp_df1[shift_2])
            
            df['Hotelling T^{2}'] = df['Hotelling T^{2}'].astype(float).round(2)
            
            for col in epsilon_select_list:
                df[col] = df[col].astype(float)
            
            
            df[epsilon_select_list] = df[epsilon_select_list].round(2)
            


            ##################################    draw figures  ################################################
            
            epsilon_select_to_draw_list = ['0.1', '0.5',   '1','5']
            shift_size_select = [0.0, 0.5, 1]

            #### 
            df_draw = tmp_df3[ ['epsilon']+ epsilon_select_to_draw_list ]
            df_draw['Hotelling T^{2}'] = list(tmp_df1[shift_2])
            
            df_draw['Hotelling T^{2}'] = df_draw['Hotelling T^{2}'].astype(float)
            
            for col in epsilon_select_to_draw_list:
                df_draw[col] = df_draw[col].astype(float)
                
            plt.subplot(231 + cur_shift )
            
            line_style = ['-','--',  '-.', ':',  'dashed', 'dashdot', 'dotted','solid'] 
            k = 0 
            color_list  = ['red','orange','gold','green','blue','purple'] 
            
            
            l1 = plt.plot( list(df_draw.loc[ shift_size_select , 'epsilon' ]),  list(df_draw.loc[ shift_size_select , 'Hotelling T^{2}' ].astype(float) ) ,'b-', alpha = 0.5, linewidth = 3, label='Without Privacy Protection')
 
            for epsilon in epsilon_select_to_draw_list:    
                
                l3=plt.plot( list(df_draw.loc[ shift_size_select , 'epsilon' ]) , list(df_draw.loc[ shift_size_select , epsilon ].astype(float)) , linestyle = line_style[k] , linewidth = 1, color = color_list[k], label='Simulation $ARL_{1}$ with epsilon = ' + epsilon )
                k += 1
            plt.title( subfig_title_list[ cur_shift ] , size= 15, weight="bold")   
            plt.tick_params(labelsize=16)
            plt.xlabel('Shift Size',size=12 , weight="bold")
            
        plt.subplot(231 + cur_shift + 1 )
        
        l1 = plt.plot( [0], [0] ,'b-',alpha = 0.5,linewidth = 3,label='Without Privacy Protection')
        k = 0
        for epsilon in epsilon_select_to_draw_list:    
            
            l3=plt.plot( [0] , [0] , linestyle = line_style[k] ,linewidth = 1, color = color_list[k], label='Simulation $ARL_{1}$ with $\epsilon$ = ' + epsilon )
            k += 1
            
        plt.legend(loc='center', fontsize=16 )
        plt.axis('off')
        plt.tight_layout()
        
        # save figures
        
        #os.chdir( saveloc )
        #plt.savefig(  'Linechart_' + sig_type + '_n=' + str(n) + '.pdf' )
    
