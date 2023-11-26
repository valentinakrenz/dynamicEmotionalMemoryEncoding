def add_full_label(df):
    df['full_label'] = ''
    df['short_label'] = ''
    
    for index, row in df.iterrows():
        nii_name = row['nii_name']
        
        if nii_name == 'RH_DefaultA_IPL_1':
            full_label = 'angular gyrus'
            short_label = 'AG'
            
        elif nii_name == 'LH_DefaultB_IPL_1':
            full_label = 'angular gyrus'
            short_label = 'AG'

        elif nii_name == 'LH_DefaultB_Temp_1':
            full_label = 'temporal pole'
            short_label = 'tempPol'

        elif nii_name == 'LH_DefaultB_Temp_3':
            full_label = 'anterior superior temporal sulcus'
            short_label = 'aSTS'
            
        elif nii_name == 'LH_SomMotB_Aud_2':
            full_label = 'posterior superior temporal gyrus'
            short_label = 'pSTG'
            
        elif nii_name == 'RH_DefaultB_AntTemp_1':
            full_label = 'temporal pole'
            short_label = 'tempPol'
            
        elif nii_name == 'RH_DefaultB_PFCv_1':
            full_label = 'inferior frontal gyrus'
            short_label = 'IFG'
            
        elif nii_name == 'RH_DefaultB_Temp_1':
            full_label = 'posterior superior temporal sulcus'
            short_label = 'pSTS'  
            
        elif nii_name == 'RH_TempPar_1':
            full_label = 'superior temporal sulcus'
            short_label = 'STS'   
            
        elif nii_name == 'RH_SomMotB_S2_1':
            full_label = 'posterior insula'
            short_label = 'PI'
            
        elif nii_name == 'RH_SomMotB_S2_2':
            full_label = 'parietal operculum'
            short_label = 'PO'
            
        elif nii_name == 'RH_SomMotB_S2_3':
            full_label = 'rolandic operculum'
            short_label = 'RO'
            
        elif nii_name == 'RH_SomMotB_S2_4':
            full_label = 'central operculum'
            short_label = 'CO'
            
        elif nii_name == 'RH_ContC_Cingp_1':
            full_label = 'posterior cingulate cortex'
            short_label = 'pCC'
            
        elif nii_name == 'LH_SomMotA_1':
            full_label = 'mid cingulate cortex'
            short_label = 'mCC'
            
        elif nii_name == 'LH_DefaultA_PFCm_3':
             full_label = 'anterior cingulate cortex'
             short_label = 'ACC'
             
        elif nii_name == 'RH_TempPar_2' or nii_name == 'LH_DefaultB_Temp_4':
            full_label = 'superior temporal sulcus'
            short_label = 'STS'
            
        elif nii_name == 'LH_DefaultB_PFCv_2':
            full_label = 'lateral orbitofrontal cortex'
            short_label = 'lOFC'
            
        elif nii_name == 'LH_LimbicB_OFC_1':
            full_label = 'lateral orbitofrontal cortex'
            short_label = 'lOFC'
            
        elif nii_name == 'LH_LimbicA_TempPole_4':
            full_label = 'medial superior temporal pole'
            short_label = 'msTP'
            
        elif nii_name == 'RH_LimbicB_OFC_1':
            full_label = 'medial orbitofrontal cortex'
            short_label = 'mOFC'
                    
        elif nii_name == 'RH_DorsAttnB_PostC_4' or nii_name == 'RH_DorsAttnA_SPL_3':
            full_label = 'superior parietal lobule'
            short_label = 'SPL'
            
        elif nii_name == 'RH_SomMotA_5':
            full_label = 'postcentral sulcus'
            short_label = 'PoCS'
        
        else:
            full_label = 'Unknown'
            short_label = 'Unknown'

        # Update the DataFrame
        df.at[index, 'full_label'] = full_label
        df.at[index, 'short_label'] = short_label
    
    # Return the updated DataFrame
    return df
