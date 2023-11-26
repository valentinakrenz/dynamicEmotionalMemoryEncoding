# get short_label
def shorten_labels(filtered_df):
    # Create a new DataFrame to hold the results
    new_df = filtered_df.copy()
    new_df['short_label'] = new_df['full_label'].copy()
    
    # Dictionary mapping full strings to short forms
    replace_dict = {
        'temporo-occipital': 'tempOcc',
        'orbitofrontal cortex' : 'OFC',
        'orbito-frontal cortex':'OFC',
        'cingulate cortex': 'CC',
        'precuneus': 'prec',
        'retrosplenial cortex': 'RSC',
        'superior parietal lobe': 'SPL',
        'intraparietal sulcus': 'IPS',
        'prefrontal cortex': 'PFC',
        'fusiform gyrus': 'fusGyr',
        'fronto-medial': 'mPFC',
        'mid cingulum': 'mCC',
        'motoric cortex': 'MotC',
        'central gyrus': 'CentG',
        'superior temporal gyrus': 'STG',
        'occipital pole': 'occPole',
        'parieto-occipital': 'parOcc',
        'occipital cortex': 'OC',
        'calcarine cortex': 'calc',
        'temporo occipital gyrus': 'tempOcc',
        'lingual gyrus': 'lingual',
        'inferior temporal': 'ITG',
        'anterior insula' : 'antIns',
        'middle temporal': 'MTG',
        'mid temporal gyrus': 'MTG',
        'parietal operculum': 'parOperc',
        'central operculum': 'centOperc',
        'rolandic operculum': 'rolOperc',
        'inferior frontal gyrus': 'IFG',
        'temporal pole': 'tempPol',
        ' pars triangularis': 'pt',
        'frontal cortex': 'FC',
        'dorso-lateral ': 'dl',
        'dorsal ': 'd',
        'medial ': 'm',
        'ventral ': 'v',
        'lateral ': 'l',
        'superior ': 's',
        'anterior ': 'a',
        'posterior ': 'p',
        'inferior ': 'inf',
        'mid ': 'm',
        'cortex':'',
        'gyrus':''
    }
    
    # Replace the values in the DataFrame
    new_df['short_label'] = new_df['full_label'].replace(replace_dict, regex=True)

    # Replace the substrings
    return new_df
