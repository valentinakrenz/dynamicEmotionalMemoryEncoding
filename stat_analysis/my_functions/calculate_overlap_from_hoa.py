# compute overlap between local nii images and hoa from niilearn datasets
def calculate_overlap_from_hoa(df, full_roi_path, hoa_atlas):
    atlas_img = hoa_atlas.maps
    atlas_labels = hoa_atlas.labels
    overlap_results = {}

    for index, row in df.iterrows():
        nii_name = row['nii_name']
        roi_path = os.path.join(full_roi_path, f"{nii_name}.nii")

        roi_img = nib.load(roi_path)
        roi_mni_coords = get_non_zero_mni_coordinates(roi_img)
        roi_volume = len(roi_mni_coords)

        formatted_overlap_entries = []

        for label_index, label in enumerate(atlas_labels[1:]):  # Exclude background
            atlas_roi_data = (atlas_img.get_fdata() == (label_index + 1))
            atlas_mni_coords = get_non_zero_mni_coordinates(atlas_img, data=atlas_roi_data)
            
            overlap_coords = roi_mni_coords.intersection(atlas_mni_coords)
            
            if overlap_coords:
                overlap_volume = len(overlap_coords)
                overlap_percentage = (overlap_volume / roi_volume) * 100
                overlap_percentage = round(overlap_percentage)
                
                formatted_entry = f"{label} ({overlap_percentage}%)"
                formatted_overlap_entries.append(formatted_entry)
                
            overlap_results[nii_name] = formatted_overlap_entries
        
    overlap_series = df['nii_name'].apply(lambda x: ", ".join(overlap_results.get(x, [])))
    return overlap_series
