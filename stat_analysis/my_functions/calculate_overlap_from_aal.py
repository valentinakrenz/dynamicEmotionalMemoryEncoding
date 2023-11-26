# compute overlap between local nii images and aal from niilearn datasets
def calculate_overlap_from_aal(df, full_roi_path, aal_atlas):
    aal_atlas_img = nib.load(aal_atlas.maps)
    aal_atlas_data = aal_atlas_img.get_fdata()
    atlas_labels = aal_atlas.labels
    overlap_results = {}

    for index, row in df.iterrows():
        nii_name = row['nii_name']
        roi_path = os.path.join(full_roi_path, f"{nii_name}.nii")
        
        roi_img = nib.load(roi_path)
        roi_mni_coords = get_non_zero_mni_coordinates(roi_img)
        roi_volume = len(roi_mni_coords)

        label_counter = collections.Counter()

        for mni_coord in roi_mni_coords:
            vox_coords = np.round(nib.affines.apply_affine(np.linalg.inv(aal_atlas_img.affine), mni_coord)).astype(int)
            
            atlas_value = aal_atlas_data[tuple(vox_coords)]

            if atlas_value != 0:
                atlas_index = aal.indices.index(str(int(atlas_value)))
                label = aal.labels[atlas_index]
                label_counter[label] += 1
                
        formatted_overlap_entries = []

        for label, overlap_volume in label_counter.items():
            overlap_percentage = (overlap_volume / roi_volume) * 100
            overlap_percentage = round(overlap_percentage, 2)
            formatted_entry = f"{label} ({overlap_percentage}%)"
            formatted_overlap_entries.append(formatted_entry)
        
        overlap_results[nii_name] = formatted_overlap_entries

    overlap_series = df['nii_name'].apply(lambda x: ", ".join(overlap_results.get(x, [])))
    return overlap_series
