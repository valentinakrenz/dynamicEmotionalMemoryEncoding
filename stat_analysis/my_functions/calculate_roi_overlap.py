# compute overlap between mni coordinates of .nii images in local folder
def calculate_roi_overlap(df, full_roi_path, atlas_path, file_name_filters=None):
    overlap_results = {}

    # Collect the atlas files that meet the criteria
    all_files = os.listdir(atlas_path)
    if file_name_filters:
        prefix = file_name_filters.get("prefix", "")
        substring = file_name_filters.get("substring", "")
        all_files = [f for f in all_files if f.startswith(prefix) and substring in f]

    for index, row in df.iterrows():
        nii_name = row['nii_name']
        roi_path = os.path.join(full_roi_path, f"{nii_name}.nii")

        roi_img = nib.load(roi_path)
        roi_mni_coords = get_non_zero_mni_coordinates(roi_img)
        roi_volume = len(roi_mni_coords)

        formatted_overlap_entries = []

        for atlas_nii in all_files:
            atlas_path_full = os.path.join(atlas_path, atlas_nii)
            atlas_img = nib.load(atlas_path_full)
            atlas_mni_coords = get_non_zero_mni_coordinates(atlas_img)

            overlap_coords = roi_mni_coords.intersection(atlas_mni_coords)

            if overlap_coords:
                overlap_volume = len(overlap_coords)
                overlap_percentage = (overlap_volume / roi_volume) * 100
                overlap_percentage = round(overlap_percentage)

                atlas_name_without_extension = os.path.splitext(atlas_nii)[0]
                formatted_entry = f"{atlas_name_without_extension} ({overlap_percentage}%)"
                formatted_overlap_entries.append(formatted_entry)

        overlap_results[nii_name] = formatted_overlap_entries

    overlap_series = df['nii_name'].apply(lambda x: ", ".join(overlap_results.get(x, [])))
    return overlap_series
