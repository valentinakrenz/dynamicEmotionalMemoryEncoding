def find_center_coordinates(df, full_roi_path):
    # initialize lists to store results
    nii_names = []
    x_coords = []
    y_coords = []
    z_coords = []
    center_coordinates = []
    unique_nii_names = df['nii_name'].unique()

    for nii_name in unique_nii_names:
        # Append '.nii' to nii_name
        nii_filename = nii_name + '.nii'
        # Full path to the file
        full_path = full_roi_path + nii_filename
        # Load the image
        roi_image = nib.load(full_path)
        roi_data = roi_image.get_fdata()
        
        # Convert the data into binary (non-zero values)
        binary_roi_data = roi_data > 0
        
         # Check if the ROI is not empty
        if binary_roi_data.any():
            center_of_mass_vox = ndi.center_of_mass(binary_roi_data)
            center_of_mass_mni = nib.affines.apply_affine(roi_image.affine, center_of_mass_vox)
            x, y, z = [round(coord) for coord in center_of_mass_mni]
            coords_tuple = (x, y, z)
        else:
            # Handle empty ROI if needed
            x, y, z = [None, None, None]
            coords_tuple = None
            
        ## Get the center of mass of the ROI in voxel coordinates
        #center_of_mass_vox = ndi.center_of_mass(roi_data)
        ## Convert voxel coordinates to MNI space using the affine transformation
        #center_of_mass_mni = nib.affines.apply_affine(roi_image.affine, center_of_mass_vox)
        # Round the MNI coordinates to nearest integer
        #center_of_mass_mni = tuple(round(coord) for coord in center_of_mass_mni)

        nii_names.append(nii_name)
        x_coords.append(x)
        y_coords.append(y)
        z_coords.append(z)
        center_coordinates.append(coords_tuple)

    df_center_coordinates = pd.DataFrame({
        'nii_name': nii_names,
        'x_coord': x_coords,
        'y_coord': y_coords,
        'z_coord': z_coords,
        'center_coordinates': center_coordinates
    })

    df = pd.merge(df, df_center_coordinates, on=['nii_name'], how='left')

    return(df)
