# function to combine t images for plotting
def setup_t_map(sub_df, full_roi_path):
   # Initialize the combined_img with zero array
    first_nii_image = nib.load(os.path.join(full_roi_path, sub_df['nii_name'].iloc[0] + '.nii'))
    combined_img = nib.Nifti1Image(np.zeros_like(first_nii_image.get_fdata()), 
                                   first_nii_image.affine, first_nii_image.header)
    #print(f"Initialized combined_img with shape: {combined_img.shape}")                               
    # Loop over the rows of the sub dataframe
    for i, row in sub_df.iterrows():
        nii_name = row['nii_name']
        t_value = row['T']
        # Append '.nii' to nii_name
        nii_filename = nii_name + '.nii'
        # Full path to the file
        full_path = full_roi_path + nii_filename
        #print(f"Loading {full_path} with T value: {t_value}")
        # Load the image
        roi_image = nib.load(full_path)
        roi_data = roi_image.get_fdata()
        # Create a new image data with the same shape as roi_data
        new_data = np.zeros_like(roi_data)
        # Set the voxels in the ROI to the T-value
        new_data[roi_data != 0] = t_value
        # Create a new Nifti image
        t_image = nib.Nifti1Image(new_data, roi_image.affine, roi_image.header)
    
        combined_img = image.math_img("img1 + img2", img1=combined_img, img2=t_image)
    #print("Combining images completed.")
    # Get the data from the image
    image_data = combined_img.get_fdata()
    # Replace all zeros with np.nan
    image_data[image_data == 0] = np.nan
    #print(f"Non-zero voxels in combined image: {np.count_nonzero(~np.isnan(image_data))}")

    # Create a new Nifti image with the updated data
    new_img = nib.Nifti1Image(image_data, combined_img.affine, combined_img.header)
    return new_img
