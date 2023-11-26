# Function to load a NIfTI file and label =a binary array
def load_nii_as_binary(file_path):
    nii_image = nib.load(file_path)
    nii_data = nii_image.get_fdata()
    binary_data = nii_data > 0  # Assuming nonzero values represent the ROI
    return binary_data
