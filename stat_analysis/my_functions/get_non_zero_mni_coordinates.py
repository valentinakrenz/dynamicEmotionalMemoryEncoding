# Function to get all non-zero MNI coordinates from a roi image
def get_non_zero_mni_coordinates(img, data=None):
    if data is None:
        data = img.get_fdata()
    affine = img.affine
    non_zero_coords_vox = np.argwhere(data > 0)
    non_zero_coords_mni = nib.affines.apply_affine(affine, non_zero_coords_vox)
    return set(map(tuple, np.round(non_zero_coords_mni).astype(int)))
