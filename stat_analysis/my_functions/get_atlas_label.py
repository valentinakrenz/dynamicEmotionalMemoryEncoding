# get harvard oxford atlas labels from central ROI-coordinats
def get_atlas_label(coord):
    # Convert the coordinates to the atlas voxel space
    x, y, z = coord_transform(*coord, np.linalg.inv(atlas_img.affine))
    x, y, z = int(round(x)), int(round(y)), int(round(z))
    # Check if the voxel is inside the image
    if (0 <= x < atlas_data.shape[0] and
        0 <= y < atlas_data.shape[1] and
        0 <= z < atlas_data.shape[2]):
        return atlas_labels[int(atlas_data[x, y, z])]
    else:
        return 'Outside Atlas'
