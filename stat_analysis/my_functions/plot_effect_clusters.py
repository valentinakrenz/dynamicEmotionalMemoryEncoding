import matplotlib.pyplot as plt
from nilearn import plotting

# requires my convenience function setup_t_map.py which should be in the same folder as this function
#exec(open("setup_t_map.py").read())

def plot_effect_clusters(full_roi_path, result_df, mni_ims):
    for effect in result_df['effect'].unique():
        # Loop through each unique cluster in this effect and plot each central voxel in each ROI within cluster
        effect_df = result_df[result_df["effect"] == effect]
        new_img = setup_t_map(effect_df, full_roi_path)
        
        # Get the absolute maximum T-value for setting vmax
        vmax = int(np.ceil(np.abs(effect_df['T'].max())))
        effect_name = effect
        
        # Loop through each unique cluster name
        for cluster in effect_df['cluster_name'].unique():
            cluster_data = effect_df[effect_df['cluster_name'] == cluster]
            unique_rois = cluster_data["nii_name"].unique()
      
            # Number of ROIs you want to display side by side
            n_cols = 2
            n_rows = (len(unique_rois) + n_cols - 1) // n_cols 
      
            fig, axes = plt.subplots(n_rows, n_cols, figsize=(2 * n_cols, 2 * n_rows))
            fig.suptitle(f"{effect}: {cluster}", fontsize=16, y=1.05)
      
            for i, ROI in enumerate(unique_rois):
                row_of_interest = cluster_data[cluster_data["nii_name"] == ROI].iloc[0]
                title = row_of_interest["full_label"]
                coords = [row_of_interest["x_coord"]]
                marker_coords = [row_of_interest["x_coord"], 
                                 row_of_interest["y_coord"], row_of_interest["z_coord"]]
                ax = axes[i] if n_rows == 1 else axes[i // n_cols, i % n_cols]
                display = plotting.plot_stat_map(new_img, bg_img=mni_ims, axes=ax, colorbar=False,
                                                 black_bg=False, vmax=vmax, draw_cross=False, 
                                                 cmap='coolwarm', cut_coords=coords, display_mode='x')
                display.add_markers(marker_coords=[marker_coords], marker_color='black', 
                                    marker_size=10)
                ax.set_title(title, loc="center") 
      
            # Remove any unused subplots
            for j in range(i + 1, n_rows * n_cols):
                ax_to_turn_off = axes[j] if n_rows == 1 else axes[j // n_cols, j % n_cols]
                ax_to_turn_off.axis('off')
      
            plt.tight_layout()
            plt.show()
            
