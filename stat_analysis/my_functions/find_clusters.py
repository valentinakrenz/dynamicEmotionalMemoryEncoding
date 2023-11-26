# Function to check if two binary arrays are adjacent
def are_adjacent(array1, array2):
    # Apply a 3D convolution with a kernel of 1's to identify adjacent regions
    kernel = np.ones((3, 3, 3), dtype=bool)
    convolved = ndi.convolve(array1.astype(int), kernel, mode='constant', cval=0)
    return np.any(convolved * array2)

def find_clusters(df, full_roi_path):
    # List of unique NIfTI names
    unique_nii_names = df['nii_name'].unique()
    
    # Create all possible combinations of unique_nii_names, taken two at a time
    combinations_of_nii_names = list(combinations(unique_nii_names, 2))
    
    # Create a graph
    G = nx.Graph()
    
    # Add nodes and edges to the graph based on adjacency
    for nii_name1, nii_name2 in combinations_of_nii_names:
        full_path1 = full_roi_path + nii_name1 + '.nii'
        full_path2 = full_roi_path + nii_name2 + '.nii'
    
        binary_data1 = load_nii_as_binary(full_path1)
        binary_data2 = load_nii_as_binary(full_path2)
    
        if are_adjacent(binary_data1, binary_data2):
            #print(f"The ROIs {nii_name1} and {nii_name2} are adjacent.")
            G.add_edge(nii_name1, nii_name2)
    
    # Find connected components, i.e., clusters
    clusters = list(nx.connected_components(G))
        
    # Create a set of all unique nii_names
    all_nii_names = set(unique_nii_names)
    # Create a set of all nii_names that are part of a cluster
    nii_names_in_clusters = set().union(*clusters)
    # Find the difference between the two sets to get nii_names not in any cluster
    nii_names_not_in_clusters = all_nii_names - nii_names_in_clusters
    
    # Create a dictionary to map each nii_name to a cluster code
    nii_name_to_cluster_code = {}
    
    # Iterate through the clusters and assign cluster codes
    for i, cluster in enumerate(clusters, start=1):
        for nii_name in cluster:
            nii_name_to_cluster_code[nii_name] = f'cluster{i}'
    
   # Counter for nii_names not in clusters
    no_cluster_counter = 1
    
    # Assign 'no cluster' to nii_names that are not in any cluster
    for nii_name in unique_nii_names:
        if nii_name not in nii_name_to_cluster_code:
            nii_name_to_cluster_code[nii_name] = f'no cluster{no_cluster_counter}'
            no_cluster_counter += 1
            
    # After finding the clusters apply them to the original DataFrame
    for nii_name, cluster_code in nii_name_to_cluster_code.items():
        df.loc[df['nii_name'] == nii_name, 'cluster_code'] = cluster_code
        
    return df
