def add_max_overlap_columns(df):
    # Find all columns that include the word 'overlap'
    overlap_columns = [col for col in df.columns if 'overlap' in col]
    
    # Create new columns to store maximum overlap for each original overlap column
    for col in overlap_columns:
        df[f"max_{col}"] = ""
    
    # Initialize a column to store the maximum overlap across all columns
    df['overall_max_overlap'] = ""

    for index, row in df.iterrows():
        overall_max_value = 0
        overall_max_region = ""

        for col in overlap_columns:
            overlap_entries = re.findall(r'([\w\s,]+)\s\(([\d.]+)%\)', row[col])
            
            max_value = 0
            max_region = ""

            for entry in overlap_entries:
                region, overlap_str = entry
                overlap_value = float(overlap_str)

                if overlap_value > max_value:
                    max_value = overlap_value
                    max_region = region

            if max_value > 0:
                df.at[index, f"max_{col}"] = f"{max_region} ({max_value}%)"

            if max_value > overall_max_value:
                overall_max_value = max_value
                overall_max_region = max_region

        df.at[index, 'overall_max_overlap'] = f"{overall_max_region} ({overall_max_value}%)"

    return df
