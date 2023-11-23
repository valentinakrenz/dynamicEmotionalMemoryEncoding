# dynamicEmotionalMemoryEncoding
code for the manuscript "Memory boost for recurring emotional events is driven by initial amygdala response and stable neocortical patterns across encoding repetitions" by Valentina Krenz, Arjen Alink, Benno Roozendaal, Tobias Sommer, and Lars Schwabe.

### raw and processed data are stored at OSF
https://osf.io/8eztf/?view_only=b2f5b2bda8734e299d0e25b36cc23651

### univariate parameterextraction and RSAs
to extract univariate parameters and run RSAs, the beta images in data/processed/ need to be unzipped and transformed from 4D to 3D images
extract_trialwise_betaImages_from4Dgz.ipynb shows how to do this using Python

run_in_parfor.m runs the following functions parallelized over subjects
- trialwise_parameterextraction.m for univaraite parameterextraction
- create_trialwise_t_values.m to transform beta-images into t-images
- RSA_EES.m runs RSA analyses on these t-images

results are stored in data/neuro/extracted and are used for statistical analyses in R

### statistical analyses
analyses.Rmd runs all statistical analyses in R and some fMRI applications in Python and exports nifti images for plotting in BrainNet Viewer
code/my_functions includes R and Python functions that are used by this script

### regions of interest
- anatomical masks for the amygdala were derived from the Harvard-Oxford-Atlas as included in the FMRIB Software Library (https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FSL) with a probability threshold of 50%
- anatomical masks of the anterior, mid and posterior hippocampus (left and right) were derived using the WFU pick-atlas (www.rad.wfubmc.edu/fmri)
- for analyses across the neocortex, we used a parcellation based scheme by Schaefer et al. 2018, as described in https://doi.org/10.1093/cercor/bhx179 
