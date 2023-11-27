# RECURRENT EMOTIONAL MEMORY ENCODING
code for the manuscript 
"**Memory boost for recurring emotional events is driven by initial amygdala response and stable neocortical patterns across encoding repetitions**" 
by Valentina Krenz, Arjen Alink, Benno Roozendaal, Tobias Sommer, and Lars Schwabe

---

### raw and processed data are stored at University Hamburg's research data repository
http://doi.org/10.25592/uhhfdm.13783

---
### parameter extraction
extraction and RSAs
to extract univariate parameters and run RSAs, the beta images as *fMRI_processed-sj-###.7z* need to be unzipped into a common parent file (e.g. *data/*) and transformed from 4D to 3D images
**fmri_paramExtract/extract_trialwise_betaImages_from4Dgz.ipynb** shows how to do this using Python

**fmri_paramExtract/run_in_parfor.m** runs the following functions parallelized over subjects
- **fmri_paramExtract/trialwise_parameterextraction.m** for univaraite parameterextraction
- **fmri_paramExtract/create_trialwise_t_values.m** to transform beta-images into *t*-images
- **fmri_paramExtract/RSA_EES.m** runs RSA analyses on these *t*-images

results are stored at the repository as *fmri_paramExtr.7z* and are used for statistical analyses in R

---

### statistical analyses
**stat_analysis/EncMem_stat_analyses.Rmd** runs all statistical analyses in R and some fMRI applications in Python and exports nifti images for plotting in BrainNet Viewer.

*stat_analysis/my_functions* includes R and Python functions that are used by this script.

The folder *results* and its subfolder *results/R_results* include dataframes which can be loaded inside EncMem_stat_analyses.Rmd to reproduce all results and figures without having to run more extensive, parallelized computations.

---

### regions of interest
- anatomical masks for the amygdala were derived from the Harvard-Oxford-Atlas as included in the FMRIB Software Library (https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FSL) with a probability threshold of 50%
- anatomical masks of the anterior, mid and posterior hippocampus (left and right) were derived using the WFU pick-atlas (www.rad.wfubmc.edu/fmri)
- for analyses across the neocortex, we used a parcellation based scheme by Schaefer et al. 2018, as described in https://doi.org/10.1093/cercor/bhx179 
