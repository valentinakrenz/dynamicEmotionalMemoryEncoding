%% EXTRACT TRIALWISE BETA PARAMETERS PER ROI USING SPM12
% DRAFTED BY TOBIAS SOMMER ADAPTED BY VALENTINA KRENZ
function X = trialwise_parameterextraction(sj, ROIdir, input_dir)

%% define path to betas
VPFolder = dir(strcat(input_dir,'sj-', sprintf('%03d', sj),'_*'));
vpName = VPFolder.name;
vpPath = [input_dir vpName filesep];
subFolder = 'firstlevel\trialwiseGLM\';
betas_imgs = spm_select('FPList',[vpPath subFolder],'^beta.*nii$'); % change to your naming 
nBetas = 180;

%% define ROIs
ROIdat = dir([ROIdir  '*.nii']);%load in all rois from folder
nRois = numel(ROIdat);
ROInames = cell(nRois,1);
ROIpaths = cell(nRois,1);

for r = 1:nRois
    ROInames{r} = ROIdat(r).name(1:end-4);
    ROIpaths{r} = [ROIdat(r).folder filesep ROIdat(r).name];
end

masks = ROIpaths';

disp(['starting sj' sprintf('%03d', sj)])

Z = [nan(nBetas,nRois)];

for jj = 1:size(masks,2) %loop through all ROI-masks
    
    c_mask = spm_vol(masks{jj}); % read in ROI
    disp(['starting p' sprintf('%03d', sj) ' ' ROInames{jj}])
    
    Y = [];
    
    for j = 1:nBetas%180%loop through first 180 betas of subj
        c_vol = spm_vol(betas_imgs(j,:));
        [T, T_XYZmm]            = spm_read_vols(c_vol);
        [R,C,P]                 = ndgrid(1:c_vol.dim(1),1:c_vol.dim(2),1:c_vol.dim(3));
        T_XYZ                   = [R(:)';C(:)';P(:)'];
        mXYZ                    = inv(c_mask.mat)*[T_XYZmm;ones(1,size(T_XYZmm,2))];
        tmpQ                    = spm_sample_vol(c_mask,mXYZ(1,:),mXYZ(2,:),mXYZ(3,:),0);
        tmpQ(~isfinite(tmpQ)) 	= 0;
        Q                       = find(tmpQ);
        Y                       = [Y nanmean(spm_get_data(c_vol,T_XYZ(:,Q)),2)];
    end
    
    Z(:,jj)= Y';
    
end

%% save results in one table per subj with one row per beta/ trial
X = array2table(Z);

% ROInames as column header
X.Properties.VariableNames = ROInames';
X.sj = repmat(sj, size(X, 1), 1);
X.nBeta = (1:nBetas)';
toc

end

