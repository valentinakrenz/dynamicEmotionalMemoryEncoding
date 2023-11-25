%% COMPUTE ENCODING-PATTERN SIMILARITY PER ROI
% drafted by Arjen Alink adapted by Valentina Krenz

function [EESTable] = RSA_EES(sj, ROIdir, dataFolder)

disp(['running subject number: ', num2str(sj)]);

%% define input data
SubFolder = '\firstlevel\trialwiseGLM\';
SJfolder = dir(fullfile(dataFolder, sprintf('NORMEM_%03d_*', sj))); % analyze sj that is defined by RunSL_inParfor
betaPath=[dataFolder SJfolder.name SubFolder]; 
betaFiles=dir([betaPath, 'spmT*.nii']); % read in files that start with spmT and end with .nii
nBetas = 180; 
betaFiles = betaFiles(1:nBetas);

%% READ IN PATHS AND FILES

for b=1:length(betaFiles)%nBetas
    betaDat=load_nii([betaPath,betaFiles(b).name]);
    if b==1
        BetaMaps=zeros([numel(betaFiles),size(betaDat.img)],'single'); % if b==1 BetaMaps is initialized
    end
    BetaMaps(b,:,:,:) = betaDat.img; %assign betaDat to the bth element of the BetaMaps variable
end

%% DEFINE COMPARISONS AND INDICES

emotions = {'negative', 'neutral'};
emo_itemType = {'Neg','Neu'};
% emotion inds for emotion loop
NegNeu_Inds = [1:30, 61:90, 121:150; 31:60, 91:120, 151:180]; % first row is neg, second row is neutral 

% define indices for each run
Enc1_Inds = 1:30; 
Enc2_Inds = 31:60;
Enc3_Inds = 61:90;

% save run indices for loop
enc_runs = {Enc1_Inds, Enc2_Inds, Enc3_Inds};

% matrix indicating the image number in item specific comparison
ImageInds = NaN(30);
ImageInds(1:30+1:end) = 1:30;

%% PREPARE RSM

RoiNames=dir([ROIdir  'r*.nii']);
nRois=numel(RoiNames);
ROInames=cell(nRois,1);

% read in roi names
for r=1:nRois
    ROInames{r}=RoiNames(r).name(1:end-4);
end

% prepare RSMs
for r=1:nRois
    ROI=load_nii([ROIdir RoiNames(r).name]);
    ROIdata.([RoiNames(r).name(1:end-4) '_Inds'])=find(ROI.img);
    ROIdata.([RoiNames(r).name(1:end-4) '_RSMs'])=cell(2,3,2); % Emo x run x mem dimensions
    ROIdataMemInd.([RoiNames(r).name(1:end-4) '_Inds'])=find(ROI.img);
    ROIdataMemInd.([RoiNames(r).name(1:end-4) '_RSMs'])=cell(2,3); % Emo x run
    ROInames{r}=RoiNames(r).name(1:end-4);
end

%% COMPUTE RSM BETWEEN ALL TRIALS AND FOR EACH ROI

for r=1:nRois
    RoiBetas=BetaMaps(:,ROIdata.([RoiNames(r).name(1:end-4) '_Inds'])); %extract beta values of the voxels that fall within the current ROI for all beta images
    % remove voxels within the current ROI that have NaN values
    zeroVoxels = all(RoiBetas == 0, 1); % Find columns (voxels) with all zeros
    RoiBetas(:, zeroVoxels) = [];      % drop those columns before computing RSMs
    ROIdata.([RoiNames(r).name(1:end-4) '_RSMs'])=corr(squeeze(RoiBetas)'); %correlates beta values across images for each voxel within the ROI
end

%% SAVE FISHER Z-TRANSFORMED ITEM-WISE EES VALUES IN OUTPUT TABLE

EESTable = table('Size',[0 6],'VariableTypes',{'string','string','string','string','string','double'},...
    'VariableNames',{'sj','ROI','EncRuns','emotion','itemType','EES'});

for roi = 1:nRois
    
    % get RSM data from current ROI
    roiRSM=ROIdata.([ROInames{roi} '_RSMs']);
    
    for e = 1:length(emotions)
        Sel_EmoInds = NegNeu_Inds(e,:); %take indices from current emo loop
        
        for i = 1:length(enc_runs)-1 %loop through runs for run comparison but skip last run for i to avoid redundancy
            Sel_RunInds1 = Sel_EmoInds(enc_runs{i}); % take only indices from current emo and current run 
            
            for j = i + 1:length(enc_runs) %loop through runs for run comparison but skip first run for j to avoid redundancy
                Sel_RunInds2 = Sel_EmoInds(enc_runs{j}); % take only indices from current emo and current run 
                currRSM = roiRSM(Sel_RunInds1, Sel_RunInds2); % reduce RSM to current EncRun-EncRun comparison
            
                for stim = 1:30 %loop through item number
                    corr_value = atanh(currRSM(ImageInds == stim)); %take only EES for current item and Fisher-transform
                    % preparing temporary output table
                    tempRow = {string(['sj' sprintf('%03d', sj)]), string(ROInames{roi}),string(['Enc',num2str(i),'Enc',num2str(j)]), emotions{e}, string([emo_itemType{e}, '_', sprintf('%02d', stim)]), corr_value};
                    % saving current data to temporary table 
                    tempTable = cell2table(tempRow, 'VariableNames',{'sj','ROI','EncRuns','emotion','itemType','EES'});   
                    % append the temporary table to the main table:
                    EESTable = [EESTable; tempTable];
                end
            end
        end
    end
end

disp(['done with subject number: ', num2str(sj)]);

end

