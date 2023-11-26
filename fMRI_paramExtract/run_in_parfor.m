%% 2023 Valentina Krenz
% extract and prepare neuro data for analyses in R

%% SETTINGS
clear all
addpath YOURPATH\MATLAB\spm12 % add spm12 to path
addpath YOURPATH\NiftiTools % add nifti tools to path


% % define output path and name
outputDir = 'YOUROUTPUTPATH/data/extracted'; %
inputDir = 'YOURPATH/data/neuro/raw';
ROIdir = 'YOURROIPATH';
% Define your settings
sub_all = 1:104; % write down all your sjs here
sub_excluded = 55; % write down the sjs that you don't want to analyze
sub = setdiff(sub_all, sub_excluded); % analyze only non-excluded sjs
nSubs = length(sub); % number of sjs

% % prepare parfor
% % adapt parpool workers depending on number of subjects % using all workers
% % is not recommended
if length(sub) < 80
    workers = length(sub);
else
    workers = 70;
end

%% RUN TRIALWISE PARAMETEREXTRACTION PER ROI, TRIAL AND SUBJECT

outputName = 'univ'; % will be included in output file name
% initialize output
results = cell(1, length(sub)); %initialize cell in which output will be stored

% run function for all defined subs
% change to for, if you can't use parfor loop
delete(gcp('nocreate'))
parpool(workers)
parfor s = 1:length(sub)
    sj = sub(s);
    results{s}= trialwise_parameterextraction(sj, ROIdir, inputDir); % define the function you want to run
end

% prepare and save output
% combine results into a single table
combinedTable = vertcat(results{:});
% add emotion
combinedTable.emotion = repmat({'Neg'}, size(combinedTable, 1), 1);
combinedTable.emotion(combinedTable.nBeta > 30 & combinedTable.nBeta <= 60) = {'Neu'};
combinedTable.emotion(combinedTable.nBeta > 90 & combinedTable.nBeta <= 120) = {'Neu'};
combinedTable.emotion(combinedTable.nBeta > 150 & combinedTable.nBeta <= 180) = {'Neu'};
% add run
combinedTable.run = repmat({'1'}, size(combinedTable, 1), 1);
combinedTable.run(combinedTable.nBeta > 60 & combinedTable.nBeta <= 120) = {'2'};
combinedTable.run(combinedTable.nBeta > 120) = {'3'};
% convert 'sj' column to a string with leading zeros
combinedTable.sj = strcat('sj', num2str(combinedTable.sj, '%03d'));
% create the 'item_num' column within each level of 'sj', 'roi', 'emotion', and 'run'
combinedTable.item_num = mod(combinedTable.nBeta - 1, 30) + 1;
% convert item_num to string with leading zero
combinedTable.item_num = num2str(combinedTable.item_num, '%02d');
% create an 'item' column by concatenating 'emotion' and 'nBeta' with '_'
combinedTable.item = strcat(combinedTable.emotion, '_', combinedTable.item_num);
% rename 'Neg' to 'negative' and 'Neu' to 'neutral' in the 'emotion' column
combinedTable.emotion = replace(combinedTable.emotion, 'Neg', 'negative');
combinedTable.emotion = replace(combinedTable.emotion, 'Neu', 'neutral');
% reorder the table variables
combinedTable = movevars(combinedTable, 'sj', 'Before', 1);
combinedTable = movevars(combinedTable, 'emotion', 'Before', 2);
combinedTable = movevars(combinedTable, 'run', 'Before', 3);
combinedTable = movevars(combinedTable, 'item', 'Before', 4);
combinedTable = movevars(combinedTable, 'item_num', 'Before', 5);
combinedTable = movevars(combinedTable, 'nBeta', 'Before', 6);
% get the variable names for the ROI columns
roiColumns = combinedTable.Properties.VariableNames(7:end);
% reshape the table by stacking the ROI columns into 'ROI' and 'value' columns
combinedTable = stack(combinedTable, roiColumns, 'NewDataVariableName', 'value', 'IndexVariableName', 'ROI');

% Create the output directory if it doesn't exist
if ~exist(outputDir, 'dir')
    mkdir(outputDir);
end

% Save the updated table to a file using the previously defined outputDir and outputName
outputFile = fullfile(outputDir, [outputName '_df.csv']);
% If you want to save as CSV
writetable(combinedTable, outputFile, 'Delimiter', ',');

%% PREPARE T-IMAGES 

delete(gcp('nocreate'))
parpool(workers)
% write t-images
parfor s = 1:nSubs
    sj = sub(s);
    create_trialwise_t_values(sj, inputDir) 
end
 
%% COMPUTE ENCODING PATTERN SIMILARITY

 % define output name
 output_name = 'EES';
 % initialize results table
 EESTables = cell(nSubs,1); % prepare cell to store output tables
 
 % change to for, if you can't use parfor loop
 delete(gcp('nocreate'))
 parpool(workers)
 parfor s = 1:nSubs
     sj = sub(s);
     [EESTables{s}] = RSA_EES(sj, ROIdir, dataFolder); %define the function you want to run
 end
 delete(gcp('nocreate'))
 
 % combine results of all sjs
 % Combine tables from sjs
 hugeEESTable = vertcat(EESTables{:}); % transform output tables in cell into one long table
 
 % Save as CSV
 output_file = fullfile(output_dir, [output_name '_df.csv']);
 writetable(hugeEESTable, output_file);