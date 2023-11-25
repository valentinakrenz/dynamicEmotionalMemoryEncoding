%% TRANSFORM TRIALWISE BETAS TO TRIALWISE T-IMAGED

function create_trialwise_t_values(sj, dataFolder) 

clear matlabbatch;

%% initialize spm batch
% spm needs to be in path
% addpath 'PATH_TO_SPM'

spm('defaults','fmri');
spm_jobman('initcfg');
clear matlabbatch;

sj

%% define input data
SubFolder = '\firstlevel\trialwiseGLM\';

VPFolder = dir(fullfile(dataFolder, sprintf('sj-%03d', sj)));

matlabbatch{1}.spm.stats.con.spmmat = {fullfile(dataFolder, VPFolder.name, SubFolder, 'SPM.mat')};

for tr = 1:180 % number of trials
    matlabbatch{1}.spm.stats.con.consess{tr}.tcon.name = ['trial', num2str(tr)];
    matlabbatch{1}.spm.stats.con.consess{tr}.tcon.weights = [zeros(1,tr-1),1];
    matlabbatch{1}.spm.stats.con.consess{tr}.tcon.sessrep = 'none';
end

matlabbatch{1}.spm.stats.con.delete = 1;

spm_jobman('run', matlabbatch)
    
end


