Executable = /mnt/lustre/Tools/bin/gcta64

Arguments = --reml  --grm ../GLACIERV2.HRCimputed.plink2_relat_cross_rm_025  --pheno ../final_persistant_phenotype_dataset_2908.csv --out ../continous_var_explained

Log = /mnt/lustre/Home/jerneja_m/Condor_files/GCTA_command3.log

Output = /mnt/lustre/Home/jerneja_m/Condor_files/GCTA_command3.out

Error = /mnt/lustre/Home/jerneja_m/Condor_files/GCTA_command3.err

request_memory = 500000.

notify_user = jerneja.mislej@med.lu.se

notification = Always

Queue
