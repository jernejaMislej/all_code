Executable = /mnt/lustre/Tools/bin/gcta64

Arguments = --grm ../GLACIERV2.HRCimputed.plink2_relat_cross  --grm-cutoff 0.025 --make-grm  --out ../GLACIERV2.HRCimputed.plink2_relat_cross_rm_025

Log = /mnt/lustre/Home/jerneja_m/Condor_files/GCTA_command2.log

Output = /mnt/lustre/Home/jerneja_m/Condor_files/GCTA_command2.out

Error = /mnt/lustre/Home/jerneja_m/Condor_files/GCTA_command2.err

request_memory = 500000.

notify_user = jerneja.mislej@med.lu.se

notification = Always

Queue
