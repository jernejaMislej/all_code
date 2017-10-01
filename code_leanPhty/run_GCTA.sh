Executable = /mnt/lustre/Tools/bin/gcta64

Arguments = --bfile /mnt/lustre/Active_Projects/Multimodality/Private/heritability/genetics/GLACIERV2.HRCimputed.plink2 --autosome --make-grm  --out ../GLACIERV2.HRCimputed.plink2_relat_cross

Log = /mnt/lustre/Home/jerneja_m/Condor_files/GCTA_command1.log

Output = /mnt/lustre/Home/jerneja_m/Condor_files/GCTA_command1.out

Error = /mnt/lustre/Home/jerneja_m/Condor_files/GCTA_command1.err

request_memory = 1000000.

notify_user = jerneja.mislej@med.lu.se

notification = Always

Queue
