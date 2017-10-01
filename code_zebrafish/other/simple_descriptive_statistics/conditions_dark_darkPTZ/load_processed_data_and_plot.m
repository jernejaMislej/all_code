DarkLengths=csvread('../../../processed_data/simple_descriptive_statistics/condition_dark_darkPTZ/action_sequence_length/grouped/DarkLengthsData');
DarkmeanLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/condition_dark_darkPTZ/mean_bout_length_per_timeframe/grouped/DarkmeanLengthOfBoutData');
DarkNumberOfBouts=csvread('../../../processed_data/simple_descriptive_statistics/condition_dark_darkPTZ/number_of_bouts_per_timeframe/grouped/DarkNumberOfBoutsData');
DarksdLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/condition_dark_darkPTZ/sd_bout_length_per_timeframe/grouped/DarksdLengthOfBoutData');

PTZLengths=csvread('../../../processed_data/simple_descriptive_statistics/condition_dark_darkPTZ/action_sequence_length/grouped/PTZLengthsData');
PTZmeanLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/condition_dark_darkPTZ/mean_bout_length_per_timeframe/grouped/PTZmeanLengthOfBoutData');
PTZNumberOfBouts=csvread('../../../processed_data/simple_descriptive_statistics/condition_dark_darkPTZ/number_of_bouts_per_timeframe/grouped/PTZNumberOfBoutsData');
PTZsdLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/condition_dark_darkPTZ/sd_bout_length_per_timeframe/grouped/PTZsdLengthOfBoutData');

for subj=1:144
 
   figure(1)
   p1_dl=plot(DarkLengths(subj,:),'b');
   hold on
   
   figure(2)
   p1_mlb=plot(DarkmeanLengthOfBout(subj,:),'b');
   hold on    
   
   figure(3)
   p1_slb=plot(DarksdLengthOfBout(subj,:),'b');
   hold on  
   
   figure(4)
   p1_nb=plot(DarkNumberOfBouts(subj,:),'b');
   hold on 
   
   figure(5)
   p1_pl=plot(PTZLengths(subj,:),'b');
   hold on
   
   figure(6)
   p1_pmlb=plot(PTZmeanLengthOfBout(subj,:),'b');
   hold on    
   
   figure(7)
   p1_pslb=plot(PTZsdLengthOfBout(subj,:),'b');
   hold on  
   
   figure(8)
   p1_pnb=plot(PTZNumberOfBouts(subj,:),'b');
   hold on
   
end

ylim1=max(max(max(DarkLengths))+2*std(std(DarkLengths)),max(max(PTZLengths))+2*std(std(PTZLengths)));
figure(1)
hold on
p2_dl=plot(mean(DarkLengths),'k','LineWidth', 3);
hold on
p3_dl=plot(mean(DarkLengths)+std(DarkLengths),'r--','LineWidth', 3);
hold on
plot(mean(DarkLengths)-std(DarkLengths),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('action sequence length')
ylim([0 ylim1])
title('Length of action sequences for 144 subjects over 65 minutes with 5 minute grouping in the dark control group')
legend([p1_dl,p2_dl,p3_dl],'144 subjects','mean','+/-SD')

ylim2=max(max(max(DarkmeanLengthOfBout))+2*std(std(DarkmeanLengthOfBout)),max(max(PTZmeanLengthOfBout))+2*std(std(PTZmeanLengthOfBout)));
figure(2)
hold on
p2_mlb=plot(mean(DarkmeanLengthOfBout),'k','LineWidth', 3);
hold on
p3_mlb=plot(mean(DarkmeanLengthOfBout)+std(DarkmeanLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(DarkmeanLengthOfBout)-std(DarkmeanLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('mean length of a bout per action sequence')
ylim([0 ylim2])
title('Mean length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the dark control group')
legend([p1_mlb,p2_mlb,p3_mlb],'144 subjects','mean','+/-SD')

ylim3=max(max(max(DarksdLengthOfBout))+2*std(std(DarksdLengthOfBout)),max(max(PTZsdLengthOfBout))+2*std(std(PTZsdLengthOfBout)));
figure(3)
hold on
p2_slb=plot(mean(DarksdLengthOfBout),'k','LineWidth', 3);
hold on
p3_slb=plot(mean(DarksdLengthOfBout)+std(DarksdLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(DarksdLengthOfBout)-std(DarksdLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('SD of bout lengths per action sequence')
ylim([0 ylim3])
title('SD of length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the dark control group')
legend([p1_slb,p2_slb,p3_slb],'144 subjects','mean','+/-SD')

ylim4=max(max(max(DarkNumberOfBouts))+2*std(std(PTZNumberOfBouts)),max(max(PTZNumberOfBouts))+2*std(std(PTZNumberOfBouts)));
figure(4)
hold on
p2_nb=plot(mean(DarkNumberOfBouts),'k','LineWidth', 3);
hold on
p3_nb=plot(mean(DarkNumberOfBouts)+std(DarkNumberOfBouts),'r--','LineWidth', 3);
hold on
plot(mean(DarkNumberOfBouts)-std(DarkNumberOfBouts),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('number of bouts per action sequence')
ylim([0 ylim4])
title('Number of bouts for 144 subjects over 65 minutes with 5 minute grouping in the dark control group')
legend([p1_nb,p2_nb,p3_nb],'144 subjects','mean','+/-SD')

figure(5)
hold on
p2_pl=plot(mean(PTZLengths),'k','LineWidth', 3);
hold on
p3_pl=plot(mean(PTZLengths)+std(PTZLengths),'r--','LineWidth', 3);
hold on
plot(mean(PTZLengths)-std(PTZLengths),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('action sequence length')
ylim([0 ylim1])
title('Length of action sequences for 144 subjects over 65 minutes with 5 minute grouping in the PTZ control group')
legend([p1_pl,p2_pl,p3_pl],'144 subjects','mean','+/-SD')


figure(6)
hold on
p2_pmlb=plot(mean(PTZmeanLengthOfBout),'k','LineWidth', 3);
hold on
p3_pmlb=plot(mean(PTZmeanLengthOfBout)+std(PTZmeanLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(PTZmeanLengthOfBout)-std(PTZmeanLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('mean length of a bout per action sequence')
ylim([0 ylim2])
title('Mean length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the PTZ control group')
legend([p1_pmlb,p2_pmlb,p3_pmlb],'144 subjects','mean','+/-SD')

figure(7)
hold on
p2_pslb=plot(mean(PTZsdLengthOfBout),'k','LineWidth', 3);
hold on
p3_pslb=plot(mean(PTZsdLengthOfBout)+std(PTZsdLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(PTZsdLengthOfBout)-std(PTZsdLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('SD of bout lengths per action sequence')
ylim([0 ylim3])
title('SD of length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the PTZ control group')
legend([p1_pslb,p2_pslb,p3_pslb],'144 subjects','mean','+/-SD')

figure(8)
hold on
p2_pnb=plot(mean(PTZNumberOfBouts),'k','LineWidth', 3);
hold on
p3_pnb=plot(mean(PTZNumberOfBouts)+std(PTZNumberOfBouts),'r--','LineWidth', 3);
hold on
plot(mean(PTZNumberOfBouts)-std(PTZNumberOfBouts),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('number of bouts per action sequence')
ylim([0 ylim4])
title('Number of bouts for 144 subjects over 65 minutes with 5 minute grouping in the PTZ control group')
legend([p1_pnb,p2_pnb,p3_pnb],'144 subjects','mean','+/-SD')

