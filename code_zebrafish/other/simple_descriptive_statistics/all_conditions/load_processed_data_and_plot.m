%HEALTHY CONTROL

%dark
AllDarkLengths=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/DarkLengthsData');
AllDarkmeanLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/DarkmeanLengthOfBoutData');
AllDarkNumberOfBouts=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/DarkNumberOfBoutsData');
AllDarksdLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/DarksdLengthOfBoutData');

%light
AllLightLengths=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/LightLengthsData');
AllLightmeanLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/LightmeanLengthOfBoutData');
AllLightNumberOfBouts=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/LightNumberOfBoutsData');
AllLightsdLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/LightsdLengthOfBoutData');

%light-dark
AllLightDarkLengths=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/LightDarkLengthsData');
AllLightDarkmeanLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/LightDarkmeanLengthOfBoutData');
AllLightDarkNumberOfBouts=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/LightDarkNumberOfBoutsData');
AllLightDarksdLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/LightDarksdLengthOfBoutData');

%DISEASE CONTROL

%APO low
AllLightApoLowLengths=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/LightApoLowLengthsData');
AllLightApoLowmeanLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/LightApoLowmeanLengthOfBoutData');
AllLightApoLowNumberOfBouts=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/LightApoLowNumberOfBoutsData');
AllLightApoLowsdLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/LightApoLowsdLengthOfBoutData');

%APO high
AllDarkApoHighLengths=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/AllDarkApoHighLengthsData');
AllDarkApoHighmeanLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/AllDarkApoHighmeanLengthOfBoutData');
AllDarkApoHighNumberOfBouts=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/AllDarkApoHighNumberOfBoutsData');
AllDarkApoHighsdLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/AllDarkApoHighsdLengthOfBoutData');

%PTZ
AllLightDarkPTZLengths=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/AllLightDarkPTZLengthsData');
AllLightDarkPTZmeanLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/AllLightDarkPTZmeanLengthOfBoutData');
AllLightDarkPTZNumberOfBouts=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/AllLightDarkPTZNumberOfBoutsData');
AllLightDarkPTZsdLengthOfBout=csvread('../../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/AllLightDarkPTZsdLengthOfBoutData');

for subj=1:144
    
   %HEALTHY CONTROL

   %dark
   figure(1)
   p1=plot(AllDarkLengths(subj,:),'b');
   hold on
   
   figure(2)
   p2=plot(AllDarkMeanLengthOfBout(subj,:),'b');
   hold on    
   
   figure(3)
   p3=plot(AllDarkSdLengthOfBout(subj,:),'b');
   hold on  
   
   figure(4)
   p4=plot(AllDarkNumberOfBouts(subj,:),'b');
   hold on 
   
   %light
   figure(5)
   p5=plot(AllLightLengths(subj,:),'b');
   hold on
   
   figure(6)
   p6=plot(AllLightMeanLengthOfBout(subj,:),'b');
   hold on    
   
   figure(7)
   p7=plot(AllLightSdLengthOfBout(subj,:),'b');
   hold on  
   
   figure(8)
   p8=plot(AllLightNumberOfBouts(subj,:),'b');
   hold on 
   
   %light-dark
   figure(9)
   p9=plot(AllLightDarkLengths(subj,:),'b');
   hold on
   
   figure(10)
   p10=plot(AllLightDarkMeanLengthOfBout(subj,:),'b');
   hold on    
   
   figure(11)
   p11=plot(AllLightDarkSdLengthOfBout(subj,:),'b');
   hold on  
   
   figure(12)
   p12=plot(AllLightDarkNumberOfBouts(subj,:),'b');
   hold on 
   
   %DISEASE CONTROL

   %APO low
   figure(13)
   p13=plot(AllLightApoLowLengths(subj,:),'b');
   hold on
   
   figure(14)
   p14=plot(AllLightApoLowMeanLengthOfBout(subj,:),'b');
   hold on    
   
   figure(15)
   p15=plot(AllLightApoLowSdLengthOfBout(subj,:),'b');
   hold on  
   
   figure(16)
   p16=plot(AllLightApoLowNumberOfBouts(subj,:),'b');
   hold on 
   
   %APO high
   figure(17)
   p17=plot(AllDarkApoHighLengths(subj,:),'b');
   hold on
   
   figure(18)
   p18=plot(AllDarkApoHighMeanLengthOfBout(subj,:),'b');
   hold on    
   
   figure(19)
   p19=plot(AllDarkApoHighSdLengthOfBout(subj,:),'b');
   hold on  
   
   figure(20)
   p20=plot(AllDarkApoHighNumberOfBouts(subj,:),'b');
   hold on
   
   %PTZ
   figure(21)
   p21=plot(AllLightDarkPTZLengths(subj,:),'b');
   hold on
   
   figure(22)
   p22=plot(AllLightDarkPTZMeanLengthOfBout(subj,:),'b');
   hold on    
   
   figure(23)
   p23=plot(AllLightDarkPTZSdLengthOfBout(subj,:),'b');
   hold on  
   
   figure(24)
   p24=plot(AllLightDarkPTZNumberOfBouts(subj,:),'b');
   hold on 
   
end

%limits for all 4  stats
ylimLengths=max([max(max(AllLightLengths))+2*std(std(AllLightLengths)),max(max(AllDarkLengths))+2*std(std(AllDarkLengths)),max(max(AllLightDarkPTZLengths))...
    +2*std(std(AllLightDarkPTZLengths)),max(max(AllLightDarkLengths))+2*std(std(AllLightDarkLengths)),max(max(AllLightApoLowLengths))+2*std(std(AllLightApoLowLengths)),...
    max(max(AllDarkApoHighLengths))+2*std(std(AllDarkApoHighLengths))]);

ylimMeanLengthOfBout=max([max(max(AllLightMeanLengthOfBout))+2*std(std(AllLightMeanLengthOfBout)),max(max(AllDarkMeanLengthOfBout))+2*std(std(AllDarkMeanLengthOfBout)),max(max(AllLightDarkPTZMeanLengthOfBout))...
    +2*std(std(AllLightDarkPTZMeanLengthOfBout)),max(max(AllLightDarkMeanLengthOfBout))+2*std(std(AllLightDarkMeanLengthOfBout)),max(max(AllLightApoLowMeanLengthOfBout))+2*std(std(AllLightApoLowMeanLengthOfBout)),...
    max(max(AllDarkApoHighMeanLengthOfBout))+2*std(std(AllDarkApoHighMeanLengthOfBout))]);

ylimSdLengthOfBout=max([max(max(AllLightSdLengthOfBout))+2*std(std(AllLightSdLengthOfBout)),max(max(AllDarkSdLengthOfBout))+2*std(std(AllDarkSdLengthOfBout)),max(max(AllLightDarkPTZSdLengthOfBout))...
    +2*std(std(AllLightDarkPTZSdLengthOfBout)),max(max(AllLightDarkSdLengthOfBout))+2*std(std(AllLightDarkSdLengthOfBout)),max(max(AllLightApoLowSdLengthOfBout))+2*std(std(AllLightApoLowSdLengthOfBout)),...
    max(max(AllDarkApoHighSdLengthOfBout))+2*std(std(AllDarkApoHighSdLengthOfBout))]);

ylimNumberOfBouts=max([max(max(AllLightNumberOfBouts))+2*std(std(AllLightNumberOfBouts)),max(max(AllDarkNumberOfBouts))+2*std(std(AllDarkNumberOfBouts)),max(max(AllLightDarkPTZNumberOfBouts))...
    +2*std(std(AllLightDarkPTZNumberOfBouts)),max(max(AllLightDarkNumberOfBouts))+2*std(std(AllLightDarkNumberOfBouts)),max(max(AllLightApoLowNumberOfBouts))+2*std(std(AllLightApoLowNumberOfBouts)),...
    max(max(AllDarkApoHighNumberOfBouts))+2*std(std(AllDarkApoHighNumberOfBouts))]);

%dark
figure(1)
hold on
p1A=plot(mean(AllDarkLengths),'k','LineWidth', 3);
hold on
p1B=plot(mean(AllDarkLengths)+std(AllDarkLengths),'r--','LineWidth', 3);
hold on
plot(mean(AllDarkLengths)-std(AllDarkLengths),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('action sequence length')
ylim([0 ylimLengths])
title('Length of action sequences for 144 subjects over 65 minutes with 5 minute grouping in the dark control group')
legend([p1,p1A,p1B],'144 subjects','mean','+/-SD')

figure(2)
hold on
p2A=plot(mean(AllDarkMeanLengthOfBout),'k','LineWidth', 3);
hold on
p2B=plot(mean(AllDarkMeanLengthOfBout)+std(AllDarkMeanLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllDarkMeanLengthOfBout)-std(AllDarkMeanLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('mean length of a bout per action sequence')
ylim([0 ylimMeanLengthOfBout])
title('Mean length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the dark control group')
legend([p2,p2A,p2B],'144 subjects','mean','+/-SD')

figure(3)
hold on
p3A=plot(mean(AllDarkSdLengthOfBout),'k','LineWidth', 3);
hold on
p3B=plot(mean(AllDarkSdLengthOfBout)+std(AllDarkSdLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllDarkSdLengthOfBout)-std(AllDarkSdLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('SD of bout lengths per action sequence')
ylim([0 ylimSdLengthOfBout])
title('SD of length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the dark control group')
legend([p3,p3A,p3B],'144 subjects','mean','+/-SD')

figure(4)
hold on
p4A=plot(mean(AllDarkNumberOfBouts),'k','LineWidth', 3);
hold on
p4B=plot(mean(AllDarkNumberOfBouts)+std(AllDarkNumberOfBouts),'r--','LineWidth', 3);
hold on
plot(mean(AllDarkNumberOfBouts)-std(AllDarkNumberOfBouts),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('number of bouts per action sequence')
ylim([0 ylimNumberOfBouts])
title('Number of bouts for 144 subjects over 65 minutes with 5 minute grouping in the dark control group')
legend([p4,p4A,p4B],'144 subjects','mean','+/-SD')


%light
figure(5)
hold on
p5A=plot(mean(AllLightLengths),'k','LineWidth', 3);
hold on
p5B=plot(mean(AllLightLengths)+std(AllLightLengths),'r--','LineWidth', 3);
hold on
plot(mean(AllLightLengths)-std(AllLightLengths),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('action sequence length')
ylim([0 ylimLengths])
title('Length of action sequences for 144 subjects over 65 minutes with 5 minute grouping in the Light control group')
legend([p5,p5A,p5B],'144 subjects','mean','+/-SD')

figure(6)
hold on
p6A=plot(mean(AllLightMeanLengthOfBout),'k','LineWidth', 3);
hold on
p6B=plot(mean(AllLightMeanLengthOfBout)+std(AllLightMeanLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllLightMeanLengthOfBout)-std(AllLightMeanLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('mean length of a bout per action sequence')
ylim([0 ylimMeanLengthOfBout])
title('Mean length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the Light control group')
legend([p6,p6A,p6B],'144 subjects','mean','+/-SD')

figure(7)
hold on
p7A=plot(mean(AllLightSdLengthOfBout),'k','LineWidth', 3);
hold on
p7B=plot(mean(AllLightSdLengthOfBout)+std(AllLightSdLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllLightSdLengthOfBout)-std(AllLightSdLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('SD of bout lengths per action sequence')
ylim([0 ylimSdLengthOfBout])
title('SD of length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the Light control group')
legend([p7,p7A,p7B],'144 subjects','mean','+/-SD')

figure(8)
hold on
p8A=plot(mean(AllLightNumberOfBouts),'k','LineWidth', 3);
hold on
p8B=plot(mean(AllLightNumberOfBouts)+std(AllLightNumberOfBouts),'r--','LineWidth', 3);
hold on
plot(mean(AllLightNumberOfBouts)-std(AllLightNumberOfBouts),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('number of bouts per action sequence')
ylim([0 ylimNumberOfBouts])
title('Number of bouts for 144 subjects over 65 minutes with 5 minute grouping in the Light control group')
legend([p8,p8,p8B],'144 subjects','mean','+/-SD')

%light-dark
figure(9)
hold on
p9A=plot(mean(AllLightDarkLengths),'k','LineWidth', 3);
hold on
p9B=plot(mean(AllLightDarkLengths)+std(AllLightDarkLengths),'r--','LineWidth', 3);
hold on
plot(mean(AllLightDarkLengths)-std(AllLightDarkLengths),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('action sequence length')
ylim([0 ylimLengths])
title('Length of action sequences for 144 subjects over 65 minutes with 5 minute grouping in the LightDark control group')
legend([p9,p9A,p9B],'144 subjects','mean','+/-SD')

figure(10)
hold on
p10A=plot(mean(AllLightDarkMeanLengthOfBout),'k','LineWidth', 3);
hold on
p10B=plot(mean(AllLightDarkMeanLengthOfBout)+std(AllLightDarkMeanLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllLightDarkMeanLengthOfBout)-std(AllLightDarkMeanLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('mean length of a bout per action sequence')
ylim([0 ylimMeanLengthOfBout])
title('Mean length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the LightDark control group')
legend([p10,p10A,p10B],'144 subjects','mean','+/-SD')

figure(11)
hold on
p11A=plot(mean(AllLightDarkSdLengthOfBout),'k','LineWidth', 3);
hold on
p11B=plot(mean(AllLightDarkSdLengthOfBout)+std(AllLightDarkSdLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllLightDarkSdLengthOfBout)-std(AllLightDarkSdLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('SD of bout lengths per action sequence')
ylim([0 ylimSdLengthOfBout])
title('SD of length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the LightDark control group')
legend([p11,p11A,p11B],'144 subjects','mean','+/-SD')

figure(12)
hold on
p12A=plot(mean(AllLightDarkNumberOfBouts),'k','LineWidth', 3);
hold on
p12B=plot(mean(AllLightDarkNumberOfBouts)+std(AllLightDarkNumberOfBouts),'r--','LineWidth', 3);
hold on
plot(mean(AllLightDarkNumberOfBouts)-std(AllLightDarkNumberOfBouts),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('number of bouts per action sequence')
ylim([0 ylimNumberOfBouts])
title('Number of bouts for 144 subjects over 65 minutes with 5 minute grouping in the LightDark control group')
legend([p12,p12A,p12B],'144 subjects','mean','+/-SD')

%APO low
figure(13)
hold on
p13A=plot(mean(AllLightApoLowLengths),'k','LineWidth', 3);
hold on
p13B=plot(mean(AllLightApoLowLengths)+std(AllLightApoLowLengths),'r--','LineWidth', 3);
hold on
plot(mean(AllLightApoLowLengths)-std(AllLightApoLowLengths),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('action sequence length')
ylim([0 ylimLengths])
title('Length of action sequences for 144 subjects over 65 minutes with 5 minute grouping in the LightApoLow control group')
legend([p13,p13A,p13B],'144 subjects','mean','+/-SD')

figure(14)
hold on
p14A=plot(mean(AllLightApoLowMeanLengthOfBout),'k','LineWidth', 3);
hold on
p14B=plot(mean(AllLightApoLowMeanLengthOfBout)+std(AllLightApoLowMeanLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllLightApoLowMeanLengthOfBout)-std(AllLightApoLowMeanLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('mean length of a bout per action sequence')
ylim([0 ylimMeanLengthOfBout])
title('Mean length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the LightApoLow control group')
legend([p14,p14A,p14B],'144 subjects','mean','+/-SD')

figure(15)
hold on
p15A=plot(mean(AllLightApoLowSdLengthOfBout),'k','LineWidth', 3);
hold on
p15B=plot(mean(AllLightApoLowSdLengthOfBout)+std(AllLightApoLowSdLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllLightApoLowSdLengthOfBout)-std(AllLightApoLowSdLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('SD of bout lengths per action sequence')
ylim([0 ylimSdLengthOfBout])
title('SD of length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the LightApoLow control group')
legend([p15,p15A,p15B],'144 subjects','mean','+/-SD')

figure(16)
hold on
p16A=plot(mean(AllLightApoLowNumberOfBouts),'k','LineWidth', 3);
hold on
p16B=plot(mean(AllLightApoLowNumberOfBouts)+std(AllLightApoLowNumberOfBouts),'r--','LineWidth', 3);
hold on
plot(mean(AllLightApoLowNumberOfBouts)-std(AllLightApoLowNumberOfBouts),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('number of bouts per action sequence')
ylim([0 ylimNumberOfBouts])
title('Number of bouts for 144 subjects over 65 minutes with 5 minute grouping in the LightApoLow control group')
legend([p16,p16A,p16B],'144 subjects','mean','+/-SD')

%APO high
figure(17)
hold on
p17A=plot(mean(AllDarkApoHighLengths),'k','LineWidth', 3);
hold on
p17B=plot(mean(AllDarkApoHighLengths)+std(AllDarkApoHighLengths),'r--','LineWidth', 3);
hold on
plot(mean(AllDarkApoHighLengths)-std(AllDarkApoHighLengths),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('action sequence length')
ylim([0 ylimLengths])
title('Length of action sequences for 144 subjects over 65 minutes with 5 minute grouping in the DarkApoHigh control group')
legend([p17,p17A,p17B],'144 subjects','mean','+/-SD')

figure(18)
hold on
p18A=plot(mean(AllDarkApoHighMeanLengthOfBout),'k','LineWidth', 3);
hold on
p18B=plot(mean(AllDarkApoHighMeanLengthOfBout)+std(AllDarkApoHighMeanLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllDarkApoHighMeanLengthOfBout)-std(AllDarkApoHighMeanLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('mean length of a bout per action sequence')
ylim([0 ylimMeanLengthOfBout])
title('Mean length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the DarkApoHigh control group')
legend([p18,p18A,p18B],'144 subjects','mean','+/-SD')

figure(19)
hold on
p19A=plot(mean(AllDarkApoHighSdLengthOfBout),'k','LineWidth', 3);
hold on
p19B=plot(mean(AllDarkApoHighSdLengthOfBout)+std(AllDarkApoHighSdLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllDarkApoHighSdLengthOfBout)-std(AllDarkApoHighSdLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('SD of bout lengths per action sequence')
ylim([0 ylimSdLengthOfBout])
title('SD of length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the DarkApoHigh control group')
legend([p19,p19A,p19B],'144 subjects','mean','+/-SD')

figure(20)
hold on
p20A=plot(mean(AllDarkApoHighNumberOfBouts),'k','LineWidth', 3);
hold on
p20B=plot(mean(AllDarkApoHighNumberOfBouts)+std(AllDarkApoHighNumberOfBouts),'r--','LineWidth', 3);
hold on
plot(mean(AllDarkApoHighNumberOfBouts)-std(AllDarkApoHighNumberOfBouts),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('number of bouts per action sequence')
ylim([0 ylimNumberOfBouts])
title('Number of bouts for 144 subjects over 65 minutes with 5 minute grouping in the DarkApoHigh control group')
legend([p20,p20A,p20B],'144 subjects','mean','+/-SD')

%PTZ
figure(21)
hold on
p21A=plot(mean(AllLightDarkPTZLengths),'k','LineWidth', 3);
hold on
p21B=plot(mean(AllLightDarkPTZLengths)+std(AllLightDarkPTZLengths),'r--','LineWidth', 3);
hold on
plot(mean(AllLightDarkPTZLengths)-std(AllLightDarkPTZLengths),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('action sequence length')
ylim([0 ylimLengths])
title('Length of action sequences for 144 subjects over 65 minutes with 5 minute grouping in the LightDarkPTZ control group')
legend([p21,p21A,p21B],'144 subjects','mean','+/-SD')

figure(22)
hold on
p22A=plot(mean(AllLightDarkPTZMeanLengthOfBout),'k','LineWidth', 3);
hold on
p22B=plot(mean(AllLightDarkPTZMeanLengthOfBout)+std(AllLightDarkPTZMeanLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllLightDarkPTZMeanLengthOfBout)-std(AllLightDarkPTZMeanLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('mean length of a bout per action sequence')
ylim([0 ylimMeanLengthOfBout])
title('Mean length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the LightDarkPTZ control group')
legend([p22,p22A,p22B],'144 subjects','mean','+/-SD')

figure(23)
hold on
p23A=plot(mean(AllLightDarkPTZSdLengthOfBout),'k','LineWidth', 3);
hold on
p23B=plot(mean(AllLightDarkPTZSdLengthOfBout)+std(AllLightDarkPTZSdLengthOfBout),'r--','LineWidth', 3);
hold on
plot(mean(AllLightDarkPTZSdLengthOfBout)-std(AllLightDarkPTZSdLengthOfBout),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('SD of bout lengths per action sequence')
ylim([0 ylimSdLengthOfBout])
title('SD of length of a bout for 144 subjects over 65 minutes with 5 minute grouping in the LightDarkPTZ control group')
legend([p23,p23A,p23B],'144 subjects','mean','+/-SD')

figure(24)
hold on
p24A=plot(mean(AllLightDarkPTZNumberOfBouts),'k','LineWidth', 3);
hold on
p24B=plot(mean(AllLightDarkPTZNumberOfBouts)+std(AllLightDarkPTZNumberOfBouts),'r--','LineWidth', 3);
hold on
plot(mean(AllLightDarkPTZNumberOfBouts)-std(AllLightDarkPTZNumberOfBouts),'r--','LineWidth', 3)
set(gca,'XTick',[1:14])
xlabel('5 minutes time frames')
ylabel('number of bouts per action sequence')
ylim([0 ylimNumberOfBouts])
title('Number of bouts for 144 subjects over 65 minutes with 5 minute grouping in the LightDarkPTZ control group')
legend([p24,p24A,p24B],'144 subjects','mean','+/-SD')



