#Prepare Environment-----------------------------------------------
# Code written for the Analysis of "Sleep And Light Exposure Behaviour"                                      
# Code Authors: Rafael Lazar                                                                 


rm(list=ls())
graphics.off()

#----- check if pacman is installed - if not install it
if(!require(pacman)) install.packages("pacman")

#----- use pacman function p_load to check all packages that you are using in this script
pacman::p_load(stringr, reshape2, Hmisc, tidyverse, doBy, DescTools,
               BayesFactor, effectsize, gtsummary, mctq)


# Read-in data ----------------------------------------------------------------

#Read the whole dataset
rawdata=read.csv('01_dataimport/raw_data/SpitschanSleepSurvey_DATA_2024-08-07_1229.csv')

# filter out incomplete data
complete_df <- rawdata %>% filter(!is.na(slypos_leba_50))


# reduce dataset 

# take sample of the full dataset
#uncomment this later
set.seed(123)
complete_df <- complete_df %>% sample_n(50)



# set data for analysis to sample data (30 obs.)
data <- complete_df

rm(complete_df, rawdata)


# Data cleaning-----------------------------------------------------------------


## exclusion due to attention check items  -------------------------------------

#filter for  only correct attention check items --> n=27/30
data <- data %>% filter (
  slypos_demographics_attentioncheck==9,
  slypos_ase_attentioncheck==4,
  slypos_mctq_attentioncheck==19 | slypos_mctq_attentioncheck_2==19,
  slypos_leba_attentioncheck==0
)

## exclusion due to nonsensical replies-----------------------------------------
# sleep time later than bedtime in mctq etc.









# Data labelling-----------------------------------------------------------------
#Setting Labels 
#CAVE: Other packages might mask the label function by hmisc.

Leba_labels<- c(" lights after waking up", 
                "open curtains after waking up.",
                "phone screen  after waking up",
                "alarm dawn simulation light",
                "breakfast 3 meters from window",
                "breakfast brightly lit room",
                "walk/exercise outside 2h after waking up",
                "30 min or less per day outside",
                "30 min to 1h per day outside",
                "1 to 3h per day outside",
                "> 3h per day outside",
                "as much time outside as possible",
                "sunglasses outside in bright daylight",
                "cap  outside in bright daylight",
                "shade outside in bright daylight",
                "blueblockers indoors during the day",
                "blueblockers outdoors during the day",
                "light therapy  white light box",
                "light therapy  blue light box",
                "light therapy light visor",
                "light therapy other light device",
                "most of  daytime in bright environment",
                "close  curtains during day",
                "most of indoor time within 3m from  window",
                "desk lamp when focused work",
                "ceiling room light when light outside",
                "mobile phone 1h before sleep",
                "computer 1h before sleep",
                " TV within 1h before sleep",
                "smartwatch 1h before sleep",
                "dim room light 1h before sleep",
                "dim phone 1h before sleep",
                "dim computer 1h before sleep.",
                "blue-filter app on phone 1h before sleep",
                "blue-filter app on computer 1h before sleep",
                "blueblocker 1h before sleep",
                "light in sleep environment while sleeping",
                "little light as possible during night get up",
                "lights on during night get up",
                "check phone when wake up at night",
                "check smartwatch when  wake up at night",
                "close curtains if I want to sleep",
                "use sleep mask that covers eyes",
                "modify light environment to match needs",
                "use LEDs to create healthy light environment",
                "tunable lights to create healthy light",
                "discuss effects of light with other people",
                "seek out knowledge on light exposure")


#Demographic labelling
label(data$demographic_information_timestamp)="Survey Timestamp"
label(data$slypos_demographics_age)="What is your age in years?"
label(data$slypos_demographics_sex)="What is your sex?If you are one or more of non-binary, transgender, have variations of sex characteristics, sometimes also known as intersex, the answer you give can be different from what is on your birth certificate.If youre not sure how to answer, use the sex registered on your official documents, such as passport or driving licence, or whichever answer best describes your sex.A later question gives the option to tell us if your gender is different from your sex registered at birth, and, if different, to record your gender."
label(data$slypos_demographics_gender)="Is your gender the same as the sex you were registered at birth?If your gender is not the same as the sex recorded on your birth certificate when you were born (for example, you are transgender or non-binary), tick No.If you answered no, please enter the term you use to describe your gender. This is also voluntary, so you can leave it blank if you prefer.If you would like to record that you have variations of sex characteristics, sometimes also known as intersex, you can use this write-in box. If you would like to, you can also write-in your gender (for example: intersex, non-binary)."
label(data$slypos_demographics_gender_text)="Please enter your gender:"
label(data$slypos_demographics_language)="Are you a native speaker of English?"
label(data$slypos_demographics_tz)="Please select your country of residence and time zone:"
label(data$slypos_demographics_work_or_school)="Do you primarily go to work or to school?"
label(data$slypos_demographics_school)="During the last four weeks, I have worked/studied in..."
label(data$slypos_demographics_attentioncheck)="We want to make sure you are paying attention. What is 4+5?"
label(data$further_information_about_you_timestamp)="Survey Timestamp"

#Pubertal Stage labelling
label(data$slypos_puberty_01)="Would you say that your growth in height: "
label(data$slypos_puberty_02)="And how about the growth of your body hair?  Would you say that your body hair growth"
label(data$slypos_puberty_03)="Have you noticed any skin changes, especially pimples? "
label(data$slypos_puberty_boys_01)="Have you noticed a deepening of your voice? "
label(data$slypos_puberty_boys_02)="Have you begun to grow hair on your face? "
label(data$slypos_puberty_girl_01)="Have you noticed that your breasts have begun to grow? "
label(data$slypos_puberty_girls_02)="Have you begun to menstruate (started to have your period)? "
label(data$slypos_puberty_girls_03)="If yes, how old were you when you started to menstruate? "
label(data$your_sleep_environment_timestamp)="Survey Timestamp"

# ASE labelling
label(data$slypos_ase_001)="Has too much light"
label(data$slypos_ase_002)="Is too dark"
label(data$slypos_ase_003)="Is too noisy"
label(data$slypos_ase_004)="Is too quiet"
label(data$slypos_ase_005)="Is too warm"
label(data$slypos_ase_006)="Is too cool "
label(data$slypos_ase_007)="Is too humid"
label(data$slypos_ase_attentioncheck)="We want to make sure that you are paying attention. Please select Strongly disagree here."
label(data$slypos_ase_008)="Has an uncomfortable smell"
label(data$slypos_ase_009)="Has uncomfortable pillows or blankets"
label(data$slypos_ase_010)="Has a mattress or other sleeping surface that is too firm"
label(data$slypos_ase_011)="Has a mattress or other sleeping surface that is too soft"
label(data$slypos_ase_012)="Has a mattress or other sleeping surface that is uncomfortable for another reason"
label(data$slypos_ase_0123)="Does not feel safe or secure"
label(data$your_sensitivity_to_light_timestamp)="Survey Timestamp"

# PAQ labelling
label(data$slypos_paq_1)="I prefer summer to winter because winter dreariness makes me sad. "
label(data$slypos_paq_2)="If I could, I would be happier to go out after dusk rather than during the day."
label(data$slypos_paq_3)="Often in winter, Id like to go to the other hemisphere where it is summer time."
label(data$slypos_paq_4)="My ideal house has large windows. "
label(data$slypos_paq_5)="I like cloudy days."
label(data$slypos_paq_6)="Sunlight is so annoying to me, that I have to wear sunglasses when I go out."
label(data$slypos_paq_7)="I prefer to stay at home on sunny days, even if it is not warm."
label(data$slypos_paq_8)="I feel reborn in spring when the days start to become longer."
label(data$slypos_paq_9)="Usually, strong sunlight annoys me."
label(data$slypos_paq_10)="I prefer rooms that are in semi-darkness."
label(data$slypos_paq_11)="I prefer sunlight to semi-darkness. "
label(data$slypos_paq_12)="Looking at a very bright view annoys me."
label(data$slypos_paq_13)="I cant stand light reflecting off snow."
label(data$slypos_paq_14)="I think summer annoys me because its too bright. "
label(data$slypos_paq_15)="Sunlight is like therapy for me. "
label(data$slypos_paq_16)="I prefer walking in the sunlight if the weather is cool."

label(data$your_sleep_timestamp)="Survey Timestamp"

# PROMIS labelling
label(data$slypos_promis_sd_ped_01)="I had difficulty falling asleep."
label(data$slypos_promis_sd_ped_02)="I slept through the night."
label(data$slypos_promis_sd_ped_03)="I had a problem with my sleep."
label(data$slypos_promis_sd_ped_04)="I had trouble sleeping."
label(data$slypos_promis_si_ped_01)="I was sleepy during the daytime."
label(data$slypos_promis_si_ped_02)=" I had a hard time concentrating because I was sleepy."
label(data$slypos_promis_si_ped_03)="I had a hard time getting things done because I was sleepy."
label(data$slypos_promis_si_ped_04)="I had problems during the day because of poor sleep."
label(data$slypos_promis_sd_ad_01)="My sleep quality was..."
label(data$slypos_promis_sd_ad_02)="My sleep was refreshing."
label(data$slypos_promis_sd_ad_03)="I had a problem with my sleep."
label(data$slypos_promis_sd_ad_04)="I had difficulty falling asleep."
label(data$slypos_promis_si_ad_01)="I had a hard time getting things done because I was sleepy."
label(data$slypos_promis_si_ad_02)="I had problems during the day because of poor sleep."
label(data$slypos_promis_si_ad_03)="I had a hard time concentrating because of poor sleep."
label(data$slypos_promis_si_ad_04)="I was sleepy during the daytime."
label(data$your_wakeup_and_sleep_times_timestamp)="Survey Timestamp"

# MCTQ labelling
# label(data$slypos_mctq_01)="I have a regular work schedule (this includes being, for example, a housewife or househusband)"
# label(data$slypos_mctq_02)="If Yes: I work on"
# label(data$slypos_mctq_03)="I go to bed at "
# label(data$slypos_mctq_04)="I actually get ready to fall asleep at"
# label(data$slypos_mctq_05)="I need ___ minutes to fall asleep."
# label(data$slypos_mctq_06)="I wake up at "
# label(data$slypos_mctq_07)="After ___ minutes I get up."
# label(data$slypos_mctq_08)="I use an alarm clock on workdays: "
# label(data$slypos_mctq_09)="If Yes: I regularly wake up before the alarm rings:"
# label(data$slypos_mctq_32)="On workdays I usually have my first meal at"
# label(data$slypos_mctq_33)="On workdays I usually have my last meal at"
# label(data$slypos_mctq_27)="I regularly drink caffeinated drinks."
# label(data$slypos_mctq_28)="On workdays I usually drink my first caffeinated drink at"
# label(data$slypos_mctq_29)="On workdays I usually drink my last caffeinated drink at"
# label(data$slypos_mctq_10)="I go to bed at "
# label(data$slypos_mctq_11)="I actually get ready to fall asleep at"
# label(data$slypos_mctq_12)="I need ___ minutes to fall asleep."
# label(data$slypos_mctq_13)="I wake up at "
# label(data$slypos_mctq_14)="After ___ minutes I get up."
# label(data$slypos_mctq_15)="My wake-up time is due to the use of an alarm clock: "
# label(data$slypos_mctq_16)="There are particular reasons why I cannot freely choose my sleep times on free days: "
# label(data$slypos_mctq_17)="If Yes: "
# label(data$slypos_mctq_34)="On free days I usually have my first meal at"
# label(data$slypos_mctq_35)="On free days I usually have my last meal at"
# label(data$slypos_mctq_30)="On free days I usually drink my first caffeinated drink at"
# label(data$slypos_mctq_31)="On free days I usually drink my last caffeinated drink at"
# label(data$slypos_mctq_18)="In the last 3 months, I worked as a shift worker:  "
# label(data$slypos_mctq_19)="My usual work schedule starts at "
# label(data$slypos_mctq_20)="My usual work schedule ends at "
# label(data$slypos_mctq_21)="My work schedules are..."
# label(data$slypos_mctq_22)="I travel to work... "
# label(data$slypos_mctq_23)="For the commute to work, I need ___ hours and ___ minutes."
# label(data$slypos_mctq_24)="For the commute from work, I need ___ hours and ___ minutes."
# label(data$slypos_mctq_25)="on workdays: ___ hours and ___ minutes"
# label(data$slypos_mctq_26)="on free days: ___ hours and ___ minutes"
# label(data$slypos_mctq_attentioncheck_2)="We want to make sure you are paying attention. Please type in nineteen as a number."
# label(data$slypos_mctq_01_ped)="I go to school on a regular basis"
# label(data$slypos_mctq_02_ped)="If Yes: I go to school on"
# label(data$slypos_mctq_03_ped)="I got to bed at "
# label(data$slypos_mctq_03_ped_2)="What is the main reason you usually go to bed at this time on school days?"
# label(data$slypos_mctq_04_ped)="I actually get ready to fall asleep at"
# label(data$slypos_mctq_05_ped)="I need ___ minutes to fall asleep."
# label(data$slypos_mctq_06_ped)="I wake up at "
# label(data$slypos_mctq_07_ped)="After ___ minutes I get up."
# label(data$slypos_mctq_08_ped)="I use an alarm clock on school days or my parents wake me up:"
# label(data$slypos_mctq_09_ped)="If Yes: I regularly wake up before the alarm rings:"
# label(data$slypos_mctq_32_ped)="On school days I usually have my first meal at"
# label(data$slypos_mctq_33_ped)="On school days I usually have my last meal at"
# label(data$slypos_mctq_27_ped)="I regularly drink caffeinated drinks."
# label(data$slypos_mctq_28_ped)="On school days I usually drink my first caffeinated drink at"
# label(data$slypos_mctq_29_ped)="On school days I usually drink my last caffeinated drink at"
# label(data$slypos_mctq_10_ped)="I go to bed at "
# label(data$slypos_mctq_11_ped)="I actually get ready to fall asleep at"
# label(data$slypos_mctq_12_ped)="I need ___ minutes to fall asleep."
# label(data$slypos_mctq_13_ped)="I wake up at "
# label(data$slypos_mctq_14_ped)="After ___ minutes I get up."
# label(data$slypos_mctq_15_ped)="My wake-up time is due to the use of an alarm clock or my parents waking me up: "
# label(data$slypos_mctq_16_ped)="There are particular reasons why I cannot freely choose my sleep times on free days: "
# label(data$slypos_mctq_17_ped)="If Yes: "
# label(data$slypos_mctq_34_ped)="On free days I usually have my first meal at"
# label(data$slypos_mctq_35_ped)="On free days I usually have my last meal at"
# label(data$slypos_mctq_30_ped)="On free days I usually drink my first caffeinated drink at"
# label(data$slypos_mctq_31_ped)="On free days I usually drink my last caffeinated drink at"
# label(data$slypos_mctq_19_ped)="My usual school schedule starts at "
# label(data$slypos_mctq_20_ped)="My usual school schedule ends at "
# label(data$slypos_mctq_21_ped)="My school schedules are..."
# label(data$slypos_mctq_22_ped)="I travel to work... "
# label(data$slypos_mctq_23_ped)="For the commute to school, I need ___ hours and ___ minutes."
# label(data$slypos_mctq_24_ped)="For the commute from school, I need ___ hours and ___ minutes."
# label(data$slypos_mctq_25_ped)="on school days: ___ hours and ___ minutes"
# label(data$slypos_mctq_26_ped)="on free days: ___ hours and ___ minutes"
# label(data$slypos_mctq_attentioncheck)="We want to make sure you are paying attention. Please type in nineteen as a number."
# label(data$your_light_behaviour_timestamp)="Survey Timestamp"

# Leba Labels
label(data$slypos_leba_01)="I turn on the lights immediately after waking up."
label(data$slypos_leba_02)="I open the curtains or blinds immediately after waking up."
label(data$slypos_leba_03)="I look at my mobile phone screen immediately after waking up."
label(data$slypos_leba_04)="I use an alarm with a dawn simulation light."
label(data$slypos_leba_05)="I have breakfast within 3 meters from a window."
label(data$slypos_leba_06)="I have breakfast in a brightly lit room (illuminated by electric light)."
label(data$slypos_leba_07)="I go for a walk or exercise outside within 2 hours after waking up."
label(data$slypos_leba_08)="I spend 30 minutes or less per day (in total) outside."
label(data$slypos_leba_09)="I spend between 30 minutes and 1 hour per day (in total) outside."
label(data$slypos_leba_10)="I spend between 1 and 3 hours per day (in total) outside."
label(data$slypos_leba_11)="I spend more than 3 hours per day (in total) outside."
label(data$slypos_leba_12)="I spend as much time outside as possible."
label(data$slypos_leba_13)="I use sunglasses when I go outside in bright daylight."
label(data$slypos_leba_14)="I wear a visor or cap when I go outside in bright daylight."
label(data$slypos_leba_15)="I seek shade when I am outside in bright daylight."
label(data$slypos_leba_16)="I wear blue-filtering, orange-tinted, and/or red-tinted glasses indoors during the day."
label(data$slypos_leba_17)="I wear blue-filtering, orange-tinted, and/or red-tinted glasses outdoors during the day."
label(data$slypos_leba_18)="I use light therapy applying a white light box."
label(data$slypos_leba_19)="I use light therapy applying a blue light box."
label(data$slypos_leba_20)="I use light therapy applying a light visor."
label(data$slypos_leba_21)="I use light therapy applying another form of light device."
label(data$slypos_leba_22)="I spend most of my daytime in a brightly lit environment."
label(data$slypos_leba_23)="I close the curtains or blinds during the day if the light from outside is bright."
label(data$slypos_leba_24)="I spend most of my indoor time within 3 meters from a window."
label(data$slypos_leba_25)="I use a desk lamp when I do focused work."
label(data$slypos_leba_26)="I turn on my ceiling room light when it is light outside."
label(data$slypos_leba_attentioncheck)="We want to make sure you are paying attention. Please select Does not apply/I dont know. here."
label(data$slypos_leba_27)="I use my mobile phone within 1 hour before attempting to fall asleep."
label(data$slypos_leba_28)="I use my computer/laptop/tablet within 1 hour before attempting to fall asleep."
label(data$slypos_leba_29)="I watch television within 1 hour before attempting to fall asleep."
label(data$slypos_leba_30)="I look at my smartwatch within 1 hour before attempting to fall asleep."
label(data$slypos_leba_31)="I dim my room light within 1 hour before attempting to fall asleep."
label(data$slypos_leba_32)="I dim my mobile phone screen within 1 hour before attempting to fall asleep."
label(data$slypos_leba_33)="I dim my computer screen within 1 hour before attempting to fall asleep."
label(data$slypos_leba_36)="I use a blue-filter app on my mobile phone screen within 1 hour before attempting to fall asleep."
label(data$slypos_leba_37)="I use a blue-filter app on my computer screen within 1 hour before attempting to fall asleep."
label(data$slypos_leba_38)="I wear blue-filtering, orange-tinted, and/or red-tinted glasses within 1 hour before attempting to fall asleep."
label(data$slypos_leba_39)="I purposely leave a light on in my sleep environment while sleeping."
label(data$slypos_leba_40)="I use as little light as possible when I get up during the night."
label(data$slypos_leba_41)="I turn on the lights when I get up during the night."
label(data$slypos_leba_42)="I check my phone when I wake up at night."
label(data$slypos_leba_43)="I look at my smartwatch when I wake up at night."
label(data$slypos_leba_44)="I close curtains or blinds to prevent light from entering the bedroom if I want to sleep."
label(data$slypos_leba_45)="I use a sleep mask that covers my eyes."
label(data$slypos_leba_46)="I modify my light environment to match my current needs."
label(data$slypos_leba_47)="I use LEDs to create a healthy light environment."
label(data$slypos_leba_48)="I use tunable lights to create a healthy light environment."
label(data$slypos_leba_49)="I discuss the effects of light on my body with other people."
label(data$slypos_leba_50)="I seek out knowledge on how to improve my light exposure."
#Setting Units


# Creating factors--------------------------------------------------------------

#Setting Factors(will create new variable for factors)

data$slypos_demographics_sex.factor = factor(data$slypos_demographics_sex,levels=c("1","2","3"))
data$slypos_demographics_gender.factor = factor(data$slypos_demographics_gender,levels=c("1","0"))
data$slypos_demographics_language.factor = factor(data$slypos_demographics_language,levels=c("1","0"))
data$slypos_demographics_tz.factor = factor(data$slypos_demographics_tz,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283","284","285","286","287","288","289","290","291","292","293","294","295","296","297","298","299","300","301","302","303","304","305","306","307","308","309","310","311","312","313","314","315","316","317","318","319","320","321","322","323","324","325","326","327","328","329","330","331","332","333","334","335","336","337","338","339","340","341","342","343","344","345","346","347","348","349","350","351","352","353","354","355","356","357","358","359","360","361","362","363","364","365","366","367","368","369","370","371","372","373","374","375","376","377","378","379","380","381","382","383","384","385","386","387","388","389","390","391","392","393","394","395","396","397","398","399","400","401","402","403","404","405","406","407","408","409","410","411","412","413","414","415","416","417","418","419","420","421","422","423","424","425"))
data$slypos_demographics_work_or_school.factor = factor(data$slypos_demographics_work_or_school,levels=c("1","2","3"))
data$slypos_demographics_school.factor = factor(data$slypos_demographics_school,levels=c("1","2","3","4"))

data$slypos_puberty_01.factor = factor(data$slypos_puberty_01,levels=c("1","2","3","4","5","6"))
data$slypos_puberty_02.factor = factor(data$slypos_puberty_02,levels=c("1","2","3","4","5","6"))
data$slypos_puberty_03.factor = factor(data$slypos_puberty_03,levels=c("1","2","3","4","5","6"))
data$slypos_puberty_boys_01.factor = factor(data$slypos_puberty_boys_01,levels=c("1","2","3","4","5","6"))
data$slypos_puberty_boys_02.factor = factor(data$slypos_puberty_boys_02,levels=c("1","2","3","4","5","6"))
data$slypos_puberty_girl_01.factor = factor(data$slypos_puberty_girl_01,levels=c("1","2","3","4","5","6"))
data$slypos_puberty_girls_02.factor = factor(data$slypos_puberty_girls_02,levels=c("1","2","3"))

data$slypos_ase_001.factor = factor(data$slypos_ase_001,levels=c("1","2","3","4"))
data$slypos_ase_002.factor = factor(data$slypos_ase_002,levels=c("1","2","3","4"))
data$slypos_ase_003.factor = factor(data$slypos_ase_003,levels=c("1","2","3","4"))
data$slypos_ase_004.factor = factor(data$slypos_ase_004,levels=c("1","2","3","4"))
data$slypos_ase_005.factor = factor(data$slypos_ase_005,levels=c("1","2","3","4"))
data$slypos_ase_006.factor = factor(data$slypos_ase_006,levels=c("1","2","3","4"))
data$slypos_ase_007.factor = factor(data$slypos_ase_007,levels=c("1","2","3","4"))
data$slypos_ase_attentioncheck.factor = factor(data$slypos_ase_attentioncheck,levels=c("1","2","3","4"))
data$slypos_ase_008.factor = factor(data$slypos_ase_008,levels=c("1","2","3","4"))
data$slypos_ase_009.factor = factor(data$slypos_ase_009,levels=c("1","2","3","4"))
data$slypos_ase_010.factor = factor(data$slypos_ase_010,levels=c("1","2","3","4"))
data$slypos_ase_011.factor = factor(data$slypos_ase_011,levels=c("1","2","3","4"))
data$slypos_ase_012.factor = factor(data$slypos_ase_012,levels=c("1","2","3","4"))
data$slypos_ase_0123.factor = factor(data$slypos_ase_0123,levels=c("1","2","3","4"))

data$slypos_paq_1.factor = factor(data$slypos_paq_1,levels=c("1","0"))
data$slypos_paq_2.factor = factor(data$slypos_paq_2,levels=c("1","0"))
data$slypos_paq_3.factor = factor(data$slypos_paq_3,levels=c("1","0"))
data$slypos_paq_4.factor = factor(data$slypos_paq_4,levels=c("1","0"))
data$slypos_paq_5.factor = factor(data$slypos_paq_5,levels=c("0","1"))
data$slypos_paq_6.factor = factor(data$slypos_paq_6,levels=c("1","0"))
data$slypos_paq_7.factor = factor(data$slypos_paq_7,levels=c("1","0"))
data$slypos_paq_8.factor = factor(data$slypos_paq_8,levels=c("1","0"))
data$slypos_paq_9.factor = factor(data$slypos_paq_9,levels=c("1","0"))
data$slypos_paq_10.factor = factor(data$slypos_paq_10,levels=c("1","0"))
data$slypos_paq_11.factor = factor(data$slypos_paq_11,levels=c("1","0"))
data$slypos_paq_12.factor = factor(data$slypos_paq_12,levels=c("1","0"))
data$slypos_paq_13.factor = factor(data$slypos_paq_13,levels=c("1","0"))
data$slypos_paq_14.factor = factor(data$slypos_paq_14,levels=c("1","0"))
data$slypos_paq_15.factor = factor(data$slypos_paq_15,levels=c("1","0"))
data$slypos_paq_16.factor = factor(data$slypos_paq_16,levels=c("1","0"))

data$slypos_promis_sd_ped_01.factor = factor(data$slypos_promis_sd_ped_01,levels=c("1","2","3","4","5"))
data$slypos_promis_sd_ped_02.factor = factor(data$slypos_promis_sd_ped_02,levels=c("5","4","3","2","1"))
data$slypos_promis_sd_ped_03.factor = factor(data$slypos_promis_sd_ped_03,levels=c("1","2","3","4","5"))
data$slypos_promis_sd_ped_04.factor = factor(data$slypos_promis_sd_ped_04,levels=c("1","2","3","4","5"))
data$slypos_promis_si_ped_01.factor = factor(data$slypos_promis_si_ped_01,levels=c("1","2","3","4","5"))
data$slypos_promis_si_ped_02.factor = factor(data$slypos_promis_si_ped_02,levels=c("1","2","3","4","5"))
data$slypos_promis_si_ped_03.factor = factor(data$slypos_promis_si_ped_03,levels=c("1","2","3","4","5"))
data$slypos_promis_si_ped_04.factor = factor(data$slypos_promis_si_ped_04,levels=c("1","2","3","4","5"))
data$slypos_promis_sd_ad_01.factor = factor(data$slypos_promis_sd_ad_01,levels=c("5","4","3","2","1"))
data$slypos_promis_sd_ad_02.factor = factor(data$slypos_promis_sd_ad_02,levels=c("5","4","3","2","1"))
data$slypos_promis_sd_ad_03.factor = factor(data$slypos_promis_sd_ad_03,levels=c("1","2","3","4","5"))
data$slypos_promis_sd_ad_04.factor = factor(data$slypos_promis_sd_ad_04,levels=c("1","2","3","4","5"))
data$slypos_promis_si_ad_01.factor = factor(data$slypos_promis_si_ad_01,levels=c("1","2","3","4","5"))
data$slypos_promis_si_ad_02.factor = factor(data$slypos_promis_si_ad_02,levels=c("1","2","3","4","5"))
data$slypos_promis_si_ad_03.factor = factor(data$slypos_promis_si_ad_03,levels=c("1","2","3","4","5"))
data$slypos_promis_si_ad_04.factor = factor(data$slypos_promis_si_ad_04,levels=c("1","2","3","4","5"))

data$slypos_mctq_01.factor = factor(data$slypos_mctq_01,levels=c("1","2"))
data$slypos_mctq_02.factor = factor(data$slypos_mctq_02,levels=c("1","2","3","4","5","6","7"))
data$slypos_mctq_08.factor = factor(data$slypos_mctq_08,levels=c("1","0"))
data$slypos_mctq_09.factor = factor(data$slypos_mctq_09,levels=c("1","0"))
data$slypos_mctq_27.factor = factor(data$slypos_mctq_27,levels=c("1","0"))
data$slypos_mctq_15.factor = factor(data$slypos_mctq_15,levels=c("1","0"))
data$slypos_mctq_16.factor = factor(data$slypos_mctq_16,levels=c("1","0"))
data$slypos_mctq_17.factor = factor(data$slypos_mctq_17,levels=c("1","2","3"))
data$slypos_mctq_18.factor = factor(data$slypos_mctq_18,levels=c("1","2"))
data$slypos_mctq_21.factor = factor(data$slypos_mctq_21,levels=c("1","2","3","4"))
data$slypos_mctq_22.factor = factor(data$slypos_mctq_22,levels=c("1","2","3"))
data$slypos_mctq_01_ped.factor = factor(data$slypos_mctq_01_ped,levels=c("1","2"))
data$slypos_mctq_02_ped.factor = factor(data$slypos_mctq_02_ped,levels=c("1","2","3","4","5","6","7"))
data$slypos_mctq_03_ped_2.factor = factor(data$slypos_mctq_03_ped_2,levels=c("1","2","3","4","5","6","7","8"))
data$slypos_mctq_08_ped.factor = factor(data$slypos_mctq_08_ped,levels=c("1","0"))
data$slypos_mctq_09_ped.factor = factor(data$slypos_mctq_09_ped,levels=c("1","0"))
data$slypos_mctq_27_ped.factor = factor(data$slypos_mctq_27_ped,levels=c("1","0"))
data$slypos_mctq_15_ped.factor = factor(data$slypos_mctq_15_ped,levels=c("1","0"))
data$slypos_mctq_16_ped.factor = factor(data$slypos_mctq_16_ped,levels=c("1","0"))
data$slypos_mctq_17_ped.factor = factor(data$slypos_mctq_17_ped,levels=c("1","2","3"))
data$slypos_mctq_21_ped.factor = factor(data$slypos_mctq_21_ped,levels=c("1","2","3","4"))
data$slypos_mctq_22_ped.factor = factor(data$slypos_mctq_22_ped,levels=c("5","6","7"))

data$slypos_leba_01.factor = factor(data$slypos_leba_01,levels=c("0","1","2","3","4","5"))
data$slypos_leba_02.factor = factor(data$slypos_leba_02,levels=c("0","1","2","3","4","5"))
data$slypos_leba_03.factor = factor(data$slypos_leba_03,levels=c("0","1","2","3","4","5"))
data$slypos_leba_04.factor = factor(data$slypos_leba_04,levels=c("0","1","2","3","4","5"))
data$slypos_leba_05.factor = factor(data$slypos_leba_05,levels=c("0","1","2","3","4","5"))
data$slypos_leba_06.factor = factor(data$slypos_leba_06,levels=c("0","1","2","3","4","5"))
data$slypos_leba_07.factor = factor(data$slypos_leba_07,levels=c("0","1","2","3","4","5"))
data$slypos_leba_08.factor = factor(data$slypos_leba_08,levels=c("0","1","2","3","4","5"))
data$slypos_leba_09.factor = factor(data$slypos_leba_09,levels=c("0","1","2","3","4","5"))
data$slypos_leba_10.factor = factor(data$slypos_leba_10,levels=c("0","1","2","3","4","5"))
data$slypos_leba_11.factor = factor(data$slypos_leba_11,levels=c("0","1","2","3","4","5"))
data$slypos_leba_12.factor = factor(data$slypos_leba_12,levels=c("0","1","2","3","4","5"))
data$slypos_leba_13.factor = factor(data$slypos_leba_13,levels=c("0","1","2","3","4","5"))
data$slypos_leba_14.factor = factor(data$slypos_leba_14,levels=c("0","1","2","3","4","5"))
data$slypos_leba_15.factor = factor(data$slypos_leba_15,levels=c("0","1","2","3","4","5"))
data$slypos_leba_16.factor = factor(data$slypos_leba_16,levels=c("0","1","2","3","4","5"))
data$slypos_leba_17.factor = factor(data$slypos_leba_17,levels=c("0","1","2","3","4","5"))
data$slypos_leba_18.factor = factor(data$slypos_leba_18,levels=c("0","1","2","3","4","5"))
data$slypos_leba_19.factor = factor(data$slypos_leba_19,levels=c("0","1","2","3","4","5"))
data$slypos_leba_20.factor = factor(data$slypos_leba_20,levels=c("0","1","2","3","4","5"))
data$slypos_leba_21.factor = factor(data$slypos_leba_21,levels=c("0","1","2","3","4","5"))
data$slypos_leba_22.factor = factor(data$slypos_leba_22,levels=c("0","1","2","3","4","5"))
data$slypos_leba_23.factor = factor(data$slypos_leba_23,levels=c("0","1","2","3","4","5"))
data$slypos_leba_24.factor = factor(data$slypos_leba_24,levels=c("0","1","2","3","4","5"))
data$slypos_leba_25.factor = factor(data$slypos_leba_25,levels=c("0","1","2","3","4","5"))
data$slypos_leba_26.factor = factor(data$slypos_leba_26,levels=c("0","1","2","3","4","5"))

data$slypos_leba_attentioncheck.factor = factor(data$slypos_leba_attentioncheck,levels=c("0","1","2","3","4","5"))

data$slypos_leba_27.factor = factor(data$slypos_leba_27,levels=c("0","1","2","3","4","5"))
data$slypos_leba_28.factor = factor(data$slypos_leba_28,levels=c("0","1","2","3","4","5"))
data$slypos_leba_29.factor = factor(data$slypos_leba_29,levels=c("0","1","2","3","4","5"))
data$slypos_leba_30.factor = factor(data$slypos_leba_30,levels=c("0","1","2","3","4","5"))
data$slypos_leba_31.factor = factor(data$slypos_leba_31,levels=c("0","1","2","3","4","5"))
data$slypos_leba_32.factor = factor(data$slypos_leba_32,levels=c("0","1","2","3","4","5"))
data$slypos_leba_33.factor = factor(data$slypos_leba_33,levels=c("0","1","2","3","4","5"))
data$slypos_leba_36.factor = factor(data$slypos_leba_36,levels=c("0","1","2","3","4","5"))
data$slypos_leba_37.factor = factor(data$slypos_leba_37,levels=c("0","1","2","3","4","5"))
data$slypos_leba_38.factor = factor(data$slypos_leba_38,levels=c("0","1","2","3","4","5"))
data$slypos_leba_39.factor = factor(data$slypos_leba_39,levels=c("0","1","2","3","4","5"))
data$slypos_leba_40.factor = factor(data$slypos_leba_40,levels=c("0","1","2","3","4","5"))
data$slypos_leba_41.factor = factor(data$slypos_leba_41,levels=c("0","1","2","3","4","5"))
data$slypos_leba_42.factor = factor(data$slypos_leba_42,levels=c("0","1","2","3","4","5"))
data$slypos_leba_43.factor = factor(data$slypos_leba_43,levels=c("0","1","2","3","4","5"))
data$slypos_leba_44.factor = factor(data$slypos_leba_44,levels=c("0","1","2","3","4","5"))
data$slypos_leba_45.factor = factor(data$slypos_leba_45,levels=c("0","1","2","3","4","5"))
data$slypos_leba_46.factor = factor(data$slypos_leba_46,levels=c("0","1","2","3","4","5"))
data$slypos_leba_47.factor = factor(data$slypos_leba_47,levels=c("0","1","2","3","4","5"))
data$slypos_leba_48.factor = factor(data$slypos_leba_48,levels=c("0","1","2","3","4","5"))
data$slypos_leba_49.factor = factor(data$slypos_leba_49,levels=c("0","1","2","3","4","5"))
data$slypos_leba_50.factor = factor(data$slypos_leba_50,levels=c("0","1","2","3","4","5"))

# Creating levels for factor vars--------------------------------------------------------------


levels(data$slypos_demographics_sex.factor)=c("Female","Male","Other")
levels(data$slypos_demographics_gender.factor)=c("Yes","No")
levels(data$slypos_demographics_language.factor)=c("Yes","No")

levels(data$slypos_demographics_tz.factor)=c("Aland Islands - Europe/Mariehamn (UTC +02:00)","Afghanistan - Asia/Kabul (UTC +04:30)","Albania - Europe/Tirane (UTC +01:00)","Algeria - Africa/Algiers (UTC +01:00)","American Samoa - Pacific/Pago_Pago (UTC -11:00)","Andorra - Europe/Andorra (UTC +01:00)","Angola - Africa/Luanda (UTC +01:00)","Anguilla - America/Anguilla (UTC -04:00)","Antarctica - Antarctica/Casey (UTC +11:00)","Antarctica - Antarctica/Davis (UTC +07:00)","Antarctica - Antarctica/DumontDUrville (UTC +10:00)","Antarctica - Antarctica/Mawson (UTC +05:00)","Antarctica - Antarctica/McMurdo (UTC +13:00)","Antarctica - Antarctica/Palmer (UTC -03:00)","Antarctica - Antarctica/Rothera (UTC -03:00)","Antarctica - Antarctica/Syowa (UTC +03:00)","Antarctica - Antarctica/Troll (UTC)","Antarctica - Antarctica/Vostok (UTC +06:00)","Antigua and Barbuda - America/Antigua (UTC -04:00)","Argentina - America/Argentina/Buenos_Aires (UTC -03:00)","Argentina - America/Argentina/Catamarca (UTC -03:00)","Argentina - America/Argentina/Cordoba (UTC -03:00)","Argentina - America/Argentina/Jujuy (UTC -03:00)","Argentina - America/Argentina/La_Rioja (UTC -03:00)","Argentina - America/Argentina/Mendoza (UTC -03:00)","Argentina - America/Argentina/Rio_Gallegos (UTC -03:00)","Argentina - America/Argentina/Salta (UTC -03:00)","Argentina - America/Argentina/San_Juan (UTC -03:00)","Argentina - America/Argentina/San_Luis (UTC -03:00)","Argentina - America/Argentina/Tucuman (UTC -03:00)","Argentina - America/Argentina/Ushuaia (UTC -03:00)","Armenia - Asia/Yerevan (UTC +04:00)","Aruba - America/Aruba (UTC -04:00)","Australia - Antarctica/Macquarie (UTC +11:00)","Australia - Australia/Adelaide (UTC +10:30)","Australia - Australia/Brisbane (UTC +10:00)","Australia - Australia/Broken_Hill (UTC +10:30)","Australia - Australia/Currie (UTC +11:00)","Australia - Australia/Darwin (UTC +09:30)","Australia - Australia/Eucla (UTC +08:45)","Australia - Australia/Hobart (UTC +11:00)","Australia - Australia/Lindeman (UTC +10:00)","Australia - Australia/Lord_Howe (UTC +11:00)","Australia - Australia/Melbourne (UTC +11:00)","Australia - Australia/Perth (UTC +08:00)","Australia - Australia/Sydney (UTC +11:00)","Austria - Europe/Vienna (UTC +01:00)","Azerbaijan - Asia/Baku (UTC +04:00)","Bahamas - America/Nassau (UTC -04:00)","Bahrain - Asia/Bahrain (UTC +03:00)","Bangladesh - Asia/Dhaka (UTC +06:00)","Barbados - America/Barbados (UTC -04:00)","Belarus - Europe/Minsk (UTC +03:00)","Belgium - Europe/Brussels (UTC +01:00)","Belize - America/Belize (UTC -06:00)","Benin - Africa/Porto-Novo (UTC +01:00)","Bermuda - Atlantic/Bermuda (UTC -03:00)","Bhutan - Asia/Thimphu (UTC +06:00),Plurinational State of - America/La_Paz (UTC -04:00)","Bolivia,Sint Eustatius and Saba - America/Kralendijk (UTC -04:00)","Bonaire","Bosnia and Herzegovina - Europe/Sarajevo (UTC +01:00)","Botswana - Africa/Gaborone (UTC +02:00)","Brazil - America/Araguaina (UTC -03:00)","Brazil - America/Bahia (UTC -03:00)","Brazil - America/Belem (UTC -03:00)","Brazil - America/Boa_Vista (UTC -04:00)","Brazil - America/Campo_Grande (UTC -04:00)","Brazil - America/Cuiaba (UTC -04:00)","Brazil - America/Eirunepe (UTC -05:00)","Brazil - America/Fortaleza (UTC -03:00)","Brazil - America/Maceio (UTC -03:00)","Brazil - America/Manaus (UTC -04:00)","Brazil - America/Noronha (UTC -02:00)","Brazil - America/Porto_Velho (UTC -04:00)","Brazil - America/Recife (UTC -03:00)","Brazil - America/Rio_Branco (UTC -05:00)","Brazil - America/Santarem (UTC -03:00)","Brazil - America/Sao_Paulo (UTC -03:00)","British Indian Ocean Territory - Indian/Chagos (UTC +06:00)","Brunei Darussalam - Asia/Brunei (UTC +08:00)","Bulgaria - Europe/Sofia (UTC +02:00)","Burkina Faso - Africa/Ouagadougou (UTC)","Burundi - Africa/Bujumbura (UTC +02:00)","Cambodia - Asia/Phnom_Penh (UTC +07:00)","Cameroon - Africa/Douala (UTC +01:00)","Canada - America/Atikokan (UTC -05:00)","Canada - America/Blanc-Sablon (UTC -04:00)",
                                             "Canada - America/Cambridge_Bay (UTC -06:00)","Canada - America/Creston (UTC -07:00)","Canada - America/Dawson (UTC -07:00)","Canada - America/Dawson_Creek (UTC -07:00)","Canada - America/Edmonton (UTC -06:00)","Canada - America/Fort_Nelson (UTC -07:00)","Canada - America/Glace_Bay (UTC -03:00)","Canada - America/Goose_Bay (UTC -03:00)","Canada - America/Halifax (UTC -03:00)","Canada - America/Inuvik (UTC -06:00)","Canada - America/Iqaluit (UTC -04:00)","Canada - America/Moncton (UTC -03:00)","Canada - America/Nipigon (UTC -04:00)","Canada - America/Pangnirtung (UTC -04:00)","Canada - America/Rainy_River (UTC -05:00)","Canada - America/Rankin_Inlet (UTC -05:00)","Canada - America/Regina (UTC -06:00)","Canada - America/Resolute (UTC -05:00)","Canada - America/St_Johns (UTC -02:30)","Canada - America/Swift_Current (UTC -06:00)","Canada - America/Thunder_Bay (UTC -04:00)","Canada - America/Toronto (UTC -04:00)","Canada - America/Vancouver (UTC -07:00)","Canada - America/Whitehorse (UTC -07:00)","Canada - America/Winnipeg (UTC -05:00)","Canada - America/Yellowknife (UTC -06:00)","Cape Verde - Atlantic/Cape_Verde (UTC -01:00)","Cayman Islands - America/Cayman (UTC -05:00)","Central African Republic - Africa/Bangui (UTC +01:00)","Chad - Africa/Ndjamena (UTC +01:00)","Chile - America/Punta_Arenas (UTC -03:00)","Chile - America/Santiago (UTC -03:00)","Chile - Pacific/Easter (UTC -05:00)","China - Asia/Shanghai (UTC +08:00)","China - Asia/Urumqi (UTC +06:00)","Christmas Island - Indian/Christmas (UTC +07:00)","Cocos (Keeling) Islands - Indian/Cocos (UTC +06:30)","Colombia - America/Bogota (UTC -05:00)","Comoros - Indian/Comoro (UTC +03:00)","Congo - Africa/Brazzaville (UTC +01:00),the Democratic Republic of the - Africa/Kinshasa (UTC +01:00)","Congo,the Democratic Republic of the - Africa/Lubumbashi (UTC +02:00)","Congo","Cook Islands - Pacific/Rarotonga (UTC -10:00)","Costa Rica - America/Costa_Rica (UTC -06:00)","Croatia - Europe/Zagreb (UTC +01:00)","Cuba - America/Havana (UTC -04:00)","Curaçao - America/Curacao (UTC -04:00)","Cyprus - Asia/Famagusta (UTC +02:00)","Cyprus - Asia/Nicosia (UTC +02:00)","Czech Republic - Europe/Prague (UTC +01:00)","Côte dIvoire - Africa/Abidjan (UTC)","Denmark - Europe/Copenhagen (UTC +01:00)","Djibouti - Africa/Djibouti (UTC +03:00)","Dominica - America/Dominica (UTC -04:00)","Dominican Republic - America/Santo_Domingo (UTC -04:00)","Ecuador - America/Guayaquil (UTC -05:00)","Ecuador - Pacific/Galapagos (UTC -06:00)","Egypt - Africa/Cairo (UTC +02:00)","El Salvador - America/El_Salvador (UTC -06:00)","Equatorial Guinea - Africa/Malabo (UTC +01:00)","Eritrea - Africa/Asmara (UTC +03:00)","Estonia - Europe/Tallinn (UTC +02:00)","Ethiopia - Africa/Addis_Ababa (UTC +03:00)","Falkland Islands (Malvinas) - Atlantic/Stanley (UTC -03:00)","Faroe Islands - Atlantic/Faroe (UTC)","Fiji - Pacific/Fiji (UTC +12:00)","Finland - Europe/Helsinki (UTC +02:00)","France - Europe/Paris (UTC +01:00)","French Guiana - America/Cayenne (UTC -03:00)","French Polynesia - Pacific/Gambier (UTC -09:00)","French Polynesia - Pacific/Marquesas (UTC -09:30)","French Polynesia - Pacific/Tahiti (UTC -10:00)","French Southern Territories - Indian/Kerguelen (UTC +05:00)","Gabon - Africa/Libreville (UTC +01:00)","Gambia - Africa/Banjul (UTC)","Georgia - Asia/Tbilisi (UTC +04:00)","Germany - Europe/Berlin (UTC +01:00)","Germany - Europe/Busingen (UTC +01:00)","Ghana - Africa/Accra (UTC)","Gibraltar - Europe/Gibraltar (UTC +01:00)","Greece - Europe/Athens (UTC +02:00)","Greenland - America/Danmarkshavn (UTC)","Greenland - America/Nuuk (UTC -03:00)","Greenland - America/Scoresbysund (UTC -01:00)","Greenland - America/Thule (UTC -03:00)","Grenada - America/Grenada (UTC -04:00)","Guadeloupe - America/Guadeloupe (UTC -04:00)","Guam - Pacific/Guam (UTC +10:00)","Guatemala - America/Guatemala (UTC -06:00)","Guernsey - Europe/Guernsey (UTC)","Guinea - Africa/Conakry (UTC)",
                                             "Guinea-Bissau - Africa/Bissau (UTC)","Guyana - America/Guyana (UTC -04:00)","Haiti - America/Port-au-Prince (UTC -04:00)","Holy See (Vatican City State) - Europe/Vatican (UTC +01:00)","Honduras - America/Tegucigalpa (UTC -06:00)","Hong Kong - Asia/Hong_Kong (UTC +08:00)","Hungary - Europe/Budapest (UTC +01:00)","Iceland - Atlantic/Reykjavik (UTC)","India - Asia/Kolkata (UTC +05:30)","Indonesia - Asia/Jakarta (UTC +07:00)","Indonesia - Asia/Jayapura (UTC +09:00)","Indonesia - Asia/Makassar (UTC +08:00)","Indonesia - Asia/Pontianak (UTC +07:00),Islamic Republic of - Asia/Tehran (UTC +03:30)","Iran","Iraq - Asia/Baghdad (UTC +03:00)","Ireland - Europe/Dublin (UTC)","Isle of Man - Europe/Isle_of_Man (UTC)","Israel - Asia/Jerusalem (UTC +02:00)","Italy - Europe/Rome (UTC +01:00)","Jamaica - America/Jamaica (UTC -05:00)","Japan - Asia/Tokyo (UTC +09:00)","Jersey - Europe/Jersey (UTC)","Jordan - Asia/Amman (UTC +03:00)","Kazakhstan - Asia/Almaty (UTC +06:00)","Kazakhstan - Asia/Aqtau (UTC +05:00)","Kazakhstan - Asia/Aqtobe (UTC +05:00)","Kazakhstan - Asia/Atyrau (UTC +05:00)","Kazakhstan - Asia/Oral (UTC +05:00)","Kazakhstan - Asia/Qostanay (UTC +06:00)","Kazakhstan - Asia/Qyzylorda (UTC +05:00)","Kenya - Africa/Nairobi (UTC +03:00)","Kiribati - Pacific/Enderbury (UTC +13:00)","Kiribati - Pacific/Kiritimati (UTC +14:00)","Kiribati - Pacific/Tarawa (UTC +12:00),Democratic Peoples Republic of - Asia/Pyongyang (UTC +09:00)","Korea,Republic of - Asia/Seoul (UTC +09:00)","Korea","Kuwait - Asia/Kuwait (UTC +03:00)","Kyrgyzstan - Asia/Bishkek (UTC +06:00)","Lao Peoples Democratic Republic - Asia/Vientiane (UTC +07:00)","Latvia - Europe/Riga (UTC +02:00)","Lebanon - Asia/Beirut (UTC +02:00)","Lesotho - Africa/Maseru (UTC +02:00)","Liberia - Africa/Monrovia (UTC)","Libya - Africa/Tripoli (UTC +02:00)","Liechtenstein - Europe/Vaduz (UTC +01:00)","Lithuania - Europe/Vilnius (UTC +02:00)","Luxembourg - Europe/Luxembourg (UTC +01:00)","Macao - Asia/Macau (UTC +08:00),the Former Yugoslav Republic of - Europe/Skopje (UTC +01:00)","Macedonia","Madagascar - Indian/Antananarivo (UTC +03:00)","Malawi - Africa/Blantyre (UTC +02:00)","Malaysia - Asia/Kuala_Lumpur (UTC +08:00)","Malaysia - Asia/Kuching (UTC +08:00)","Maldives - Indian/Maldives (UTC +05:00)","Mali - Africa/Bamako (UTC)","Malta - Europe/Malta (UTC +01:00)","Marshall Islands - Pacific/Kwajalein (UTC +12:00)","Marshall Islands - Pacific/Majuro (UTC +12:00)","Martinique - America/Martinique (UTC -04:00)","Mauritania - Africa/Nouakchott (UTC)","Mauritius - Indian/Mauritius (UTC +04:00)","Mayotte - Indian/Mayotte (UTC +03:00)","Mexico - America/Bahia_Banderas (UTC -06:00)","Mexico - America/Cancun (UTC -05:00)","Mexico - America/Chihuahua (UTC -07:00)","Mexico - America/Hermosillo (UTC -07:00)","Mexico - America/Matamoros (UTC -05:00)","Mexico - America/Mazatlan (UTC -07:00)","Mexico - America/Merida (UTC -06:00)","Mexico - America/Mexico_City (UTC -06:00)","Mexico - America/Monterrey (UTC -06:00)","Mexico - America/Ojinaga (UTC -06:00)","Mexico - America/Tijuana (UTC -07:00),Federated States of - Pacific/Chuuk (UTC +10:00)","Micronesia,Federated States of - Pacific/Kosrae (UTC +11:00)","Micronesia,Federated States of - Pacific/Pohnpei (UTC +11:00)","Micronesia,Republic of - Europe/Chisinau (UTC +02:00)","Moldova","Monaco - Europe/Monaco (UTC +01:00)","Mongolia - Asia/Choibalsan (UTC +08:00)","Mongolia - Asia/Hovd (UTC +07:00)","Mongolia - Asia/Ulaanbaatar (UTC +08:00)","Montenegro - Europe/Podgorica (UTC +01:00)","Montserrat - America/Montserrat (UTC -04:00)","Morocco - Africa/Casablanca (UTC +01:00)","Mozambique - Africa/Maputo (UTC +02:00)","Myanmar - Asia/Yangon (UTC +06:30)","Namibia - Africa/Windhoek (UTC +02:00)","Nauru - Pacific/Nauru (UTC +12:00)","Nepal - Asia/Kathmandu (UTC +05:45)","Netherlands - Europe/Amsterdam (UTC +01:00)","New Caledonia - Pacific/Noumea (UTC +11:00)","New Zealand - Pacific/Auckland (UTC +13:00)",
                                             "New Zealand - Pacific/Chatham (UTC +13:45)","Nicaragua - America/Managua (UTC -06:00)","Niger - Africa/Niamey (UTC +01:00)","Nigeria - Africa/Lagos (UTC +01:00)","Niue - Pacific/Niue (UTC -11:00)","Norfolk Island - Pacific/Norfolk (UTC +12:00)","Northern Mariana Islands - Pacific/Saipan (UTC +10:00)","Norway - Europe/Oslo (UTC +01:00)","Oman - Asia/Muscat (UTC +04:00)","Pakistan - Asia/Karachi (UTC +05:00)","Palau - Pacific/Palau (UTC +09:00),State of - Asia/Gaza (UTC +02:00)","Palestine,State of - Asia/Hebron (UTC +02:00)","Palestine","Panama - America/Panama (UTC -05:00)","Papua New Guinea - Pacific/Bougainville (UTC +11:00)","Papua New Guinea - Pacific/Port_Moresby (UTC +10:00)","Paraguay - America/Asuncion (UTC -03:00)","Peru - America/Lima (UTC -05:00)","Philippines - Asia/Manila (UTC +08:00)","Pitcairn - Pacific/Pitcairn (UTC -08:00)","Poland - Europe/Warsaw (UTC +01:00)","Portugal - Atlantic/Azores (UTC -01:00)","Portugal - Atlantic/Madeira (UTC)","Portugal - Europe/Lisbon (UTC)","Puerto Rico - America/Puerto_Rico (UTC -04:00)","Qatar - Asia/Qatar (UTC +03:00)","Romania - Europe/Bucharest (UTC +02:00)","Russian Federation - Asia/Anadyr (UTC +12:00)","Russian Federation - Asia/Barnaul (UTC +07:00)","Russian Federation - Asia/Chita (UTC +09:00)","Russian Federation - Asia/Irkutsk (UTC +08:00)","Russian Federation - Asia/Kamchatka (UTC +12:00)","Russian Federation - Asia/Khandyga (UTC +09:00)","Russian Federation - Asia/Krasnoyarsk (UTC +07:00)","Russian Federation - Asia/Magadan (UTC +11:00)","Russian Federation - Asia/Novokuznetsk (UTC +07:00)","Russian Federation - Asia/Novosibirsk (UTC +07:00)","Russian Federation - Asia/Omsk (UTC +06:00)","Russian Federation - Asia/Sakhalin (UTC +11:00)","Russian Federation - Asia/Srednekolymsk (UTC +11:00)","Russian Federation - Asia/Tomsk (UTC +07:00)","Russian Federation - Asia/Ust-Nera (UTC +10:00)","Russian Federation - Asia/Vladivostok (UTC +10:00)","Russian Federation - Asia/Yakutsk (UTC +09:00)","Russian Federation - Asia/Yekaterinburg (UTC +05:00)","Russian Federation - Europe/Astrakhan (UTC +04:00)","Russian Federation - Europe/Kaliningrad (UTC +02:00)","Russian Federation - Europe/Kirov (UTC +03:00)","Russian Federation - Europe/Moscow (UTC +03:00)","Russian Federation - Europe/Samara (UTC +04:00)","Russian Federation - Europe/Saratov (UTC +04:00)","Russian Federation - Europe/Ulyanovsk (UTC +04:00)","Russian Federation - Europe/Volgograd (UTC +04:00)","Rwanda - Africa/Kigali (UTC +02:00)","Réunion - Indian/Reunion (UTC +04:00)","Saint Barthélemy - America/St_Barthelemy (UTC -04:00),Ascension and Tristan da Cunha - Atlantic/St_Helena (UTC)","Saint Helena","Saint Kitts and Nevis - America/St_Kitts (UTC -04:00)","Saint Lucia - America/St_Lucia (UTC -04:00)","Saint Martin (French part) - America/Marigot (UTC -04:00)","Saint Pierre and Miquelon - America/Miquelon (UTC -02:00)","Saint Vincent and the Grenadines - America/St_Vincent (UTC -04:00)","Samoa - Pacific/Apia (UTC +14:00)","San Marino - Europe/San_Marino (UTC +01:00)","Sao Tome and Principe - Africa/Sao_Tome (UTC)","Saudi Arabia - Asia/Riyadh (UTC +03:00)","Senegal - Africa/Dakar (UTC)","Serbia - Europe/Belgrade (UTC +01:00)","Seychelles - Indian/Mahe (UTC +04:00)","Sierra Leone - Africa/Freetown (UTC)","Singapore - Asia/Singapore (UTC +08:00)","Sint Maarten (Dutch part) - America/Lower_Princes (UTC -04:00)","Slovakia - Europe/Bratislava (UTC +01:00)","Slovenia - Europe/Ljubljana (UTC +01:00)","Solomon Islands - Pacific/Guadalcanal (UTC +11:00)","Somalia - Africa/Mogadishu (UTC +03:00)","South Africa - Africa/Johannesburg (UTC +02:00)","South Georgia and the South Sandwich Islands - Atlantic/South_Georgia (UTC -02:00)","South Sudan - Africa/Juba (UTC +03:00)","Spain - Africa/Ceuta (UTC +01:00)","Spain - Atlantic/Canary (UTC)","Spain - Europe/Madrid (UTC +01:00)","Sri Lanka - Asia/Colombo (UTC +05:30)","Sudan - Africa/Khartoum (UTC +02:00)",
                                             "Suriname - America/Paramaribo (UTC -03:00)","Svalbard and Jan Mayen - Arctic/Longyearbyen (UTC +01:00)","Swaziland - Africa/Mbabane (UTC +02:00)","Sweden - Europe/Stockholm (UTC +01:00)","Switzerland - Europe/Zurich (UTC +01:00)","Syrian Arab Republic - Asia/Damascus (UTC +03:00),Province of China - Asia/Taipei (UTC +08:00)","Taiwan","Tajikistan - Asia/Dushanbe (UTC +05:00),United Republic of - Africa/Dar_es_Salaam (UTC +03:00)","Tanzania","Thailand - Asia/Bangkok (UTC +07:00)","Timor-Leste - Asia/Dili (UTC +09:00)","Togo - Africa/Lome (UTC)","Tokelau - Pacific/Fakaofo (UTC +13:00)","Tonga - Pacific/Tongatapu (UTC +13:00)","Trinidad and Tobago - America/Port_of_Spain (UTC -04:00)","Tunisia - Africa/Tunis (UTC +01:00)","Turkey - Europe/Istanbul (UTC +03:00)","Turkmenistan - Asia/Ashgabat (UTC +05:00)","Turks and Caicos Islands - America/Grand_Turk (UTC -04:00)","Tuvalu - Pacific/Funafuti (UTC +12:00)","Uganda - Africa/Kampala (UTC +03:00)","Ukraine - Europe/Kiev (UTC +02:00)","Ukraine - Europe/Simferopol (UTC +03:00)","Ukraine - Europe/Uzhgorod (UTC +02:00)","Ukraine - Europe/Zaporozhye (UTC +02:00)","United Arab Emirates - Asia/Dubai (UTC +04:00)","United Kingdom - Europe/London (UTC)","United States - America/Adak (UTC -09:00)","United States - America/Anchorage (UTC -08:00)","United States - America/Boise (UTC -06:00)","United States - America/Chicago (UTC -05:00)","United States - America/Denver (UTC -06:00)","United States - America/Detroit (UTC -04:00)","United States - America/Indiana/Indianapolis (UTC -04:00)","United States - America/Indiana/Knox (UTC -05:00)","United States - America/Indiana/Marengo (UTC -04:00)","United States - America/Indiana/Petersburg (UTC -04:00)","United States - America/Indiana/Tell_City (UTC -05:00)","United States - America/Indiana/Vevay (UTC -04:00)","United States - America/Indiana/Vincennes (UTC -04:00)","United States - America/Indiana/Winamac (UTC -04:00)","United States - America/Juneau (UTC -08:00)","United States - America/Kentucky/Louisville (UTC -04:00)","United States - America/Kentucky/Monticello (UTC -04:00)","United States - America/Los_Angeles (UTC -07:00)","United States - America/Menominee (UTC -05:00)","United States - America/Metlakatla (UTC -08:00)","United States - America/New_York (UTC -04:00)","United States - America/Nome (UTC -08:00)","United States - America/North_Dakota/Beulah (UTC -05:00)","United States - America/North_Dakota/Center (UTC -05:00)","United States - America/North_Dakota/New_Salem (UTC -05:00)","United States - America/Phoenix (UTC -07:00)","United States - America/Sitka (UTC -08:00)","United States - America/Yakutat (UTC -08:00)","United States - Pacific/Honolulu (UTC -10:00)","United States Minor Outlying Islands - Pacific/Midway (UTC -11:00)","United States Minor Outlying Islands - Pacific/Wake (UTC +12:00)","Uruguay - America/Montevideo (UTC -03:00)","Uzbekistan - Asia/Samarkand (UTC +05:00)","Uzbekistan - Asia/Tashkent (UTC +05:00)","Vanuatu - Pacific/Efate (UTC +11:00),Bolivarian Republic of - America/Caracas (UTC -04:00)","Venezuela","Viet Nam - Asia/Ho_Chi_Minh (UTC +07:00),British - America/Tortola (UTC -04:00)","Virgin Islands,U.S. - America/St_Thomas (UTC -04:00)","Virgin Islands","Wallis and Futuna - Pacific/Wallis (UTC +12:00)","Western Sahara - Africa/El_Aaiun (UTC +01:00)","Yemen - Asia/Aden (UTC +03:00)","Zambia - Africa/Lusaka (UTC +02:00)","Zimbabwe - Africa/Harare (UTC +02:00)")

levels(data$slypos_demographics_work_or_school.factor)=c("Work","School","Neither")
levels(data$slypos_demographics_school.factor)=c("Home office/Home schooling","Face-to-face work/Face-to-face schooling","Combination of home- and face-to-face- work/schooling","Neither (no work or school, or in vacation)")
levels(data$slypos_puberty_01.factor)=c("Has not yet begun to spurt","Has barely started","Is definitely underway","Seems completed","I dont know","Prefer not to say")
levels(data$slypos_puberty_02.factor)=c("Has not yet begun","Has barely started","Is definitely underway","Seems completed","I dont know","Prefer not to say")
levels(data$slypos_puberty_03.factor)=c("Skin has not yet started changing","Skin has barely started changing","Skin changes are definitely underway","Skin changes seem complete","I dont know","Prefer not to say")
levels(data$slypos_puberty_boys_01.factor)=c("Voice has not yet started changing","Voice has barely started changing","Voice changes are definitely underway","Voice changes seem complete","I dont know","Prefer not to say")
levels(data$slypos_puberty_boys_02.factor)=c("Facial hair has not yet started growing","Facial hair has barely started growing","Facial hair growth has definitely started","Facial hair growth seems complete","I dont know","Prefer not to say")
levels(data$slypos_puberty_girl_01.factor)=c("Breasts have not yet started growing","Breasts have barely started growing","Breast growth is definitely underway","Breast growth seems complete","I dont know","Prefer not to say")
levels(data$slypos_puberty_girls_02.factor)=c("Yes","No","Prefer not to say")

levels(data$slypos_ase_001.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_002.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_003.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_004.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_005.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_006.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_007.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_attentioncheck.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_008.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_009.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_010.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_011.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_012.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")
levels(data$slypos_ase_0123.factor)=c("Strongly agree","Agree","Disagree","Strongly disagree")

levels(data$slypos_paq_1.factor)=c("Yes","No")
levels(data$slypos_paq_2.factor)=c("Yes","No")
levels(data$slypos_paq_3.factor)=c("Yes","No")
levels(data$slypos_paq_4.factor)=c("Yes","No")
levels(data$slypos_paq_5.factor)=c("Yes","No")
levels(data$slypos_paq_6.factor)=c("Yes","No")
levels(data$slypos_paq_7.factor)=c("Yes","No")
levels(data$slypos_paq_8.factor)=c("Yes","No")
levels(data$slypos_paq_9.factor)=c("Yes","No")
levels(data$slypos_paq_10.factor)=c("Yes","No")
levels(data$slypos_paq_11.factor)=c("Yes","No")
levels(data$slypos_paq_12.factor)=c("Yes","No")
levels(data$slypos_paq_13.factor)=c("Yes","No")
levels(data$slypos_paq_14.factor)=c("Yes","No")
levels(data$slypos_paq_15.factor)=c("Yes","No")
levels(data$slypos_paq_16.factor)=c("Yes","No")

levels(data$slypos_promis_sd_ped_01.factor)=c("Never","Almost never","Sometimes","Almost always","Always")
levels(data$slypos_promis_sd_ped_02.factor)=c("Never","Almost never","Sometimes","Almost always","Always")
levels(data$slypos_promis_sd_ped_03.factor)=c("Never","Almost never","Sometimes","Almost always","Always")
levels(data$slypos_promis_sd_ped_04.factor)=c("Never","Almost never","Sometimes","Almost always","Always")
levels(data$slypos_promis_si_ped_01.factor)=c("Never","Almost never","Sometimes","Almost always","Always")
levels(data$slypos_promis_si_ped_02.factor)=c("Never","Almost never","Sometimes","Almost always","Always")
levels(data$slypos_promis_si_ped_03.factor)=c("Never","Almost never","Sometimes","Almost always","Always")
levels(data$slypos_promis_si_ped_04.factor)=c("Never","Almost never","Sometimes","Almost always","Always")

levels(data$slypos_promis_sd_ad_01.factor)=c("Very poor","Poor","Fair","Good","Very good")
levels(data$slypos_promis_sd_ad_02.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$slypos_promis_sd_ad_03.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$slypos_promis_sd_ad_04.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$slypos_promis_si_ad_01.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$slypos_promis_si_ad_02.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$slypos_promis_si_ad_03.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
levels(data$slypos_promis_si_ad_04.factor)=c("Not at all","A little bit","Somewhat","Quite a bit","Very much")

levels(data$slypos_mctq_01.factor)=c("Yes","No")
levels(data$slypos_mctq_02.factor)=c("1 day per week","2 days per week","3 days per week","4 days per week","5 days per week","6 days per week","7 days per week")
levels(data$slypos_mctq_08.factor)=c("Yes","No")
levels(data$slypos_mctq_09.factor)=c("Yes","No")
levels(data$slypos_mctq_27.factor)=c("Yes","No")
levels(data$slypos_mctq_15.factor)=c("Yes","No")
levels(data$slypos_mctq_16.factor)=c("Yes","No")
levels(data$slypos_mctq_17.factor)=c("Child(ren) / pet(s)","Hobbies","Others")
levels(data$slypos_mctq_18.factor)=c("No","Yes")
levels(data$slypos_mctq_21.factor)=c("... very flexible.","... a little flexible.","... rather inflexible.","... very inflexible.")
levels(data$slypos_mctq_22.factor)=c("... within an enclosed vehicle (e.g. car, bus, underground).","... not within an enclosed vehicle (e.g. on foot, by bike).","I work at home.")

levels(data$slypos_mctq_01_ped.factor)=c("Yes","No")
levels(data$slypos_mctq_02_ped.factor)=c("1 day per week","2 days per week","3 days per week","4 days per week","5 days per week","6 days per week","7 days per week")
levels(data$slypos_mctq_03_ped_2.factor)=c("My parents have set my bedtime","I feel sleepy","I finish my homework","My TV shows are over","My brother(s) or sister(s) go to bed","I finish socializing","I get home from my job","Other")
levels(data$slypos_mctq_08_ped.factor)=c("Yes","No")
levels(data$slypos_mctq_09_ped.factor)=c("Yes","No")
levels(data$slypos_mctq_27_ped.factor)=c("Yes","No")
levels(data$slypos_mctq_15_ped.factor)=c("Yes","No")
levels(data$slypos_mctq_16_ped.factor)=c("Yes","No")
levels(data$slypos_mctq_17_ped.factor)=c("Family members / pet(s)","Hobbies","Others")
levels(data$slypos_mctq_21_ped.factor)=c("... very flexible.","... a little flexible.","... rather inflexible.","... very inflexible.")
levels(data$slypos_mctq_22_ped.factor)=c("... within an enclosed vehicle (e.g. car, bus, underground).","... not within an enclosed vehicle (e.g. on foot, by bike).","Im homeschooled.")

levels(data$slypos_leba_01.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_02.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_03.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_04.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_05.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_06.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_07.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_08.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_09.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_10.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_11.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_12.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_13.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_14.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_15.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_16.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_17.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_18.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_19.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_20.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_21.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_22.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_23.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_24.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_25.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_26.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")

levels(data$slypos_leba_attentioncheck.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")

levels(data$slypos_leba_27.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_28.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_29.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_30.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_31.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_32.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_33.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_36.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_37.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_38.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_39.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_40.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_41.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_42.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_43.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_44.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_45.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_46.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_47.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_48.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_49.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")
levels(data$slypos_leba_50.factor)=c("Does not apply/I dont know.","Never","Rarely","Sometimes","Often","Always")


# Variable selection ----------------------------------------------------------


data<- data %>% select(-starts_with("slypos_consent"),
                       -ends_with("_complete"))# add more unnecesary vars to eliminiate



## create and save dataset for analysis ---------------------------------------


save(data, file="./02_data_wrangling/data.rda")
