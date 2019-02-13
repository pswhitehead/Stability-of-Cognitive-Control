addition <- function(x,y){
  x+y
}

load.data.exp1 <- function(){
  library(dplyr)
  library(data.table)
  library(readr)
  #load the data
  data.raw<- fread("Data/Experiment1.csv")
  
  return(data.raw)
}
  
clean.simon <- function(){
  ##Clean the data
  #Make a dataframe of only the necessary variables because these datafiles are yuuuuge
  data <- data.raw %>% dplyr::select(Subject, BlockNum, Congruency, 
                                     StimSlideSimon.RT, StimSlideFlanker.RT, StimSlideStroop.RT,
                                     StimSlideSimon.ACC, StimSlideFlanker.ACC, StimSlideStroop.ACC)
  
  #subjects to include based on Whitehead et al., (2018)
  includesubs <- c(507,508,513,514,517,518,519,520,521,506,515,523,524,525,526,527,528,529,530,532,
                   533,534,535,537,539,540,541,542,544,545,546,547,548,549,551,553,555,559,560,561,
                   562,567,568,569,571,573,576,577,578,579,583,584,585,586,587,588,591,592,593,594,
                   595,596,597,599,601,602,603,605,606,607,608,609,610,611,613,614,615,616,617,619,
                   620,621,622,623,624,625,626,629,630,631,633,634,635,637,639,640,641,642,643,645,
                   647,648,649,650,651,652,654,655,656,657,658,659,660,661,662,663,664,665,666,667,
                   668,669,670,671,672,673,674,675,676,680,678,679,681,683,685,687,689,690,692,693,
                   694,696,697,698,699,700,701,702,704,705,706,708,710,711,712,713,715,716,718,719,
                   720,721,723,724,725,726,728,729,730,732,733,734,736,737,738,739,740,741)
  
  #Filter and clean data
  df.simon <- data %>% mutate(prevcon = lag(Congruency)) %>% #creat previous congruency
    mutate(acc = lag(StimSlideSimon.ACC)) %>% #create previous accuracy
    mutate(RT = (StimSlideSimon.RT)) %>% #create general RT variable
    filter(Subject %in% includesubs & StimSlideSimon.RT != "" &
             (StimSlideSimon.RT > 200 & StimSlideSimon.RT < 3000) & #liberal filter
             StimSlideSimon.ACC == 1 & prevcon != 'NA' & acc == 1 & #accuracy
             BlockNum > 2) #experimental only blocks
  
  return(df.simon)
}

clean.flanker <- function() {
  ##Clean the data
  #Make a dataframe of only the necessary variables because these datafiles are yuuuuge
  data <- data.raw %>% dplyr::select(Subject, BlockNum, Congruency, 
                                     StimSlideSimon.RT, StimSlideFlanker.RT, StimSlideStroop.RT,
                                     StimSlideSimon.ACC, StimSlideFlanker.ACC, StimSlideStroop.ACC)
  
  #subjects to include based on Whitehead et al., (2018)
  includesubs <- c(507,508,513,514,517,518,519,520,521,506,515,523,524,525,526,527,528,529,530,532,
                   533,534,535,537,539,540,541,542,544,545,546,547,548,549,551,553,555,559,560,561,
                   562,567,568,569,571,573,576,577,578,579,583,584,585,586,587,588,591,592,593,594,
                   595,596,597,599,601,602,603,605,606,607,608,609,610,611,613,614,615,616,617,619,
                   620,621,622,623,624,625,626,629,630,631,633,634,635,637,639,640,641,642,643,645,
                   647,648,649,650,651,652,654,655,656,657,658,659,660,661,662,663,664,665,666,667,
                   668,669,670,671,672,673,674,675,676,680,678,679,681,683,685,687,689,690,692,693,
                   694,696,697,698,699,700,701,702,704,705,706,708,710,711,712,713,715,716,718,719,
                   720,721,723,724,725,726,728,729,730,732,733,734,736,737,738,739,740,741)
  
  df.flanker <- data %>% mutate(prevcon = lag(Congruency)) %>%
    mutate(acc = lag(StimSlideFlanker.ACC)) %>%
    mutate(RT = (StimSlideFlanker.RT)) %>%
    filter(Subject %in% includesubs & StimSlideFlanker.RT != "" &
             (StimSlideFlanker.RT > 200 & StimSlideFlanker.RT < 3000) &
             StimSlideFlanker.ACC == 1 & prevcon != 'NA' & acc == 1 &
             BlockNum > 2)
  
  return(df.flanker)
}

clean.stroop <- function(){
  
  ##Clean the data
  #Make a dataframe of only the necessary variables because these datafiles are yuuuuge
  data <- data.raw %>% dplyr::select(Subject, BlockNum, Congruency, 
                                     StimSlideSimon.RT, StimSlideFlanker.RT, StimSlideStroop.RT,
                                     StimSlideSimon.ACC, StimSlideFlanker.ACC, StimSlideStroop.ACC)
  
  #subjects to include based on Whitehead et al., (2018)
  includesubs <- c(507,508,513,514,517,518,519,520,521,506,515,523,524,525,526,527,528,529,530,532,
                   533,534,535,537,539,540,541,542,544,545,546,547,548,549,551,553,555,559,560,561,
                   562,567,568,569,571,573,576,577,578,579,583,584,585,586,587,588,591,592,593,594,
                   595,596,597,599,601,602,603,605,606,607,608,609,610,611,613,614,615,616,617,619,
                   620,621,622,623,624,625,626,629,630,631,633,634,635,637,639,640,641,642,643,645,
                   647,648,649,650,651,652,654,655,656,657,658,659,660,661,662,663,664,665,666,667,
                   668,669,670,671,672,673,674,675,676,680,678,679,681,683,685,687,689,690,692,693,
                   694,696,697,698,699,700,701,702,704,705,706,708,710,711,712,713,715,716,718,719,
                   720,721,723,724,725,726,728,729,730,732,733,734,736,737,738,739,740,741)
  
  df.stroop <- data %>% mutate(prevcon = lag(Congruency)) %>%
    mutate(acc = lag(StimSlideStroop.ACC)) %>%
    mutate(RT = (StimSlideStroop.RT)) %>%
    filter(Subject %in% includesubs &  StimSlideStroop.RT != "" &
             (StimSlideStroop.RT > 200 & StimSlideStroop.RT < 3000) &
             StimSlideStroop.ACC == 1 & prevcon != 'NA' & acc == 1 &
             BlockNum > 2)
  
  return(df.stroop)
}