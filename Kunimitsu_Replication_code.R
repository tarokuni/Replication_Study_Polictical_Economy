###Replication Study
##Scacco, Warren, 
##"Can Social Contact Reduce Prejudice and Discrimination? 
## Evidence from a Field Experiment in Nigeria"

library(haven)
library(estimatr)
library(tidyverse)

Data_APSR <- read_dta("Data_APSR.dta")
Data_APSR_long <- read_dta("Data_APSR_long.dta")

###Replications
##Figure 
Attendance <- Data_APSR[(which(colnames(Data_APSR)=="session_attend1")):(which(colnames(Data_APSR)=="session_attend29"))]
Data_APSR$totatt <- rowSums(Attendance)

ggplot(data=Data_APSR) +
  geom_histogram(mapping = aes(x=totatt))


##Figure 3
Attendance_sum <- data.frame(colSums(Attendance, na.rm=T))
Attendance_sum$num <- 1:nrow(Attendance_sum)
names(Attendance_sum) <- c("att", "num")

ggplot(data=tibble(Attendance_sum) )+
  geom_point(mapping = aes(x=num, y=att)) +
  ylim(0,550)


##Table 2

neg_UYVT <- lm_robust(prejudice_negative_end ~ assign_AvB, data=Data_APSR )
neg_UYVT_M <- lm_robust(prejudice_negative_end ~ assign_AvB, data=Data_APSR %>% filter(religion==0))
neg_UYVT_C <- lm_robust(prejudice_negative_end ~ assign_AvB, data=Data_APSR %>% filter(religion==1))

neg_het_cl <- lm_robust(prejudice_negative_end ~ assign_CvD, data=Data_APSR )
neg_het_cl_M <- lm_robust(prejudice_negative_end ~ assign_CvD, data=Data_APSR  %>% filter(religion==0))
neg_het_cl_C <- lm_robust(prejudice_negative_end ~ assign_CvD, data=Data_APSR%>% filter(religion==1))

neg_het_pair <- lm_robust(prejudice_negative_end ~ assign_EvF, data=Data_APSR )
neg_het_pair_M <- lm_robust(prejudice_negative_end ~ assign_EvF, data=Data_APSR  %>% filter(religion==0))
neg_het_pair_C <- lm_robust(prejudice_negative_end ~ assign_EvF, data=Data_APSR %>% filter(religion==1))


##Table 3

pos_UYVT <- lm_robust(prejudice_positive_end ~ assign_AvB, data=Data_APSR )
pos_UYVT_M <- lm_robust(prejudice_positive_end ~ assign_AvB, data=Data_APSR %>% filter(religion==0))
pos_UYVT_C <- lm_robust(prejudice_positive_end ~ assign_AvB, data=Data_APSR %>% filter(religion==1))

pos_het_cl <- lm_robust(prejudice_positive_end ~ assign_CvD, data=Data_APSR )
pos_het_cl_M <- lm_robust(prejudice_positive_end ~ assign_CvD, data=Data_APSR  %>% filter(religion==0))
pos_het_cl_C <- lm_robust(prejudice_positive_end ~ assign_CvD, data=Data_APSR%>% filter(religion==1))

pos_het_pair <- lm_robust(prejudice_positive_end ~ assign_EvF, data=Data_APSR )
pos_het_pair_M <- lm_robust(prejudice_positive_end ~ assign_EvF, data=Data_APSR  %>% filter(religion==0))
pos_het_pair_C <- lm_robust(prejudice_positive_end ~ assign_EvF, data=Data_APSR %>% filter(religion==1))



##Table 4

out_UYVT <- lm_robust(prejudice_eval_end ~ assign_AvB, data=Data_APSR )
out_UYVT_M <- lm_robust(prejudice_eval_end ~ assign_AvB, data=Data_APSR %>% filter(religion==0))
out_UYVT_C <- lm_robust(prejudice_eval_end ~ assign_AvB, data=Data_APSR %>% filter(religion==1))

out_het_cl <- lm_robust(prejudice_eval_end ~ assign_CvD, data=Data_APSR )
out_het_cl_M <- lm_robust(prejudice_eval_end ~ assign_CvD, data=Data_APSR  %>% filter(religion==0))
out_het_cl_C <- lm_robust(prejudice_eval_end ~ assign_CvD, data=Data_APSR%>% filter(religion==1))

out_het_pair <- lm_robust(prejudice_eval_end ~ assign_EvF, data=Data_APSR )
out_het_pair_M <- lm_robust(prejudice_eval_end ~ assign_EvF, data=Data_APSR  %>% filter(religion==0))
out_het_pair_C <- lm_robust(prejudice_eval_end ~ assign_EvF, data=Data_APSR %>% filter(religion==1))


##Table 5

dic_UYVT <- lm_robust(h1_end ~ assign_AvB + AvB_otherh1 + otherh1, data = Data_APSR_long, clusters=primary_id ,fixed_effects = row_id)
dic_UYVT_M <- lm_robust(h1_end ~ assign_AvB + AvB_otherh1 + otherh1, data = Data_APSR_long %>% filter(religion==0), clusters=primary_id ,fixed_effects = row_id)
dic_UYVT_C <- lm_robust(h1_end ~ assign_AvB + AvB_otherh1 + otherh1, data = Data_APSR_long %>% filter(religion==1), clusters=primary_id ,fixed_effects = row_id)

dic_cl <- lm_robust(h1_end ~ assign_CvD + CvD_otherh1 + otherh1, data = Data_APSR_long, clusters=primary_id ,fixed_effects = row_id)
dic_cl_M <- lm_robust(h1_end ~ assign_CvD + CvD_otherh1 + otherh1, data = Data_APSR_long %>% filter(religion==0), clusters=primary_id ,fixed_effects = row_id)
dic_cl_C <- lm_robust(h1_end ~ assign_CvD + CvD_otherh1 + otherh1, data = Data_APSR_long %>% filter(religion==1), clusters=primary_id ,fixed_effects = row_id)

dic_pair <- lm_robust(h1_end ~ assign_EvF + EvF_otherh1 + otherh1, data = Data_APSR_long, clusters=primary_id ,fixed_effects = row_id)
dic_pair_M <- lm_robust(h1_end ~ assign_EvF + EvF_otherh1 + otherh1, data = Data_APSR_long %>% filter(religion==0), clusters=primary_id ,fixed_effects = row_id)
dic_pair_C <- lm_robust(h1_end ~ assign_EvF + EvF_otherh1 + otherh1, data = Data_APSR_long %>% filter(religion==1), clusters=primary_id ,fixed_effects = row_id)


##Table 6
#information included in t-test
cont_test <- t.test(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==0], 
       Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==1])
se_in <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==0], na.rm = T)/
            sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==0]))))
se_out <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==1], na.rm = T)/
      sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==1]))))
#(3 values and 3 standard errors)
control_balance <- c(cont_test$estimate[1], cont_test$estimate[2], cont_test$estimate[1]-cont_test$estimate[2],
                     se_in, se_out, cont_test$stderr)

cont_test_hom <- t.test(Data_APSR_long$h1_end[ Data_APSR_long$assign_CvD==0& Data_APSR_long$otherh1==0], 
                    Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==1])
se_in_hom <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==0], na.rm = T)/
            sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==0]))))
se_out_hom <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==1], na.rm = T)/
             sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==1]))))
#(3 values and 3 standard errors)
homog_balance <- c(cont_test_hom$estimate[1], cont_test_hom$estimate[2], cont_test_hom$estimate[1]-cont_test_hom$estimate[2],
                     se_in_hom, se_out_hom, cont_test_hom$stderr)


cont_test_het <- t.test(Data_APSR_long$h1_end[ Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==0], 
                        Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==1])
se_in_het <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==0], na.rm = T)/
                sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==0]))))
se_out_het <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==1], na.rm = T)/
                 sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==1]))))
#(3 values and 3 standard errors)
heterog_balance <- c(cont_test_het$estimate[1], cont_test_het$estimate[2], cont_test_het$estimate[1]-cont_test_het$estimate[2],
                   se_in_het, se_out_het, cont_test_het$stderr)


##Muslims
cont_test_M <- t.test(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 0], 
                    Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 0])
se_in_M <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 0], na.rm = T)/
            sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 0]))))
se_out_M <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 0], na.rm = T)/
             sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 0]))))
#(3 values and 3 standard errors)
control_balance_M <- c(cont_test_M$estimate[1], cont_test_M$estimate[2], cont_test_M$estimate[1]-cont_test_M$estimate[2],
                     se_in_M, se_out_M, cont_test_M$stderr)

cont_test_hom_M <- t.test(Data_APSR_long$h1_end[ Data_APSR_long$assign_CvD==0& Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 0], 
                        Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 0])
se_in_hom_M <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 0], na.rm = T)/
                sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 0]))))
se_out_hom_M <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 0], na.rm = T)/
                 sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 0]))))
#(3 values and 3 standard errors)
homog_balance_M <- c(cont_test_hom_M$estimate[1], cont_test_hom_M$estimate[2], cont_test_hom_M$estimate[1]-cont_test_hom_M$estimate[2],
                   se_in_hom_M, se_out_hom_M, cont_test_hom_M$stderr)

cont_test_het_M <- t.test(Data_APSR_long$h1_end[ Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 0], 
                        Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 0])
se_in_het_M <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 0], na.rm = T)/
                sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 0]))))
se_out_het_M <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 0], na.rm = T)/
                 sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 0]))))
#(3 values and 3 standard errors)
heterog_balance_M <- c(cont_test_het_M$estimate[1], cont_test_het_M$estimate[2], cont_test_het_M$estimate[1]-cont_test_het_M$estimate[2],
                     se_in_het_M, se_out_het_M, cont_test_het_M$stderr)


##Christians
cont_test_C <- t.test(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 1], 
                    Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 1])
se_in_C <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 1], na.rm = T)/
            sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 1]))))
se_out_C <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 1], na.rm = T)/
             sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_AvB==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 1]))))
#(3 values and 3 standard errors)
control_balance_C <- c(cont_test_C$estimate[1], cont_test_C$estimate[2], cont_test_C$estimate[1]-cont_test_C$estimate[2],
                     se_in_C, se_out_C, cont_tes_Ct$stderr)

cont_test_hom_C <- t.test(Data_APSR_long$h1_end[ Data_APSR_long$assign_CvD==0& Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 1], 
                        Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 1])
se_in_hom_C <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 1], na.rm = T)/
                sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 1]))))
se_out_hom_C <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 1], na.rm = T)/
                 sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==0 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 1]))))
#(3 values and 3 standard errors)
homog_balance_C <- c(cont_test_hom_C$estimate[1], cont_test_hom_C$estimate[2], cont_test_hom_C$estimate[1]-cont_test_hom_C$estimate[2],
                   se_in_hom_C, se_out_hom_C, cont_test_hom_C$stderr)


cont_test_het_C <- t.test(Data_APSR_long$h1_end[ Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 1], 
                        Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 1])
se_in_het_C <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 1], na.rm = T)/
                sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==0 & Data_APSR_long$religion == 1]))))
se_out_het_C <- (sd(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 1], na.rm = T)/
                 sqrt(sum(!is.na(Data_APSR_long$h1_end[Data_APSR_long$assign_CvD==1 & Data_APSR_long$otherh1==1 & Data_APSR_long$religion == 1]))))
#(3 values and 3 standard errors)
heterog_balance_C <- c(cont_test_het_C$estimate[1], cont_test_het_C$estimate[2], cont_test_het_C$estimate[1]-cont_test_het_C$estimate[2],
                     se_in_het_C, se_out_het_C, cont_test_het_C$stderr)
  

##Table 7

des_UYVT <- lm_robust(h2_end ~ assign_AvB + AvB_otherh2 + otherh2, data = Data_APSR_long, clusters=primary_id ,fixed_effects = row_id, se_type="stata")
des_UYVT_M <- lm_robust(h2_end ~ assign_AvB + AvB_otherh2 + otherh2, data = Data_APSR_long %>% filter(religion==0), clusters=primary_id ,fixed_effects = row_id, se_type="stata")
des_UYVT_C <- lm_robust(h2_end ~ assign_AvB + AvB_otherh2 + otherh2, data = Data_APSR_long %>% filter(religion==1), clusters=primary_id ,fixed_effects = row_id, se_type="stata")

des_cl <- lm_robust(h2_end ~ assign_CvD + CvD_otherh2 + otherh2, data = Data_APSR_long, clusters=primary_id ,fixed_effects = row_id, se_type="stata")
des_cl_M <- lm_robust(h2_end ~ assign_CvD + CvD_otherh2 + otherh2, data = Data_APSR_long %>% filter(religion==0), clusters=primary_id ,fixed_effects = row_id, se_type="stata")
des_cl_C <- lm_robust(h2_end ~ assign_CvD + CvD_otherh2 + otherh2, data = Data_APSR_long %>% filter(religion==1), clusters=primary_id ,fixed_effects = row_id, se_type="stata")

des_pair <- lm_robust(h2_end ~ assign_EvF + EvF_otherh2 + otherh2, data = Data_APSR_long, clusters=primary_id ,fixed_effects = row_id, se_type="stata")
dic_pair_M <- lm_robust(h2_end ~ assign_EvF + EvF_otherh2 + otherh2, data = Data_APSR_long %>% filter(religion==0), clusters=primary_id ,fixed_effects = row_id, se_type="stata")
des_pair_C <- lm_robust(h2_end ~ assign_EvF + EvF_otherh2 + otherh2, data = Data_APSR_long %>% filter(religion==1), clusters=primary_id ,fixed_effects = row_id, se_type="stata")


des_UYVT 
des_UYVT_M 
des_UYVT_C 
des_cl 
des_cl_M 
des_cl_C
des_pair 
dic_pair_M 
des_pair_C 




###Extensions

temp <- Data_APSR_long$primary_id[1]
k <- 1
Data_APSR_long$h1round <- 0
for (i in 1:length(Data_APSR_long$primary_id)){
  if(Data_APSR_long$primary_id[i]==temp & (!is.na(Data_APSR_long$otherh1[i]))){
    Data_APSR_long$h1round[i] <- k
    k <- k+1
  }else if(is.na(Data_APSR_long$otherh1[i])){
  }else{
    temp <- Data_APSR_long$primary_id[i]
    k <- 1
    Data_APSR_long$h1round[i] <- k
  }
}


temp <- Data_APSR_long$primary_id[1]
k <- 1
Data_APSR_long$h2round <- 0
for (i in 1:length(Data_APSR_long$primary_id)){
  if(Data_APSR_long$primary_id[i]==temp & (!is.na(Data_APSR_long$otherh2[i]))){
    Data_APSR_long$h2round[i] <- k
    k <- k+1
  }else if(is.na(Data_APSR_long$otherh2[i])){
  }else{
    temp <- Data_APSR_long$primary_id[i]
    k <- 1
    Data_APSR_long$h2round[i] <- k
  }
}

#long term
Data_APSR_long$h1bond <- 0
Data_APSR_long$h1bondtemp <- 0
for (i in 1:length(Data_APSR_long$primary_id)){
  if(Data_APSR_long$h1round[i]>1){
    if(Data_APSR_long$h1bondtemp[i-1]==1){
    Data_APSR_long$h1bond[i] <- 1
    Data_APSR_long$h1bondtemp[i] <- 1
    }
  }
  if(Data_APSR_long$h1round[i]>0 &Data_APSR_long$otherh1[i]==0){
    Data_APSR_long$h1bondtemp[i] <- 1
  }
}

#short term
Data_APSR_long$h1bonds <- 0
for (i in 1:length(Data_APSR_long$primary_id)){
  if(Data_APSR_long$h1round[i]>1){
    if(Data_APSR_long$otherh1[i-1]==0){
      Data_APSR_long$h1bonds[i] <- 1
    }
  }
}

#long term
Data_APSR_long$h2bond <- 0
Data_APSR_long$h2bondtemp <- 0
for (i in 1:length(Data_APSR_long$primary_id)){
  if(Data_APSR_long$h2round[i]>1){
    if(Data_APSR_long$h2bondtemp[i-1]==1){
      Data_APSR_long$h2bond[i] <- 1
      Data_APSR_long$h2bondtemp[i] <- 1
    }
  }
  if(Data_APSR_long$h2round[i]>0 &Data_APSR_long$otherh2[i]==0){
    Data_APSR_long$h2bondtemp[i] <- 1
  }
}


#short term
Data_APSR_long$h2bonds <- 0
for (i in 1:length(Data_APSR_long$primary_id)){
  if(Data_APSR_long$h2round[i]>1){
    if(Data_APSR_long$otherh2[i-1]==0){
      Data_APSR_long$h2bonds[i] <- 1
    }
  }
}



lm_robust(h1_end ~ assign_EvF * otherh1 * h1bond, data = Data_APSR_long %>% filter(religion==0), clusters = primary_id, fixed_effects = row_id, se_type = "stata")

t1 <- lm(h1_end ~ assign_AvB * otherh1 * h1bond + as.factor(row_id), data = Data_APSR_long,  se_type = "stata")
t2 <- lm(h1_end ~ assign_AvB * otherh1 * h1bond+ as.factor(row_id), data = Data_APSR_long %>% filter(religion==0), se_type = "stata")
t3 <- lm(h1_end ~ assign_AvB * otherh1 * h1bond+ as.factor(row_id), data = Data_APSR_long %>% filter(religion==1),  se_type = "stata")

t4 <- lm(h1_end ~ assign_CvD * otherh1 * h1bond+ as.factor(row_id), data = Data_APSR_long, se_type = "stata")
t5 <- lm(h1_end ~ assign_CvD * otherh1 * h1bond+ as.factor(row_id), data = Data_APSR_long %>% filter(religion==0),  se_type = "stata")
t6 <- lm(h1_end ~ assign_CvD * otherh1 * h1bond+ as.factor(row_id), data = Data_APSR_long %>% filter(religion==1),  se_type = "stata")

t7 <- lm(h1_end ~ assign_EvF * otherh1 * h1bond+ as.factor(row_id), data = Data_APSR_long,  se_type = "stata")
t8 <- lm(h1_end ~ assign_EvF * otherh1 * h1bond+ as.factor(row_id), data = Data_APSR_long %>% filter(religion==0),  se_type = "stata")
t9 <- lm(h1_end ~ assign_EvF * otherh1 * h1bond+ as.factor(row_id), data = Data_APSR_long %>% filter(religion==1), se_type = "stata")

Data_APSR_long0 <- Data_APSR_long%>% filter(religion==0)
Data_APSR_long1 <- Data_APSR_long%>% filter(religion==1)


se_prep <-  c(starprep(t1,clusters = Data_APSR_long$primary_id[!is.na(Data_APSR_long$h1_end)], se_type="stata"), 
              starprep(t2,clusters = Data_APSR_long0$primary_id[!is.na(Data_APSR_long0$h1_end)], se_type = "stata"),
              starprep(t3,clusters = Data_APSR_long1$primary_id[!is.na(Data_APSR_long1$h1_end)], se_type = "stata"),
              starprep(t4,clusters = Data_APSR_long$primary_id[!is.na(Data_APSR_long$h1_end) & !is.na(Data_APSR_long$assign_CvD)], se_type = "stata"),
              starprep(t5,clusters = Data_APSR_long0$primary_id[!is.na(Data_APSR_long0$h1_end)& !is.na(Data_APSR_long0$assign_CvD)], se_type = "stata"),
              starprep(t6,clusters = Data_APSR_long1$primary_id[!is.na(Data_APSR_long1$h1_end)& !is.na(Data_APSR_long1$assign_CvD)], se_type = "stata"),
              starprep(t7,clusters = Data_APSR_long$primary_id[!is.na(Data_APSR_long$h1_end)& !is.na(Data_APSR_long$assign_EvF)], se_type = "stata"),
              starprep(t8,clusters = Data_APSR_long0$primary_id[!is.na(Data_APSR_long0$h1_end) & !is.na(Data_APSR_long0$assign_EvF)], se_type = "stata"),
              starprep(t9,clusters = Data_APSR_long1$primary_id[!is.na(Data_APSR_long1$h1_end)& !is.na(Data_APSR_long1$assign_EvF)], se_type = "stata"))

p_prep <-  c(starprep(t1,clusters = Data_APSR_long$primary_id[!is.na(Data_APSR_long$h1_end)], stat = "p.value"), 
             starprep(t2,clusters = Data_APSR_long0$primary_id[!is.na(Data_APSR_long0$h1_end)], stat = "p.value"),
             starprep(t3,clusters = Data_APSR_long1$primary_id[!is.na(Data_APSR_long1$h1_end)], stat = "p.value"),
             starprep(t4,clusters = Data_APSR_long$primary_id[!is.na(Data_APSR_long$h1_end) & !is.na(Data_APSR_long$assign_CvD)], stat = "p.value"),
             starprep(t5,clusters = Data_APSR_long0$primary_id[!is.na(Data_APSR_long0$h1_end)& !is.na(Data_APSR_long0$assign_CvD)],stat = "p.value"),
             starprep(t6,clusters = Data_APSR_long1$primary_id[!is.na(Data_APSR_long1$h1_end)& !is.na(Data_APSR_long1$assign_CvD)], stat = "p.value"),
             starprep(t7,clusters = Data_APSR_long$primary_id[!is.na(Data_APSR_long$h1_end)& !is.na(Data_APSR_long$assign_EvF)], stat = "p.value"),
             starprep(t8,clusters = Data_APSR_long0$primary_id[!is.na(Data_APSR_long0$h1_end) & !is.na(Data_APSR_long0$assign_EvF)], stat = "p.value"),
             starprep(t9,clusters = Data_APSR_long1$primary_id[!is.na(Data_APSR_long1$h1_end)& !is.na(Data_APSR_long1$assign_EvF)], stat = "p.value"))


stargazer(t1, t2, t3, t4,t5, t6, t7, t8, t9,
          se = se_prep,
          p = p_prep,
          omit.stat = "f")

