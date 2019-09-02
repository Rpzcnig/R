library(tidyverse)
library(ggpubr)
library(lubridate)
library(Hmisc)
data0 <- data
data0$nationality <- case_when(
  data0$nationality == 1 ~ 1,
  TRUE ~ 2
) #考虑到汉族，最多，其他族人少，就直接合并除汉族外其他族为‘2’
data0$survey_time <- sapply(str_split(data0$survey_time, pattern = ' '), '[', 1) %>%
  ymd(.) %>% month(.)
data0$religion <- if_else(data0$religion == -8, 0, data0$religion) #将宗教无法回答的置为否。
data0$religion_freq <- case_when(data0$religion_freq <= 1 ~ 1, 
                                 data0$religion_freq == 2 ~ 2,
                                 data0$religion_freq == 3 ~ 3,
                                 data0$religion_freq == 4 ~ 4,
                                 T ~ 5
                                 ) 
data0$edu <- case_when(data0$edu == -8 ~ 4,
                       data0$edu <= 4 ~ data0$edu,
                       data0$edu <= 9 ~ 5,
                       data0$edu <= 12 ~ 6,
                       TRUE ~ 7
) #将-8无法回答的置为4，1,2,3,4不变，5,6,7,8,9置为5,101112置为6,13,14置为7.分成6类
#教育其他，考虑到95%以上的缺失值，直接删除该变量
############################################################################
knn插值

library(dbscan)
edu_status_na <- function(x, y, z, k = 7) {
  d <- rbind(y,x) %>% 
    scale()
  knn <- kNN(d, k)
  stt <- NULL
  st <- knn$id %>% 
    data.frame()
  for(i in 1:k) {
    stt[i] <- z[st[1,i]]
  }
  stt <- as.tibble(stt)
  stt <- stt[order(stt$value),]
  return (distinct(stt)[[1]][which.max(group_by(stt,value) %>% group_size())])
}
x <- data0[-which(is.na(data0$edu_status)),c("birth", "edu", "survey_type")] %>% data.frame()
y <- data0[which(is.na(data0$edu_status)),c("birth", "edu", "survey_type")] %>% data.frame()
z <- data0$edu_status[-which(is.na(data0$edu_status))]
tt <- apply(y, 1, edu_status_na, x=x,z=z,k = 3) %>% 
  data.frame() %>% 
  mutate(id = which(is.na(data0$edu_status)))
data0$edu_status <- data0[,c('edu_status','id')] %>% 
  data.frame() %>% left_join(tt, by = 'id') %>%
  sapply( function(x){if_else(is.na(x), 0, x)}) %>% 
  data.frame() %>%
  select(-c('id')) %>% 
  apply(1,sum)
data0$edu_status[which(data0$edu_status == -8)] <- 4 

##################################################################################
data0$birth <- 2015-data0$birth #根据调查时间为2015，用其减去出生年月，转化得到年龄，
data0$income[which(data0$income < 0 )] <- mean(data0$income[-which(data0$income < 0)])
data0$inc_exp <- if_else(data0$inc_exp < 0, data0$income, data0$inc_exp)
data0$inc_exp <- log(data0$inc_exp+100)  #正态分布变换


data0$political[which(data0$political == -8)] <-  1  #将无法回答的划到群众里面去
data0$political[which(data0$political == 3)] <- 2 #将民主党派，划分到共青团员 

data0$health[which(data0$health == -8)] <- 4 
data0$health_problem[which(data0$health_problem == -8)] <- 4  
data0$depression[which(data0$depression == -8)] <- 4


data0$media_1[which(data0$media_1== -8)] <- 1 #媒体把-8类用众数代替
data0$media_2[which(data0$media_2== -8)] <- 1
data0$media_3[which(data0$media_3== -8)] <- 1
data0$media_4[which(data0$media_4== -8)] <- 4
data0$media_5[which(data0$media_5== -8)] <- 1
data0$media_6[which(data0$media_6== -8)] <- 1

#which(data0$leisure_1 == -8)
data0$leisure_1[which(data0$leisure_1== -8)] <- 1
data0$leisure_2[which(data0$leisure_2== -8)] <- 5
data0$leisure_3[which(data0$leisure_3== -8)] <- 3
data0$leisure_4[which(data0$leisure_4== -8)] <- 5
#data0$leisure_5[which(data0$leisure_5== -8)] <- 5
data0$leisure_7[which(data0$leisure_7== -8)] <- 1
data0$leisure_8[which(data0$leisure_8== -8)] <- 5
data0$leisure_9[which(data0$leisure_9== -8)] <- 5
data0$leisure_10[which(data0$leisure_10== -8)] <- 5
data0$leisure_11[which(data0$leisure_11== -8)] <- 5
data0$leisure_12[which(data0$leisure_12== -8)] <- 5
data0$socialize[which(data0$socialize== -8)] <- 3 #用众数3 补充-8
data0$relax[which(data0$relax== -8)] <- 4
data0$learn[which(data0$learn== -8)] <- 1
#chisq.test(table(data0$social_friend,data0$social_neighbor))
data0$socia_outing[which(data0$socia_outing== -8)] <- 1
data0$equity[which(data0$equity== -8)] <- 4
train_data$happiness[which(train_data$happiness== -8)] <- 4



data0$class[which(data0$class== -8)] <- 5
data0$class_10_before[which(data0$class_10_before== -8)] <- 3
data0$class_10_after <- if_else(data0$class_10_after == -8, 
                                data0$class, 
                                data0$class_10_after) ##将无法告诉的用现在的class补

data0$class_14[which(data0$class_14== -8)] <- 2


data0$work_status <- if_else(is.na(data0$work_status), 0, data0$work_status)
data0$work_status <- case_when(data0$work_status == -8 ~ 3,
                               data0$work_status <= 4 ~ data0$work_status,
                               data0$work_status <=8 ~ 5,
                               T ~ 6)

data0$work_yr <- if_else(is.na(data0$work_yr) | data0$work_yr == -1, 
                         0, data0$work_yr)

data0$work_yr <- if_else(data0$work_yr < -1, 
                         mean(data0$work_yr[-which(data0$work_yr < 0)]), data0$work_yr)
data0$hukou <- case_when(
  data0$hukou == 1 ~ 1,
  data0$hukou == 2 ~ 2,
  data0$hukou == 4 ~ 3,
  data0$hukou == 5 ~ 4,
  TRUE ~ 5
)  ##由于3,6,7,8类人数较少，将这些都归为其他，重新编号，减少后面的onehot。

data0$work_type[which(data0$work_type== -8)] <- 1
data0$work_type <- if_else(is.na(data0$work_type), 0, data0$work_type)

data0$work_manage <- if_else(is.na(data0$work_manage), 0, data0$work_manage)
data0$work_manage[which(data0$work_manage == -8)] = 3
data0$insur_1 <- if_else(data0$insur_1 < 0, 2, data0$insur_1)
data0$insur_2 <- if_else(data0$insur_2 < 0, 2, data0$insur_2)
data0$insur_3 <- if_else(data0$insur_3 < 0, 2, data0$insur_3)

data0$family_m[which(data0$family_m == 50)] = 5
data0$family_m[which(data0$family_m < 0)] = 2 
data0$family_status[which(data0$family_status == -8)] = 3 

data0$s_income <- if_else(is.na(data0$s_income) | data0$s_income == -1, 0, data0$s_income) #
data0$s_income[which(data0$s_income < 0)] <- median(data0$s_income[-which(data0$s_income < 0)])

data0$family_income[is.na(data0$family_income)] <- data0$income[which(is.na(data0$family_income))] +
  data0$s_income[which(is.na(data0$family_income))]  ##缺失值一个，用自己收入和配偶的收入总和作为家庭收入
  # median(data0$family_income[-which(data0$family_income < 0)], 
  #        na.rm = T) #考虑到平均有较大差异，用中位数4000补na。


data0$family_income[which(data0$family_income == -1)] <- 0 
lmmodel <- lm(family_income~income + s_income+class+family_status,
              data0[-which(data0$family_income < 0),])
#data0$s_income <- log(data0$s_income+1000)
data0$family_income[which(data0$family_income < 0)]
prefamily <- predict(lmmodel, data0[which(data0$family_income < 0),
                                    c("income", "s_income", "class", "family_status")]) %>%
  data.frame() %>% 
  mutate(id = which((data0$family_income < 0)))

data0$family_income <- data0[,c('family_income','id')] %>% 
  data.frame() %>% left_join(prefamily, by = 'id') %>%
  sapply( function(x){if_else(is.na(x), 0, x)}) %>% 
  data.frame() %>%
  select(-c('id')) %>% 
  apply(1,sum)   ##family_income,考虑到有较多的无法回答和拒绝回答，若是都用中位数填补会使得该变量变差，所以选择几个与因变量相关度较高的变量进行简单线性回归拟合。
data0$family_income[which(data0$family_income < 0)] <- median(data0$family_income[-which(data0$family_income < 0)], 
                                                            na.rm = T) 

data0$son[which(data0$son == -8)] <- 1   #将-8的补为众数1
data0$daughter[which(data0$daughter == -8)] <- 0   #将-8补为众数0  ，后面可以将儿子和女儿加起来作为新的特征

data0$minor_child[which(data0$minor_child == -8)] <- 0  #将-8 补为众数0
data0$minor_child <- if_else(is.na(data0$minor_child), 0, data0$minor_child) #经查看，na值都是没有儿子女儿的，所以设置为0.


data0$house[which(data0$house == -1)] <- 0 #将不适用的补为0，其他-1-2补为众数。
data0$house[which(data0$house < -1)] <- 1
data0$car[which(data0$car < 0)] <- 2

data0$marital_1st <- 2015-data0$marital_1st  #先将第一次结婚时间换位，距2015多久。
data0$marital_1st <- if_else(is.na(data0$marital_1st), 0, data0$marital_1st) #将未婚的na补为0
data0$marital_1st[which(data0$marital_1st > 2015)] <- mean(
  data0$marital_1st[-which(data0$marital_1st > 2015)])  ##将时间<0的置为均值

data0$marital_now <- 2015 - data0$marital_now  #先将第一次结婚时间换位，距2015多久。
data0$marital_now <- if_else(is.na(data0$marital_now), 0, data0$marital_now) #将未婚的na补为0
data0$marital_now[which(data0$marital_now > 2015)] <- mean(
  data0$marital_now[-which(data0$marital_now > 2015)])  ##将时间<0的置为均值
ggplot(train_data, aes(as.character(f_work_14), happiness)) + geom_boxplot()
data0$marital <- case_when(data0$marital == 1 ~ 1,
                           data0$marital <= 3 ~ 2,
                           data0$marital <= 6 ~ 3,
                           T ~ 4)
data0$s_edu <- if_else(data0$s_edu == -8,
                       4, data0$s_edu)    #将无法回答的用众数补，

data0$s_edu <- if_else(is.na(data0$s_edu),
                       0, data0$s_edu) #将缺失值归为单独一类，
data0$s_political[which(data0$s_political == -8)] <-  1  #将无法回答的划到群众里面去
data0$s_political <- if_else(is.na(data0$s_political),
                             0, data0$s_political) 
data0$s_hukou[is.na(data0$s_hukou)] <- 0
data0$s_hukou[which(data0$s_hukou == -8)] <-1
data0$s_hukou <- case_when(data0$s_hukou == 1 ~ 1,
                           data0$s_hukou == 2 ~ 2,
                           data0$s_hukou == 4 ~ 3,
                           data0$s_hukou == 5 ~ 4,
                           T ~ 5)
data0$s_work_exper <- if_else(is.na(data0$s_work_exper),
                              0, data0$s_work_exper) #将缺失值补为新的一类。


data0$f_birth <- if_else(data0$f_birth < 0, 0, 1) # 知道为1，不知道为0
data0$f_political <- if_else(data0$f_political == -8, 1, data0$f_political) #补为众数。
data0$f_political <- if_else(data0$f_political == 4, 3, data0$f_political) #将34合并。

data0$status_peer[which(data0$status_peer == -8)] <- 2
data0$status_3_before[which(data0$status_3_before == -8)] <- 2
#ggplot(train_data, aes(as.character(m_political), happiness)) + geom_boxplot()
data0$view[which(data0$view == -8)] <- 4  # 将无法告诉的补为4  
data0$inc_ability[which(data0$inc_ability == -8)] <- 2    #强相关变量


data0$trust_1[which(data0$trust_1 == -8)] <- 4
data0$trust_2[which(data0$trust_2 == -8)] <- 4
data0$trust_3[which(data0$trust_3 == -8)] <- 4
data0$trust_5[which(data0$trust_5 == -8)] <- 5
data0$trust_7[which(data0$trust_7 == -8)] <- 3
data0$trust_9[which(data0$trust_9 == -8)] <- 2
data0$trust_13[which(data0$trust_13 == -8)] <- 1
data0$trust_4[which(data0$trust_4 == -8)] <- 0
data0$trust_10[which(data0$trust_10 == -8)] <- 0
data0$trust_6[which(data0$trust_6 == -8)] <- 0
data0$trust_8[which(data0$trust_8 == -8)] <- 0
data0$trust_11[which(data0$trust_11 == -8)] <- 0
data0$trust_12[which(data0$trust_12 == -8)] <- 0
data0$neighbor_familiarity[which(data0$neighbor_familiarity == -8)] <- 4

#which(data0$public_service_1==-3)
data0$public_service_1[which(data0$public_service_1 < 0)] <- 
  mean(data0$public_service_1[-which(data0$public_service_1 < 0)]) 
data0$public_service_2[which(data0$public_service_2 < 0)] <- 
  mean(data0$public_service_2[-which(data0$public_service_2 < 0)])
data0$public_service_3[which(data0$public_service_3 < 0)] <- 
  mean(data0$public_service_3[-which(data0$public_service_3 < 0)])
data0$public_service_4[which(data0$public_service_4 < 0)] <- 
  mean(data0$public_service_4[-which(data0$public_service_4 < 0)])
data0$public_service_5[which(data0$public_service_5 < 0)] <- 
  mean(data0$public_service_5[-which(data0$public_service_5 < 0)])
data0$public_service_6[which(data0$public_service_6 < 0)] <- 
  mean(data0$public_service_6[-which(data0$public_service_6 < 0)])
data0$public_service_7[which(data0$public_service_7 < 0)] <- 
  mean(data0$public_service_7[-which(data0$public_service_7 < 0)])
data0$public_service_8[which(data0$public_service_8 < 0)] <- 
  mean(data0$public_service_8[-which(data0$public_service_8 < 0)])
data0$public_service_9[which(data0$public_service_9 < 0)] <- 
  mean(data0$public_service_9[-which(data0$public_service_9 < 0)])
##补上
#data0$m_work_14[which(data0$m_work_14 == -8)] <- 0
data0$m_work_14 <- case_when(data0$m_work_14 == -8 ~ 0,
                             data0$m_work_14 == 1 ~ 1,
                             data0$m_work_14 == 2 ~ 2,
                             data0$m_work_14 <= 9 ~ 3,
                             data0$m_work_14 == 10 ~ 4,
                             data0$m_work_14 <= 12 ~ 5,
                             data0$m_work_14 == 13 ~ 6,
                             data0$m_work_14 == 14 ~ 7,
                             data0$m_work_14 == 15 ~ 8,
                             data0$m_work_14 == 16 ~ 9,
                             T ~ 10
                             )
data0$m_political <- case_when(data0$m_political <=1 ~ 1,
                               data0$m_political ==2 ~ 2,
                               T ~ 3)
data0$f_work_14[which(data0$f_work_14 == -8)] <- 0
data0$f_edu[which(data0$f_edu == -8)] <- 0
data0$insur_3[which(data0$insur_3 == -1)] <- 0
data0$leisure_6[which(data0$leisure_6 == -8)] <- 0
data0$health_problem[which(data0$health_problem == -8)] <- 4
ggplot(data0, aes(m_political)) + geom_bar()

data0$social_friend[which(data0$social_friend == -8)] <- 3
data0$social_friend[which(is.na(data0$social_friend))] <- case_when(
  data0$socia_outing[which(is.na(data0$social_friend))] == 1 ~ 7,
  T ~ 3
)
data0$social_neighbor[which(data0$social_neighbor == -8)] <- 2
data0$social_neighbor[which(is.na(data0$social_neighbor))] <- case_when(
  data0$socia_outing[which(is.na(data0$social_neighbor))] == 1 ~ 7,
  T ~ 2
)  #因为观察到这两个都是同时缺失，并且发现，大多数都是从未出过门的，所以联系起来。

#data1.0$edu_status <- if_else(is.na(data1.0$edu_status), 0, 1)

