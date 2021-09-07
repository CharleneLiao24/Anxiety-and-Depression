library(readxl)
library(ggplot2)
rmd = read_excel("research_method_data.xlsx", sheet = "data_for_model")
View(rmd)

# 檢查焦慮與憂鬱分數超高的兩位：
rmd[rmd$ad_sum == 29,c(1,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85)]



# ============================================================================================
# 處理變數

#將sex 的 coding改為0、1
rmd$sex = replace(rmd$sex, rmd$sex == "M", 0)
rmd$sex = replace(rmd$sex, rmd$sex == "F", 1)

# > summary(rmd$b2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.500   3.000   3.405   5.000  15.000 
# > summary(rmd$b2[rmd$ad_sum>14])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.50    2.00    3.00    3.61    5.00   13.00 
# > summary(rmd$b2[rmd$ad>=1])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.500   2.000   3.000   3.547   5.000  14.000 
# > summary(rmd$b2[rmd$ad>=2])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.700   2.000   3.000   3.949   5.000  13.000 

# 看男生的數據
rmd[rmd$sex == 0,c(1,2,3,4,5,6,7,9,76,77,81)]
# ============================================================================================


# 查看是否有 na
sum(is.na(rmd$sex))
sum(is.na(rmd$age))
sum(is.na(rmd$edu))
sum(is.na(rmd$job))
sum(is.na(rmd$salary)) # 3
sum(is.na(rmd$marriage))
sum(is.na(rmd$children))
# a8
sum(is.na(rmd$trad_dummy))
sum(is.na(rmd$a8_7))
# a9
sum(is.na(rmd$health))
# a10
sum(is.na(rmd$a10_1)) # 2
sum(is.na(rmd$a10_2))
sum(is.na(rmd$a10_3))
sum(is.na(rmd$a10_4))
sum(is.na(rmd$a10_5))
sum(is.na(rmd$a10_6))
sum(is.na(rmd$a10_7))
sum(is.na(rmd$a10_8))
sum(is.na(rmd$a10_9))
sum(is.na(rmd$a10_10))
sum(is.na(rmd$a10_11))
#a11
sum(is.na(rmd$a11_1))
sum(is.na(rmd$a11_2))
sum(is.na(rmd$a11_3))
sum(is.na(rmd$a11_4))
sum(is.na(rmd$a11_5))
sum(is.na(rmd$a11_6))
sum(is.na(rmd$a11_7))
sum(is.na(rmd$a11_8))
sum(is.na(rmd$a11_9))
sum(is.na(rmd$a11_10)) # 1
sum(is.na(rmd$a11_11)) # 此題每一位受試者都選擇否，因此不會出現在model中
sum(is.na(rmd$a11_12)) # 1
# a12
sum(is.na(rmd$a12_1))
sum(is.na(rmd$a12_2))
sum(is.na(rmd$a12_3))
sum(is.na(rmd$a12_4))
sum(is.na(rmd$a12_5))
sum(is.na(rmd$a12_6)) # 1
sum(is.na(rmd$a12_7))
sum(is.na(rmd$a12_8))
# a13(有點不想放)
sum(is.na(rmd$a13_1))
sum(is.na(rmd$a13_2))
sum(is.na(rmd$a13_3))
sum(is.na(rmd$a13_4))

# b 大題的NA是.，不能用這個式子判斷(後來手動改excel了，因為沒辦法解決文字轉成數字的問題)
sum(is.na(rmd$b1))
sum(is.na(rmd$b2)) # 1
sum(is.na(rmd$b3)) # 143
sum(is.na(rmd$b4)) # 6
sum(is.na(rmd$b5)) # 3
sum(is.na(rmd$b6))

# for Y ，反應變數有遺失值不能補，要刪除
sum(is.na(rmd$anxiety_sum)) # 1
sum(is.na(rmd$depression_sum)) # 2

# ============================================================================================

# 處理 NA-1 (anxiety)
A = rmd[is.na(rmd$anxiety_sum),]
rmd[is.na(rmd$anxiety_sum),][,c(1, c(67,69,71,73,75,77,79,81))]
#       id HADS1 HADS3 HADS5 HADS7 HADS9 HADS11 HADS13 anxiety_sum
#       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>
#   1    35     1    NA     0     1     1      1      1     NA
sum(rmd[is.na(rmd$anxiety_sum),][1,c(67,69,71,73,75,77,79,81)], na.rm=TRUE) # 5
rmd[35, 81] = round(sum(A[1,c(67,69,71,73,75,77,79,81)], na.rm=TRUE)*(7/6), 0)
rmd[35,c(1, c(67,69,71,73,75,77,79,81))]


# 處理 NA-2(depression)
B = rmd[is.na(rmd$depression_sum),]
rmd[is.na(rmd$depression_sum),][,c(1,c(68,70,72,74,76,78,80,82))]
#         id  HADS2 HADS4 HADS6 HADS8 HADS10 HADS12 HADS14   depression_sum
#       <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>        <dbl>
#   1    24     0     2     1     1      0     NA      1            NA
#   2   291     1     2    NA     1      0      0      1            NA
sum(rmd[is.na(rmd$depression_sum),][1,c(68,70,72,74,76,78,80,82)], na.rm=TRUE) # 5
sum(rmd[is.na(rmd$depression_sum),][2,c(68,70,72,74,76,78,80,82)], na.rm=TRUE) # 5
rmd[24, 82] = round(sum(B[1,c(68,70,72,74,76,78,80,82)], na.rm=TRUE)*(7/6), 0)
rmd[291,82] = round(sum(B[2,c(68,70,72,74,76,78,80,82)], na.rm=TRUE)*(7/6), 0)
rmd[c(24, 291),c(1, c(68,70,72,74,76,78,80,82))]


# 處理 NA-3(salary)
rmd[is.na(rmd$salary),]
rmd[c(212, 364, 391),1:10]
# A tibble: 3 x 10
#       id    sex     age   edu   job   salary    marriage `child-boy` `child-girl` children
#     <dbl> <chr>     <dbl> <dbl> <dbl>  <dbl>    <dbl>       <dbl>        <dbl>    <dbl>
#   1   212   F         3     4     3     NA        0           0            1        1
#   2   364   F         4     2     4     NA        0           0            0        0
#   3   391   F         3     5     4     NA        0           0            0        0
# find 212
rmd[c(rmd$sex == "1"& rmd$age == 3& rmd$edu == 4& rmd$job == 3),]
rmd[c(rmd$sex == "1"& rmd$age == 4& rmd$job == 4),]
rmd[c(rmd$sex == "1"& rmd$age == 3& rmd$edu == 5& rmd$job == 4& rmd$children == 0),]
# 212 補 3
rmd[212, 6] = sum(rmd[c(rmd$sex == "1"& rmd$age == 3& rmd$edu == 4& rmd$job == 3),][,6], na.rm=TRUE)
# 364 補 5
rmd[364, 6] = round(sum(rmd[c(rmd$sex == "1"& rmd$age == 4& rmd$job == 4),][,6], na.rm=TRUE)/15, 0)
# 391 補 4
rmd[391, 6] = round(sum(rmd[c(rmd$sex == "1"& rmd$age == 3& rmd$edu == 5& rmd$job == 4& rmd$children == 0),][,6], na.rm=TRUE)/13, 0)


# 處理 NA-4(a10_sum--physical)
rmd[is.na(rmd$a10_sum),]
rmd[c(63, 190),c(1, c(24,25,26,27,28,29,30,31,32,33,34,35))]
# 63 補 0 (猜測，因為有勾其他，可能不是漏答)
rmd[63, 24] = 0
# 190 補 0 (有勾以上皆非)
rmd[190, 24] = 0


# 處理 NA-5(a11_10--mental)
rmd[is.na(rmd$a11_sum),]
rmd[287,c(1,c(36,37,38,39,40,41,42,43,44,45,46,47,48))]
# 287 補 0 (只有選疲倦，其他皆無)
rmd[287, 45] = 0


# 處理 NA-6(a12_6-habit_exercise)
rmd[is.na(rmd$a12_6),] # 245
rmd[245,c(1,c(49,50,51,52,53,54,55,56))]
# 選了以上皆非，所以捕0
rmd[245,54] = 0



# 處理 NA-7
# 決定不採用b3(接受人工生殖治療總月數)
# 決定刪除322、323、328
# 只補 b4(84、95、98的遺失值)
rmd[is.na(rmd$b2),][,c(1, 61, 62, 63, 64, 65, 66)] # 1：322
rmd[is.na(rmd$b3),][,c(1, 61, 62, 63, 64, 65, 66)] # 143
rmd[is.na(rmd$b4),][,c(1, 61, 62, 63, 64, 65, 66)] # 6：84、95、98、322、323、328
rmd[is.na(rmd$b5),][,c(1, 61, 62, 63, 64, 65, 66)] # 3：322、323、328
# 補84、95、98的遺失值(補眾數0，如果不好直接刪也行)
rmd[84,64] = 0
rmd[95,64] = 0
rmd[98,64] = 0
rmd[c(84, 95, 98), c(1, 61, 62, 63, 64, 65, 66)]


# 處理 NA-7
# 刪除322、323、328(最後一步才能做，不然會錯)
rmd = rmd[-322,]
rmd = rmd[-322,] # 323
rmd = rmd[-326,] # 328


# 刪除y有遺失值的樣本 3
rmd = rmd[-c(24,35,291),]

# 刪除男性的樣本 24
#print(rmd$id[rmd$sex==0])
rmd = rmd[-c(8,34,61,62,63,151,174,177,182,184,190,236,255,269,286,288,
             294,295,296,328,334,336,340,390),]
#rmd[c(8,34,61,62,63,151,174,177,182,184,190,236,255,269,286,288,
#      294,295,296,328,334,336,340,390),]
View(rmd)


# 刪除b3
rmd = rmd[,-63]


# 將第8題再分類
# (trad_dummy代表有勾選a8_1到a8_6任意項的人，代表有傳統觀念)
# (a8_7是想要有自己的小孩，代表不是外在壓力)
# (fertility_concept為自創代表想看有傳統觀念vs想要自己的小孩或是皆有或皆無)
rmd$fertility_concept[ rmd$trad_dummy == 0 & rmd$a8_7 == 0 ] = 0   # 皆無
rmd$fertility_concept[ rmd$trad_dummy == 1 & rmd$a8_7 == 0 ] = 1   # 有傳統觀念
rmd$fertility_concept[ rmd$trad_dummy == 0 & rmd$a8_7 == 1 ] = 2   # 想要自己的小孩
rmd$fertility_concept[ rmd$trad_dummy == 1 & rmd$a8_7 == 1 ] = 3   # 皆有


# 將第10題加總，得到的是總身體疾病數
rmd$physical = rmd$a10_1 + rmd$a10_2 + rmd$a10_3 + rmd$a10_4 + rmd$a10_5 +
  rmd$a10_6 + rmd$a10_7 + rmd$a10_8 + rmd$a10_9 + rmd$a10_10
# rmd$disease_none = rmd$a10_11

# 將第11題加總，得到的是總心理壓力數
rmd$mental = rmd$a11_1 + rmd$a11_2 + rmd$a11_3 + rmd$a11_4 + rmd$a11_5 +
  rmd$a11_6 + rmd$a11_7 + rmd$a11_8 + rmd$a11_9 + rmd$a11_10 + rmd$a11_11
# rmd$mental_none = rmd$a11_12

# 將第12題：將a12_1~a12_3選出，若有任一項有則 habit_bad 為1，否則為0
rmd$habit_bad = rmd$a12_1 + rmd$a12_2 + rmd$a12_3
rmd$habit_exercise = rmd$a12_6

# -or-(因為細格數問題最後併完變成這樣哈哈)
rmd$habit_bad[ rmd$a12_1 + rmd$a12_2 + rmd$a12_3 == 0 ] = 0
rmd$habit_bad[ rmd$a12_1 + rmd$a12_2 + rmd$a12_3 == 1 ] = 1
rmd$habit_bad[ rmd$a12_1 + rmd$a12_2 + rmd$a12_3 == 2 ] = 1
rmd$habit_bad[ rmd$a12_1 + rmd$a12_2 + rmd$a12_3 == 3 ] = 1
#rmd$habit_other = rmd$a12_7
# rmd$habit_none = rmd$a12_8

# 將第13題：暫不更動

# 將第b題：暫不更動

# ============================================================================================
# 新增變數
# anxiety(1-2)
rmd$anxiety[ rmd$anxiety_sum <= 7 ] = 1
rmd$anxiety[ rmd$anxiety_sum >= 8 & rmd$anxiety_sum <= 10 ] = 2
rmd$anxiety[ rmd$anxiety_sum >= 11 ] = 3
table(rmd$anxiety) #檢視交叉表，確定是否完成分類
#rmd = cbind(rmd, cbind(rmd$anxiety)) #將新變數加到原本的資料檔

# depression(1-3)
rmd$depression[ rmd$depression_sum <= 7 ] = 1
rmd$depression[ rmd$depression_sum >= 8 & rmd$depression_sum <= 10 ] = 2
rmd$depression[ rmd$depression_sum >= 11 ] = 3
table(rmd$depression) #檢視交叉表，確定是否完成分類
#rmd = cbind(rmd, cbind(rmd$depression)) #將新變數加到原本的資料檔

# anxiety + depression (total score)
rmd$ad_sum = rmd$anxiety_sum + rmd$depression_sum
#rmd = cbind(rmd, cbind(rmd$ad_sum))

# anxiety + depression (1-3)
# 將醫學上沒有焦慮與沒有憂鬱傾向或得病的設為0，有的設為1
# 目前分0-2三類
rmd$ad[ rmd$anxiety == 1 & rmd$depression == 1 ] = 0  # (完全)無焦慮與憂鬱
rmd$ad[ rmd$anxiety == 2 & rmd$depression == 1  |  rmd$anxiety == 1 & rmd$depression == 2 ] = 1 # 有疑似焦慮/憂鬱
rmd$ad[ rmd$anxiety == 3 & rmd$depression == 1  |  rmd$anxiety == 1 & rmd$depression == 3 ] = 1 # 有焦慮/憂鬱
rmd$ad[ rmd$anxiety == 2 & rmd$depression == 2 ] = 1  # 疑似焦慮且疑似憂鬱
rmd$ad[ rmd$anxiety == 3 & rmd$depression == 2  |  rmd$anxiety == 2 & rmd$depression == 3 ] = 2 # 疑似焦慮/憂鬱且有焦慮/憂鬱
rmd$ad[ rmd$anxiety == 3 & rmd$depression == 3 ] = 2  # 有焦慮且有憂鬱
table(rmd$ad) #檢視交叉表，確定是否完成分類
# 0   1   2 
# 242  85  43 





# ============================================================================================

# EDA 的畫圖
# for Y
ggplot(data = rmd) +
  geom_bar(mapping = aes(x = ad), fill = "royalblue4")
table(rmd$ad)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = ad_sum), fill = "royalblue4")
table(rmd$ad_sum)

# for X
ggplot(data = rmd) +
  geom_bar(mapping = aes(x = sex), fill = "royalblue4")
table(rmd$sex)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = age), fill = "royalblue4")
table(rmd$age)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = edu), fill = "royalblue4")
table(rmd$edu)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = job), fill = "royalblue4")
table(rmd$job)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = salary), fill = "royalblue4")
table(rmd$salary)

boxplot(rmd$marriage, data = rmd, fill = "royalblue4")
table(rmd$marriage)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = children), fill = "royalblue4")
table(rmd$children)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = fertility_concept), fill = "royalblue4")
table(rmd$fertility_concept)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = health), fill = "royalblue4")
table(rmd$health)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = physical), fill = "royalblue4")
table(rmd$physical)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = mental), fill = "royalblue4")
table(rmd$mental)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = habit_bad), fill = "royalblue4")
table(rmd$habit_bad)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = habit_exercise), fill = "royalblue4")
table(rmd$habit_exercise)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = b1), fill = "royalblue4")
table(rmd$b1)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = b2), fill = "royalblue4")
table(rmd$b2)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = b4), fill = "royalblue4")
table(rmd$b4)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = b5), fill = "royalblue4")
table(rmd$b5)

ggplot(data = rmd) +
  geom_bar(mapping = aes(x = b6), fill = "royalblue4")
table(rmd$b6)

# ============================================================================================

# 看X間的相關性多強
cor.test(x = rmd$edu, y = rmd$salary, method = 'spearman')
# Spearman's rank correlation rho
# 
# data:  rmd$edu and rmd$salary
# S = 6384184, p-value = 1.069e-15
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3878076
table(rmd$edu, rmd$salary)


cor.test(x = rmd$edu, y = rmd$job, method = 'spearman')
# Spearman's rank correlation rho
# 
# data:  rmd$edu and rmd$job
# S = 6592582, p-value = 2.432e-15
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3819416
table(rmd$edu, rmd$job)

cor.test(x = rmd$salary, y = rmd$job, method = 'spearman')
# Spearman's rank correlation rho
# 
# data:  rmd$salary and rmd$job
# S = 6174118, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.4079513 
table(rmd$job, rmd$salary)

cor.test(x = rmd$salary, y = rmd$age, method = 'spearman')
# Spearman's rank correlation rho
# 
# data:  rmd$salary and rmd$age
# S = 8597324, p-value = 0.00044
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1755852
table(rmd$age, rmd$salary)

cor.test(x = rmd$age, y = rmd$marriage, method = 'spearman')
# Spearman's rank correlation rho
# 
# data:  rmd$age and rmd$marriage
# S = 6122197, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.4260405 

cor(rmd$physical, rmd$health, method = 'spearman')
cor(rmd$mental, rmd$health, method = 'spearman')
# ============================================================================================

# 檢查細格數不可以小於5
table(rmd$sex, rmd$ad)   # 考慮是否以不孕女性為母體/但現在男生的細格數都有大於5

table(rmd$age, rmd$ad)   # 4、5併為一組
#     0   1   2
# 1  28   7   6
# 2 122  40  15
# 3  76  34  16
# 4  33  11   7
# 5   0   1   1
table(rmd$edu, rmd$ad)   # 2、3、4併為一組
#     0   1   2
# 2   2   0   0
# 3   1   1   1
# 4  32  13   8
# 5 181  64  30
# 6  43  15   6
table(rmd$job, rmd$ad)   #5、6併為一組  job合併可能會有問題?
#     0   1   2
# 1  43  17   9
# 2  27  11   8
# 3 116  42  22
# 4  59  18   4
# 5  12   4   2
# 6   2   1   0
table(rmd$salary, rmd$ad)
#    0  1  2
# 1 27 13  1
# 2 32 17  2
# 3 58 18 12
# 4 60 15 14
# 5 34 14 12
# 6 48 16  4
#table(rmd$depression, rmd$marriage)   # 連續變數
table(rmd$children, rmd$ad)   # 改成有無小孩(1、2、3併為一組)
#     0   1   2
# 0 239  83  40
# 1  18   7   5
# 2   2   2   0
# 3   0   1   0
table(rmd$fertility_concept, rmd$ad)   #
#     0   1   2
# 0   7   7   2
# 1  50   6   8
# 2 143  52  21
# 3  59  28  14
table(rmd$health, rmd$ad)   # 1、2併為一組、4、5併為一組
#     0   1   2
# 1   1   0   0
# 2   9   5   9
# 3  73  47  15
# 4 145  38  20
# 5  31   3   1
table(rmd$physical, rmd$ad)   # 連續變數
#     0   1   2
# 0 191  59  21
# 1  57  27  22
# 2   9   6   2
# 3   2   1   0
table(rmd$mental, rmd$ad)   # 連續變數
#     0  1  2
# 0  58 10  7
# 1  67 11  6
# 2  65 36 15
# 3  41 18  5
# 4  12  9  4
# 5   6  4  4
# 6   7  2  3
# 7   2  2  0
# 8   0  1  0
# 9   0  0  1
# 10  1  0  0



table(rmd$habit_bad, rmd$ad)   # 1、2併為一組
#     0   1   2
# 0 240  90  39
# 1  17   3   5
# 2   2   0   1

table(rmd$habit_exercise , rmd$ad)
#     0   1   2
# 0 201  71  38
# 1  58  22   7
table(rmd$b1, rmd$ad)
#     0   1   2
# 1  49  19   5
# 2  14   7   6
# 3  86  30  23
# 4 107  36  11
# 5   3   1   0
table(rmd$b2, rmd$ad)   # 連續變數
#     0  1  2
# 0   2  0  0
# 1  51 19  4
# 2  55 21 11
# 3  49 16 10
# 4  12  7  6
# 5  37 10  3
# 6  14  8  3
# 7   9  2  2
# 8   8  4  2
# 9   2  0  2
# 10 11  2  1
# 11  1  1  0
# 12  0  1  0
# 13  0  0  1
# 14  0  1  0
# 15  4  0  0
# 25  1  0  0
# 35  2  1  0
# 45  1  0  0
table(rmd$b4, rmd$ad)   # 連續變數
#     0   1   2
# 0 107  34  12
# 1  60  27   9
# 2  34  13  11
# 3  36  13   7
# 4  17   4   5
# 5   2   0   0
# 6   1   2   1
# 8   1   0   0
# 9   1   0   0
table(rmd$b5, rmd$ad)   # 連續變數
#      0   1   2
# 0  150  53  24
# 1   42  21  13
# 2   20   7   5
# 3   20   2   1
# 4    6   1   2
# 5   14   4   0
# 6    1   4   0
# 7    2   0   0
# 8    3   0   0
# 10   0   1   0
# 11   1   0   0
View(rmd)


# 併組
# age -> 1~4
rmd$age = replace(rmd$age, rmd$age == 5, 4)
# edu -> 4~6
rmd$edu = replace(rmd$edu, rmd$edu == 2, 4)
rmd$edu = replace(rmd$edu, rmd$edu == 3, 4)
# job -> 1~5
rmd$job = replace(rmd$job, rmd$job == 6, 5)
# children -> 0、1
rmd$children = replace(rmd$children, rmd$children == 2, 1)
rmd$children = replace(rmd$children, rmd$children == 3, 1)
# fertility_concept -> 0~4
rmd$fertility_concept = replace(rmd$fertility_concept, rmd$fertility_concept == 4, 3)
rmd$fertility_concept = replace(rmd$fertility_concept, rmd$fertility_concept == 5, 3)
# health -> 2~4
rmd$health = replace(rmd$health, rmd$health == 1, 2)
#rmd$health = replace(rmd$health, rmd$health == 5, 4)
# habit_bad -> 0、1
rmd$habit_bad = replace(rmd$habit_bad, rmd$habit_bad == 2, 1)


# ============================================================================================

# 看Y與X間的相關性(不高)
cor.test(x = rmd$age, y = rmd$ad, method = 'spearman')  # rho = 0.07237071 
cor.test(x = rmd$edu, y = rmd$ad, method = 'spearman')  # -0.0456974 
cor.test(x = rmd$job, y = rmd$ad, method = 'spearman')  # -0.08650208
cor.test(x = rmd$salary, y = rmd$ad, method = 'spearman')  # 0.007678994
cor.test(x = rmd$children, y = rmd$ad, method = 'spearman')  # 0.05251267
cor.test(x = rmd$fertility_concept, y = rmd$ad, method = 'spearman')  # 0.07683049
cor.test(x = rmd$children, y = rmd$ad, method = 'spearman')  # 0.05251267
cor.test(x = rmd$physical, y = rmd$ad, method = 'spearman')  # 0.1730895
cor.test(x = rmd$mental, y = rmd$ad, method = 'spearman')  # 0.2006661
cor.test(x = rmd$habit_bad, y = rmd$ad, method = 'spearman')  # 0.006733321
cor.test(x = rmd$habit_exercise, y = rmd$ad, method = 'spearman')  # -0.02626821
cor.test(x = rmd$a13_1, y = rmd$ad, method = 'spearman')  # -0.08573623
cor.test(x = rmd$a13_2, y = rmd$ad, method = 'spearman')  # 0.04921119 
cor.test(x = rmd$a13_3, y = rmd$ad, method = 'spearman')  # 0.06271774
cor.test(x = rmd$a13_4, y = rmd$ad, method = 'spearman')  # 0.03020883 
cor.test(x = rmd$b1, y = rmd$ad, method = 'spearman')  # -0.0636099 
cor.test(x = rmd$b2, y = rmd$ad, method = 'spearman')  # 0.03077221 
cor.test(x = rmd$b4, y = rmd$ad, method = 'spearman')  # 0.0782513 
cor.test(x = rmd$b5, y = rmd$ad, method = 'spearman')  # -0.01238419 
cor.test(x = rmd$b6, y = rmd$ad, method = 'spearman')  # 0.1662167
# ============================================================================================


# ============================================================================================

library(devtools)
install_git("https://github.com/ccolonescu/PoEdata")
library(PoEdata)
# AIC: 
ad_sum.mlr <- lm(rmd$ad_sum ~  rmd$age + rmd$edu + rmd$job + rmd$salary 
                 + rmd$children +rmd$fertility_concept  + rmd$health + rmd$marriage 
                 + rmd$physical + rmd$mental + rmd$habit_bad + rmd$habit_exercise
                 + rmd$b1 + rmd$b2 + rmd$b4 + rmd$b5 ,
                 data = rmd)
summary(ad_sum.mlr)


# model selection - backward
# 1. 先建立一個完整的線性迴歸
ad_sum.mlr

# 2. 用`step()`，一個一個把變數移除，看移除哪個變數後 AIC 下降最多！
backward.lm = step(ad_sum.mlr, 
                   # 這裡可以加下界(lower=null)，也可以不加
                   scope = list(upper=ad_sum.mlr), 
                   direction="backward")  

# 最後得到的模型
# Step:  AIC=1399.26

ad_sum.mlr2 <- lm(rmd$ad_sum ~ rmd$age + rmd$edu + rmd$health + rmd$mental +
                    rmd$b4 + rmd$b5, data = rmd)
summary(ad_sum.mlr2)


# ============================================================================================


library(mlogit)
rmd$ad1 = as.factor(rmd$ad) #將 rmd 設定為類別尺度
# > typeof(rmd$ad)
# [1] "double"
# > typeof(rmd$ad1)
# [1] "integer"
ad_data = mlogit.data(rmd, varying = NULL, choice = "ad1", shape="wide")


model = mlogit(formula = ad1 ~ 1 |  age + edu + job + salary + fertility_concept 
               + health + marriage + children 
               + a10_1 + a10_2 + a10_3+ a10_4 + a10_5 + a10_6 
               + a10_7 + a10_8 + a10_9 + a10_10 + a10_11
               + a11_1 + a11_2 + a11_3 + a11_4 + a11_5 + a11_6 
               + a11_7 + a11_8 + a11_9 + a11_10 + a11_12
               + a12_1 + a12_2 + a12_3 + a12_4 + a12_5 + a12_6 
               + a12_7 + a12_8 + b1 + b2 + b4 + b5 ,
               data = ad_data,
               reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(model)   #輸出報表

# ============================================================================================

# 更新變數後再做

model1 = mlogit(formula = ad1 ~ 1 |  age + edu + job + salary + fertility_concept 
                + health + marriage + children
                + physical + mental + habit_bad + habit_exercise
                + b1 + b2 + b4 + b5 ,
                data = ad_data,
                reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(model1)   #輸出報表


# ============================================================================================


# final model

model2 = mlogit(formula = ad1 ~ 1 |health + mental + b4 + b5,
                data = ad_data,
                reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(model2)   #輸出報表


# ============================================================================================

# 單個做

m1 = mlogit(formula = ad1 ~ 1 |  sex,
            data = ad_data,
            reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m1)   #輸出報表




m2 = mlogit(formula = ad1 ~ 1 |  age,
            data = ad_data,
            reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m2)   #輸出報表





m3 = mlogit(formula = ad1 ~ 1 |edu,
            data = ad_data,
            reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m3)   #輸出報表




m4 = mlogit(formula = ad1 ~ 1 | job,
            data = ad_data,
            reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m4)   #輸出報表




m5 = mlogit(formula = ad1 ~ 1 |salary,
            data = ad_data,
            reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m5)   #輸出報表




m6 = mlogit(formula = ad1 ~ 1 |fertility_concept,
            data = ad_data,
            reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m6)   #輸出報表




m7 = mlogit(formula = ad1 ~ 1 |health,
            data = ad_data,
            reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m7)   #輸出報表


# ===============================================================================================
# 刪除ad_sum = 29的兩位
rmd[rmd$ad_sum==29,c(1, 88, 89)]
#     id    ad_sum  ad
#     <dbl>  <dbl> <dbl>
# 1   167     29     2
# 2   374     29     2
rmd = rmd[-c(159,345),]

m7_1 = lm(formula = rmd$ad_sum ~ rmd$health, data = rmd)  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m7_1)   #輸出報表


ggplot(data = rmd, aes(x = rmd$health)) +
  geom_smooth(aes(y = ad_sum), method = 'lm') +
  geom_point(aes(y = ad_sum)) +
  labs(title = 'Simple Linear Regression for X = health')
# ===============================================================================================

m8 = mlogit(formula = ad1 ~ 1 |marriage,
            data = ad_data,
            reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m8)   #輸出報表




m9 = mlogit(formula = ad1 ~ 1 |children,
            data = ad_data,
            reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m9)   #輸出報表




m10 = mlogit(formula = ad1 ~ 1 |physical,
             data = ad_data,
             reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m10)   #輸出報表




m11 = mlogit(formula = ad1 ~ 1 |mental,
             data = ad_data,
             reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m11)   #輸出報表

# ===============================================================================================
m11_1 = lm(formula = rmd$ad_sum ~ rmd$mental, data = rmd)  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m11_1)   #輸出報表


ggplot(data = rmd, aes(x = rmd$mental)) +
  geom_smooth(aes(y = ad_sum), method = 'lm') +
  geom_point(aes(y = ad_sum))
# ===============================================================================================



m12 = mlogit(formula = ad1 ~ 1 |a12_1 + a12_2 + a12_3 + a12_4 + a12_5 + a12_6 + a12_7,
             data = ad_data,
             reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m12)   #輸出報表




m13 = mlogit(formula = ad1 ~ 1 |a13_1 + a13_2 + a13_3 + a13_4,
             data = ad_data,
             reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m13)   #輸出報表




m14 = mlogit(formula = ad1 ~ 1 |b1 + b2 + b4 + b5 + b6,
             data = ad_data,
             reflevel="0")  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m14)   #輸出報表

# ===============================================================================================
m15_1 = lm(formula = rmd$ad_sum ~ rmd$b2, data = rmd)  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m15_1)   #輸出報表


ggplot(data = rmd, aes(x = rmd$b2)) +
  geom_smooth(aes(y = ad_sum), method = 'lm') +
  geom_point(aes(y = ad_sum))
# ===============================================================================================
# ===============================================================================================
m16_1 = lm(formula = rmd$ad_sum ~ rmd$b4, data = rmd)  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m16_1)   #輸出報表


ggplot(data = rmd, aes(x = rmd$b4)) +
  geom_smooth(aes(y = ad_sum), method = 'lm') +
  geom_point(aes(y = ad_sum))
# ===============================================================================================
# ===============================================================================================
m18_1 = lm(formula = rmd$ad_sum ~ rmd$b6, data = rmd)  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m18_1)   #輸出報表


ggplot(data = rmd, aes(x = rmd$b6)) +
  geom_smooth(aes(y = ad_sum), method = 'lm') +
  geom_point(aes(y = ad_sum))
# ===============================================================================================



m15 = lm(formula = ad_sum ~ b2,
         data = ad_data)  #進行迴歸分析，並設定 ad = 0 為參考點
summary(m15)   #輸出報表


# testing：但即使用SLR，不孕年數依舊不顯著
ad_sum.mlr4 <- lm(rmd$ad_sum ~ rmd$b2,
                  data = rmd)
summary(ad_sum.mlr4)




# ============================================================================================

# collinearity
XX = cor(rmd[3:60])
kappa(XX,exact=TRUE) #exact=TRUE表示精确计算条件数；
View(rmd)

# 做VIF看共線性，但是不是線性的才能做?
library(car)
library(regclass)
VIF(ad_sum.mlr3)


# ============================================================================================


# model selection - backward
# 1. 先建立一個完整的線性迴歸
model

# 2. 用`step()`，一個一個把變數移除，看移除哪個變數後 AIC 下降最多！
backward.lm = step(model, 
                   # 這裡可以加下界(lower=null)，也可以不加
                   scope = list(upper=model), 
                   direction="backward")



















