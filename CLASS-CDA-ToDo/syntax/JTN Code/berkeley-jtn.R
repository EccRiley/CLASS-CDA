#############################
#### Berkeley admissions data 
#### Uses dataset already in R
#### See also berkeley1.R for a different code 
#### See also related berkeleyLoglin.R 
#############################

#### Dataset already exist in R library

UCBAdmissions 

#### To test the odds-ratios in the marginal table and each of the subtables

library(vcd)

#### marginal table Admit x Gender

admit.gender=margin.table(UCBAdmissions, c(1,2))
admit.gender
admit.gender/4526

exp(oddsratio(admit.gender))
chisq.test(admit.gender)

#### Tests for partial tables AdmitxGender for each level of Dept.

chisq.test(UCBAdmissions[,,1])
exp(oddsratio(UCBAdmissions[,,1]))

chisq.test(UCBAdmissions[,,2])
exp(oddsratio(UCBAdmissions[,,2]))

chisq.test(UCBAdmissions[,,3])
exp(oddsratio(UCBAdmissions[,,3]))

chisq.test(UCBAdmissions[,,4])
exp(oddsratio(UCBAdmissions[,,4]))

chisq.test(UCBAdmissions[,,5])
exp(oddsratio(UCBAdmissions[,,5])

chisq.test(UCBAdmissions[,,6])
exp(oddsratio(UCBAdmissions[,,6]))

#### To visualize graphically these association explore fourfold() function in the vcd() package!

#### CMH test
    
mantelhaen.test(UCBAdmissions)

