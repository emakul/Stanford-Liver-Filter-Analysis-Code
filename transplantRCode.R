knitr::opts_chunk$set(echo = TRUE)
require(haven);require(dplyr);require(ggplot2);require(lubridate);require(tableone)
theme_set(theme_classic())


print('-------------Start of Everything-------------')
extra <- read.delim('/Users/ethanlouie528/Desktop/code/kwong code/redcap.csv',header=TRUE,stringsAsFactors=FALSE, sep=",")
extra_mod <- extra[, c("mrn", "Functional.Status.at.Transplant", "Karnofsky", "NMP", "History.of.previous.liver.transplantation.")]

codebook <- read.delim('/Users/ethanlouie528/Desktop/code/kwong code/patientCodebook.csv',header=TRUE,stringsAsFactors=FALSE, sep=",")
demographics <- read.delim('/Users/ethanlouie528/Desktop/code/kwong code/demographics.csv',header=TRUE,stringsAsFactors=FALSE, sep=",")
encounters <-
read.delim('/Users/ethanlouie528/Desktop/code/kwong code/encounters.csv',header=TRUE,stringsAsFactors=FALSE, sep=",") %>%
mutate(Admission.Time=as.Date(Admission.Time,origin="1970-01-01")) %>%
mutate(Discharge.Time=as.Date(Discharge.Time,origin="1970-01-01")) %>%
dplyr::select(Patient.Id, Date ,Appt.Type, Admission.Time, Discharge.Time, Length.of.Stay.Hours, Dept.Name) %>%
filter(Appt.Type %in% c("Historical Inpatient Encounter","Admission (Discharged)", "Admission (Admission)"))
master <- merge(codebook, encounters, by.x="Patient.id", by.y="Patient.Id") %>% filter(Length.of.Stay.Hours>=24)

# load stanford transplant database, select transplants from 2016 to 2023 @ Stanford
stanford <-
read.delim('/Users/ethanlouie528/Desktop/code/kwong code/StanfordLiverTranspl_DATA_LABELS_2025-01-12_1539.csv',header=TRUE,stringsAsFactors=FALSE, sep=",") %>%
mutate(Date.of.transplant=as.Date(Date.of.transplant,origin="1970-01-01")) %>%
mutate(Date.of.follow.up=as.Date(Date.of.follow.up)) %>%
mutate(Date.of.death.1=as.Date(Date.of.death.1)) %>%
mutate(TXYEAR=year(Date.of.transplant)) %>%
filter(TXYEAR>=2016&TXYEAR<=2023) %>% filter(Transplant.center=="Stanford") %>%
dplyr::select(Last.name, mrn, Date.of.transplant, Date.of.follow.up, Date.of.death.1) %>% unique()


# merge encounter data with the stanford transplant data, only transplant and death dates for now
master1 <- merge(master, stanford, by="mrn")

#calculate lengths of stay
master2 <- master1 %>%
#select admissions occurring post-transplant
filter(Discharge.Time>=Date.of.transplant) %>%
mutate(Year1=Date.of.transplant+365) %>%
#select admissions before reaching first year  transplant
filter(Admission.Time<Year1) %>%
mutate(Discharge.Time=as.Date(Discharge.Time)) %>%
# if admission started before transplant, reset start time to the transplant date
mutate(Admission.Time2=ifelse(Admission.Time<Date.of.transplant,Date.of.transplant,as.Date(Admission.Time,origin="1970-01-01"))) %>%
mutate(Admission.Time2=as.Date(Admission.Time2,origin="1970-01-01")) %>%
# if discharge was >1 year post transplant, set end time to 1 year
mutate(Discharge.Time2=ifelse(Discharge.Time>Year1,Year1,as.Date(Discharge.Time,origin="1970-01-01"))) %>%
mutate(Discharge.Time2=as.Date(Discharge.Time2,origin="1970-01-01")) %>%
mutate(LOS=as.numeric(Discharge.Time-Admission.Time2))

number <- master2 %>% group_by(Patient.id) %>% dplyr::summarize(count=n())
summary(number$count)
summary(master2$LOS)
# In total there were 1485 discrete admissions among 625 patients, median # was 2 (IQR 1-3), median LOS was 8 days (IQR 4-14)


firstadmit <- master2 %>% group_by(Patient.id) %>% arrange(Admission.Time2) %>% slice_head()
summary(firstadmit$LOS)
# median LOS on the first admit was 13 days (IQR 9-23)

#SECOND CHUNK-----------------------------------
rollup <- master2 %>%
group_by(name, mrn, Patient.id, Date.of.transplant, Date.of.death.1, Year1) %>% dplyr::summarise(totalLOS=sum(LOS)) %>%
mutate(Date.of.death.1=as.Date(Date.of.death.1,origin="1970-01-01")) %>%
mutate(Date.of.death.1=ifelse(Date.of.death.1>=Year1|is.na(Date.of.death.1),Year1,Date.of.death.1)) %>%
mutate(Date.of.death.1=as.Date(Date.of.death.1,origin="1970-01-01")) %>%
mutate(deaddays=as.numeric(Year1-Date.of.death.1)) %>%
# days dead and/or in hospital
mutate(ddih=totalLOS+deaddays) %>%
mutate(daoh=365-ddih)

#investigate, 5 MRNs in redcap but no data from STARR
cohort <- rollup$mrn
cohort <- rollup$mrn
stanford %>% filter(mrn %ni% cohort) %>% print()


#THIRD CHUNK ------- Merge in more complete transplant data from redcap
redcap <-
read.delim('/Users/ethanlouie528/Desktop/code/kwong code/StanfordLiverTranspl_DATA_LABELS_2025-01-12_1539.csv',header=TRUE,stringsAsFactors=FALSE, sep=",") %>%
mutate(Date.of.birth=as.Date(Date.of.birth), orgin="1970-01-01") %>%
mutate(Date.of.transplant=as.Date(Date.of.transplant)) %>%
mutate(Date.of.follow.up=as.Date(Date.of.follow.up)) %>%
mutate(Date.of.death.1=as.Date(Date.of.death.1)) %>%
mutate(TXYEAR=year(Date.of.transplant)) %>%
filter(TXYEAR>=2016&TXYEAR<=2023) %>% filter(Transplant.center=="Stanford") %>%
dplyr::select(Date.of.transplant,Record.ID, First.name, Last.name, mrn, Date.of.birth, Sex, Race, Primary.etiology.of.liver.disease, Primary.insurance,Limited.sobriety....6.months..,
              MELD.at.listing, Dialysis.at.listing.,Blood.type,Allocation.MELD,Status.1A, Biochemical.MELD,Dialysis.at.transplant.,
              Medical.condition.at.transplant,Multiorgan.,DCD.liver., Split.liver.,Cold.ischemia.time..hours.,Last.drink, BMI, Living.donor, Ascites,
              DiabetesT1.Comorbidity, DiabetesT2.Comorbidity, DiabetesT2I.Comorbidity, Hypertension.Comorbidity, Hyperlipidemia.Comorbidity) %>% unique()

comp <- merge(rollup, redcap, by=c("mrn","Date.of.transplant")) %>%
mutate(nonslkmulti=ifelse(Multiorgan. %in% c("Yes, liver/kidney","No"),"No","Yes")) %>%
mutate(Age=as.numeric((Date.of.transplant-Date.of.birth)/365)) %>%
mutate(daoh=ifelse(daoh<0,0,daoh)) %>%
mutate(daoh330=ifelse(daoh>330,"Yes","No")) %>%
filter(deaddays==0) %>% # remove the people who died
filter(Last.name!="Barragan") %>% #intestine only
mutate(Limited.sobriety....6.months..=ifelse(Primary.etiology.of.liver.disease=="","No",Limited.sobriety....6.months..))

everyone <- merge(rollup, redcap, by=c("mrn","Date.of.transplant")) %>%
mutate(nonslkmulti=ifelse(Multiorgan. %in% c("Yes, liver/kidney","No"),"No","Yes")) %>%
mutate(Age=as.numeric((Date.of.transplant-Date.of.birth)/365)) %>%
mutate(daoh=ifelse(daoh<0,0,daoh)) %>%
mutate(daoh330=ifelse(daoh>330,"Yes","No")) %>%
filter(Last.name!="Barragan") %>% #intestine only
mutate(Limited.sobriety....6.months..=ifelse(Primary.etiology.of.liver.disease=="","No",Limited.sobriety....6.months..))

#FOURTH CHUNK---- Summary Statistics
print('----------Summary Statistics----------')
head(comp)
table1 <- CreateTableOne(vars=c("Age","Sex","Race","Primary.etiology.of.liver.disease", "Primary.insurance",
                              "Limited.sobriety....6.months..","Blood.type","Status.1A",
                              "Biochemical.MELD","Dialysis.at.transplant.",
              "Medical.condition.at.transplant.","Multiorgan.","DCD.liver.", "nonslkmulti", "Split.liver.","Cold.ischemia.time..hours.","daoh","daoh330"),data=comp,strata="daoh330")
table1toexport <- print(table1,nonnormal= c("Biochemical.MELD","Cold.ischemia.time..hours."))

options(max.print=300)
plot(comp$Cold.ischemia.time..hours.,comp$daoh)
abline(lm(daoh ~ Cold.ischemia.time..hours.,data=comp),col='red')
plot(comp$Cold.ischemia.time..hours., comp$Biochemical.MELD)
abline(lm(Cold.ischemia.time..hours. ~ Biochemical.MELD,data=comp),col='red')

#table(comp$Multiorgan.)

#FIFTH CHUNK --- Alcohol only
print('----------Alcohol Only----------')
alcohol <- comp %>% filter(Primary.etiology.of.liver.disease=="Alcohol")
summary(alcohol$daoh[alcohol$Limited.sobriety....6.months..=="Yes"])
hist(alcohol$daoh[alcohol$Limited.sobriety....6.months..=="Yes"])
summary(alcohol$daoh[alcohol$Limited.sobriety....6.months..=="No"])
hist(alcohol$daoh[alcohol$Limited.sobriety....6.months..=="No"])
table1 <- CreateTableOne(vars=c("daoh","Biochemical.MELD","daoh330"),data=alcohol,strata="Limited.sobriety....6.months..")
table1toexport <- print(table1,nonnormal=c("Biochemical.MELD","daoh"))


#SIXTH CHUNK ------ alcohol only
alcohol <- comp %>% filter(Primary.etiology.of.liver.disease=="Alcohol")
summary(alcohol$daoh[alcohol$Limited.sobriety....6.months..=="Yes"])
hist(alcohol$daoh[alcohol$Limited.sobriety....6.months..=="Yes"])
summary(alcohol$daoh[alcohol$Limited.sobriety....6.months..=="No"])
hist(alcohol$daoh[alcohol$Limited.sobriety....6.months..=="No"])
table1 <- CreateTableOne(vars=c("daoh","Biochemical.MELD","daoh330"),data=alcohol,strata="Limited.sobriety....6.months..")
table1toexport <- print(table1,nonnormal=c("Biochemical.MELD","daoh"))


#SEVENTH CHUNK ---- Modeling predictors of DAOH, adjusted for MELD
comp_unique <- comp %>%
distinct(mrn, .keep_all = TRUE)

special <- merge(comp_unique, extra_mod, by="mrn")
special_unique <- special %>%
distinct(mrn, .keep_all = TRUE)

#everyone <- merge(rollup, redcap, by=c("mrn","Date.of.transplant"))

everyone_unique <- everyone %>%
distinct(mrn, .keep_all = TRUE)

ultimate <- merge(everyone_unique, extra_mod, by="mrn")


print('----------Modeling predictors of DAOH, adjusted for MELD----------')
print('--------------Age-----------')
model <- glm(daoh ~ Age + Biochemical.MELD, family=quasipoisson(link="log"), data=comp_unique %>% filter(nonslkmulti=="No"))
summary(model)

print('--------------Sex-----------')
model <- glm(daoh ~ Sex + Biochemical.MELD, family=quasipoisson(link="log"), data=comp_unique %>% filter(nonslkmulti=="No"))
summary(model)


print('--------------Race-----------')
model <- glm(daoh ~ Race + Biochemical.MELD, family=quasipoisson(link="log"), data=comp_unique %>% filter(nonslkmulti=="No"))
summary(model)


print('--------------DCD Liver-----------')
model <- glm(daoh ~ DCD.liver. + Biochemical.MELD, family=quasipoisson(link="log"), data=comp_unique %>% filter(DCD.liver.!="" & Multiorgan.=="No"))
summary(model)


print('--------------Dialysis at transplant-----------')
model <- glm(daoh ~ Dialysis.at.transplant. + Biochemical.MELD, family=quasipoisson(link="log"), data=comp_unique %>% filter(Dialysis.at.transplant.!=""))
summary(model)


print('--------------Cold Ischemia Time-----------')
model <- glm(daoh ~ Cold.ischemia.time..hours. + Biochemical.MELD, family=quasipoisson(link="log"), data=comp %>% filter(Multiorgan.=="No"))
summary(model)
#head(comp)
#hist(comp$daoh)

#further analysis was conducted in the console



