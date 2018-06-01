library(data.table)

#NOTE: must change "path-to-file" to the directory of the data files
chartevents <- fread("path-to-file/CHARTEVENTS.csv",sep=",", 
select = c("SUBJECT_ID","ITEMID","CHARTTIME","VALUENUM","ICUSTAY_ID", "HADM_ID"))

outputevents <- fread("path-to-file/OUTPUTEVENTS.csv",sep=",", 
select = c("SUBJECT_ID","ITEMID","CHARTTIME","VALUE","ICUSTAY_ID", "HADM_ID"))

D_ITEMS <- read.csv("path-to-file/D_ITEMS.csv")

icu <- read.csv("path-to-file/ICUSTAYS.csv")

patients <- read.csv("path-to-file/PATIENTS.csv")

admissions <- read.csv("path-to-file/ADMISSIONS.csv")

input_MV <- fread("path-to-file/INPUTEVENTS_MV.csv",sep=",",select=c("SUBJECT_ID","ITEMID","STARTTIME","AMOUNT","ICUSTAY_ID"))

D_ICD <- read.csv("path-to-file/D_ICD_DIAGNOSES.csv")

ICD <- read.csv("path-to-file/DIAGNOSES_ICD.csv")


#These are the ITEMID's corresponding to Mean Arterial Blood Pressure in the metavision database
#      ROW_ID ITEMID                LABEL         ABBREVIATION   DBSOURCE     LINKSTO    CATEGORY 		UNITNAME PARAM_TYPE 
#10165  13110 224322            IABP Mean       MAP - Assisted metavision chartevents        IABP     		mmHg    Numeric        
#11505  12718 220052         Arterial Blood Pressure mean ABPm metavision chartevents Routine Vital Signs    mmHg 	Numeric
#11523  12736 220181     Non Invasive Blood Pressure meanNBPm  metavision chartevents Routine Vital Signs    mmHg 	Numeric
#12446  13689 225312             ART BP mean       ART BP mean metavision chartevents Routine Vital Signs    mmHg 	Numeric

id.meta1 <- which(chartevents$ITEMID == 224322)
id.meta2 <- which(chartevents$ITEMID == 220052)
id.meta3 <- which(chartevents$ITEMID == 220181)
id.meta4 <- which(chartevents$ITEMID == 225312)


#collecting rows of chartevents that correspond to metavision MAP's
ids.metavision <- sort(c(id.meta1, id.meta2, id.meta3, id.meta4))

tmp <- chartevents[ids.metavision,]

tmp1 <- order(tmp$CHARTTIME)

tmp2 <- tmp[tmp1,]

tmp3 <- order(tmp2$SUBJECT_ID)

chart.meta <- tmp2[tmp3,]

tmp <- which(!is.na(chart.meta$VALUENUM))

tmp2 <- chart.meta[tmp,]

chart.meta <- tmp2

start <- c()
end<-c()
s=1
i=1
while(i < nrow(chart.meta)){
	#find 2 bpa above 60 from save subject
	if(chart.meta$VALUENUM[i] > 60 & chart.meta$VALUENUM[i+1] > 60 & (chart.meta$SUBJECT_ID[i] == chart.meta$SUBJECT_ID[i+1])){
		#find for 2 bps below 60 (start of HE), followed by 2 bps above 60 (end of HE)
		idx <- which(chart.meta$SUBJECT_ID > chart.meta$SUBJECT_ID[i])[1]-1
		for(j in (i+2):idx){
			if(chart.meta$VALUENUM[j] <= 60 & chart.meta$VALUENUM[j+1] <= 60){
				for(k in (j+2):idx){
					if(chart.meta$VALUENUM[k] > 60 & chart.meta$VALUENUM[k+1] > 60 & 
							(chart.meta$SUBJECT_ID[k] == chart.meta$SUBJECT_ID[k+1])){
						start[s] <- j + 1
						end[s] <- k+1
						i = idx+1
						s=s+1
						break
					}
				}
			}
			if(i > j){break}
			if(j == idx){
				i=idx+1
			}
		}
	}else{
		i=i+1
	}
}
#Note: Error at end of loop has no impact on "start" or "end" 

ID.mat <- cbind(start, end, chart.meta$SUBJECT_ID[start], chart.meta$SUBJECT_ID[end])

ID.mat <- ID.mat[which(ID.mat[,4]-ID.mat[,3]==0),]

temp <- ID.mat[!duplicated(ID.mat[,3]),]

ID.mat.new <- temp

#find subjects with MAP value 3 hours before HE onset
MAP.under.3 <- unlist(lapply(1:nrow(ID.mat.new), function(i) ifelse(
	difftime(chart.meta$CHARTTIME[ID.mat.new[i,1]],chart.meta$CHARTTIME[which(chart.meta$SUBJECT_ID==ID.mat.new[i,3])[1]],units="sec") >= 10800, 1, 0
					 )))
					 
ID.mat.new2 <- ID.mat.new[which(MAP.under.3 == 1),]

CREAT <- D_ITEMS$ITEMID[which(grepl("Creatinine", D_ITEMS$LABEL))]

creat <- which(chartevents$ITEMID %in% CREAT)

chart.creat <- chartevents[creat,]

chart.ids <- which(chart.creat$SUBJECT_ID %in% ID.mat.new2[,3])

chart.creat2 <- chart.creat[chart.ids,]

tmp <- which(ID.mat.new2[,3] %in% chart.creat2$SUBJECT_ID)
ID.mat.new3 <- ID.mat.new2[tmp,]

tmp <- order(chart.creat2$CHARTTIME)
temp <- chart.creat2[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.creat2 <- temp[tmp2,]

creat.value <- rep(NA, nrow(ID.mat.new3))
for(i in 1:nrow(ID.mat.new3)){
	ind <- which(chart.creat2$SUBJECT_ID == ID.mat.new3[i,3])
	ind2 <- ind[length(ind)]+1
	for(j in 1:length(ind)){
		time.diff <- difftime(chart.meta$CHARTTIME[ID.mat.new3[i,1]],
			chart.creat2$CHARTTIME[ind2 - j],units="hours")
		if(time.diff >= 0 & time.diff <= 24){
			creat.value[i] <- chart.creat2$VALUENUM[ind2 - j]
		}
	}
}

#mean MAP 3 hours prior to HE onset
mean_MAP_3h<-c()
for(i in 1:nrow(ID.mat.new3)){
	holder<-c()
	j = 1
	for(k in 1:length(which(chart.meta$SUBJECT_ID == ID.mat.new3[i,3]))){
		if((ID.mat.new3[i,1]-k) > 0){
			tmp <- difftime(chart.meta$CHARTTIME[ID.mat.new3[i,1]],
				chart.meta$CHARTTIME[ID.mat.new3[i,1]-k],units="sec")
			if(tmp > 0){
				if(tmp > 10800){
					mean_MAP_3h[i] <- mean(as.numeric(holder))
					break
				}else{
					holder[j] = chart.meta$VALUENUM[ID.mat.new3[i,1]-k]
					j = j+1
				}
			}else{
				mean_MAP_3h[i] <- mean(as.numeric(holder))
			}
		}
	}
}


HE.min <- c()
for(i in 1:nrow(ID.mat.new3)){
	HE.min[i]<-difftime(chart.meta$CHARTTIME[ID.mat.new3[i,2]], chart.meta$CHARTTIME[ID.mat.new3[i,1]],units="mins")
}

tmp <- which(ID.mat.new3[,3] %in% chart.creat2$SUBJECT_ID)

temp<-cbind(ID.mat.new3, HE.min, mean_MAP_3h[tmp], creat.value)

data.meta <- temp

CAREUNIT_ENTER <- rep(NA, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(which(icu$ICUSTAY_ID == chart.meta$ICUSTAY_ID[ID.mat.new3[i,1]]))>0)
	CAREUNIT_ENTER[i] <- icu$FIRST_CAREUNIT[which(icu$ICUSTAY_ID == chart.meta$ICUSTAY_ID[ID.mat.new3[i,1]])]
} 


pat.ids <- c(); for(i in 1:nrow(ID.mat.new3)){
	pat.ids[i]<-which(patients$SUBJECT_ID == ID.mat.new3[i,3])
}

#patient data
patients2 <- patients[pat.ids,]

GENDER <- ifelse(patients2$GENDER == "M", 1, 0)

DOBs <- as.Date(substr(patients2$DOB, 1, 10))

#NOTE: THIS AGE IS SHIFTED
AGE <- difftime(as.Date(chart.meta$CHARTTIME[ID.mat.new3[,1]]),DOBs,units="days")

#keep only ages 89 and under (unmasked/non-missing ages)
AGE.new <- ifelse(as.numeric(AGE) < 60000, as.numeric(AGE)/365.25, NA)

patients.adm <- admissions[which(admissions$SUBJECT_ID %in% ID.mat.new3[,3]),]

tmp <- order(patients.adm$ADMITTIME)
pat.dat <- patients.adm[tmp,]

tmp2 <- order(pat.dat$SUBJECT_ID)

pat.dat2 <- pat.dat[tmp2,]

HADM <- chart.meta$HADM_ID[ID.mat.new3[,1]]

adm.id <- which(pat.dat2$HADM_ID %in% HADM)

temp <- pat.dat2[adm.id,]

#contains hospital admission data, such as marital status, ethnicity, and diagnosis
pat.dat2 <- temp


ordm <- order(input_MV$STARTTIME)
input_MV_ordered <- input_MV[ordm,]
ordm2 <- order(input_MV_ordered$SUBJECT_ID)
input_MV_ordered2 <- input_MV_ordered[ordm2,]

colnames(input_MV_ordered2)[3]<-"CHARTTIME"

HE.starttime <- chart.meta$CHARTTIME[ID.mat.new3[,1]]
HE.endtime <- chart.meta$CHARTTIME[ID.mat.new3[,2]]

inputs.during.HE <- list()
input.val.during.HE <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(input_MV_ordered2$SUBJECT_ID==ID.mat.new3[i,3])
	temp2 <- which(input_MV_ordered2[temp]$CHARTTIME < HE.starttime[i])
	ind1 = length(temp2)+1
	temp3 <- which(input_MV_ordered2[temp]$CHARTTIME > HE.endtime[i])
	ind2 = temp3[1]-1
	if(!is.na(ind1) & !is.na(ind2) & (ind1 <= ind2)){
		inputs.during.HE[[i]] <- input_MV_ordered2$ITEMID[temp[ind1:ind2]]
		input.val.during.HE[[i]] <- input_MV_ordered2$AMOUNT[temp[ind1:ind2]]
	}else{
		inputs.during.HE[[i]] <- NULL
		input.val.during.HE[[i]] <- NULL
	}
}


t1<- which(grepl("loid", D_ITEMS$CATEGORY[which(D_ITEMS$ITEMID %in% unique(unlist(inputs.during.HE)))]))
 
t2 <-which(grepl("loid", D_ITEMS$LABEL[which(D_ITEMS$ITEMID %in% unique(unlist(inputs.during.HE)))]))

t <- sort(unique(c(t1,t2)))

fluid.ids <- D_ITEMS$ITEMID[which(D_ITEMS$ITEMID %in% unique(unlist(inputs.during.HE)))][t]

crystal.ids <-  c(226364, 226375)
colloid.ids <- fluid.ids[-which(fluid.ids %in% c(226364, 226375))]

s1<- which(grepl("Epinephrine", D_ITEMS$LABEL[which(D_ITEMS$ITEMID %in% unique(unlist(inputs.during.HE)))]))

 
s2 <-which(grepl("Dobutamine", D_ITEMS$LABEL[which(D_ITEMS$ITEMID %in% unique(unlist(inputs.during.HE)))]))


s3 <-which(grepl("Dopamine", D_ITEMS$LABEL[which(D_ITEMS$ITEMID %in% unique(unlist(inputs.during.HE)))]))

s4 <-which(grepl("Phenylephrine", D_ITEMS$LABEL[which(D_ITEMS$ITEMID %in% unique(unlist(inputs.during.HE)))]))

s5 <-which(grepl("Norepinephrine", D_ITEMS$LABEL[which(D_ITEMS$ITEMID %in% unique(unlist(inputs.during.HE)))]))

s6 <-which(grepl("Vasopressin", D_ITEMS$LABEL[which(D_ITEMS$ITEMID %in% unique(unlist(inputs.during.HE)))]))

s <- sort(unique(c(s1,s2,s3,s4,s5,s6)))

vaso.ids <- D_ITEMS$ITEMID[which(D_ITEMS$ITEMID %in% unique(unlist(inputs.during.HE)))][s]

fluid.th <- rep(0, length(inputs.during.HE))
vaso.th <- rep(0, length(inputs.during.HE))


for(i in 1:length(inputs.during.HE)){
	if(sum(unique(inputs.during.HE[[i]]) %in% vaso.ids)>0){
		vaso.th[i] <- 1		
	}
	if(sum(unique(inputs.during.HE[[i]]) %in% colloid.ids)>0){
		fluid.th[i] <- 1
	}else if(sum(unique(inputs.during.HE[[i]]) %in% crystal.ids)>0){
		fluid.th[i] <- 2
	}
}


#get times subjects were treated
trt.time <- list()
for(i in 1:nrow(ID.mat.new3)){
		temp <- which(input_MV_ordered2$SUBJECT_ID==ID.mat.new3[i,3])
		trt.time[[i]] <- input_MV_ordered2$CHARTTIME[temp][which(input_MV_ordered2$ITEMID[temp] %in% c(fluid.ids,vaso.ids))]
}

#get differences in times between treatment and beginning of HE
trt.time.diff <- list()
for(i in 1:nrow(ID.mat.new3)){
	if(length(trt.time[[i]]) < 1)
	trt.time.diff[[i]] <- NA
	else
	trt.time.diff[[i]] <- difftime(trt.time[[i]], chart.meta$CHARTTIME[ID.mat.new3[i,1]], units = "secs")
}

#gets difference in times between first treatment and beginning of HE
first.trt.time.diff <- list()
for(i in 1:nrow(ID.mat.new3)){
	if(length(which(trt.time.diff[[i]] > 0)) < 1){
		first.trt.time.diff[[i]] <- NULL
	}else{
	first.trt.time.diff[[i]] <- min(trt.time.diff[[i]][which(trt.time.diff[[i]] > 0)],
		na.rm=TRUE)
	}
}

#indicates if treated during HE
indic <- rep(0, nrow(ID.mat.new3))
for(i in 1:(nrow(ID.mat.new3)-1)){
	if(length(first.trt.time.diff[[i]]) >= 1)
		indic[i] <- ifelse(first.trt.time.diff[[i]]/60 < HE.min[[i]], 1, 0)
}

#gets length of HE after treatment in seconds
trt.HE.length <- lapply(1:nrow(ID.mat.new3), function(i) NA)
for(i in which(indic == 1)){
	trt.HE.length[[i]] <- HE.min[[i]]*60 - first.trt.time.diff[[i]]
}	

#Simplified Acute Physiology Score (SAPS II): https://www.thecalculator.co/health/Simplified-Acute-Physiology-Score-(SAPS-II)-Calculator-1021.html
#Age:
#Below 40	: 0 points
#40-59		: 7 points
#60-69		: 12 points
#70-74		: 15 points
#75-79		: 16 points
#80+		: 18 points

#Type of admission:
#Scheduled surgical	: 0 points
#Medical			: 6 points
#Unscheduled surgical: 8 points

#Heart rate: "Heart Rate"
#Below 40	: 11 points
#40-69		: 2 points
#70-119		: 0 points
#120-159	: 4 points
#160+		: 7 points

#Systolic BP: "NBP [Systolic]" "Non Invasive Blood Pressure systolic"
#below 70	: 13 points
#70-99		: 5 points
#100-199	: 0 points
#200+		: 2 points

#Temperature: "Temperature C (calc)" "Temperature F" "Temperature Fahrenheit"
#Below 39c 		: 0 points
#39c+			: 3 pints
#Below 102.2 F	: 0 points
#102.2 F +		: 3 points

#Glagow coma score: "GCS Total"
#Below 6:	26 points
#6-8	:	13 points
#9-10	:	7 points
#11-13	:	5 points
#14-15	:	0 points

#PaO2 (mmHg)/FiO2 (%) if mechanical ventilation or CPAP
#N/A		: 0 points
#Below 100	: 11 points
#100-199	: 9 points
#200+		: 6 points

#Urine output (L/24h)
#Below 0.5	: 11 points
#0.5-0.99	: 4 points
#1+			: 0 points

#Serum urea (g/L) or BUN (mg/dL): "BUN"
#SU: below 0.6	: 0 points
#SU: 0.6-1.79	: 6 points
#SU: 1.8+		: 10 points
#BUN: below 28	: 0 points
#BUN: 28-83		: 6 points
#BUN: 84+		: 10 points

#Sodium (mEg/L)  "Sodium (serum)" 
#Below 125	: 5 points
#125-144	: 0 points
#145+		: 1 point

#Potassium (mEq/L) "Potassium (serum)"
#Below 3	: 3 points
# 3-4.9		: 0 points
#5+			: 3 points

#Bicarbonate (mEq/L): "HCO3 (serum)"
#Below 15	: 6 points
# 15-19		: 3 points
# 20+		: 0 points

#Bilirubin (mg/dL): "Total Bilirubin"
#Below 4	: 0 points
# 4-5.9		: 4 points
# 6+		: 9 points

#White blood cells/mm^3 "WBC"
#Less than 1000	: 12 points
#1000-19000		: 0 points
#20000+			: 3 points

#Chronic diseases
#None					: 0 points
#Metastatic cancer		: 9 points
#Hematologic malignancy	: 10 points
#AIDS					: 17 points

temp<-which(chartevents$SUBJECT_ID %in% ID.mat.new3[,3])

#Item ID for heart rate: 220045
temp2 <- chartevents[temp,]
chart.HR <- which(temp2$ITEMID == 220045)

temp <- temp2[chart.HR,]

chart.HR<-temp

tmp <- order(chart.HR$CHARTTIME)

temp <- chart.HR[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.HR <- temp[tmp2,]

patients.icu <- icu[which(icu$SUBJECT_ID %in% ID.mat.new3[,3]),]

tmp <- order(patients.icu$INTIME)
pat.icu <- patients.icu[tmp,]

tmp2 <- order(pat.icu$SUBJECT_ID)

pat.icu2 <- pat.icu[tmp2,]

ICUID <- chart.meta$ICUSTAY_ID[ID.mat.new3[,1]]

tmp3 <- which(chart.HR$ICUSTAY_ID %in% ICUID)

temp <- chart.HR[tmp3,]

chart.HR <- temp

#heart rate values within 24 hours of HE
HR.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.HR$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.HR$CHARTTIME[temp],units="hours")
	temp2 <- which(time.diff <= 24 & time.diff >= 0)
	HR.24h[[i]] <- chart.HR$VALUENUM[temp[temp2]]
}

#get SAPS heart rate score
hr.score <- rep(0, nrow(ID.mat.new3))
hr.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(HR.24h[[i]]) > 0){
		hr.ind[i] <- 1
	}
	if(min(HR.24h[[i]]) < 40 & min(HR.24h[[i]]) > -Inf){
		hr.score[i] <- 11
	}else if(max(HR.24h[[i]]) > 159 & max(HR.24h[[i]]) < Inf){
		hr.score[i] <- 7
	}else if(max(HR.24h[[i]]) > 119 & max(HR.24h[[i]]) < Inf){
		hr.score[i] <- 4
	}else if(min(HR.24h[[i]]) < 70 & min(HR.24h[[i]]) > -Inf){
		hr.score[i] <- 2
	}
}

#get SAPS age score
age.score <- rep(NA, nrow(ID.mat.new3))
age.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(!is.na(AGE.new[i])){
		age.ind[i] <- 1
	if(AGE.new[i] > 80){
		age.score[i] <- 18
	}else if(AGE.new[i] > 75){
		age.score[i] <- 16
	}else if(AGE.new[i] > 70){
		age.score[i] <- 15
	}else if(AGE.new[i] > 60){
		age.score[i] <- 12
	}else if(AGE.new[i] > 40){
		age.score[i] <- 7
	}else{
		age.score[i] <- 0
	}
	}
}

temp<-which(chartevents$SUBJECT_ID %in% ID.mat.new3[,3])

temp2 <- chartevents[temp,]
chart.SBP <- which(temp2$ITEMID %in% c(224167, 227243, 226850, 226852, 220050, 220059, 220179, 225309))

temp <- temp2[chart.SBP,]

chart.SBP<-temp

tmp <- order(chart.SBP$CHARTTIME)

temp <- chart.SBP[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.SBP <- temp[tmp2,]

#SBP values within 24 hours of HE
SBP.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.SBP$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.SBP$CHARTTIME[temp],units="hours")
	temp2 <- which(time.diff <= 24 & time.diff >= 0)
	SBP.24h[[i]] <- chart.SBP$VALUENUM[temp[temp2]]
}

#SAPS SBP score
sbp.score <- rep(NA, nrow(ID.mat.new3))
sbp.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(SBP.24h[[i]])>0){
		sbp.ind[i] <- 1
	}
	if(min(SBP.24h[[i]])<70 & min(SBP.24h[[i]]) > -Inf){
		sbp.score[i] <- 13
	}else if(min(SBP.24h[[i]])<100 & min(SBP.24h[[i]]) > -Inf){
		sbp.score[i] <- 5
	}else if(min(SBP.24h[[i]])<199 & min(SBP.24h[[i]]) > -Inf){
		sbp.score[i] <- 0
	}else{
		sbp.score[i] <- 2
	}
}

#Temp F itemid: 223761
temp.ids<-which(chartevents$SUBJECT_ID %in% ID.mat.new3[,3])

temp2 <- chartevents[temp.ids,]
chart.tempF <- which(temp2$ITEMID == 223761)

temp <- temp2[chart.tempF,]

chart.tempF<-temp

tmp <- order(chart.tempF$CHARTTIME)

temp <- chart.tempF[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.tempF <- temp[tmp2,]

#temperature values within 24 hours of HE
tempF.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.tempF$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.tempF$CHARTTIME[temp],units="hours")
	temp2 <- which(time.diff <= 24 & time.diff >= 0)
	tempF.24h[[i]] <- chart.tempF$VALUENUM[temp[temp2]]
}

#Temp C itemid: 223762
temp.ids<-which(chartevents$SUBJECT_ID %in% ID.mat.new3[,3])

temp2 <- chartevents[temp.ids,]
chart.tempC <- which(temp2$ITEMID == 223762)

temp <- temp2[chart.tempC,]

chart.tempC<-temp

tmp <- order(chart.tempC$CHARTTIME)

temp <- chart.tempC[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.tempC <- temp[tmp2,]

#temperature values within 24 hours of HE
tempC.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.tempC$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.tempC$CHARTTIME[temp],units="hours")
	temp2 <- which(time.diff <= 24 & time.diff >= 0)
	tempC.24h[[i]] <- chart.tempC$VALUENUM[temp[temp2]]
}

tempF.calc <- list()

for(i in 1:length(tempC.24h)){
	if(length(tempC.24h[[i]]) > 0){
		if(!is.null(tempC.24h[[i]]) & !is.na(tempC.24h[[i]])){
		tempF.calc[[i]] <- 1.8*tempC.24h[[i]] + 32
		}
	}else{
		tempF.calc[[i]] <- NA
	}
}

temp.score <- rep(0, nrow(ID.mat.new3))
temp.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(tempF.24h[[i]]) > 0){
		temp.ind[i] <- 1
		if(!is.null(tempF.24h[[i]]) & !is.na(tempF.24h[[i]])){
		if(max(tempF.24h[[i]])>102.2 & max(tempF.24h[[i]]) < Inf){
		temp.score[i] <- 3
		}
		}
	}
	if(length(tempF.calc[[i]]) > 0){
		temp.ind[i] <- 1
		if(!is.null(tempF.calc[[i]]) & !is.na(tempF.calc[[i]])){
		if(max(tempF.calc[[i]])>102.2 & max(tempF.calc[[i]]) < Inf){
		temp.score[i] <- 3
		}
		}
	}
}

#GCS Eye
temp2 <- chartevents[temp.ids,]

chart.GCS_E <- which(temp2$ITEMID == 220739)

temp <- temp2[chart.GCS_E,]

chart.GCS_E<-temp

tmp <- order(chart.GCS_E$CHARTTIME)

temp <- chart.GCS_E[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.GCS_E <- temp[tmp2,]

#gcs e values within 24 hours of HE
GCS_E.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.GCS_E$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.GCS_E$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	GCS_E.24h[[i]] <- chart.GCS_E$VALUENUM[temp[temp2.n]]
}

#GCS Verbal
chart.GCS_V <- which(temp2$ITEMID == 223900)

temp <- temp2[chart.GCS_V,]

chart.GCS_V<-temp

tmp <- order(chart.GCS_V$CHARTTIME)

temp <- chart.GCS_V[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.GCS_V <- temp[tmp2,]

#gcs verbal values within 24 hours of HE
GCS_V.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.GCS_V$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.GCS_V$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	GCS_V.24h[[i]] <- chart.GCS_V$VALUENUM[temp[temp2.n]]
}


#GCS motor
chart.GCS_M <- which(temp2$ITEMID == 223901)

temp <- temp2[chart.GCS_M,]

chart.GCS_M<-temp

tmp <- order(chart.GCS_M$CHARTTIME)

temp <- chart.GCS_M[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.GCS_M <- temp[tmp2,]

#gcs m values within 24 hours of HE
GCS_M.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.GCS_M$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.GCS_M$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	GCS_M.24h[[i]] <- chart.GCS_M$VALUENUM[temp[temp2.n]]
}

GCS.total <- list()
GCS.samelength <- rep(0, length(ID.mat.new3[,1]))
GCS_E.ind <- rep(0, nrow(ID.mat.new3))
GCS_M.ind <- rep(0, nrow(ID.mat.new3))
GCS_V.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(GCS_E.24h[[i]])>0){
		GCS_E.ind[i] <- 1 
	}
	if(length(GCS_M.24h[[i]])>0){
		GCS_M.ind[i] <- 1 
	}
	if(length(GCS_V.24h[[i]])>0){
		GCS_V.ind[i] <- 1 
	}
	if(GCS_E.ind[i]+GCS_M.ind[i]+GCS_V.ind[i] == 3){
		GCS.total[[i]] <- min(GCS_M.24h[[i]])+min(GCS_V.24h[[i]])+min(GCS_E.24h[[i]])
	}
	else{
	GCS.total[[i]] <- NULL
	}
}

GCS.score <- rep(NA, length(ID.mat.new3[,1]))
GCS.ind <- rep(0, nrow(ID.mat.new3))
for(i in 1:nrow(ID.mat.new3)){
	if(length(GCS.total[[i]])>0){
		GCS.ind[i] <- 1
	}
	if(min(GCS.total[[i]])<6 & min(GCS.total[[i]]) > -Inf){
		GCS.score[i] <- 26
	}else if(min(GCS.total[[i]])<9 & min(GCS.total[[i]]) > -Inf){
		GCS.score[i] <- 13
	}else if(min(GCS.total[[i]])<11 & min(GCS.total[[i]]) > -Inf){
		GCS.score[i] <- 7
	}else if(min(GCS.total[[i]])<14 & min(GCS.total[[i]]) > -Inf){
		GCS.score[i] <- 5
	}
}

#BUN
chart.BUN <- which(temp2$ITEMID == 225624)

temp <- temp2[chart.BUN,]

chart.BUN<-temp

tmp <- order(chart.BUN$CHARTTIME)

temp <- chart.BUN[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.BUN <- temp[tmp2,]

#bun values within 24 hours of HE
BUN.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.BUN$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.BUN$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	BUN.24h[[i]] <- chart.BUN$VALUENUM[temp[temp2.n]]
}

BUN.score <- rep(0, nrow(ID.mat.new3))
BUN.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(BUN.24h[[i]])>0){
		BUN.ind[i] <- 1
	}
	if(max(BUN.24h[[i]]) >= 84 & max(BUN.24h[[i]]) < Inf){
		BUN.score[i] <- 10
	}else if(max(BUN.24h[[i]]) >= 28 & max(BUN.24h[[i]]) < Inf){
		BUN.score[i] <- 6
	}
}

#Sodium serum ID = 220645
chart.NA <- which(temp2$ITEMID == 220645)

temp <- temp2[chart.NA,]

chart.NA<-temp

tmp <- order(chart.NA$CHARTTIME)

temp <- chart.NA[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.NA <- temp[tmp2,]

#NA values within 24 hours of HE
NA.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.NA$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.NA$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	NA.24h[[i]] <- chart.NA$VALUENUM[temp[temp2.n]]
}

NA.score <- rep(0, nrow(ID.mat.new3))
NA.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(NA.24h[[i]])>0){
		NA.ind[i] <- 1
	}
	if(min(NA.24h[[i]]) < 125 & min(NA.24h[[i]]) > -Inf){
		NA.score[i] <- 5
	}else if(max(NA.24h[[i]]) >= 145 & max(NA.24h[[i]]) < Inf){
		NA.score[i] <- 1
	}
}

#Potassium serum ID = 227442
chart.K <- which(temp2$ITEMID == 227442)

temp <- temp2[chart.K,]

chart.K<-temp

tmp <- order(chart.K$CHARTTIME)

temp <- chart.K[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.K <- temp[tmp2,]

#k values within 24 hours of HE
K.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.K$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.K$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	K.24h[[i]] <- chart.K$VALUENUM[temp[temp2.n]]
}

K.score <- rep(0, nrow(ID.mat.new3))
K.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(K.24h[[i]])>0){
		K.ind[i] <- 1
	}
	if(min(K.24h[[i]]) < 3 & min(K.24h[[i]]) > -Inf){
		K.score[i] <- 3
	}else if(max(K.24h[[i]]) >= 5 & max(K.24h[[i]]) < Inf){
		K.score[i] <- 3
	}
}

#HCO3 (serum)
chart.HCO3 <- which(temp2$ITEMID == 227443)

temp <- temp2[chart.HCO3,]

chart.HCO3<-temp

tmp <- order(chart.HCO3$CHARTTIME)

temp <- chart.HCO3[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.HCO3 <- temp[tmp2,]

#HCO3 values within 24 hours of HE
HCO3.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.HCO3$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.HCO3$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	HCO3.24h[[i]] <- chart.HCO3$VALUENUM[temp[temp2.n]]
}


HCO3.score <- rep(0, nrow(ID.mat.new3))
HCO3.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(HCO3.24h[[i]])>0){
		HCO3.ind[i] <- 1
	}
	if(min(HCO3.24h[[i]]) < 15 & min(HCO3.24h[[i]]) > -Inf){
		HCO3.score[i] <- 6
	}else if(min(HCO3.24h[[i]]) <= 19 & min(HCO3.24h[[i]]) > -Inf){
		HCO3.score[i] <- 3
	}
}

#Bilirubin
chart.Bili <- which(temp2$ITEMID == 225690)

temp <- temp2[chart.Bili,]

chart.Bili<-temp

tmp <- order(chart.Bili$CHARTTIME)

temp <- chart.Bili[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.Bili <- temp[tmp2,]

#Bili values within 24 hours of HE
Bili.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.Bili$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.Bili$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	Bili.24h[[i]] <- chart.Bili$VALUENUM[temp[temp2.n]]
}


Bili.score <- rep(0, nrow(ID.mat.new3))
Bili.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(Bili.24h)>0){
		Bili.ind[i] <- 1
	}
	if(max(Bili.24h[[i]]) >= 6 & max(Bili.24h[[i]]) < Inf){
		Bili.score[i] <- 9
	}else if(max(Bili.24h[[i]]) >= 4 & max(Bili.24h[[i]]) < Inf){
		Bili.score[i] <- 4
	}
}

#WBC
chart.WBC <- which(temp2$ITEMID == 220546)

temp <- temp2[chart.WBC,]

chart.WBC<-temp

tmp <- order(chart.WBC$CHARTTIME)

temp <- chart.WBC[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.WBC <- temp[tmp2,]

WBC.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.WBC$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.WBC$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	WBC.24h[[i]] <- chart.WBC$VALUENUM[temp[temp2.n]]
}


WBC.score <- rep(0, nrow(ID.mat.new3))
WBC.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(WBC.24h)>0){
		WBC.ind[i] <- 1
	}
	if(min(WBC.24h[[i]])*1000 < 1000 & min(WBC.24h[[i]])*1000 > -Inf){
		WBC.score[i] <- 12
	}else if(max(WBC.24h[[i]])*1000 >= 20000 & max(WBC.24h[[i]])*1000 < Inf){
		WBC.score[i] <- 3
	}
}


#Urine Volume Out in mL; outputevents
temp.out<-which(outputevents$SUBJECT_ID %in% ID.mat.new3[,3])

temp.out2 <- outputevents[temp.out,]

chart.UrOut <- which(temp.out2$ITEMID %in% c(226631, 226627, 226566, 227489))

temp <- temp.out2[chart.UrOut,]

chart.UrOut<-temp

tmp <- order(chart.UrOut$CHARTTIME)

temp <- chart.UrOut[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.UrOut <- temp[tmp2,]

UrOut.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.UrOut$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.UrOut$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	UrOut.24h[[i]] <- chart.UrOut$VALUE[temp[temp2.n]]
}

adm.time <- list()
for(i in which(!is.na(ICUID))){
	adm.time[[i]] <- icu$INTIME[which(icu$ICUSTAY_ID == ICUID[i])]
}

time.before.HE <- list()
for(i in which(!is.na(ICUID))){
	time.before.HE[[i]] <- difftime(HE.starttime[i], adm.time[[i]],units="hours")
}


UrOut.score <- rep(11, nrow(ID.mat.new3))
UrOut.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(UrOut.24h[[i]])>0){
		UrOut.ind[i] <- 1
	}
	if((sum(UrOut.24h[[i]])/1000)*(24/min(c(time.before.HE[[i]],24))) < 0.5 & sum(UrOut.24h[[i]]) > -Inf){
		UrOut.score[i] <- 11
	}else if(sum(UrOut.24h[[i]])/1000*(24/min(c(time.before.HE[[i]],24))) < 1 & sum(UrOut.24h[[i]]) > -Inf){
		UrOut.score[i] <- 4
	}else if(sum(UrOut.24h[[i]])/1000*(24/min(c(time.before.HE[[i]],24))) >= 1 & sum(UrOut.24h[[i]]) < Inf){
		UrOut.score[i] <- 0
	}
}

#Urine Volume Out in mL; outputevents
temp.out<-which(outputevents$SUBJECT_ID %in% ID.mat.new3[,3])

temp.out2 <- outputevents[temp.out,]

chart.UrOut <- which(temp.out2$ITEMID %in% c(226631, 226627, 226566, 227489))

temp <- temp.out2[chart.UrOut,]

chart.UrOut<-temp

tmp <- order(chart.UrOut$CHARTTIME)

temp <- chart.UrOut[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.UrOut <- temp[tmp2,]

#total volume urine output 3hour period prior to HE
UrOut.3h <- rep(0, nrow(ID.mat.new3))
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.UrOut$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.UrOut$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 3 & time.diff >= 0)
	UrOut.3h[i] <- sum(chart.UrOut$VALUE[temp[temp2.n]])
}

#PaO2 ID = 220224
chart.PaO2 <- which(temp2$ITEMID == 220224)

temp <- temp2[chart.PaO2,]

chart.PaO2<-temp

tmp <- order(chart.PaO2$CHARTTIME)

temp <- chart.PaO2[tmp,]

tmp2 <- order(temp$SUBJECT_ID)

chart.PaO2 <- temp[tmp2,]

PaO2.24h <- list()
for(i in 1:nrow(ID.mat.new3)){
	temp <- which(chart.PaO2$SUBJECT_ID==ID.mat.new3[i,3])
	time.diff <- difftime(HE.starttime[i],chart.PaO2$CHARTTIME[temp],units="hours")
	temp2.n <- which(time.diff <= 24 & time.diff >= 0)
	PaO2.24h[[i]] <- chart.PaO2$VALUENUM[temp[temp2.n]]
}

PaO2.score <- rep(0, nrow(ID.mat.new3))
PaO2.ind <- rep(0, nrow(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(PaO2.24h) > 0){
		PaO2.ind[i] <- 1
	}
	if(min(PaO2.24h[[i]]) < 100 & min(PaO2.24h[[i]]) > -Inf){
		PaO2.score[i] <- 11
	}else if(min(PaO2.24h[[i]]) < 200 & min(PaO2.24h[[i]]) > -Inf){
		PaO2.score[i] <- 9
	}else if(min(PaO2.24h[[i]]) >= 200 & min(PaO2.24h[[i]]) < Inf){
		PaO2.score[i] <- 6
	}
}


CAREUNIT_ENTER <- rep(NA, ncol(ID.mat.new3))

for(i in 1:nrow(ID.mat.new3)){
	if(length(which(icu$ICUSTAY_ID == chart.meta$ICUSTAY_ID[ID.mat.new3[i,1]]))>0)
	CAREUNIT_ENTER[i] <- icu$FIRST_CAREUNIT[which(icu$ICUSTAY_ID == chart.meta$ICUSTAY_ID[ID.mat.new3[i,1]])]
} 

#TYPE OF ADMISSION
admType.score <- rep(0, nrow(ID.mat.new3))

temp <- which(admissions$SUBJECT_ID %in% ID.mat.new3[,3])

adm.new<-admissions[temp,]

temp <- sort(adm.new$SUBJECT_ID)

admType.score <- ifelse(adm.new$ADMISSION_TYPE[temp] == "ELECTIVE", 0, 1)


HADMID <- chart.meta$HADM_ID[ID.mat.new3[,1]]
temp <- which(admissions$HADM_ID %in% HADMID)
adm.meta <- admissions[temp,]
admType.score <- ifelse(adm.meta$ADMISSION_TYPE == "ELECTIVE", 0, 1)

for(i in which(admType.score==1)){
	if(CAREUNIT_ENTER[i] %in% c(2,5)){
		admType.score[i] <- 8
	}else{
		admType.score[i] <- 6
	}
}

tmp <- order(ICD$SUBJECT_ID)

tmp2 <- ICD[tmp,]

ICD <- tmp2

tmp <- which(ICD$SUBJECT_ID %in% ID.mat.new3[,3])

tmp2 <- ICD[tmp,]

ICD <- tmp2

tmp <- which(ICD$HADM_ID %in% HADM)

tmp2 <- ICD[tmp,]

ICD <- tmp2


subj.ICDs <- list()
for(i in 1:nrow(ID.mat.new3)){
	tmp <- which(ICD$SUBJECT_ID == ID.mat.new3[i,3])
	subj.ICDs[[i]] <- unique(ICD$ICD9_CODE[tmp])
}

subj.diag <- lapply(1:nrow(ID.mat.new3), function(i) 
	D_ICD$SHORT_TITLE[which(D_ICD$ICD9_CODE %in% subj.ICDs[[i]])])

##Elixhauser comorbidity ICD9 codes
#Congestive heart failure: 
chf.codes <- c(39891, 40211, 40291, 40411, 40413, 40491, 40493, 4280, 4281, 4282,
	4283, 4284, 4285, 4286, 4287, 4288, 4289)

chf.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% chf.codes) > 0, 1, 0)))	
	
#Cardiac arrhythmias: 
card.codes <- c(42610, 42611, 42613, as.numeric(paste("426", 2:53, sep="")), 
	as.numeric(paste("426", 6:89, sep="")), 4270, 4272, 42731, 42760, 4279, 7850, "V450", "V533")

card.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% card.codes) > 0, 1, 0)))	
		
#Valvular disease 
valve.codes <- c("09320", "09321", "09322", "09323", "09324", as.numeric(paste("394", 0:100, sep="")),
	as.numeric(paste("395", 0:100, sep="")),as.numeric(paste("396", 0:100, sep="")),
	as.numeric(paste("397", 0:1, sep="")),as.numeric(paste("424", 0:91, sep="")),
	as.numeric(paste("746", 3:6, sep="")), "V422", "V433")

valve.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% valve.codes) > 0, 1, 0)))	
		
#Pulmonary circulation disorders: 
pulm.codes <- c(as.numeric(paste("416", 0:9, sep="")), 4179)

pulm.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% pulm.codes) > 0, 1, 0)))	
	
#Peripheral vascular disorders: 
periph.codes<-c(as.numeric(paste("440", 0:9, sep="")),4412, 4414, 4417, 4419,
	as.numeric(paste("443", 1:9, sep="")), 4471, 5571, 5579, "V434")

periph.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% periph.codes) > 0, 1, 0)))	
	
#Hypertension, uncomplicated: 
hyper.unc.codes<-c(4011,4019)

hyperunc.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% hyper.unc.codes) > 0, 1, 0)))	
	
	#		, complicated:	 
hyper.com.codes <- c(40210, 40290, 40410, 40490, 40511, 40519, 40591, 40599) 

hypercom.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% hyper.com.codes) > 0, 1, 0)))	
	
#Paralysis: 
paral.codes <- c(as.numeric(paste("342", 0:12, sep="")), as.numeric(paste("342", 9:100, sep="")),
	as.numeric(paste("343", 0:100, sep="")), as.numeric(paste("344", 0:9, sep="")))

paral.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% paral.codes) > 0, 1, 0)))	
		
#Other neurological disorders: 
neur.dis.codes <- c(3319, 3320, 3334, 3335, as.numeric(paste("334", 0:100, sep="")),
	as.numeric(paste("335", 0:9, sep="")), 340, as.numeric(paste("341", 1:9, sep="")),
	as.numeric(paste("3450", 0:9, sep="")),34510, 34511, as.numeric(paste("345", 40:51, sep="")),
	as.numeric(paste("345", 80:91, sep="")), 3481, 3483, 7803, 7843)

neur.dis.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% neur.dis.codes) > 0, 1, 0)))	
		
#Chronic pulmonary disease: 
cpulm.codes <- c(490, 
	as.numeric(paste("491", 0:100, sep="")), as.numeric(paste("492", 0:8, sep="")),
	as.numeric(paste("4930", 0:9, sep="")),as.numeric(paste("493", 10:91, sep="")), 
	494, as.numeric(paste("495", 0:100, sep="")), 496, 497, 498, 499, 500, 501, 502, 
	503, 504, 505, 5064)
	
cpulm.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% cpulm.codes) > 0, 1, 0)))	
	
#Diabetes, uncomplicated: 
diab.unc.codes <- c(as.numeric(paste("2500", 0:9, sep="")), as.numeric(paste("250", 10:33, sep="")))

diab.unc.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% diab.unc.codes) > 0, 1, 0)))	
	
	
#Diabetes, compicated: 
diab.com.codes <- c(as.numeric(paste("250", 40:73, sep="")), as.numeric(paste("250", 90:93, sep="")))

diab.com.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% diab.com.codes) > 0, 1, 0)))	
	

#Hypothyroidism: 
hypoth.codes <- c(243, 2441, 2442, 2448, 2449)

hypoth.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% hypoth.codes) > 0, 1, 0)))	
	

#Renal failure: 
renal.codes <- c(40311, 40391, 40412, 40492, 585, 586, "V420", "V451", "V560", "V568")

renal.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% renal.codes) > 0, 1, 0)))	
	

#Liver disease: 
liver.codes <- c("07032", "07033", "07054", 4560, 4561, 45620, 45621, 5710, 5712, 5713,
	as.numeric(paste("571", 40:49, sep="")), 5715, 5716, 5718, 5719, 5723, 5728, "V427")

liver.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% liver.codes) > 0, 1, 0)))	
	
	
#Peptic ulcer disease excluding bleeding: 
ulcer.codes <- c(53170, 53190, 53270, 53290, 53370, 53390, 53470, 53490, "V1271")

ulcer.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% ulcer.codes) > 0, 1, 0)))	
	
		
#AIDS: 
aids.codes <- "042"

aids.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% aids.codes) > 0, 1, 0)))	
	

#Lymphoma: 
lymph.codes <- c(as.numeric(paste("2000", 0:9, sep="")), as.numeric(paste("200", 10:99, sep="")),
	as.numeric(paste("202", 50:99, sep="")), 203.01, 203.00, 20100:20238,  
	as.numeric(paste("203", 8:81, sep="")), 2386, 2733, "V1071", "V1072", "V1079")

lymph.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% lymph.codes) > 0, 1, 0)))	
	
	
#Metastatic cancer: 
meta.cancer.codes <- c(as.numeric(paste("196", 0:100, sep="")), as.numeric(paste("197", 0:100, sep="")), as.numeric(paste("198", 0:100, sep="")),as.numeric(paste("199", 0:1, sep="")))

meta.cancer.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% meta.cancer.codes) > 0, 1, 0)))	
	
		
#Solid tumor without metastasis: 
tumor.wo.meta.codes <- c(as.numeric(paste("140", 0:100, sep="")),
	as.numeric(paste("141", 0:100, sep="")),as.numeric(paste("142", 0:100, sep="")),
	as.numeric(paste("143", 0:100, sep="")),as.numeric(paste("144", 0:100, sep="")),
	as.numeric(paste("145", 0:100, sep="")),as.numeric(paste("146", 0:100, sep="")),
	as.numeric(paste("147", 0:100, sep="")),as.numeric(paste("148", 0:100, sep="")),
	as.numeric(paste("149", 0:100, sep="")),as.numeric(paste("150", 0:100, sep="")),
	as.numeric(paste("151", 0:100, sep="")),as.numeric(paste("152", 0:100, sep="")),
	as.numeric(paste("153", 0:100, sep="")),as.numeric(paste("154", 0:100, sep="")),
	as.numeric(paste("155", 0:100, sep="")),as.numeric(paste("156", 0:100, sep="")),
	as.numeric(paste("157", 0:100, sep="")),as.numeric(paste("158", 0:100, sep="")),
	as.numeric(paste("159", 0:100, sep="")),as.numeric(paste("160", 0:100, sep="")),
	as.numeric(paste("161", 0:100, sep="")),as.numeric(paste("162", 0:100, sep="")),
	as.numeric(paste("163", 0:100, sep="")),as.numeric(paste("164", 0:100, sep="")),
	as.numeric(paste("165", 0:100, sep="")),as.numeric(paste("166", 0:100, sep="")),
	as.numeric(paste("167", 0:100, sep="")),as.numeric(paste("168", 0:100, sep="")),
	as.numeric(paste("169", 0:100, sep="")),as.numeric(paste("170", 0:100, sep="")),
	as.numeric(paste("171", 0:100, sep="")),as.numeric(paste("172", 0:9, sep="")),
	as.numeric(paste("174", 0:100, sep="")),as.numeric(paste("175", 0:9, sep="")),
	179:195, 
	as.numeric(paste("179", 0:100, sep="")),as.numeric(paste("180", 0:100, sep="")),
	as.numeric(paste("181", 0:100, sep="")),as.numeric(paste("182", 0:100, sep="")),
	as.numeric(paste("183", 0:100, sep="")),as.numeric(paste("184", 0:100, sep="")),
	as.numeric(paste("185", 0:100, sep="")),as.numeric(paste("186", 0:100, sep="")),
	as.numeric(paste("187", 0:100, sep="")),as.numeric(paste("188", 0:100, sep="")),
	as.numeric(paste("189", 0:100, sep="")),as.numeric(paste("190", 0:100, sep="")),
	as.numeric(paste("191", 0:100, sep="")),as.numeric(paste("192", 0:100, sep="")),
	as.numeric(paste("193", 0:100, sep="")),as.numeric(paste("194", 0:100, sep="")),
	as.numeric(paste("195", 0:8, sep="")),paste("V100", 0:100, sep=""), "V103")

tumor.wo.meta.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% tumor.wo.meta.codes) > 0, 1, 0)))	
	
			
#Rheumatoid arthritis/collagen vascular diseases: 
arth.codes <- c(7010,as.numeric(paste("710", 0:9, sep="")),
	7140:7149, 7200:7209, 725)
	
arth.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% arth.codes) > 0, 1, 0)))	
	

#Coagulopathy: 
coag.codes <- c(2860:2869, 2871, 2873:2875)

coag.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% coag.codes) > 0, 1, 0)))	
	

#Obesity: 
obesity.codes <- 2780

obesity.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% obesity.codes) > 0, 1, 0)))	
	

#Weight loss: 
weight.codes <- c(260:263, as.numeric(paste("260", 0:100, sep="")), as.numeric(paste("263", 0:100, sep="")))

weight.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% weight.codes) > 0, 1, 0)))	
	

#Fluid and electrolyte disorders: 
fluid.dis.codes <- 2760:2769

fluid.dis.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% fluid.dis.codes) > 0, 1, 0)))	
	

#Blood loss anemia: 
blood.codes <- 2800

blood.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% blood.codes) > 0, 1, 0)))	
	

#Deficiency anemias: 
deficiency.codes <- c(2801:2809, 2810:2819, 2859)

deficiency.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% deficiency.codes) > 0, 1, 0)))	
	

#Alcohol abuse: 
alcohol.codes <- c(2911, 2912, 2915, 2918, 2919, 30390:30393, 30500:30503, "V113")

alcohol.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% alcohol.codes) > 0, 1, 0)))	
	

#Drug abuse: 
drug.codes <- c(2920, 29282:29289, 2929, 30400:30493, 30520:30593)

drug.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% drug.codes) > 0, 1, 0)))	
	

#Psychoses: 
psychoses.codes <- c(29500:29799, 2980:2989, 2970:2979, 2960:2969, 2950:2959,29910,29911)

psychoses.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% psychoses.codes) > 0, 1, 0)))	
	

#Depression: 
depression.codes <- c(3004, 30112, 3090, 3091, 311)

depression.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% depression.codes) > 0, 1, 0)))	

hema.malig.codes <- c(as.numeric(paste("200", 0:100, sep="")),
	as.numeric(paste("201", 0:100, sep="")),as.numeric(paste("202", 0:100, sep="")),
	as.numeric(paste("203", 0:100, sep="")),as.numeric(paste("204", 0:100, sep="")),
	as.numeric(paste("205", 0:100, sep="")),as.numeric(paste("206", 0:100, sep="")),
	as.numeric(paste("207", 0:100, sep="")),as.numeric(paste("208", 0:100, sep="")),
	as.numeric(paste("209", 0:100, sep="")))
		
hema.malig.ind <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(sum(subj.ICDs[[i]] %in% hema.malig.codes) > 0, 1, 0)))	
		
chronDiseaseSaps.score <- unlist(lapply(1:nrow(ID.mat.new3), function(i)
	ifelse(aids.ind[i] == 1, 17, ifelse(hema.malig.ind[i] == 1, 10,
	ifelse(meta.cancer.ind[i] == 1, 9, 0)))))


SAPSII <- admType.score + PaO2.score + UrOut.score + WBC.score + Bili.score + HCO3.score + K.score + NA.score + BUN.score + GCS.score + temp.score + sbp.score + age.score + hr.score + chronDiseaseSaps.score

treated.with.fluid <- rep(NA, nrow(ID.mat.new3)-1)
treated.with.vaso <- rep(NA, nrow(ID.mat.new3)-1)
for(i in 1:(nrow(ID.mat.new3)-1)){
	if(length(inputs.during.HE[[i]]) == 0){
		treated.with.fluid[i] <- 0
		treated.with.vaso[i] <- 0
	}else{
		if(sum(fluid.ids %in% inputs.during.HE[[i]]) > 0){
			treated.with.fluid[i] <- 1
		}else{
			treated.with.fluid[i] <- 0
		}
		if(sum(vaso.ids %in% inputs.during.HE[[i]]) > 0){
			treated.with.vaso[i] <- 1			
		}else{
			treated.with.vaso[i] <- 0
		}
	}
}

#keep subjects treated with fluid or vaso but NOT both
subj.treated.ind <- ifelse(treated.with.fluid + treated.with.vaso == 1, 1, 0)

#MAP at treatment
MAP_at_trt <-rep(NA, nrow(ID.mat.new3))
for(i in 1:(nrow(ID.mat.new3)-1)){
	if(length(first.trt.time.diff[[i]]) >= 1){
	if(subj.treated.ind[i] == 1){
		actual.trt.time <- trt.time[[i]][which(trt.time.diff[[i]] == first.trt.time.diff[[i]])]
		indicator <- tail(which(difftime(chart.meta$CHARTTIME[(ID.mat.new3[i,1]-1):ID.mat.new3[i,2]], actual.trt.time )<0),n=1)
		indicator2 <- ((ID.mat.new3[i,1]-1):(ID.mat.new3[i,2]))[indicator]
		MAP_at_trt[i] <- chart.meta$VALUENUM[indicator2]
	}
	}
}


data.mat <- cbind(
unlist(trt.HE.length), c(treated.with.fluid, NA), c(treated.with.vaso, NA), mean_MAP_3h, UrOut.3h, 
creat.value, CAREUNIT_ENTER, AGE.new, GENDER, SAPSII, 
drug.ind, psychoses.ind, alcohol.ind, deficiency.ind, blood.ind, 
fluid.dis.ind, weight.ind, coag.ind, arth.ind, aids.ind, 
tumor.wo.meta.ind, lymph.ind, ulcer.ind, liver.ind, 
renal.ind, hypoth.ind, diab.com.ind, diab.unc.ind, cpulm.ind, 
neur.dis.ind, paral.ind, hyperunc.ind, periph.ind, 
pulm.ind, valve.ind, card.ind, chf.ind, MAP_at_trt)[which(subj.treated.ind == 1),] 

#Note: Indicators for Hypertension (complicated) and obesity were dropped due to low number of subjects


ind <- which(!is.na(rowSums(data.mat[,-c(2,3)])))

X <- data.mat[ind,4:ncol(data.mat)]
Y <- log(data.mat[ind,1]/60 + 1)
A <- data.mat[ind,3]
A[is.na(A)] <- 0


final.data <- cbind(X, Y, A)

##############################################
##############################################
##############BEGIN DATA ANALYSIS#############
##############################################
##############################################
p = 38

##BAC
library(bacr)
library(parallel)
library(MASS)

data <- final.data


Xtemp <- data[,1:35]
Yorig <- data[,36]
Aorig <- data[,37]

Xind1 <- ifelse(Xtemp[,4]==1, 1, 0)
Xind2 <- ifelse(Xtemp[,4]==2, 1, 0)
Xind3 <- ifelse(Xtemp[,4]==5, 1, 0)
Xind4 <- ifelse(Xtemp[,4]==6, 1, 0)

Xorig <- cbind(Xtemp[,-4], Xind1, Xind2, Xind3, Xind4)

Z <- as.data.frame(cbind(Yorig, Aorig, Xorig))

colnames(Z) <- unlist(lapply(1:ncol(Z), function(i) paste("V", i, sep="")))

result <- bac(data=Z, exposure = "V2", outcome="V1", confounders=paste("V", 3:(p+2), sep=""), familyX="binomial", familyY="gaussian", interactors=NULL,num_its=10000,burnM=1, burnB=1,thin=1)

#posterior inclusion probability (and average causal effect w/ 95% posterior interval)
summary(result)$PIP
#BAC objects:

#Exposure effect estimate:
#        posterior mean     95% posterior interval     
#                -0.096             (-0.27, 0.087)     


#Covariates with posterior inclusion probability > 0.5:
#    posterior inclusion probability
#V3                           1.0000
#V8                           1.0000
#V36                          0.8854
#V22                          0.6526
#V38                          0.5198
#    V3     V4     V5     V6     V7     V8     V9    V10    V11    V12    V13    V14    V15 
#1.0000 0.0496 0.2973 0.1252 0.1076 1.0000 0.0470 0.0380 0.1902 0.0199 0.1034 0.0788 0.0604 
#   V16    V17    V18    V19    V20    V21    V22    V23    V24    V25    V26    V27    V28 
#0.0370 0.1421 0.0597 0.0468 0.0431 0.0236 0.6526 0.0836 0.1603 0.0396 0.0496 0.0698 0.0501 
#   V29    V30    V31    V32    V33    V34    V35    V36    V37    V38    V39    V40 
#0.0830 0.0283 0.0880 0.0151 0.0533 0.0479 0.0850 0.8854 0.1452 0.5198 0.0630 0.3564 



#average causal effect
mean(result$ACE)
#-0.09603978

#lower CL
quantile(result$ACE, 0.025)
# -0.26606 

#upper CL
quantile(result$ACE, 0.975)
#0.08701753 

##SSCE: 
#code for this function is here:
library(truncnorm) 
library(MCMCpack) 
library(parcor)

#Xorig: nxp matrix of covariates 
#Yorig: nx1 vector of outcomes
#Aorig: nx1 binary vector indicating treatment assignment
#tau.2: shrinkage parameter (set to large value to remove shrinkage bias; default is 1000) 
#M: number of MCMC iterations
#burn: number of burn-in iterations
SSCE <- function(Xorig, Yorig, Aorig, tau.2 = 1000, M = 5000, burn = 0, Bilevel = TRUE){
	#this stores outcome coefficients at each MCMC iteration 
	beta.sv <- list()
	
	#this stores treatment coefficients at each MCMC iteration 
	gamma.sv <- list()
	
	#this stores pi_0 at each MCMC iteration
	pi_0.sv <- list()
	
	#this stores sigma.2 at each MCMC iteration
	sigma.2.sv <- list()

	#number of covariates
	p <- ncol(Xorig)

	#sample size
	n <- nrow(Xorig)

	#transform covariates to have mean zero and unit variance
	Xstd <- cbind(t(t(Xorig - matrix(rep(apply(Xorig, 2, mean), nrow(Xorig)), 
		nrow(Xorig),byrow=TRUE))*(1/apply(Xorig, 2, sd))), 1, Aorig)

	#initialize beta to zero
	beta <- rep(0, p+2)

	#initialize gamma to zero
	gamma <- rep(0, p+1)

	#initialize pi_0
	pi_0 <- 0.5

	#initialize sigma.2
	sigma.2 <- 1

	#these two variables are used in computations in the Gibbs sampler below
	Sigma <- 1/(n - 1 + 1/tau.2)
	D.tau.inv <-diag(rep(1/tau.2, p))

		#############################################################
		######This block is used to estimate parameters for Bilevel SSCE#############
		##################################################################

		#To use the lasso on the outcome model, we will transform the outcome so that
		#we do not need to estimate an intercept or the main effect of treatment 
		##Such a transformation is not necessary but allows for all lasso software to be used
		##in this situation: an alternative is to include the intercept and main effect of treatment
		##in the model and not penalize their coefficients (i.e., not shrink them toward zero)

		#this is used in the transformation of the outcome
		Anew <- ifelse(Aorig == 1, 1, -1)

		#mean of outcome among treated (used in the transformation of the outcome)
		meanYtrt <- mean(Yorig[Anew == 1])

		#mean of outcome among untreated (used in the transformation of the outcome)
		meanYcont <- mean(Yorig[Anew == -1])

		#mean of each covariate among the treated (used in the transformation of the outcome)
		meanXtrt <- unlist(lapply(1:ncol(Xorig), function(i) mean(Xorig[Anew==1,i]))) 

		#mean of each covariate among the untreated (used in the transformation of the outcome)
		meanXcont <- unlist(lapply(1:ncol(Xorig), function(i) mean(Xorig[Anew==-1,i]))) 
	
		#used in the transformation of the outcome
		stdx <- unlist(lapply(1:p, function(i) sd(Xorig[,i] - (1/2)*(Anew + 1)*meanXtrt[i] - (1/2)*(1 -			
			Anew)*meanXcont[i])))
	
		#create transformed covariate matrix so that covariates have mean zero and sd=1
		Xout <- matrix(
			unlist(lapply(1:p, function(i) (Xorig[,i] - (1/2)*(Anew + 1)*meanXtrt[i] - (1/2)*(1 - 	
			Anew)*meanXcont[i])/stdx[i])), n, p)	

		#center the outcomes so that there is no need to estimate the intercept and main effect of 			
		#treatment (i.e., estimates for these coefficients are exactly zero after this transformation)
		Ycent = (Yorig - (1/2)*(Anew + 1)*meanYtrt - (1/2)*(1 - Anew)*meanYcont)

		#fit the lasso to the (transformed) outcome model, which now has only p parameters
		##10 fold cv is used to choose tuning parameter lambda
		lasso.fit <- adalasso(Xout, Ycent, k = 10, intercept = FALSE)

		#get estimated lasso coefficients
		lasso.coef <- lasso.fit$coefficients.lasso

		#determine which covariate coefficients are non-zero
		nonzero.lasso.coef <- which(lasso.coef != 0)

		#find sigma.2.y_a.x for all covariates that have coefficients equal to zero according to lasso
		temp <-  (1/(n-length(nonzero.lasso.coef)))*sum((Yorig - Xstd[,c(nonzero.lasso.coef, p+1, p+2)]
			%*%(solve(t(Xstd[,c(nonzero.lasso.coef, p+1, p+2)])%*%Xstd[,c(nonzero.lasso.coef, p+1, p+2)])
			%*%t(Xstd[,c(nonzero.lasso.coef, p+1, p+2)])%*%Yorig))^2) 

		#set sigma.2.y_a.x to the value above for all covariates (we change the values below for covariates
		#that are non-zero)
		sigma.2.y_a.x <- rep(temp, p)

		#find sigma.2.y_a.x for all covariates that have non-zero coefficients according to the lasso
		temp <- unlist(lapply(nonzero.lasso.coef, function(g) 
			(1/(n-length(nonzero.lasso.coef)))*sum((Yorig - Xstd[,c(nonzero.lasso.coef, p+1, p+2)][,-g]
				%*%(solve(t(Xstd[,c(nonzero.lasso.coef, p+1, p+2)][,-g])%*%
				Xstd[,c(nonzero.lasso.coef, p+1, p+2)][,-g])%*%t(Xstd[,c(nonzero.lasso.coef, p+1, p+2)][,-g])
				%*%Yorig))^2)  ))
	
		#set sigma.2.y_a.x to the values above for the covariates with non-zero coefficients
		sigma.2.y_a.x[nonzero.lasso.coef] <- temp	
	
		#set values for the other parameters in Bilevel SSCE	
		sigma.2.a_x <- rep(1, p)	
		sigma.2.z_a.x <- rep(1, p)
		sigma.2.z_x <- rep(1, p)

		##################################################################
		######End of block used to estimate parameters for Bilevel SSCE#############
		##################################################################


	#############START GIBBS SAMPLER##################
	for(iter in 1:(burn+M)){
		#Draw Astar
		Astar <- ifelse(Aorig == 1, rtruncnorm(n=1,a = 0, b = Inf, mean = Xstd[,-(p+2)]%*%gamma, sd = 1), 
			rtruncnorm(n=1, a = -Inf, b = 0, mean = Xstd[,-(p+2)]%*%gamma, sd=1))

	for(g in 1:p){
		#mean of slab for outcome coefficient
		mu.g.out <-  Sigma*t(Xstd[,g])%*%(Yorig - Xstd[,-g]%*%beta[-g])
		#mean of slab for treatment coefficient
		mu.g.trt <-  Sigma*t(Xstd[,g])%*%(Astar - Xstd[,-c(g,p+2)]%*%gamma[-g])

		#draw conditional prob. coefficients are zero
		l.g <-  pi_0/(pi_0 + (1 - pi_0)*(tau.2*tau.2)^(-1/2)*
			(Sigma*Sigma)^(1/2)*exp(
			(1/2)*((1/sigma.2)*(Sigma^(1/2)*t(Xstd[,g])%*%(Yorig - Xstd[,-g]%*%beta[-g]))^2 + 		
    		(Sigma^(1/2)*t(Xstd[,g])%*%(Astar - Xstd[,-c(g,p+2)]%*%gamma[-g]))^2)))
		
		#draw indicators denoting which coefficients are zero/non-zero				
		zero.ind <- rbinom(1, 1, l.g)

		#get coefficient values for SSCE or for first level of Bilevel SSCE
		###############################################
		if(zero.ind == 1){
			beta[g] <- 0
			gamma[g] <- 0
		}else{
			temp <- rnorm(n = 2, mean = c(mu.g.out, mu.g.trt), 
			sd =  sqrt(c(sigma.2*Sigma, Sigma)))
	
			beta[g] <- temp[1]
		
			gamma[g] <- temp[2]
		}
		###############################################

		#This is for Bilevel SSCE only
		############################################
		if(Bilevel == TRUE){
			##For each non-zero coefficient according to SSCE, we find the change in MSE if
			##this coefficient was set to zero; we then set the coefficients to zero if doing so
			##improves MSE of the treatment effect estimator  
			if(beta[g] != 0){
			 	var_small <- sigma.2.y_a.x[g]/sigma.2.a_x[g]*1/n
    			var_big <- (sigma.2.y_a.x[g] - sigma.2.z_a.x[g]*beta[g]^2)/
    		 	 	(sigma.2.a_x[g] - sigma.2.z_x[g]*(gamma[g]/sqrt(2*pi))^2)/n
    			bias <- (beta[g]*(gamma[g]/sqrt(2*pi))*sigma.2.z_x[g])/sigma.2.a_x[g] 
    			MSE_change <- bias^2 + var_small - var_big 
				if(MSE_change < 0){
					beta[g] <- 0
					gamma[g] <- 0
				}
			}
		}
		#################################################
	}

	#draw the intercept for the outcome model
	beta[p+1] <- rnorm(1, mean = (1/n)*t(Xstd[,p+1])%*%(Yorig - Xstd[,-(p+1)]%*%beta[-(p+1)]), 
		sd  = sqrt(sigma.2/n))

	#draw the treatment effect for the outcome model
	beta[p+2] <- rnorm(1, mean = (1/sum(Aorig))*t(Xstd[,p+2])%*%(Yorig - Xstd[,-(p+2)]%*%beta[-(p+2)]), 
		sd  = sqrt(sigma.2/(sum(Aorig))))

	#draw the intercept for the treatment model
	gamma[p+1] <- rnorm(1, mean = (1/n)*t(Xstd[,p+1])%*%(Astar - Xstd[,-c(p+1,p+2)]%*%gamma[-(p+1)]), 
		sd  = sqrt(1/n))
		
	no.non.zero <- sum(ifelse(beta[1:p] == 0, 0, 1))

	#draw sigma.2 
	sigma.2 <- rinvgamma(1, shape = n/2 + (1/2)*no.non.zero + 0.1, 
		scale = (1/2)*(t(Yorig - Xstd%*%beta)%*%(Yorig - Xstd%*%beta) + 
		t(beta[1:p])%*%D.tau.inv%*%beta[1:p]) + 0.1)

	#draw p_0
	pi_0 <- rbeta(1, 1 + p - no.non.zero, 1 + no.non.zero)
	
	#store parameters for this iteration
	beta.sv[[iter]] <- beta
	gamma.sv[[iter]] <- gamma
	sigma.2.sv[[iter]] <- sigma.2
	pi_0.sv[[iter]] <- pi_0	
	}

	#matrix of parameters in outcome model, each row represents one draw from the posterior
	out.mat <-matrix(unlist(beta.sv), nrow = M + burn, ncol = p+2, 
		byrow=TRUE)[(burn+1):(M+burn),]

	#matrix of covariate coefficients in outcome model; each row represents one draw from the posterior
	out.cov.mat <- out.mat[,1:p]
	
	#estimated posterior distribution of intercept in outcome model
	out.intercept.post <- out.mat[,p+1]
	
	#estimated posterior distribution of treatment effect 
	trt.effect.post <- out.mat[,p+2]

	out.cov.mat2 <- ifelse(abs(out.cov.mat) <= 1e-10, 0, 1)

	#this gives covariate inclusion probabilities
	IP <- colMeans(out.cov.mat2)

	#this finds mean of each coefficient in outcome model
	out.cov.means <- colMeans(out.cov.mat)
	
	#this finds mean intercept in outcome model
	out.intercept.mean <- mean(out.intercept.post)
	
	#mean treatment effect
	mean.trt.effect <- mean(out.mat[,p+2])

	#find lower limit of 95% credible interval for the treatment effect
	lower.limit <- quantile(out.mat[,p+2], 0.025)

	#find upper limit of 95% credible interval for the treatment effect
	upper.limit <- quantile(out.mat[,p+2], 0.975)

	#matrix of parameters in treatment model, each row represents one draw from the posterior
	trt.mat <- matrix(unlist(gamma.sv), nrow = M + burn, ncol = p+1, 
		byrow=TRUE)[(burn+1):(M+burn),]

	#matrix of covariate coefficients in treatment model; each row represents one draw from the posterior
	trt.cov.mat <- trt.mat[,1:p]
	
	#estimated posterior distribution of intercept in treatment model
	trt.intercept.post <- trt.mat[,p+1]
	
	#mean intercept in treatment model
	trt.intercept.mean <- mean(trt.intercept.post)
	
	#mean of each covariate coefficient in treatment model
	trt.cov.means <- colMeans(trt.cov.mat)

	return(list(IP=IP, mean.trt.effect = mean.trt.effect, lower.limit=lower.limit, upper.limit=upper.limit,
		 trt.effect.post = trt.effect.post, out.cov.means = out.cov.means, 
		 trt.cov.means = trt.cov.means, out.cov.mat = out.cov.mat, 
		 trt.cov.mat = trt.cov.mat, out.intercept.mean = out.intercept.mean, 
		 trt.intercept.mean = trt.intercept.mean, out.intercept.post = out.intercept.post,
		 trt.intercept.post = trt.intercept.post))
}


SSCE.fit <- SSCE(Xorig, Yorig, Aorig, M = 10000, burn = 1, Bilevel = FALSE)

#Inclusion probability
SSCE.fit$IP
#[1] 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

#average causal effect
SSCE.fit$mean.trt.effect
#-0.09465191

#lower CL
SSCE.fit$lower.limit
#-0.2703531

#upper CL
SSCE.fit$upper.limit
#0.08652652

##BSSCE: code for this function is above

BSSCE.fit <- SSCE(Xorig, Yorig, Aorig, M = 10000, burn = 1, Bilevel = TRUE)

#Inclusion probability
BSSCE.fit$IP
#[1] 1.0000 0.0000 0.0000 0.0000 0.0000 0.6624 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
#[14] 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
#[27] 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0003 0.0000 0.0000 0.0000 0.0000

#average causal effect
BSSCE.fit$mean.trt.effect
#-0.1089361 

#lower CL
BSSCE.fit$lower.limit
#-0.300527 
  
#upper CL
BSSCE.fit$upper.limit
#0.07750631


##BSSL
M <- 10001
tau.g.out.2 <- 1000
tau.g.trt.2 <- 1000

burn <- 1
beta.sv <- list()
delta.sv <- list()
sigma.2.sv <- list()
p <- ncol(Xorig)
n <- nrow(Xorig)

#transform covariates to have mean zero and unit variance
Xstd <- cbind(t(t(Xorig - matrix(rep(apply(Xorig, 2, mean), nrow(Xorig)), nrow(Xorig),byrow=TRUE))*(1/apply(Xorig, 2, sd))), 1, Aorig)

#initialize beta
#beta <- as.numeric(coef(lm(Yorig ~ Xstd - 1)))
beta <- rep(0, p+2)

#initialize pi_0
pi_0 <- 0.5

#set Sigma.g.out (n and tau.g.out.2 are fixed)
Sigma.g.out <- 1/(n - 1 + 1/tau.g.out.2)

#initialize Sigma.g.trt
#Sigma.g.trt <-  1/(n - 1 + 1/tau.g.trt.2)

D.tau.inv <-diag(rep(1/tau.g.out.2, p))

#initialize sigma.2
sigma.2 <- 1

#############START GIBBS SAMPLER##################
for(iter in 1:M){
	for(g in 1:p){
	mu.g.out <-  Sigma.g.out*t(Xstd[,g])%*%(Yorig - Xstd[,-g]%*%beta[-g])
	
	#draw conditional prob. coefficients are zero
	l.g <-  pi_0/(pi_0 + (1 - pi_0)*(tau.g.out.2)^(-1/2)*
			(Sigma.g.out)^(1/2)*exp(
			(1/2)*((1/sigma.2)*(Sigma.g.out^(1/2)*t(Xstd[,g])%*%(Yorig - Xstd[,-g]%*%beta[-g]))^2)))
		
	zero.ind <- rbinom(1, 1, l.g)

	if(zero.ind == 1){
		beta[g] <- 0
	}else{
		beta[g] <- rnorm(n = 1, mean = mu.g.out, 
			sd =  	sqrt(sigma.2*Sigma.g.out))
	}
	
}

	beta[p+1] <- rnorm(1, mean = (1/n)*t(Xstd[,p+1])%*%(Yorig - Xstd[,-(p+1)]%*%beta[-(p+1)]), 
		sd  = sqrt(sigma.2/n))

	beta[p+2] <- rnorm(1, mean = (1/sum(Aorig))*t(Xstd[,p+2])%*%(Yorig - Xstd[,-(p+2)]%*%beta[-(p+2)]), 
		sd  = sqrt(sigma.2/(sum(Aorig))))

		
	no.non.zero <- sum(ifelse(beta[1:p] == 0, 0, 1))

	sigma.2 <- rinvgamma(1, shape = n/2 + (1/2)*no.non.zero + 0.1, 
		scale = (1/2)*(t(Yorig - Xstd%*%beta)%*%(Yorig - Xstd%*%beta) + t(beta[1:p])%*%D.tau.inv%*%beta[1:p]) + 0.1)

	pi_0 <- rbeta(1, 1 + p - no.non.zero, 1 + no.non.zero)
	
	if(iter > burn){
		beta.sv[[iter-burn]] <- beta
	}
		
}

mat <-matrix(unlist(beta.sv), nrow = M-burn, ncol = p+2, byrow=TRUE)

#inclusion probability
OUT_IP<-colMeans(ifelse(abs(mat) > 0, 1, 0))[1:p]
#[1] 0.9999 0.0000 0.0005 0.0001 0.0003 0.0010 0.0000 0.0001 0.0001 0.0001 0.0001 0.0001 0.0001
#[14] 0.0002 0.0003 0.0000 0.0004 0.0001 0.0000 0.0000 0.0005 0.0002 0.0001 0.0000 0.0001 0.0000
#[27] 0.0001 0.0000 0.0002 0.0001 0.0000 0.0001 0.0000 0.0127 0.0000 0.0002 0.0000 0.0002

#average causal effect
mean(mat[,40])
#-0.1390202

#lower CL
quantile(mat[,40], 0.025)
# -0.3181977 

#upper CL
quantile(mat[,40], 0.975)
#0.03737616 


#Figure 3:
par(mfrow=c(2,2))
hist(SSCE.fit$trt.effect.post,ylim=c(0,2200), xlim=c(-0.6,0.3), main="SSCE", 
xlab="Treatment Effect", breaks=20)
hist(BSSCE.fit$trt.effect.post,ylim=c(0,2200), xlim=c(-0.6,0.3), main="BSSCE", 
xlab="Treatment Effect", breaks=20)
hist(mat[,40],ylim=c(0,2200), xlim=c(-0.6,0.3), main="BSSL", 
xlab="Treatment Effect", breaks=20)
hist(result$ACE,ylim=c(0,2200), xlim=c(-0.6,0.3), main="BAC", 
xlab="Treatment Effect", breaks=20)

dev.off()

BAC_IP<-c(1.0000, 0.0496, 0.2973, 0.1252, 0.1076, 1.0000, 0.0470, 0.0380, 0.1902, 0.0199, 0.1034, 0.0788, 0.0604, 
0.0370, 0.1421, 0.0597, 0.0468, 0.0431, 0.0236, 0.6526, 0.0836, 0.1603, 0.0396, 0.0496, 0.0698, 0.0501, 
0.0830, 0.0283, 0.0880, 0.0151, 0.0533, 0.0479, 0.0850, 0.8854, 0.1452, 0.5198, 0.0630, 0.3564)

IP_sort <- order(BAC_IP, decreasing=TRUE)

IP_mat <- cbind(SSCE.fit$IP, BSSCE.fit$IP, OUT_IP, BAC_IP)


temp<-lapply(1:ncol(Xorig), function(i) length(unique(Xorig[,i])))

binary.ind<-unlist(lapply(1:ncol(Xorig), function(i) ifelse(temp[[i]] == 2, 1, 0)))

cov.means.trt <- colMeans(Xorig[Aorig==1,])

cov.means.con <- colMeans(Xorig[Aorig==0,])

number.trt <- colSums(Xorig[Aorig == 1,])

number.con <- colSums(Xorig[Aorig == 0,])

sd.trt <- unlist(lapply(1:ncol(Xorig), function(i) sd(Xorig[Aorig==1,i])))

sd.con <- unlist(lapply(1:ncol(Xorig), function(i) sd(Xorig[Aorig==0,i])))
	
table <- cbind(round(ifelse(binary.ind == 1, number.trt, cov.means.trt),1), round(ifelse(binary.ind == 1, 100*cov.means.trt, sd.trt),1), round(ifelse(binary.ind == 1, number.con, cov.means.con),1), round(ifelse(binary.ind == 1, 100*cov.means.con, sd.con),1), round(IP_mat,3))[IP_sort,]	

names <- c("Mean MAP 3hrs prior to trt", "Urine output 3hrs prior to trt", "Creatinine value", "Age", "Sex","SAPSII","Drug abuse", "Psychoses", "Alcohol abuse","Deficiency anemias","Blood anemias","Fluid and electrolyte disorders","Weight loss","Coagulopathy","Rheumatoid arthritis","AIDS","Solid tumor without metastasis","Lymphoma","Peptic ulcer disease excluding bleeding","Liver disease","Renal failure","Hypothyroidism","Diabetes, complicated","Diabetes, uncomplicated","Chromic pulmonary disease","Other neurological disorders","Paralysis","Hypertension, uncomplicated", "Peripheral vascular disorders","Pulmonary circulation disorders","Valvular disease","Cardiac arrhythmias","Congestive heart failure","MAP at treatment","Medical ICU","Surgical ICU", "Coronary Care Unit", "Cardiac Surgery Recovery Unit")[IP_sort]

rownames(table)<-names

library(xtable)

xtable(table,digits=c(1,1,1,1,1,3,3,3,3))

#% latex table generated in R 3.2.2 by xtable 1.8-2 package
#\begin{table}[ht]
#\centering
#\begin{tabular}{rrrrrrrrr}
#  \hline
# & V1 & V2 & V3 & V4 & V5 & V6 & OUT\_IP & BAC\_IP \\ 
#  \hline
#Mean MAP 3hrs prior to trt & 68.0 & 9.6 & 66.1 & 6.8 & 1.000 & 1.000 & 1.000 & 1.000 \\ 
#  SAPSII & 72.1 & 14.1 & 64.7 & 14.5 & 1.000 & 0.662 & 0.001 & 1.000 \\ 
#  MAP at treatment & 56.4 & 13.3 & 58.1 & 11.5 & 0.000 & 0.000 & 0.013 & 0.885 \\ 
#  Liver disease & 66.0 & 9.4 & 39.0 & 20.0 & 0.000 & 0.000 & 0.000 & 0.653 \\ 
#  Surgical ICU & 209.0 & 29.7 & 68.0 & 34.9 & 0.000 & 0.000 & 0.000 & 0.520 \\ 
#  Cardiac Surgery Recovery Unit & 92.0 & 13.1 & 33.0 & 16.9 & 0.000 & 0.000 & 0.000 & 0.356 \\ 
#  Creatinine value & 1.6 & 1.5 & 1.4 & 1.3 & 0.000 & 0.000 & 0.000 & 0.297 \\ 
#  Alcohol abuse & 53.0 & 7.5 & 24.0 & 12.3 & 0.000 & 0.000 & 0.000 & 0.190 \\ 
#  Hypothyroidism & 90.0 & 12.8 & 28.0 & 14.4 & 0.000 & 0.000 & 0.000 & 0.160 \\ 
#  Medical ICU & 79.0 & 11.2 & 7.0 & 3.6 & 0.000 & 0.000 & 0.000 & 0.145 \\ 
#  Rheumatoid arthritis & 24.0 & 3.4 & 9.0 & 4.6 & 0.000 & 0.000 & 0.000 & 0.142 \\ 
#  Age & 66.7 & 13.7 & 65.5 & 16.0 & 0.000 & 0.000 & 0.000 & 0.125 \\ 
#  Sex & 428.0 & 60.9 & 107.0 & 54.9 & 0.000 & 0.000 & 0.000 & 0.108 \\ 
#  Blood anemias & 5.0 & 0.7 & 5.0 & 2.6 & 0.000 & 0.000 & 0.000 & 0.103 \\ 
#  Peripheral vascular disorders & 75.0 & 10.7 & 18.0 & 9.2 & 0.000 & 0.000 & 0.000 & 0.088 \\ 
#  Congestive heart failure & 210.0 & 29.9 & 46.0 & 23.6 & 0.000 & 0.000 & 0.000 & 0.085 \\ 
#  Renal failure & 33.0 & 4.7 & 7.0 & 3.6 & 0.000 & 0.000 & 0.000 & 0.084 \\ 
#  Paralysis & 11.0 & 1.6 & 4.0 & 2.1 & 0.000 & 0.000 & 0.000 & 0.083 \\ 
#  Fluid and electrolyte disorders & 341.0 & 48.5 & 80.0 & 41.0 & 0.000 & 0.000 & 0.000 & 0.079 \\ 
#  Chromic pulmonary disease & 167.0 & 23.8 & 36.0 & 18.5 & 0.000 & 0.000 & 0.000 & 0.070 \\ 
#  Coronary Care Unit & 93.0 & 13.2 & 28.0 & 14.4 & 0.000 & 0.000 & 0.000 & 0.063 \\ 
#  Weight loss & 46.0 & 6.5 & 16.0 & 8.2 & 0.000 & 0.000 & 0.000 & 0.060 \\ 
#  AIDS & 3.0 & 0.4 & 1.0 & 0.5 & 0.000 & 0.000 & 0.000 & 0.060 \\ 
#  Valvular disease & 158.0 & 22.5 & 42.0 & 21.5 & 0.000 & 0.000 & 0.000 & 0.053 \\ 
#  Other neurological disorders & 74.0 & 10.5 & 14.0 & 7.2 & 0.000 & 0.000 & 0.000 & 0.050 \\ 
#  Urine output 3hrs prior to trt & 39.6 & 693.8 & 17.5 & 184.0 & 0.000 & 0.000 & 0.000 & 0.050 \\ 
#  Diabetes, uncomplicated & 182.0 & 25.9 & 48.0 & 24.6 & 0.000 & 0.000 & 0.000 & 0.050 \\ 
#  Cardiac arrhythmias & 284.0 & 40.4 & 74.0 & 37.9 & 0.000 & 0.000 & 0.000 & 0.048 \\ 
#  Drug abuse & 25.0 & 3.6 & 10.0 & 5.1 & 0.000 & 0.000 & 0.000 & 0.047 \\ 
#  Solid tumor without metastasis & 101.0 & 14.4 & 22.0 & 11.3 & 0.000 & 0.000 & 0.000 & 0.047 \\ 
#  Lymphoma & 17.0 & 2.4 & 4.0 & 2.1 & 0.000 & 0.000 & 0.000 & 0.043 \\ 
#  Diabetes, complicated & 49.0 & 7.0 & 11.0 & 5.6 & 0.000 & 0.000 & 0.000 & 0.040 \\ 
#  Psychoses & 29.0 & 4.1 & 9.0 & 4.6 & 0.000 & 0.000 & 0.000 & 0.038 \\ 
#  Coagulopathy & 162.0 & 23.0 & 49.0 & 25.1 & 0.000 & 0.000 & 0.000 & 0.037 \\ 
#  Hypertension, uncomplicated & 327.0 & 46.5 & 91.0 & 46.7 & 0.000 & 0.000 & 0.000 & 0.028 \\ 
#  Peptic ulcer disease excluding bleeding & 12.0 & 1.7 & 6.0 & 3.1 & 0.000 & 0.000 & 0.000 & 0.024 #\\ 
 # Deficiency anemias & 137.0 & 19.5 & 35.0 & 17.9 & 0.000 & 0.000 & 0.000 & 0.020 \\ 
 # Pulmonary circulation disorders & 71.0 & 10.1 & 24.0 & 12.3 & 0.000 & 0.000 & 0.000 & 0.015 \\ 
 #  \hline
#\end{tabular}
#\end{table}
