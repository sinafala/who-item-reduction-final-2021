#------------------------------------------------------------------------------#
# item_response.R: Item Response Analysis
#
# This file prepares each data set and produces the item response patterns for
# the merged data set.  There are 2 major sections:
#
#     1. Prepare Data
#     2. Item Response Patterns
#
# Code for preparing each data source is included in sub-sections with a line
# that begins with "Source:"
#
#------------------------------------------------------------------------------#

date()
library(stringr)
library(dplyr)
library(readxl)
library(vaItemResponse)
data(who2016_151_instrument)
who2016 <- who2016_151_instrument
data(who2016_151_data)

#------------------------------------------------------------------------------#
# 1. Prepare Data
#------------------------------------------------------------------------------#

#---------------------------------#
# Source: CHAMPS
#---------------------------------#
champs <- read.csv("../data/raw data/COMSA & CHAMPS/CHAMPS_deid_verbal_autopsy_V4.csv", stringsAsFactors = FALSE)
champs_ref_cod <- read.csv("../data/raw data/COMSA & CHAMPS/CHAMPS_deid_decode_results_v4.csv", stringsAsFactors = FALSE)
dim(champs)
names(champs)
table(champs$Id10013)

## lapply(champs, unique)
champs$Id10002[champs$Id10002 == "very"] <- "veryl"
champs$Id10003[champs$Id10003 == "very"] <- "veryl"
champs$Id10004[champs$Id10004 == "DRY SEASON"] <- "dry"
champs$Id10004[champs$Id10004 == "WET SEASON"] <- "wet"


champs_merge <- NULL
for (i in names(who2016_151_data)) {
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(champs)))
    if (length(index) == 0) {
        champs_merge[[i]] <- rep("", nrow(champs))
    } else {
        champs_merge[[i]] <- champs[, index]
        }
}
## add source and reference causes (if applicable)
champs_merge <- as.data.frame(champs_merge)
identical(names(champs_merge), names(who2016_151_data))
champs_merge$source <- "CHAMPS"
table(champs_merge$consented.deceased_CRVS.info_on_deceased.ageInDays == champs$ageInDays)
table(is.na(champs_merge$consented.deceased_CRVS.info_on_deceased.ageInDays), is.na(champs$ageInDays))
champs_merge$meta.instanceID <- champs$champs_deid
dim(champs)
dim(champs_ref_cod)
merge_cod <- champs_ref_cod[, c('champs_deid', 'Underlying_Cause', 'Immediate_COD')]
names(merge_cod) <- c('meta.instanceID', 'ref_cod', 'ref_immediate_cod')
table(champs_merge$meta.instanceID %in% merge_cod$meta.instanceID)
champs_merge <- merge(champs_merge, merge_cod, by = 'meta.instanceID')
## write.csv(champs_merge, "../data/clean data/champs_clean.csv", row.names = FALSE)
## champs_results <- itemMissing(champs_merge, odk_form = who2016)


#---------------------------------#
# Source: Comsa
#---------------------------------#
comsa <- read.csv("../data/raw data/COMSA & CHAMPS/Comsa_WHO_VA_20201114.csv", stringsAsFactors = FALSE)
dim(comsa)
names(comsa)
table(comsa$Id10013)

## deal with problems from unexpected characters
comsa$Id10431 <- iconv(comsa$Id10431, "UTF-8", "ASCII", sub = "")

## lapply(comsa, unique)

## (Id10114) If the baby didn't show any sign of life, was it born dead?
table(comsa$Id10114)
comsa$Id10114[comsa$Id10114 == "dead"] <- "yes"
comsa$Id10114[comsa$Id10114 == "alive"] <- "no"
table(comsa$Id10114)

comsa_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(comsa)))
    if (length(index) == 0) {
        comsa_merge[[i]] <- rep("", nrow(comsa))
    } else {
        comsa_merge[[i]] <- comsa[, index]
        }
}
## add source and reference causes (if applicable)
comsa_merge <- as.data.frame(comsa_merge)
identical(names(comsa_merge), names(who2016_151_data))
comsa_merge$source <- "COMSA"
comsa_merge$ref_cod <- NA
comsa_merge$ref_immediate_cod <- NA
## write.csv(comsa_merge, "../data/clean data/comsa_clean.csv", row.names = FALSE)
## comsa_results <- itemMissing(comsa_merge, odk_form = who2016)


## #---------------------------------#
## # Source: Egypt
## #---------------------------------#
## dir("../data/Egypt")
## egypt <- read.csv("../data/raw data/Egypt/Ramy Ghazy Data egypt.csv", stringsAsFactors = FALSE)
## dim(egypt)
## names(egypt)
## identical(names(egypt), names(who2016_151_data))
## egypt_merge <- egypt
## add source and reference causes (if applicable)
## egypt_merge$source <- "Egypt"
## egypt_merge$ref_cod <- NA
## egypt_merge$ref_immediate_cod <- NA
## egypt_results <- itemMissing(egypt, odk_form = who2016)
## write.csv(egypt_merge, "../data/clean data/egypt_clean.csv", row.names = FALSE)
## egypt_results <- itemMissing(egypt_merge, odk_form = who2016)


#---------------------------------#
# Source: Ivory Coast
#---------------------------------#
ivory_coast <- read.csv("../data/raw data/Cote d'ivoire/2016_WHO_Verbal_Autopsy_CI.csv", stringsAsFactors = FALSE)
dim(ivory_coast)
names(ivory_coast)
table(ivory_coast$respondent_backgr.Id10013)

## lapply(ivory_coast, unique)

ic_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(ivory_coast)))
    if (length(index) == 0) {
        ic_merge[[i]] <- rep("", nrow(ivory_coast))
    } else {
        ic_merge[[i]] <- ivory_coast[, index]
        }
}
## add source and reference causes (if applicable)
ic_merge <- as.data.frame(ic_merge)
identical(names(ic_merge), names(who2016_151_data))
ic_merge$source <- "Ivory Coast"
ic_merge$ref_cod <- NA
ic_merge$ref_immediate_cod <- NA
## write.csv(ic_merge, "../data/clean data/ivory_coast_clean.csv", row.names = FALSE)
## ic_results <- itemMissing(ic_merge, odk_form = who2016)


#---------------------------------#
# Source: Bongo (Ghana)
#---------------------------------#
dir("../data/raw data/Ghana-Bongo")
bongo1 <- read.csv("../data/raw data/Ghana-Bongo/DATA 1.csv", stringsAsFactors = FALSE)
bongo2 <- read.csv("../data/raw data/Ghana-Bongo/DATA 2.csv", stringsAsFactors = FALSE)
bongo3 <- read.csv("../data/raw data/Ghana-Bongo/DATA 3.csv", stringsAsFactors = FALSE)

dim(bongo1)
names(bongo1)
table(bongo1$respondent_backgr.Id10013)

dim(bongo2)
names(bongo2)
table(bongo2$respondent_backgr.Id10013)

dim(bongo3)
names(bongo3)
table(bongo3$respondent_backgr.Id10013)

## lapply(bongo1, unique)
## lapply(bongo2, unique)
## lapply(bongo3, unique)

bongo1_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(bongo1)))
    if (length(index) == 0) {
        bongo1_merge[[i]] <- rep("", nrow(bongo1))
    } else {
        bongo1_merge[[i]] <- bongo1[, index]
        }
}
bongo1_merge <- as.data.frame(bongo1_merge)
identical(names(bongo1_merge), names(who2016_151_data))

bongo2_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(bongo2)))
    if (length(index) == 0) {
        bongo2_merge[[i]] <- rep("", nrow(bongo2))
    } else {
        bongo2_merge[[i]] <- bongo2[, index]
        }
}
bongo2_merge <- as.data.frame(bongo2_merge)
identical(names(bongo2_merge), names(who2016_151_data))

bongo3_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(bongo3)))
    if (length(index) == 0) {
        bongo3_merge[[i]] <- rep("", nrow(bongo3))
    } else {
        bongo3_merge[[i]] <- bongo3[, index]
        }
}
bongo3_merge <- as.data.frame(bongo3_merge)
identical(names(bongo3_merge), names(who2016_151_data))

## add source and reference causes (if applicable)
bongo1_merge$source <- "Bongo (Ghana)"
bongo1_merge$ref_cod <- NA
bongo1_merge$ref_immediate_cod <- NA
## write.csv(bongo1_merge, "../data/clean data/bongo1_clean.csv", row.names = FALSE)
## bongo1_results <- itemMissing(bongo1_merge, odk_form = who2016)

bongo2_merge$source <- "Bongo (Ghana)"
bongo2_merge$ref_cod <- NA
bongo2_merge$ref_immediate_cod <- NA
## write.csv(bongo2_merge, "../data/clean data/bongo2_clean.csv", row.names = FALSE)
## bongo2_results <- itemMissing(bongo2_merge, odk_form = who2016)

bongo3_merge$source <- "Bongo (Ghana)"
bongo3_merge$ref_cod <- NA
bongo3_merge$ref_immediate_cod <- NA
## write.csv(bongo3_merge, "../data/clean data/bongo3_clean.csv", row.names = FALSE)
## bongo3_results <- itemMissing(bongo3_merge, odk_form = who2016)


#---------------------------------#
# Source: Kintampo (Ghana)
#---------------------------------#
dir('../data/raw data/Ghana - Kintampo Health Research Centre')
kintampo <- read.csv('../data/raw data/Ghana - Kintampo Health Research Centre/HDSS_VA_data_Ghana.csv', stringsAsFactor = FALSE)
dim(kintampo)
names(kintampo)

table(kintampo$Id10013)

# recode to WHO values
kintampo_who <- kintampo
## merge choices from Kintampo and WHO insrument for recoding
codebook <- read.csv('../data/raw data/Ghana - Kintampo Health Research Centre/Kintampo-codebook.csv', stringsAsFactor = FALSE)
names(codebook)
codebook <- subset(codebook, list_name != "")
codebook$label <- tolower(codebook$label..English)

choices <- read.csv('../data/raw data/Ghana - Kintampo Health Research Centre/WHOVA2016_v1_5_1_choices.csv', stringsAsFactor = FALSE)
choices <- subset(choices, list.name != "")
choices$label <- tolower(choices$label..English)

names(choices)
names(codebook)
dim(codebook)
merged_choices <- merge(codebook, choices, by.x = c("list_name", "label"),
                        by.y = c("list.name", "label"), all.x = TRUE)
dim(merged_choices)

for (i in 1:ncol(kintampo)) { # i = 1
    
    idx <- which(who2016$name == names(kintampo)[i])
    if (length(idx) == 0) next
    who_type <- unlist(strsplit(who2016$type[idx], " "))
    if (length(who_type) == 1) {
        idx_type <- who_type
    } else {
        idx_type <- who_type[2]
    }
    match_type <- which(tolower(merged_choices$list_name) == tolower(idx_type))
    if (length(match_type) > 0) {
        old_codes <- merged_choices$name.x[match_type]
        new_codes <- merged_choices$name.y[match_type]
        names(new_codes) <- old_codes
        if (!all(is.na(kintampo_who[, i]))) {
            kintampo_who[, i] <- recode(kintampo[,i], !!!new_codes)
        }
    }
}
# There are several items that are all NA, fill these in the empty strings
all_na <- lapply(kintampo_who, function (x) all(is.na(x)))
table(unlist(all_na))
names(all_na)[unlist(all_na)]
kintampo_who[, names(all_na)[unlist(all_na)]] <- ""

## lapply(kintampo_who, unique)

kintampo_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(kintampo)))
    if (length(index) == 0) {
        kintampo_merge[[i]] <- rep("", nrow(kintampo_who))
    } else {
        kintampo_merge[[i]] <- kintampo_who[, index]
        }
}
## add source and reference causes (if applicable)
kintampo_merge <- as.data.frame(kintampo_merge)
identical(names(kintampo_merge), names(who2016_151_data))
kintampo_merge$source <- "KINTAMPO"
kintampo_merge$ref_cod <- NA
kintampo_merge$ref_immediate_cod <- NA
## write.csv(kintampo_merge, "../data/clean data/kintampo_clean.csv", row.names = FALSE)
## kintampo_results <- itemMissing(kintampo_merge, odk_form = who2016)


#---------------------------------#
# Source: Ghana, maternal
#---------------------------------#
dir("../data/raw data/Ghana survey data on maternal deaths/COD - Ghana survey data on maternal deaths")
gha_maternal <- read.csv("../data/raw data/Ghana survey data on maternal deaths/COD - Ghana survey data on maternal deaths/ghva_all_individualcod.csv", stringsAsFactors = FALSE)
dim(gha_maternal)
names(gha_maternal)
table(gha_maternal$Id10013)

## lapply(gha_maternal, unique)
table(gha_maternal$Id10090)
gha_maternal$Id10090[gha_maternal$Id10090 == "yes, abuse"] <- "yes"
gha_maternal$Id10090[gha_maternal$Id10090 == "yes, homicide"] <- "yes"
table(gha_maternal$Id10090)

table(gha_maternal$Id10310)
gha_maternal$Id10310[gha_maternal$Id10310 == "yes, no pregnancy in 12 months before death"] <- "yes"
table(gha_maternal$Id10310)

gha_maternal_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(gha_maternal)))
    if (length(index) == 0) {
        gha_maternal_merge[[i]] <- rep("", nrow(gha_maternal))
    } else {
        gha_maternal_merge[[i]] <- gha_maternal[, index]
        }
}
## add source and reference causes (if applicable)
gha_maternal_merge <- as.data.frame(gha_maternal_merge)
identical(names(gha_maternal_merge), names(who2016_151_data))
gha_maternal_merge$source <- "Ghana (maternal)"
gha_maternal_merge$ref_cod <- NA
gha_maternal_merge$ref_immediate_cod <- NA
## write.csv(gha_maternal_merge, "../data/clean data/ghana_maternal_clean.csv", row.names = FALSE)
## gha_maternal_results <- itemMissing(gha_maternal_merge, odk_form = who2016)


#---------------------------------#
# Source: Nanoro (Burkina Faso)
#---------------------------------#
dir("../data/raw data/Nanoro Burkina Faso")
nanoro <- read.csv("../data/raw data/Nanoro Burkina Faso/nanoroHDSS_VAdata_convertWHO2016_DTH_2014_17_bd1_Dec20.csv", stringsAsFactors = FALSE)
dim(nanoro)
names(nanoro)
table(nanoro$Id10013)

## lapply(nanoro, unique)

nanoro_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(nanoro)))
    if (length(index) == 0) {
        nanoro_merge[[i]] <- rep("", nrow(nanoro))
    } else {
        nanoro_merge[[i]] <- nanoro[, index]
        }
}
## add source and reference causes (if applicable)
nanoro_merge <- as.data.frame(nanoro_merge)
identical(names(nanoro_merge), names(who2016_151_data))
nanoro_merge$source <- "Nanoro (Burkina Faso)"
nanoro_merge$ref_cod <- NA
nanoro_merge$ref_immediate_cod <- NA
## write.csv(nanoro_merge, "../data/clean data/nanoro_clean.csv", row.names = FALSE)
## nanoro_results <- itemMissing(nanoro_merge, odk_form = who2016)


## #---------------------------------#
## # Source: Nouna (Burkina Faso)
## #---------------------------------#
## dir("../data/raw data/Nouna - Burkina Faso")
## nouna <- read_excel("../data/raw data/Nouna - Burkina Faso/Nouna VA_Data Jan 2021.xlsx")
## nouna <- as.data.frame(nouna)
## dim(nouna)
## names(nouna)
## table(nouna$Id10013)

## nouna[1:40,c("DOB", "DOD")]
## nouna$Id10020 <- nouna$DOB
## nouna$Id10022 <- nouna$DOD
## nouna$ageInMonthsByYear <- nouna$ageInMonths
## nouna$Id10310 <- nouna$Id10306
## nouna$Id10414 <- nouna$Id10414__0
## nouna$Id10439_check <- nouna$Id10439_check__1
## nouna$Id10440_check <- nouna$Id10440
## nouna$Id10441_check <- nouna$Id10441

## ## lapply(nouna, unique)

## nouna_merge <- NULL
## for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
##     x <- strsplit(i, "\\.")[[1]]
##     new_lab <- tolower(x[length(x)])
##     index <- grep(paste0(new_lab, "$"), tolower(names(nouna)))
##     if (length(index) == 0) {
##         nouna_merge[[i]] <- rep("", nrow(nouna))
##     } else {
##         nouna_merge[[i]] <- nouna[, index]
##         }
## }
## ## add source and reference causes (if applicable)
## nouna_merge <- as.data.frame(nouna_merge)
## identical(names(nouna_merge), names(who2016_151_data))
## nouna_merge$source <- "Nouna (Burkina Faso)"
## nouna_merge$ref_cod <- NA
## nouna_merge$ref_immediate_cod <- NA
## ## write.csv(nouna_merge, "../data/clean data/nouna_clean.csv", row.names = FALSE)
## ## nouna_results <- itemMissing(nouna_merge, odk_form = who2016)


#---------------------------------#
# Source: Kenya
#---------------------------------#
dir("../data/raw data/Kenya - CRVS- Collins")
kenya_c <- read.csv("../data/raw data/Kenya - CRVS- Collins/Data2share.csv", stringsAsFactors = FALSE)
dim(kenya_c)
names(kenya_c)
table(kenya_c$id10013)

## lapply(kenya_c, unique)

kenya_c_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(kenya_c)))
    if (length(index) == 0) {
        kenya_c_merge[[i]] <- rep("", nrow(kenya_c))
    } else {
        kenya_c_merge[[i]] <- kenya_c[, index]
        }
}
## add source and reference causes (if applicable)
kenya_c_merge <- as.data.frame(kenya_c_merge)
identical(names(kenya_c_merge), names(who2016_151_data))
kenya_c_merge$source <- "Kenya - Collins"
kenya_c_merge$ref_cod <- NA
kenya_c_merge$ref_immediate_cod <- NA
## write.csv(kenya_c_merge, "../data/clean data/kenya_c_clean.csv", row.names = FALSE)
## kenya_c_results <- itemMissing(kenya_c_merge, odk_form = who2016)


#---------------------------------#
# Source: South Africa
# note: openned xlsx file and saved as (UTF-8) .csv file
#---------------------------------#
dir("../data/raw data/MRC data")
## rsa <- read_excel("../data/raw data/MRC data/20210316_samrc_va_with_cod.xlsx",
##                           sheet = "VA data with COD")
## rsa <- as.data.frame(rsa)
rsa_cod <- read_excel("../data/raw data/MRC data/20210316_samrc_va_with_cod.xlsx",
                      sheet = "VA data with COD")
rsa_cod <- as.data.frame(rsa_cod)
rsa <- read.csv("../data/raw data/MRC data/SAMRC_NCODV_VA VERSION 8 SEP 26_ 2019-03-12_anon2.csv", stringsAsFactors = FALSE)
rsa_odk_names <- str_extract(names(rsa), "\\.((Id)|(id))[^\\.]+|^ageIn[:alpha:]+|^is[:alnum:]+|VA.Unique.study.ID")
rsa_odk_names <- gsub("^\\.", "", rsa_odk_names)
names(rsa) <- rsa_odk_names
rsa$meta.instanceID <- rsa$VA.Unique.study.ID
dim(rsa)
names(rsa)
table(rsa$Id10013)

## lapply(rsa, unique)

## standardize responses
table(rsa$Id10310)
rsa$Id10310[rsa$Id10310 == "Yes (SHE WAS NOT PREGNANT; AND SHE DID NOT RECENTLY DELIVER, HAVE ABORTION, OR MISCARRY)"] <- "yes"
rsa$Id10310[rsa$Id10310 == "No (SHE WAS PREGNANT OR SHE RECENTLY DELIVERED, HAD AN ABORTION OR MISCARRIED)"] <- "no"
rsa$Id10310[rsa$Id10310 == "Doesn't know"] <- "dk"
rsa$Id10310[rsa$Id10310 == "Refused to answer"] <- "ref"

rsa[rsa == "Refused to answer"] <- "ref"
rsa[rsa == "Doesn't know"] <- "dk"

rsa_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(rsa)))
    if (length(index) > 1) index <- sort(index)[1]
    if (length(index) == 0) {
        rsa_merge[[i]] <- rep("", nrow(rsa))
    } else {
        rsa_merge[[i]] <- rsa[, index]
        }
}
## add source and reference causes (if applicable)
rsa_merge <- as.data.frame(rsa_merge)
identical(names(rsa_merge), names(who2016_151_data))
rsa_merge$source <- "South Africa"
dim(rsa_merge)
dim(rsa_cod)
table(rsa_merge$meta.instanceID %in% rsa_cod$id)
merge_cod <- rsa_cod[, c('id', 'UCODFinal', 'phys_cod')]
names(merge_cod) <- c('meta.instanceID', 'ref_cod', 'ref_immediate_cod')
table(rsa_merge$meta.instanceID %in% merge_cod$meta.instanceID)
rsa_merge <- merge(rsa_merge, merge_cod, by = 'meta.instanceID',
                   all.x = TRUE, all.y = FALSE)
dim(rsa_merge)
names(rsa_merge)

rsa_merge$meta.instanceID[!(rsa_merge$meta.instanceID %in% rsa_cod$id)]
rsa_cod$id[which(!(rsa_cod$id %in% rsa_merge$meta.instanceID))]
## I think these are the same: 103077 1030771 &
rsa_cod$ref_cod[rsa_cod$id == 1030771]
rsa_merge$ref_cod[rsa_merge$meta.instanceID == 103077]
rsa_merge$ref_immediate_cod[rsa_merge$meta.instanceID == 103077]

rsa_cod$UCODFinal[rsa_cod$id == 1030771]
rsa_cod$phys_cod[rsa_cod$id == 1030771]
rsa_merge$ref_cod[rsa_merge$meta.instanceID == 103077] <- rsa_cod$UCODFinal[rsa_cod$id == 1030771]
rsa_merge$ref_immediate_cod[rsa_merge$meta.instanceID == 103077] <- rsa_cod$phys_cod[rsa_cod$id == 1030771]
rsa_merge$ref_cod[rsa_merge$meta.instanceID == 103077]
rsa_merge$ref_immediate_cod[rsa_merge$meta.instanceID == 103077]
## write.csv(rsa_merge, "../data/clean data/mrc_clean.csv", row.names = FALSE)
## rsa_results <- itemMissing(rsa_merge, odk_form = who2016)


#---------------------------------#
# Source: Morocco
#---------------------------------#
dir("../data/raw data/Morocco")
morocco <- read.csv("../data/raw data/Morocco/New datas VN 2016 WHO Verbal Autopsy Form 1_5_1_1.csv",
                    stringsAsFactors = FALSE)
dim(morocco)
names(morocco)
table(morocco$respondent_backgr.Id10013)

## lapply(morocco, unique)

morocco_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(morocco)))
    if (length(index) == 0) {
        morocco_merge[[i]] <- rep("", nrow(morocco))
    } else {
        morocco_merge[[i]] <- morocco[, index]
        }
}
## add source and reference causes (if applicable)
morocco_merge <- as.data.frame(morocco_merge)
identical(names(morocco_merge), names(who2016_151_data))
morocco_merge$source <- "Morocco"
morocco_merge$ref_cod <- NA
morocco_merge$ref_immediate_cod <- NA
## write.csv(morocco_merge, "../data/clean data/morocco_clean.csv", row.names = FALSE)
## morocco_results <- itemMissing(morocco_merge, odk_form = who2016)


#---------------------------------#
# Source: Thailand
#---------------------------------#
dir("../data/raw data/thailand_data_VA")
thailand <- read.csv("../data/raw data/thailand_data_VA/DATA_VA_TH_2500case_cod.csv", stringsAsFactors = FALSE)
dim(thailand)
names(thailand)
table(thailand$"X.Id10013...Did.the.respondent.give.consent..")

thailand$"meta.instanceID" <- thailand$Qcode
names(thailand)[names(thailand) == "X.What.age.group.corresponds.to.the.deceased.."] <- "age_group"
names(thailand)[names(thailand) == "How.many.days.old.was.the.baby...Enter.neonate.s.age.in.days.."] <- "age_neonate_days"
names(thailand)[names(thailand) == "How.old.was.the.child...Enter.child.s.age.in.."] <- "age_child_unit"
names(thailand)[names(thailand) == "X.Enter.child.s.age.in.days.."] <- "age_child_days"
names(thailand)[names(thailand) == "X.Enter.child.s.age.in.months.."] <- "age_child_months"
names(thailand)[names(thailand) == "X.Enter.child.s.age.in.years.."] <- "age_child_years"
names(thailand)[names(thailand) == "X.Enter.adult.s.age.in.years.."] <- "age_adult"

tmp_new_names <- strsplit(names(thailand), "\\.")
new_names <- lapply(tmp_new_names, function (x) ifelse(length(x)==1, x[1], x[2]))
names(thailand) <- unlist(new_names)

## table(unlist(new_names))[table(unlist(new_names)) > 1]
## grab this first ones
## Id10235 -- 270:276
## Id10260 -- 308:318
## Id10315 -- 381:382
## Id10431 -- 488:500
## Id10433 -- 503:516
## Id10477 -- 557:570
## Id10478 -- 571:583
## Id10479 -- 584:592
new_thai <- thailand[,-c(271:276, 309:318, 382, 489:500, 504:516,
                         558:570, 572:583, 585:592)]
length(names(new_thai))
length(unique(names(new_thai)))

for (i in names(new_thai)) {
    if (is.character(new_thai[,i])) {
        new_thai[, i] <- tolower(new_thai[, i])
    }
    # need to impute id10114
    if (all(is.na(new_thai[,i]))) {
        new_thai[,i] <- ""
    }
}

## lapply(new_thai, unique)
table(new_thai == "doesn't know")
new_thai[new_thai == "doesn't know"] <- "dk"
table(new_thai == "doesn't know")
table(new_thai == "dk")

table(new_thai == "refused to answer")
new_thai[new_thai == "refused to answer"] <- "ref"
table(new_thai == "refused to answer")
table(new_thai == "ref")

table(new_thai$Id10310)
new_thai$Id10310[new_thai$Id10310 == "yes (she was not pregnant; and she did not recently deliver, have abortion, or miscarry)"] <- "yes"
new_thai$Id10310[new_thai$Id10310 == "no (she was pregnant or she recently delivered, had an abortion or miscarried)"] <- "no"
table(new_thai$Id10310)

thailand_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(new_thai)))
    if (length(index) == 0) {
        thailand_merge[[i]] <- rep("", nrow(new_thai))
    } else {
        thailand_merge[[i]] <- new_thai[, index]
        }
}
## add source and reference causes (if applicable)
thailand_merge <- as.data.frame(thailand_merge)
identical(names(thailand_merge), names(who2016_151_data))
thailand_merge$source <- "Thailand"
thailand_merge$ref_immediate_cod <- NA
thailand_merge$ref_cod <- thailand$COD_ICD

## write.csv(thailand_merge, "../data/clean data/thailand_clean.csv", row.names = FALSE)
## thailand_results <- itemMissing(thailand_merge, odk_form = who2016)


#---------------------------------#
# Source: Zambia
#---------------------------------#
dir("../data/raw data/Zambia")
first_ws <- read.csv("../data/raw data/who_merged_062920.csv",
                     stringsAsFactors = FALSE)
table(first_ws$X)
names(first_ws)
zambia <- subset(first_ws, X == "ZAMBIA")
dim(zambia)
table(zambia$id10013)

## lapply(zambia, unique)

identical(names(zambia), names(who2016_151_data))
zambia_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(zambia)))
    if (length(index) == 0) {
        zambia_merge[[i]] <- rep("", nrow(zambia))
    } else {
        zambia_merge[[i]] <- zambia[, index]
        }
}
## add source and reference causes (if applicable)
zambia_merge <- as.data.frame(zambia_merge)
identical(names(zambia_merge), names(who2016_151_data))
zambia_merge$source <- "Zambia"
zambia_merge$ref_cod <- NA
zambia_merge$ref_immediate_cod <- NA
## write.csv(zambia_merge, "../data/clean data/zambia_clean.csv", row.names = FALSE)
## zambia_results <- itemMissing(zambia_merge, odk_form = who2016)


#---------------------------------#
# Source: Kenya
#---------------------------------#
kenya <- subset(first_ws, X == "KENYA")
dim(kenya)
table(kenya$id10013)

## lapply(kenya, unique)

kenya_merge <- NULL
for (i in names(who2016_151_data)) { # i = names(who2016_151_data)[1]
    x <- strsplit(i, "\\.")[[1]]
    new_lab <- tolower(x[length(x)])
    index <- grep(paste0(new_lab, "$"), tolower(names(kenya)))
    if (length(index) == 0) {
        kenya_merge[[i]] <- rep("", nrow(kenya))
    } else {
        kenya_merge[[i]] <- kenya[, index]
        }
}
## add source and reference causes (if applicable)
kenya_merge <- as.data.frame(kenya_merge)
identical(names(kenya_merge), names(who2016_151_data))
kenya_merge$source <- "Kenya"
kenya_merge$ref_cod <- NA
kenya_merge$ref_immediate_cod <- NA
## write.csv(kenya_merge, "../data/clean data/kenya_clean.csv", row.names = FALSE)
## kenya_results <- itemMissing(kenya_merge, odk_form = who2016)


#---------------------------------#
# Combined results
#---------------------------------#
combined <- Reduce(function(...) merge(..., all = TRUE),
                   list(champs_merge, comsa_merge, ## egypt_merge,
                        ic_merge,
                        bongo1_merge, bongo2_merge, bongo3_merge,
                        kintampo_merge, gha_maternal_merge,
                        nanoro_merge, ## nouna_merge
                        kenya_c_merge, morocco_merge, rsa_merge, thailand_merge,
                        zambia_merge, kenya_merge))
dim(combined)
nrow(champs_merge) + nrow(comsa_merge) + #nrow(egypt_merge) +
    nrow(ic_merge) +
    nrow(bongo1_merge) + nrow(bongo2_merge) + nrow(bongo3_merge) +
    nrow(kintampo_merge) + 
    nrow(gha_maternal_merge) + 
    nrow(nanoro_merge) + #nrow(nouna_merge)
    nrow(kenya_c_merge) + nrow(morocco_merge) + nrow(rsa_merge) + nrow(thailand_merge) +
    nrow(zambia_merge) + nrow(kenya_merge)

combined <- combined %>% arrange(source)
combined$item.response.ID <- 1:nrow(combined)

# Write combined data to CSV file
write.csv(combined, "../data/clean data/combined-data.csv", row.names = FALSE)


#------------------------------------------------------------------------------#
# 2. Item Response Patterns
#------------------------------------------------------------------------------#

# Read in indicators of symptoms.
symptoms <- read_excel("../data/raw data/symptom-flag.xlsx")

## results with parent_check = TRUE
combined_results <- itemMissing(combined, odk_form = who2016,
                                id_col = "item.response.ID",
                                check_parents = TRUE)
head(combined_results$Items)

IR <- subset(combined_results$Items, !(type %in% c("begin group", "calculate", "end", "note", "start", "today")))
IR <- IR[IR$name %in% symptoms$Name[symptoms$symptom == 1],]

IR <- IR %>%
    mutate(rank_var = percent_rank(var),
           #rank_entropy = percent_rank(entropy),
           rank_n_asked = percent_rank(n_asked),
           pct_valid = 100*(1 - (n_ref + n_dk + n_miss)/n_asked),
           rank = percent_rank(pct_valid),
           rank_ref = percent_rank(pct_ref),
           rank_dk = percent_rank(pct_dk),
           rank_miss = percent_rank(pct_miss))

IR$rank_entropy <- 1
IR$rank_entropy[IR$type != "integer"] = dplyr::percent_rank(IR$entropy[IR$type != "integer"])

write.csv(IR, "../results/item response/item_response_results.csv", row.names = FALSE)

names(combined_results$DummyDF)
table(combined_results$DummyDF$item_response_ID == combined$item.response.ID)
combined_results$DummyDF$source <- combined$source
DummyDF <- combined_results$DummyDF[, !(names(combined_results$DummyDF) %in% c("instanceid", "id"))]

write.csv(DummyDF,
          "../data/clean data/item-indicator-combined-data.csv", row.names = FALSE)

date()
