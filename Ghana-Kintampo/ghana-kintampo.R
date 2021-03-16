library(vaItemResponse)
library(recode)
data(who2016_151_instrument)
who2016 <- who2016_151_instrument
data(who2016_151_data)

# setwd('~/Dropbox/item-reduction-workshop-2021/Workshop #2/data/raw data/Ghana - Kintampo Health Research Centre')
kintampo <- read.csv('HDSS_VA_data_Ghana.csv', stringsAsFactor = FALSE)
dim(kintampo)
## check_cols <- grep('^id|^is|^age', tolower(names(kintampo)))
## apply(kintampo[,check_cols], 2, table)

# recode to WHO values
kintampo_who <- kintampo
## merge choices from Kintampo and WHO insrument for recoding
codebook <- read.csv('Kintampo-codebook.csv', stringsAsFactor = FALSE)
names(codebook)
codebook <- subset(codebook, list_name != "")
codebook$label <- tolower(codebook$label..English)

choices <- read.csv('WHOVA2016_v1_5_1_choices.csv', stringsAsFactor = FALSE)
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
# apply(kintampo_who[,check_cols], 2, table)
# There are several items that are all NA, fill these in the empty strings
all_na <- lapply(kintampo_who, function (x) all(is.na(x)))
table(unlist(all_na))
names(all_na)[unlist(all_na)]
kintampo_who[, names(all_na)[unlist(all_na)]] <- ""

kintampo_results <- itemMissing(kintampo_who, odk_form = who2016)
kintampo_results$Items[,c("name", "n_asked", "pct_total", "pct_ref", "pct_dk", "pct_miss")] # label

# Data set for merging
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
kintampo_merge <- as.data.frame(kintampo_merge)
identical(names(kintampo_merge), names(who2016_151_data))
kintampo_merge$source <- "KINTAMPO"
