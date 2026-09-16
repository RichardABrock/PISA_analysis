# BIFIE model
# load packs
library(BIFIEsurvey)
library(arrow)
library(tidyverse)

# load data
PISA_2025 <- read_parquet("/Users/k1765032/Library/CloudStorage/OneDrive-King\'sCollegeLondon/QERKCL_PISA/data/pisa/2025/PISA_student_2025.parquet")

# Convert to data frame
PISA_df <- as.data.frame(PISA_2025)

# sort by school ID
PISA_filtered <- PISA_df %>%
  filter(CNT == "United Kingdom") %>%
  mutate(REGION = as.numeric(as.factor(REGION)),
         CNTSCHID = as.numeric(as.factor(CNTSCHID))) %>%
  arrange(CNTSCHID)

# Create 10 datasets, one for each plausible value
datalist <- list()

for (i in 1:10) {
  pv_math_col <- paste0("PV", i, "MATH")
  pv_read_col <- paste0("PV", i, "READ")
  pv_scie_col <- paste0("PV", i, "SCIE")
  
  dat_temp <- PISA_filtered %>%
    select(
      W_FSTUWT,
      starts_with("W_FSTURWT"),
      ESCS, HOMEPOS, REGION, GRADE, CNTSCHID,
      all_of(pv_math_col),
      all_of(pv_read_col),
      all_of(pv_scie_col)
    ) %>%
    rename(
      PVMATH = all_of(pv_math_col),
      PVREAD = all_of(pv_read_col),
      PVSCIE = all_of(pv_scie_col)
    )
  
  dat_temp$W_FSCHWT <- 1
  
  # Convert all columns to numeric safely
  dat_temp <- as.data.frame(lapply(dat_temp, function(x) as.numeric(as.character(x))))
  
  datalist[[i]] <- dat_temp
}

# Extract replicate weights
datarep <- PISA_filtered[, grep("W_FSTURWT", colnames(PISA_filtered))]

# Create BIFIE object with multiple imputed datasets
bifieobj <- BIFIE.data(
  datalist, 
  wgt = PISA_filtered[, "W_FSTUWT"],
  wgtrep = datarep,
  fayfac = 0.05  # For PISA: 1/20 = 0.05
)

# For maths
# It seems just running the univariate model is best here
res_grade_means_math <- BIFIE.univar(
  BIFIEobj = bifieobj,
  vars = "PVMATH",
  group = "GRADE"
)

summary(res_grade_means_math)
# for science
res_grade_means_scie <- BIFIE.univar(
  BIFIEobj = bifieobj,
  vars = "PVSCIE",
  group = "GRADE"
)

summary(res_grade_means_scie)

# for read
res_grade_means_read <- BIFIE.univar(
  BIFIEobj = bifieobj,
  vars = "PVREAD",
  group = "GRADE"
)

summary(res_grade_means_read)

combined <- data.frame(
  GRADE = c("-2","-1","0","1"),
  maths = res_grade_means_math[["stat"]][["M"]],
  science = res_grade_means_scie[["stat"]][["M"]],
  read = res_grade_means_read[["stat"]][["M"]])