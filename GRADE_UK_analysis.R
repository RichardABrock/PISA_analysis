# BIFIE model
# load packs
library(BIFIEsurvey)
library(arrow)
library(tidyverse)

# load data
PISA_2025 <- read_parquet("/Users/k1765032/Library/CloudStorage/OneDrive-King\'sCollegeLondon/QERKCL_PISA/data/pisa/2025/PISA_student_2025.parquet")

# Convert to data frame
PISA_df <- as.data.frame(PISA_2025)

# region look up - BIFIE only likes numeric so have to add back in
region_lookup <- PISA_df %>% 
  filter(CNT == "United Kingdom") %>% 
  distinct(REGION) %>% 
  mutate(REGION_id = as.numeric(as.factor(REGION))) %>%
  rename(REGION_name = REGION)

# sort by school ID
PISA_filtered <- PISA_df %>%
  filter(CNT == "United Kingdom") %>%
  mutate(REGION = as.numeric(as.factor(REGION)),
         CNTSCHID = as.numeric(as.factor(CNTSCHID))) %>%
  arrange(CNTSCHID)

# Create 10 datasets, one for each plausible value
datalist <- list()

PISA_filtered %>%
  select(
    W_FSTUWT,
    starts_with("W_FSTURWT"),
    ESCS, HOMEPOS, REGION, GRADE, CNTSCHID,
    matches("^PV[0-9]+MATH"),
    matches("^PV[0-9]+READ"),
    matches("^PV[0-9]+SCIE")
  ) %>%
  mutate(
    W_FSCHWT = 1,
    across(everything(), ~as.numeric(as.character(.x)))
  ) %>%
  as.data.frame()

# Create 10 datasets, one for each plausible value
datalist <- map(1:10, \(i) {
  message(i)
  PISA_filtered %>%
    select(
      W_FSTUWT,
      starts_with("W_FSTURWT"),
      ESCS, HOMEPOS, REGION, GRADE, CNTSCHID,
      all_of(paste0("PV", i, "MATH")),
      all_of(paste0("PV", i, "READ")),
      all_of(paste0("PV", i, "SCIE"))
    ) %>%
    rename(
      PVMATH = all_of(paste0("PV", i, "MATH")),
      PVREAD = all_of(paste0("PV", i, "READ")),
      PVSCIE = all_of(paste0("PV", i, "SCIE"))
    )
})


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
  group = c("GRADE","REGION")
  )

summary(res_grade_means_math)
# for science
res_grade_means_scie <- BIFIE.univar(
  BIFIEobj = bifieobj,
  vars = "PVSCIE",
  group = c("GRADE","REGION")
)

summary(res_grade_means_scie)

# for read
res_grade_means_read <- BIFIE.univar(
  BIFIEobj = bifieobj,
  vars = "PVREAD",
  group = c("GRADE", "REGION")
)

summary(res_grade_means_read)

# for the UK

combined <- res_grade_means_read[["stat"]] %>%
  select(GRADE = groupval1,
    REGION = groupval2,
    n_cases = Ncases,
    read = M) %>%
  left_join(res_grade_means_math[["stat"]] %>% select(groupval1, groupval2, maths = M),
    by = c("GRADE" = "groupval1", "REGION" = "groupval2")) %>%
  left_join( res_grade_means_scie[["stat"]] %>% select(groupval1, groupval2, science = M),
    by = c("GRADE" = "groupval1", "REGION" = "groupval2") ) %>%
  mutate(REGION = as.numeric(REGION)) %>% 
  left_join(region_lookup, by = c("REGION" = "REGION_id")) %>%
  select(GRADE, REGION_name, n_cases, read, maths, science)
