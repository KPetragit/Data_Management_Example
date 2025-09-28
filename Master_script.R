
library(rmarkdown)

files <- c(
  "1_Data_Import.Rmd",
  "2_Merge_kobo_assests.Rmd",
  "3_Repeat_groups_HH_flat.Rmd",
  "4_Split_to_5_datasets.Rmd",
  "5_Rotation_groups.Rmd",
  "6_Mobile member.Rmd",
  "7_Final_6_dataset.Rmd",
  "8_Cleaning.Rmd"
)

for (f in files) {
  render(f)
}

source("9_Data_preparation_final_ZAM.R")
render("10_FDS_ZAM_SDG_Indicators.Rmd")
render("11_FDS_ZAM_RBM_Indicators.Rmd")
source("12_WI_ZAM.R")