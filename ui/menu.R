menuHome <- menuItem("Home", tabName="homeTab", selected = TRUE)
menuCmsy <- menuItem("CMSY",
         menuSubItem("Introduction", tabName = "cmsyIntro"),
         menuSubItem("CMSY Method", tabName = "cmsyWidget"),
         menuSubItem("CMSY Sample Dataset", tabName = "cmsySampleDataset"), id="cmsy-main"
)
menuElefan <- menuItem("Elefan by TropFishR",
         menuSubItem("Introduction", tabName = "ElefanIntro"),
         menuSubItem("Elefan GA", tabName = "ElefanGaWidget"),
         # menuSubItem("Elefan SA", tabName = "ElefanSaWidget"),
         # menuSubItem("Elefan", tabName = "ElefanWidget"),
         menuSubItem("Elefan Sample Dataset", tabName = "ElefanSampleDataset")
)
menuFishMethods <- menuItem("Fish Methods",
         menuSubItem("Introduction", tabName = "FishMethodsIntro"),
         menuSubItem("SBPR", tabName = "SBPRWidget"),
         menuSubItem("YPR", tabName = "YPRWidget"),
         menuSubItem("Fishmethods Sample Dataset", tabName = "FishMethodsSampleDataset")
)
menuSupportingTools <- menuItem("Supporting Tools",
         menuSubItem("Schaefer logistic growth", tabName = "BasicSchaefer"),
         menuSubItem("Von Bertalanffy growth function", tabName = "BasicVonBertalannfy"),
         menuSubItem("Seasonal Von Bertalanffy", tabName = "SeasonalVonBertalannfy"),
         menuSubItem("Natural Mortality Estimators", tabName = "NaturalMortality")
)