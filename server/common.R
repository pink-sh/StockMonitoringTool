getDataConsiderationTextForCmsy <- function() {
    text <- "<strong>Use the sample dataset as a template to prepare your data.</strong><br/>"
    text <- paste0(text, "Mandatory fields to run CMSY are")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>Stock (fish stock name)</li>")
    text <- paste0(text, "<li>yr (year of the catch)</li>")
    text <- paste0(text, "<li>ct (catch)</li>")
    text <- paste0(text, "<li>bt (biomass estimates, if available; otherwise input “NA”)</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<p>Other columns are identifiers that you may choose to include, but they are not necessary to run the model.</p>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<p>Ensure your data are in .csv format and use a “.” to separate decimals in the data.</p>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<h5><p><b>Please ensure your time-series at least 15 years in length from starting year to ending year.<br> (Note that years with missing data should be filled with an 'NA' value.</b></p></h5>")
    text <- paste0(text, "<i>", "**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of natural mortality (M) for the Optional Parameters section.", "</i>")
    text
}

getDataConsiderationTextForElefan <- function() {
    text <- "<b>Required input data must include:</b>"
    text <- paste0(text, "<ul>")
    text <- paste0(text,
                   "<li>A column indicating the length classes of measured individuals (first column of dataset).</li>")
    text <- paste0(text, "<li>A row indicating the dates when idividuals were measured (first row of dataset).</li>")
    text <- paste0(text,
                   "<li>The number of caught individuals per length class (rows) and per sampling date (columns).</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, '<p> An example dataset in this format can be downloaded from <a href="https://goo.gl/tsqt64"> here </a>. </p>')
    text <- paste0(text, "<br>")
    text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>", "Save your dataset in 'csv' format.", "</li>")
    text <- paste0(text, "<li>Select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format to select 'Day Month Year'.</li>")
    text <- paste0(text, "<li>Ensure that your dates are given in chronological order.</li>")
    text <- paste0(text, "<li>", "Ensure that the 'midLength' column name is identical to the sample dataset.", "</li>")
    text <- paste0(text, "<li>", "Use a '.' to separate decimals in the data.", "</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, '<p> Further information about differnet formats of length-frequency datasets ',
                   'and how to convert one format to the other are described ',
                   '<a href="https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html"> here </a>. </p>')
    return (text)
}


getMethodConsiderationTextForElefan <- function() {
    text <- "<b>Steps of length-based stock assessment routine:</b>"
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<ul>")
    text <- paste0(text,
                   "<li><b>Estimating growth parameters</b> of the von Bertlanffy growth (VBG; Bertlanffy 1989) function using <b>ELEFAN</b> (Pauy 1980).</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Estimating natural mortality</b> rate using an <b>empirical formula</b>, such as the one suggested by Pauly (1980) or Then et al. (2015).</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Estimating total mortality</b> rate using the length-converted <b>catch curve</b> (Pauly 1980).</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Estimating stock status</b> in terms of F/Fmax, F/F0.1 and SPR by the <b>yield per recruit model (YPR)</b>.</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text,
                   "<p>These methods make specific assumptions about the data, the stock, or the fisheries targeting the stock. It is important to be aware of these assumptions and, thus, the limitations of the results.</p>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<b>Assumptions of TropFishR:</b>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Representative length measurements:</b> ",
                   "Dataset is representative of the length distribution in the catch. ",
                   "This means that either the length of all individuals in the catch are measured or ",
                   "an equal proportion of individuals of different length was measured (i.e. ",
                   "proportion of measured larger individuals in not larger than proportion of measured smaller ",
                   "individuals).",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Gear/fleet selectivity:</b> ",
                   "The analysis assumes sigmoidal selectivity as it is commonly assumed for trawl-like ",
                   "fishing gear. While the selectivity of gillnets and hook-based methods might correspond ",
                   "to a bell-shaped curve, the combination of various mesh and hook sizes might lead to ",
                   "an overall 'trawl-like' fleet selectivity.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Equilibrium conditions:</b> ",
                   "The analysis assumes constant recruitment, fishing and natural mortality as well as somatic growth and maturation over time, i.e. within the year and over all years covered by the dataset.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Density dependence:</b> ",
                   "The analysis assumes density independent maturity and somatic growth. As the refernece points ",
                   "are based on the 'per recruit' model, no assumptions are made about the stock recruitment ",
                   "relationship.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Correlation of life-history parameters:</b> ",
                   "The use of empirical formulae for the estimation of natural mortality assumes that the ",
                   "growth parameters (",withMathJax("\\(L_\\infty\\)")," and ",withMathJax("\\(K\\)"),") are ",
                   "reliable predictors of the natural mortality.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Length-independent natural mortality:</b> ",
                   "The current implementation of SMT assumes that the natural mortality is equal for all length ",
                   "classes.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Somatic growth following VBG:</b> ",
                   "The estimatoin of growth parameters with ELEFAN assumes logistic growth in length of individuals ",
                   "following the von Bertlanffy growth (VBG) function. This is an often made assumption for the ",
                   "growth of fish, but might not reflect well the growth of species with an exoskeleton, ",
                   "such as crustaceans, nor the growth of early life stages of fish.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Closed population:</b> ",
                   "The catch curve and yield per recruit model assume that the stock (population) under study ",
                   "is closed, meaning that there is no immigration or emmigration taking place. Immi- and emi-",
                   "gration can both bias estimated mortality rates and stock status. Furthermore, fish migrations ",
                   "often vary for various life stages and might thus affect the representativeness of the length ",
                   "measurements if the population is not closed.",
                   "</li>")
    text <- paste0(text, "</ul>")
    return (text)
}

getErrorMessage <- function(forWhat) {
    if(forWhat=="CMSY"){
        return (paste0("Ops! Unfortunately the ",forWhat, " method experienced a problem with the server.<br/>Don't give up and try again in a few minutes or refresh your Stock Monitoring Tool instance.<hr/> <b>%s</b>"))
    }else{
        return (paste0("Ops! unfortunately something went wrong running the ",forWhat," method<br/>Don't give up and try again in a few minutes.<hr/> <b>%s</b>"))
    }
}

fishMethodsDataConsiderationText <- function() {
    text <- "<div>"
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>Mandatory fields to run YPR/SBPR are:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>age (the age of the fish)</li>")
    text <- paste0(text, "<li>ssbwgt (the spawning stock weights for each age)</li>")
    text <- paste0(text, "<li>partial (the recruitment at each age that is used to determine how much fishing mortality (F) each age group receives)</li>")
    text <- paste0(text, "<li>pmat (the proportion of mature fish at each age (used only for SBPR)</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "If you are creating your own dataset:<br/>")
    text <- paste0(text, "Ensure that the column names are identical to the sample dataset. Ensure your data are in .csv format. Use a “.” to separate decimals in the data.")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "<strong>Ensure that spawning stock weight-at-age data is representative of the full population, i.e., are all age groups sampled?</strong>")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "<i>**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M in the Optional Parameters section.</i>")
    text <- paste0(text, "</p>")

    text <- paste0(text, "</div>")

    return (text)
}
