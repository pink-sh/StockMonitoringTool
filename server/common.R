getDataConsiderationTextForCmsy <- function() {
  text <- "<strong>Dataset must include:</strong><br/>"
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><strong>Stock:</strong> a unique fish stock name or identifier (e.g. “cod-2532”), repeated for each year.</li>")
  text <- paste0(text, "<li><strong>yr:</strong> the reporting year of the catch (e.g. 2004). One row for each year. Years have to be consecutive from the first to the last year without any missing years.</li>")
  text <- paste0(text, "<li><strong>ct:</strong> catch value, in tonnes (e.g. 12345). One row for each year. Gaps with no entries are not accepted and must be filled by interpolating missing or incorrect values, e.g., do not accept zero as entry if data are missing, instead use mean of adjacent values to replace zero or fill any gaps.</li>")
  text <- paste0(text, "<li><strong>bt:</strong> the value of the biomass (in tonnes, e.g. 34567), or the value of the CPUE or stock size index (e.g. 0.123), or NA if there is no information. Gaps filled with NA are acceptable for bt, i.e., abundance data can be fewer than catch data.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "<p>Other columns are identifiers that you may choose to include, but they are not necessary to run the model.</p>")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "<p>Use the ",
                 '<a href="https://data.d4science.net/qhX2"> sample dataset </a>',
                 " as a template to prepare your data.</p>")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "<strong>Specific considerations regarding your own dataset:</strong><br/>")
  text <- paste0(text, "<li>Save your dataset in 'csv' format.</li>")
  text <- paste0(text, "<li>The separator for the .csv file should be a comma ‘,’. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).</li>")
  text <- paste0(text, "<li>Note that years with missing data should be filled with an 'NA' value.</li>")
  text <- paste0(text, "<li>Note that the column names of your dataset should exactly match those of the sample dataset.</li>")
  text <- paste0(text, "<li><strong>Please ensure your time-series at least 15 years in length from starting year to ending year.</strong></li>")
  
}

getWorkflowConsiderationTextForCMSY <- function() {
  text <- "<h4> To run the CMSY method in the Stock Monitoring Tool :</h4>"
  text <- paste0(text, "<ol>")
  text <- paste0(text, "<li>Upload a csv file data set of catch time series for one or multiple stocks (see Data Considerations or the <a href='https://data.d4science.net/qhX2'>CMSY Sample dataset</a>).</li>")
  text <- paste0(text, "<ol type='a'>")
  text <- paste0(text, "<li> Select the stock upon which to perform the analysis </li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "<li> Adjust the Assessment Settings")
  text <- paste0(text, "<ol type='a'>")
  text <- paste0(text, "<li> Data Selection - select the years of data to include in the analysis,and over which to calculate the catchability. </li>")
  text <- paste0(text, "<li> Assessment settings - set the search space of the CMSY algorithm to estimate probalble r-K pairs (set resilience and depletion). </li>")
  text <- paste0(text, "<li> Optional information - if you have reference point values from previous assessments, you can choose to enter and compare them to the results of the present analysis.</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "<li> Check and Run the Assessment:")
  text <- paste0(text, "<ol type='a'>")
  text <- paste0(text, "<li> Run a check on the parameterisations prior to running the full assessment (Run Check button)</li>")
  text <- paste0(text, "<li> Run the assessment (Run Assessment button). This may take some time. For example, the sample dataset takes about 1 min.</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "<li> Download the report as a pdf (Download Report button)</li>")
  text <- paste0(text, "<li> The Reset button removes the uploaded data and resets the settings to default</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Data, Methods, and Results Considerations tabs. Note that error messages may display in the center of the page and in place of any figures where an error has occurred.")
  text <- paste0(text, "</p>")    
  return (text)
}

getMethodConsiderationTextForCmsy <- function() {
  text <- "<b> </b>"
  # text <- paste0(text, "<br>")
  # text <- paste0(text, "<ol>")
  text <- paste0(text, "The Schaefer production model parameters are r and k. Different combinations of these parameters will produce different time series of 
                 biomass. CMSY is a Monte Carlo method where the Schaefer model is run many times to calculate annual biomasses for r-k pairs randomly drawn from prior distributions.
                 The model determines which r-k pairs are valid: e.g., those pairs that result in a biomass time series that do not <br><br>
                <ol>(1) result in a stock collapse, or </ol>
                <ol>(2) allow the stock to exceed carrying capacity.</ol> <br>
                 Those r-k pairs that result in a final relative biomass estimate between the values specified in the inputs (the final depletion range) are accepted 
                 and used to calculate MSY (rk/4) and biomass over time. The geometric means of the resulting density distributions of r, k and MSY are taken as the most 
                 probable values. CMSY estimates fisheries reference points (MSY, F_msy, B_msy), relative stock size (B/B_msy), and fishing mortality or exploitation (F/F_msy). 
                 These are estimated from catch data, a prior value for the resilience, productivity, or intrinsic growth rate (r), and broad
                 priors ranges for the ratio of biomass to unfished biomass (B/k) at the beginning and the end of the time series and an intermediate year.")
  # text <- paste0(text, "</ol>")
  text <- paste0(text, "<br><br>")
  text <- paste0(text,
                 "<p>The CMSY method for data-limited stock assessment is described in ",
                 '<a href="https://www.researchgate.net/publication/309283306_Estimating_fisheries_reference_points_from_catch_and_resilience"> Froese et al. (2017) Estimating fisheries reference points from catch and resilience. Fish and Fisheries. 18. 506-526. 10.1111/faf.12190.</a>',
                 ", <br>and compiled into an R algorithm available on Github : ",
                 '<a href="https://github.com/SISTA16/cmsy"> CMSY 2019 v9f </a>',
                 ". <br>A complete user guide with best practice advice by the authors is available : ", '<a href="https://github.com/SISTA16/cmsy/blob/master/CMSY_2019_9f_UserGuide.pdf"> CMSY_2019_9f_UserGuide</a>',
                 ". <br><br>The CMSY+ version available in the Stock Monitoring Tool ( CMSY_2019_9f .R) is a further development of the one used in Froese et al.
(2017). The main differences are faster execution because of parallel processing and more emphasis on the graphical presentation of the outputs, e.g. by the addition 
of a Kobe stock status plot and several analytical plots. Also, estimation of default B/k priors has been improved and some labels in the input files have changed, 
as indicated below. A major improvement for CMSY+ is the introduction of multivariate normal priors for r and k in log space, replacing the previous uniform prior distributions. 
This have allowed also for a simplified determination of the ‘best’ r-k pair in CMSY+, and faster run times. <br><br>

<strong>The CMSY+ version hosted on the Stock Monitoring Tool does not currently allow for abundance data to be included in the analyses, thus the Bayesian state-space implementation of 
the Schaefer surplus production model (BSM) is not activated.</strong> </p>")
  return (text)
}




getResultConsiderationTextForCmsy <- function() {
  text <- "<b>CMSY method outputs </b><br>When running CMSY+ it will first do a Monte-Carlo analysis of catch and priors for r and B/k. If CMSY+ does not find any viable points, 
 review all your priors to verify that these are indeed plausible. Increase the final prior biomass range if it is very narrow (e.g. change 0.01-0.1 to 0.01 – 0.3)."
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "
<li> <b>Catch time series : Figure 1</b> 
<br>Directly after the successful upload of your data set,  shows the catch time series for the selected stock (x-axis = year, y-axis = catch in tonnes). 
                 The time series should be at least 15 years long. The longer the time series the more confidence in the results. Use the catch time series to 
<br> identify the range of years over which to perform the cmsy analysis. The start year should correspond to the first year when data are considered reliable.
<br> determine over what range to calculate the catchability (q), ideally corresponding to at least 5 recent years where catches are were stable or had similar trends, and 
<br> identify if there is a  year with particularly high or low biomass in the time series, e.g. exploitation changed from light to full, or where an extraordinarily large year class entered the fishery. This can be 
                 used as an intermediate year to inform the intermediate depletion range prior.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "After successfully running the cmsy method (click 'Run Method'), two figures with sub-figures summarise the results of the CMSY method. ")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li> <b>CMSY : Figure 2 </b> 
                 <br>The upper left panel shows catches relative to the estimate of MSY, with indication of 95% confidence limits in grey. 
                 <br>The upper right panel shows the development of relative total biomass (B/Bmsy), with the grey area indicating uncertainty. 
                 <br>The lower left graph shows relative exploitation (F/Fmsy), with Fmsy corrected for reduced recruitment below 0.5 Bmsy. 
                 <br>The lower-right panel shows the trajectory of relative stock size (B/Bmsy) over relative exploitation (F/Fmsy).",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>CMSY : Figure 3 </b> 
                 <br>Panel A shows in black the time series of catches and in blue the three-years moving average with indication of highest and lowest catch, as used in the estimation of prior biomass by the default rules. 
                 <br>Panel B shows the explored r-k log space and in dark grey the r-k pairs which were found by the CMSY model to be compatible with the catches and the prior information. 
                 <br>Panel C shows the most probable r-k pair and its approximate 95% confidence limits in blue. 
                 <br>Panel D shows in blue the biomass trajectory estimated by CMSY. Dotted lines indicate the 2.5th and 97.5th percentiles. Vertical blue lines indicate the prior biomass ranges. 
                 <br>Panel E shows in blue the harvest rate from CMSY. 
                 <br>Panel F shows the Schaefer equilibrium curve of catch/MSY relative to B/k, here indented at B/k < 0.25 to account for reduced recruitment at low stock sizes. The blue dots are scaled by CMSY estimates.",
                 "</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  return (text)
}







getDataConsiderationTextForElefan <- function() {
    text <- "<b>Dataset must include:</b>"
    text <- paste0(text, "<ul>")
    text <- paste0(text,
                   "<li>A column indicating the length classes of measured individuals (first column of dataset).</li>")
    text <- paste0(text, "<li>A row indicating the dates when individuals were measured (first row of dataset excl. first column).</li>")
    text <- paste0(text,
                   "<li>The number of individuals caught per length class (rows) and per sampling date (columns).</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, '<p> An example dataset in this format can be downloaded <a href="https://goo.gl/tsqt64"> here </a>. </p>')
    text <- paste0(text, "<br>")
    text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>", "Save your dataset in 'csv' format.", "</li>")
    text <- paste0(text, "<li>", "The separator for the .csv file should be a comma ‘,’, semicolon ';', or tab '\t'. Default might differ dependent on the language settings of your spreadsheet manipulation programm (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "Use a '.' to separate decimals in the data. Default might differ dependent on the language settings of your spreadsheet manipulation programm (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "The first column of the data set should include the mid lengths of the length classes.", "</li>")
    text <- paste0(text, "<li>Select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format to select 'Day Month Year'.</li>")
    text <- paste0(text, "<li>The date must include a specification of the day. If the data is aggregated or no information about the day is available, you could consider to set the day to the midpoint of the month, e.g. 15/01/2021.</li>")
    text <- paste0(text, "<li>At least, your data set should be representative of a whole year. This is in particular important if seasonally varying growth is estimated.</li>")


    ## text <- paste0(text, "<li>Ensure that your dates are given in chronological order.</li>")
    ## text <- paste0(text, "<li>", "Ensure that the 'midLength' column name is identical to the sample dataset.", "</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, '<p> Further information about differnet formats of length-frequency datasets ',
                   'and how to convert one format to the other are described ',
                   '<a href="https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html"> here </a>. </p>')
    return (text)
}

getWorkflowConsiderationTextForElefan <- function() {
    text <- "<h4> To run this length-based workflow in the Stock Monitoring Tool :</h4>"
    text <- paste0(text, "<ol>")
    text <- paste0(text, "<li> Upload a size frequency data set (see Data Considerations or the <a href='https://goo.gl/tsqt64'> Elefan Sample dataset </a>)</li>")
    text <- paste0(text, "<li> Adjust the Assessment Settings")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> Data Settings - select the years of data to include in the analysis,adjust the aggregation, bin size and moving average (MA) of the dataset to optimise cohort recognition by the Elefan algorithm </li>")
    text <- paste0(text, "<li> Elefan - set the search space of the Elefan algorithm to estimate growth parameters </li>")
    text <- paste0(text, "<li> Stock status - set the parameters required to estimate mortality and perform YPR (and SPR; optional) </li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</li>")
    text <- paste0(text, "<li> Check and Run the Assessment:")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> Run a check on the parameterisations prior to running the full assessment (Run Cheack button)</li>")
    text <- paste0(text, "<li> Run the assessment (Run Assessment button). Watch for the progress bar in the center of the screen</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</li>")
    text <- paste0(text, "<li> Download the report as a pdf (Download Report button)</li>")
    text <- paste0(text, "<li> The Reset button removes the uploaded data and resets the settings to default</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Data, Methods, and Results Considerations tabs. Note that error messages may display in the center of the page and in place of any figures where an error has occurred.")
    text <- paste0(text, "</p>")    
    return (text)
}


getMethodConsiderationTextForElefan <- function() {
    text <- "<b>Consecutive steps of the length-based stock assessment routine workflow:</b>"
    text <- paste0(text, "<br>")
    text <- paste0(text, "<ol>")
    text <- paste0(text, "<li> ELEFAN: Estimation of growth parameters of the von Bertlanffy growth (VBG) function.</li>")
    text <- paste0(text, "<li> Empirical formulae: Estimation of the natural mortality rate (M).</li>")
    text <- paste0(text, "<li> Catch curve: Estimation of the total (Z) and fishing (F) mortality rate.</li>")
    text <- paste0(text, "<li> Yield per recruit analysis (YPR): Estimation of reference points based on fishing ",
                   "mortality and Spawning potential ratio (SPR), such as Fmax, F0.1, F35, as well as the stock status ",
                   "(e.g. F/F0.1 or SPR).</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "<br>")
    text <- paste0(text,
                   "<p>This length-based stock assessment routine was outlined in the Technical report ",
                   '<a href="http://www.fao.org/documents/card/en/c/9bb12a06-2f05-5dcb-a6ca-2d6dd3080f65/"> "Introduction to Tropical Fish Stock Assessment" (FAO, 1998)</a>',
                   ", and compiled into the R package ",
                   '<a href="https://cran.r-project.org/web/packages/TropFishR"> TropFishR </a>',
                   " by ", '<a href="https://doi.org/10.1111/2041-210X.12791"> Mildenberger et al. (2017)</a>',
                   ". Above mentioned methods make specific assumptions about the data, the stock, or the ",
                   "fisheries targeting the stock. It is important to be aware of these assumptions and, thus, the ",
                   "limitations of the results.</p>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<b>Assumptions of TropFishR:</b>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Representative length measurements:</b> ",
                   "The routine assumes that the dataset is representative of the length distributions of the whole catch. ",
                   "This means that either the length of all individuals in the catch were measured or ",
                   "a randomised subsample of the catch was measured.", "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Gear/fleet selectivity:</b> ",
                   "The routine assumes sigmoidal selectivity as it is commonly assumed for 'trawl-like' ",
                   "fishing gear. While the selectivity of gillnets and hook-based methods might correspond ",
                   "to a bell-shaped curve, the combination of various mesh and hook sizes might lead to ",
                   "an overall 'trawl-like' fleet selectivity.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Equilibrium conditions:</b> ",
                   "The routine assumes constant recruitment, fishing and natural mortality as well as somatic growth and maturation over time, i.e. within the year and over all years covered by the dataset.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Density dependence:</b> ",
                   "The routine assumes density independent maturity and somatic growth. All reference points ",
                   "are based on the 'per recruit' model, thus no assumptions are made about the stock recruitment ",
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
                   "classes. (Note this assumption will be relaxed in the future.)",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Somatic growth follows VBG:</b> ",
                   "The estimation of growth parameters with ELEFAN assumes that the growth of individuals in length ",
                   "follows the logistic von Bertlanffy growth (VBG) function. This is an often made assumption for the ",
                   "growth of fish, but might not reflect well the growth of species with an exoskeleton, ",
                   "such as crustaceans, nor the growth of early life stages of fish.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Closed population:</b> ",
                   "The routine assumes that the stock (population) under study ",
                   "is closed, meaning that there is no immigration or emmigration taking place. Immi- and emi-",
                   "gration can both bias estimated mortality rates and stock status. Furthermore, fish migrations ",
                   "often vary for various life stages and might thus affect the representativeness of the length ",
                   "measurements if the population is not closed.",
                   "</li>")
    text <- paste0(text, "</ul>")
    return (text)
}

getResultConsiderationTextForElefan <- function() {
    text <- "<b>Data exploration:</b>"
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "Direct after the successful upload of your data set, <b>Figure 1</b> shows the length-frequency distributions per sampling time (x axis). Panel A shows the raw data, while panel B shows the restructured data. This means after subtracting the moving average (MA) of the count in each length class. The combination of bin size and MA critically affects the separation of peaks (i.e. potential cohorts) in the dataset and thus the estimation of growth parameters by ELEFAN. Blue shading indicates a high count per length bin (panel A) and a high positive value (panel B). Red shading indicates a negative value (only pane B). A good bin size value reduces noise in the data by aggregation. A good bin size value reduces noise in the data by aggregation and is defined before the MA value. A good MA value leads to visually distinct peaks in particular among small length classes.")
    text <- paste0(text, "<br>")
    text <- paste0(text, "After the successful stock assessment (click 'Run Assessment'), two figures and a table summarise the results of the estimation of growth parameters by ELEFAN with genetic algorithm (GA) and four figures and three tables summarise the stock status results.")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text,"<b>ELEFAN:</b>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Figure 2</b> shows the same length frequency distributions as Figure 1 with overlaid growth curves estimated by ELEFAN. This plot allows the visual inspection how well estimated curves connect the peaks (i.e. potential cohorts) in the raw (A) and restructured (B) data set.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Table 1</b> includes estimated von Bertlanffy growth parameters (",
                  withMathJax("\\(L_\\infty, K, t_a\\)"),"), the growth performance coefficient phi', and the best score value (Rn). The growth performance index is calculated based on the formula ",withMathJax("\\(phi' = \\log_{10} K + 2 \\log_{10} L_\\infty\\)"), "and provides a metric that accounts for the correlation of ",withMathJax("\\(L_\\infty\\)")," and ",withMathJax("\\(K\\)")," to compare growth parameters among analyses or species. The Rn value is the sum of all values associated with the the bins after restructuring which intersect with the growth curves. The value can be used to compare the fit of estimated growth parameters to the data set for different MA values, growth paramater search spaces, or ELEFAN optimisation parameters (e.g. population size). However, it cannot be used to compare the fit for different data sets or different bin sizes as this affects the maximum overall possible Rn value.",
               "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Figure 3 </b> shows the improvement of the ELEFAN fit in terms of the average and best score value (fitness value) of the genetic algorithm used in ELEFAN_GA over the number of iterations (generations). Ideally, the number of iterations (or generations) is large enough, so that there are no large jumps visible during the last iterations of the best and average score value.",
                   "</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text,"<b>Stock status:</b>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Figure 4</b> shows the logarithm of the catch per length interval against the relative age (x axis). ",
                   "Highlighted characters were used in the regression analysis of the catch curve for the estimation of the total ",
                   "mortality rate (Z), which ",
                   "corresponds to the slope of the displayed regression line. The selection of points is automatic and based on a ",
                   "list of expert recommendations.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Figure 5</b> shows the probability of capture as a measure of the selectivity used in the assessment.",
                   "The curve is either based on provided selectivity parameters (L50 and L75 or selection width) or was estimated ",
                   "by the catch curve. Displayed selection ogive is used for the yield per recruit analysis (YPR).",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Table 2</b> lists estimated mortality rates (Z, F, M), exploitation ",
                   "rate (E), and estimated/provided selectivity parameters (L50, L75).",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Table 3</b> includes estimated reference points (Fmax, F0.1, F0.5), ",
                   "and SPR-based reference points (F30, F35, F40) if maturity parameters (Lm50 and ",
                   "Lm75) are provided.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Table 4</b> presents estimated stock status in terms of current ",
                   "fishing mortality (F) to reference points (Fmax, F0.1, F0.5). If maturity ",
                   "parameters (Lm50 and Lm75) are provided, additional reference points (F30, F35, ",
                   "F40) and current Spawning Potential Ratio (SPR) are included as well.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Figure 6</b> shows the results of the yield per recruit model as yield and biomass per recruit ",
                   "in panel A and B, respectively, against a range of fishing mortality rates (x axis). ",
                   "Grey segments display estimated reference points. Fmax is defined as the fishing ",
                   " mortality (F) leading to the maximum yield per recruit. F0.1 corresponds to F where the slope of the yield per ",
                   "recruit curve is equal to 10% of the slope in the origin and poses a more conservative reference point than Fmax. ",
                   "F0.5 corresponds to F where the biomass per recruit is equal to 50% of the biomass per recruit without fishing. ",
                   "If the maturity parameters Lm50 and Lm75 are provided, an optional third panel (C) shows the Spawning Potential Ratio (SPR) ",
                   "for a range of fishing mortality rates. Furthermore, the three reference points F30, F35, and F40 are shown, which ",
                   "correspond to F that leads to a SPR of 30%, 35%, and 40% respectively.",
                   "</li>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<li><b>Figure 7</b> shows the yield per recruit (A) and biomass per recruit (B) for various fishing mortality ",
                   "rates (x axis) and gear selectivities (y axis). The color indicates high (red) to low (blue) yield and biomass for ",
                   "the various combinations of F and L50 values. The black dot indicates current yield and biomass.",
                   "</li>")
    text <- paste0(text, "</ul>")
    return (text)
}






getErrorMessage <- function(forWhat) {
    if(forWhat=="CMSY"){
        return (paste0("Oops! Unfortunately the ",forWhat, " method experienced a problem with the server.<br/>Don't give up and try again in a few minutes or refresh your Stock Monitoring Tool instance.<hr/> <b>%s</b>"))
    }else{
        return (paste0("Oops! unfortunately something went wrong running the ",forWhat," method<br/>Don't give up and try again in a few minutes.<hr/> <b>%s</b>"))
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
