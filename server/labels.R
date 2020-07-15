########################### ALL LABELS ###########################
output$homeInfo <- renderText({
  session$userData$page("home")
  text <- "<h3>Stock Monitoring Tools for limited data</h3>"
  text <- paste0(text, "<p>")
  text <- paste0(text, "On the left hand side you can choose between <b>CMSY</b>, <b>ELEFAN</b> and <b>YPR</b>/<b>SBPR</b> methods.")
  text <- paste0(text, "</p>")
  
  text <- paste0(text, "<p>")
  text <- paste0(text, "The <b>CMSY</b> method is provided by the <a href='http://www.bluebridge-vres.eu/' target='blank_'>iMarine Infrastructure</a>")
  text <- paste0(text, "</p>")
  
  text <- paste0(text, "<p>")
  text <- paste0(text, "For <b>ELEFAN</b> you can choose between ELEFAN_GA, ELEFAN_SA and ELEFAN. These methods are provided by the <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'>TropFishR</a> R library version ", packageVersion("TropFishR"))
  text <- paste0(text, "</p>")
  
  text <- paste0(text, "<p>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><b>ELEFAN GA:</b>&nbsp;Electronic LEngth Frequency ANalysis with genetic algorithm used for estimating growth parameters.</li>")
  text <- paste0(text, "<li><b>ELEFAN SA:</b>&nbsp;Electronic LEngth Frequency ANalysis with simulated annealing for estimating growth parameters.</li>")
  text <- paste0(text, "<li><b>ELEFAN:</b>&nbsp;Electronic LEngth Frequency ANalysis for estimating growth parameter.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</p>")
  
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>YPR</b> and <b>SBPR</b> are provided by the <a href='https://cran.r-project.org/web/packages/fishmethods/index.html' target='_blank'>fishmethods</a> R library version ", packageVersion("fishmethods"))
  text <- paste0(text, "</p>")
  
  text <- paste0(text, "<p>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><b>YPR:</b>&nbsp;Yield-per-recruit.</li>")
  text <- paste0(text, "<li><b>SBPR:</b>&nbsp;Spawning stock biomass-per-recruit.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</p>")
  
  text
})
output$cmsyIntroOut <- renderText({
  session$userData$page("cmsy-intro")
  text <- "<h3><b>CMSY - Catch-Maximum Sustainable Yield</b></h3>"
  text <- paste0(text, "<p>")
  text <- paste0(text, "The <b>CMSY</b> method for data-limited stock assessment. Described in <a target='_blank' href='https://www.researchgate.net/publication/309283306_Estimating_fisheries_reference_points_from_catch_and_resilience'>Froese et al.</a>")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "The CMSY algorithm can be found <a href='https://github.com/SISTA16/cmsy' target='_blank'>here on Github</a>")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<h4>Method Description</h4>")
  text <- paste0(text, "The Schaefer production model parameters are r and k. Different combinations of these parameters will produce different time series of biomass. In CMSY, the Schaefer model is run many times to calculate annual biomasses for r-k pairs randomly drawn from the prior distributions. The model determines which r-k pairs are valid: e.g., those pairs that result in a biomass time series that do not (1) result in a stock collapse or (2) allow the stock to exceed carrying capacity. Also, those r-k pairs that result in a final relative biomass estimate between the values specified in the inputs (the final depletion range), are accepted and used to calculate MSY (rk/4) and biomass over time.")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "The geometric means of the resulting density distributions of r, k and MSY are taken as the most probable values.")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<h4>Further information</h4>")
  text <- paste0(text, "Please visit the <a href='https://elearning.fao.org/course/view.php?id=502' target='_blank'>SDG Indicator 14.4.1 - Fish stocks sustainability eLearning course</a>, <b>Lesson 4, Slides 13-26.</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Slides 13-17 give an overview of the method;</li>")
  text <- paste0(text, "<li>Slides 20 and 23 explain how to run the method in the Stock Monitoring Tool;</li>")
  text <- paste0(text, "<li>Slide 21 describes the required dataset format;</li>")
  text <- paste0(text, "<li>Slide 22 describes the default and optional parameters;</li>")
  text <- paste0(text, "<li>Slide 24, and its popup give examples of how to interpret the outputs;</li>")
  text <- paste0(text, "<li>Slide 25 describes the method’s caveats.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<h4>Data Considerations</h4>")
  text <- paste0(text, "Mandatory fields to run CMSY are:")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Stock (fish stock name)</li>")
  text <- paste0(text, "<li>Yr (year of the catch)</li>")
  text <- paste0(text, "<li>Ct (catch)</li>")
  text <- paste0(text, "<li>bt (biomass estimates, if available; otherwise input “NA”)</li>")
  text <- paste0(text, "<li>Other columns are identifiers that may be included, but are not necessary to run the model.</li>")
  text <- paste0(text, "<li>Ensure your data are in .csv format and use a “.” to separate decimals in the data.</li>")
  text <- paste0(text, "<li>Please ensure your time-series at least 15 years in length from starting year to ending year.</li>")
  text <- paste0(text, "<li>Note that years with missing data should be filled with an 'NA' value.</li>")
  text <- paste0(text, "<li>**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of natural mortality (M) for the Optional Parameters section.
</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<strong>References</strong><br/>")
  text <- paste0(text,"&nbsp;&nbsp;Froese, Rainer & Demirel, Nazli & Coro, Gianpaolo & Kleisner, Kristin & Winker, Henning. (2017). Estimating fisheries reference points from catch and resilience. Fish and Fisheries. 18. 506-526. 10.1111/faf.12190.")
  text <- paste0(text, "</p>")
  text
})
output$elefanIntroOut <- renderText({
  session$userData$page('elefan-intro')
  text <- "<h3><b>ELEFAN methods by TropFishR</b></h3>"
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>Elefan</b> is a computational method designed to estimate life history parameters using a time series of length frequency observations.")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<h4>Method version</h4>")
  text <- paste0(text, "These methods are provided by the <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'>TropFishR library</a> version 1.6.1")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<h4>Method description</h4>")
  text <- paste0(text, "The study of fish growth involves a determination of body size as a function of age. Most stock assessment methods work essentially with age composition data. In temperate waters it is easier to acquire data to estimate age by counting of year rings on hard parts such as scales and otoliths (ear bones). These rings are formed due to strong fluctuations in environmental conditions from summer to winter and vice versa. In tropical areas such drastic changes do not occur and it is therefore very difficult, if not impossible to use this kind of seasonal rings for age determination.")
  text <- paste0(text, "</p>")
  text <- paste0(text, "ELEFAN is one of a number of numerical methods that have been developed, which allow the conversion of length-frequency data into age composition. Although these methods do not require the reading of rings on hard parts, the final interpretation of the results becomes much more reliable if at least some direct age readings are available.")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "The TropFishR has three different Elefan methods: <b>ELEFAN</b>, <b>ELEFAN GA</b> and <b>ELEFAN SA</b>")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<h4>ELEFAN</h4>")
  text <- paste0(text, "<b>E</b>lectronic <b>LE</b>ngth <b>F</b>requency <b>AN</b>alysis for estimating growth parameters.<br/>This function performs the K-Scan and Response surface analyses to estimate growth parameters. It combines the step of restructuring length-frequency data (lfqRestructure) followed by the fitting of seasonal von Bertalanffy Growth Function (VBGF) curves (see Supporting tools: Seasonal VBGF) through the restructured data (lfqFitCurves). K-Scan is a method used to search for the K parameter with the best fit while keeping ", withMathJax("\\(L_\\infty\\)"), "fixed. In contrast, with response surface analysis both parameters are estimated and the fits are displayed in a heatmap. Both methods use an optimisation to find the best time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month (t_anchor) value for each combination of K and ", withMathJax("\\(L_\\infty\\)"),". To find out more about t_anchor, please refer to the Details description of <a href='https://www.rdocumentation.org/packages/TropFishR/versions/1.6.2/topics/lfqFitCurves' target='_blank'>lfqFitCurves</a> in its RDocumentation. The score value Rn_max is not comparable with the score values of the other ELEFAN functions (ELEFAN_SA or ELEFAN_GA).")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<h4>ELEFAN GA (genetic algorithm)</h4>")
  text <- paste0(text, "<b>E</b>lectronic <b>LE</b>ngth <b>F</b>requency <b>AN</b>alysis with simulated annealing for estimating growth parameters.<br/>A more detailed description of the genetic algorithm (GA) can be found in <a href='https://pdfs.semanticscholar.org/8b54/b4c7f3c63efcfadac455a32f2c6d775c7184.pdf?_ga=2.102660097.1034706415.1573191124-1938330296.1570694384' target='_blank'>Scrucca (2013)</a>. The score value fitnessValue is not comparable with the score value of the other ELEFAN functions (ELEFAN or ELEFAN_SA).")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<h4>ELEFAN SA (simulated annealing)</h4>")
  text <- paste0(text, "<b>E</b>lectronic <b>LE</b>ngth <b>F</b>requency <b>AN</b>alysis with simulated annealing for estimating growth parameters.<br/>A more detailed description of the simulated annealing (SA) can be found in <a href='https://journal.r-project.org/archive/2013/RJ-2013-002/RJ-2013-002.pdf' target='_blank'>Xiang et al. (2013)</a>. The score value cost_value is not comparable with the score value of the other ELEFAN functions (ELEFAN or ELEFAN_GA).")
  text <- paste0(text, "</p>")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<h4>Method of operation:</h4>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<li>ELEFAN fits a seasonal version of the von Bertalanffy Growth Function (VBGF) (see Supporting Tools).</li>")
  text <- paste0(text, "<li>The first step is the restructuring of the length-frequency data using a procedure that scores the length bins based on their deviation from a moving average across neighboring bins.</li>")
  text <- paste0(text, "<li>The second step is the calculation of the cumulative score for a given set of VBGF parameters based on the bin scores that are intersected by resulting growth curves.</li>")
  text <- paste0(text, "<li>Finally, a search for VBGF parameters results in the maximum score value.</li>")
  text <- paste0(text, "<li>The ELEFAN method uses a Thompsen and Bell model to determine the F reference points and current F.</li>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<h4>Further Information</h4>")
  text <- paste0(text, "Please visit the <a href='https://elearning.fao.org/course/view.php?id=502' target='_blank'>SDG Indicator 14.4.1 - Fish stocks sustainability eLearning course</a>, <b>Lesson 4, Slides 27-34.</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Slides 27-29 give an overview of the method;</li>")
  text <- paste0(text, "<li>Slides 30-32 explain how to run the method in the Stock Monitoring Tool;</li>")
  text <- paste0(text, "<li>Slide 31 *popup* describes the required dataset format;</li>")
  text <- paste0(text, "<li>Slide 32 *popup* describes the default and optional parameters;</li>")
  text <- paste0(text, "<li>Slide 33 and its popup and Slide 35 give examples of how to interpret and use the outputs;</li>")
  text <- paste0(text, "<li>Slide 34 describes the method’s caveats.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</p>")
  
  text <- paste0(text, "<p>")
  text <- paste0(text, "<h4>Data Considerations</h4>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>")
  text <- paste0(text, "Mandatory fields for ELEFAN are")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Mid-lengths : Length classes are inserted as rows in the first column of the dataset.</li>")
  text <- paste0(text, "<li>Catches (in number) per length class (rows) and per sampling time (columns).</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "<li>Upon uploading your data file, please select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format would select 'Day Month Year'.</li>")
  text <- paste0(text, "<li>Ensure that your dates are input in chronological order.</li>")
  text <- paste0(text, "<li>Ensure that your length-frequency data is representative of the full population. (If this is not so, then estimates of fishing mortality will be biased.)</li>")
  text <- paste0(text, "<li>Ensure that all age groups were sampled.</li>")
  text <- paste0(text, "<li>Ensure that the sample was from a range of areas where different life histories might live. (e.g., if juveniles occupy nearshore habitat and adults are offshore)</li>")
  text <- paste0(text, "<li>Ensure that a variety of gears with different selectivities used to collect the samples so that the samples contain multiple age groups.</li>")
  text <- paste0(text, "<li>Ensure that the “midLength” column name is identical to the sample dataset.</li>")
  text <- paste0(text, "<li>Ensure your data are in .csv format.</li>")
  text <- paste0(text, "<li>Use a “.” to separate decimals in the data.</li>")
    text <- paste0(text, "</ul>")
  text <- paste0(text, "<h5>", "**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of ", withMathJax("\\(L_\\infty\\)"), " and von Bertalanffy K in the Optional Parameters section in the ELEFAN_GA tool.", "</h5>")
  text <- paste0(text, "</p>")
  
  link <- "<a href='https://goo.gl/tsqt64' target='_blank'>Click Here</a>"
  text <- paste0(text, "<h4>Information on the sample dataset</h4>")
  text <- paste0(text, "<p><h5>", link,"&nbsp; to download a sample dataset that can be used with <b>Elefan</b> methods", "</h5></p>")
  text <- paste0(text, "<span class=\"elefan_info\">The dataset used by this example is the <b>synLFQ7</b></span><br><br>")
  text <- paste0(text, "<span class=\"elefan_info\">Synthetic length-frequency data as generated by the function lfqGen from the fishdynr package (<a href='https://figshare.com/articles/fishdynr/4212726/2' target='_blank'>Taylor 2016</a>). Can be used by <b>ELEFAN</b>, <b>ELEFAN_SA</b>, or <b>ELEFAN_GA</b>. <br>The data are generated with the following VBGF parameters:</span>")
  text <- paste0(text, "<ul style=\"margin-top: 10px;\">")
  text <- paste0(text, "<li>Curving coefficient: K = 0.2 +/- 0.1 (CV)</li>")
  text <- paste0(text, "<li>Length infinity: ", withMathJax("\\(L_\\infty\\)")," = 123 +/- 0.05 (CV)</li>")
  text <- paste0(text, "<li>Amplitude of growth oscillation: C = 0.3</li>")
  text <- paste0(text, "<li>Summer point of oscillation: ", withMathJax("\\(t_s\\)")," = 0</li>")
  text <- paste0(text, "<li>Time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month: t_anchor between 0.16 and 0.34 (Time when yearly recruitment pulse occurs; e.g. 0 = Jan 1, 0.25 = Apr 1, 0.5 = Jul 1, 0.75 = Oct 1; repro_wt = c(0, 0, 0.2, 1, 0.6, 0, 0, 0, 0, 0, 0, 0))</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>Glossary</b><br/>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><b>lfqRestructure:</b> First step of the Electronic LEngth Frequency ANalysis (ELEFAN), which is restructuring length frequency data (lfq). This is done according to a certain protocol, described by many authors (see Details or References for more information).</li>")
  text <- paste0(text, "<li><b>lfqFitCurves:</b> This function estimates seasonal von Bertalanffy growth function (VBGF) curves for a set of growth parameters.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>References</b><br/>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Luca Scrucca (2013). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 53(4), 1-37. <a href='http://www.jstatsoft.org/v53/i04/' target='_blank'>http://www.jstatsoft.org/v53/i04/</a></li>")
  text <- paste0(text, "<li>Y. Xiang, S. Gubian. B. Suomela, J. Hoeng (2013). Generalized Simulated Annealing for Efficient Global Optimization: the GenSA Package for R. The R Journal, Volume 5/1, June 2013. <a href='https://journal.r-project.org/archive/2013/RJ-2013-002/index.html' target='_blank'>https://journal.r-project.org/archive/2013/RJ-2013-002/index.html</a></li>")
  text <- paste0(text, "<li>Taylor, Marc (2016): fishdynr. figshare. Software. <a href='https://doi.org/10.6084/m9.figshare.4212726.v2' target='_blank'>https://doi.org/10.6084/m9.figshare.4212726.v2</a></li>")
  text <- paste0(text, "</ul>")
  
  text <- paste0(text, "<p>")
  text
})
output$fishMethodsIntroOut <- renderText({
  session$userData$page('fishmethods-intro')
  text <- "<h3><b>FishMethods</b></h3>"
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>Fishmethods: </b>Fishery science methods and models from published literature and contributions from colleagues.")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "In data-limited situations where long-term, comprehensive catch data do not exist, per-recruit models can be used to determine estimates of optimal fishing mortality. Yield-per-recruit (YPR) and spawning biomass-per-recruit (SBPR) models calculate the equilibrium yield per recruit and spawning stock biomass per recruit, respectively, for a given value of fishing mortality (F) and a given length or age at first capture. Since F and Tc or Lc are values that a fishery manager can control (in theory), the idea is that by focusing on YPR or SBPR, managers can maintain a stock's population by preserving its reproductive capability. These models help to determine the optimum yield to prevent overfishing by instituting management controls on effort and length or age at first capture.")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "<h4>Methods used</h4>")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>SBPR</b> Spawning stock biomass-per-recruit(SBPR) analysis is conducted following Gabriel et al. (1989). Reference points of fishing mortality (F) and SBPR for a percentage of maximum spawning potential are calculated.")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>YPR</b> Yield-per-recruit (YPR) analysis is conducted following the modified Thompson-Bell algorithm. Reference points",  withMathJax("\\(F_{max}\\)"), "and",  withMathJax("\\(F_{0.1}\\)") ,"are calculated.")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>",withMathJax("\\(F_{0.1}\\)"),"</b> Fishing mortality rate corresponding to 10% of the slope of the YPR curve as a function of F when F=0. This is the F at which the marginal increase in equilibrium yield has dropped to 1/10 of its value when the stock was first exploited.")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>", withMathJax("\\(F_{max}\\)"), "</b>Fishing mortality rate that produces the maximum yield per recruit.")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>SBPR Options:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><b>Input file: </b> Input data file</li>")
  text <- paste0(text, "<li><b>M: </b> Single natural mortality (M) rate if M is assumed constant over all ages</li>")
  text <- paste0(text, "<li><b>pF: </b> Proportion of fishing mortality (F) that occurs before spawning</li>")
  text <- paste0(text, "<li><b>pM: </b> Proportion of natural mortality (M) that occurs before spawning</li>")
  text <- paste0(text, "<li><b>MSP: </b> Percentage of maximum spawning potential (%MSP) for which fishing mortality (F) and SBPR should be calculated</li>")
  text <- paste0(text, "<li><b>maxF: </b> Maximum value of fishing mortality (F) range over which SBPR will be calculated</li>")
  text <- paste0(text, "<li><b>incrF: </b> Fishing mortality (F) increment for SBPR calculation</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "<b>YPR Options:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><b>Input file: </b> Input data file</li>")
  text <- paste0(text, "<li><b>M: </b> Single natural mortality (M) rate if M is assumed constant over all ages</li>")
  text <- paste0(text, "<li><b>maxF: </b> Maximum value of fishing mortality (F) range over which YPR will be calculated. YPR is calculated for F = 0 to maxF</li>")
  text <- paste0(text, "<li><b>plus: </b> logical value indicating whether the last age is a plus-group</li>")
  text <- paste0(text, "<li><b>oldest: </b> if plus is checked, a numeric value indicating the oldest age in the plus group</li>")
  text <- paste0(text, "<li><b>incrF: </b> Fishing mortality (F) increment for YPR calculation</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</p>")
  text <- paste0(text, "<p>")
  text <- paste0(text, "W. L. Gabriel, M. P. Sissenwine & W. J. Overholtz (1989) Analysis of Spawning Stock Biomass per Recruit: An Example for Georges Bank Haddock, North American Journal of Fisheries Management, 9:4, 383-391, DOI: <a href='https://doi.org/10.1577/1548-8675(1989)009%3C0383:AOSSBP%3E2.3.CO;2' target='_blank'>10.1577/1548-8675(1989)009<0383:AOSSBP>2.3.CO;2</a>")
  text <- paste0(text, "</p>")
  text
})

output$cmsySampleDataset <- renderText({
  session$userData$page('cmsy-sample')
  link <- "<a href='https://data.d4science.net/qhX2' target='_blank'>Click Here</a>"
  text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>CMSY</b> methods", "</h4></p>")
  text <- paste0(text, "<hr />")
  text <- paste0(text, "<strong>If you are creating your own dataset</strong>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Ensure your time-series at least 15 years in length from the start year to end year.</li>")
  text <- paste0(text, "<li>Ensure that the column names are identical to the sample dataset.</li>")
  text <- paste0(text, "<li>Ensure your data are in .csv format.</li>")
  text <- paste0(text, "<li>Use a “.” to separate decimals in the data.</li>")
  text <- paste0(text, "<li>Years with missing data should be filled with an 'NA' value.</li>")
  text <- paste0(text, "<li>Mandatory fields required to run CMSY are</li>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Stock (fish stock name)</li>")
  text <- paste0(text, "<li>Yr (year of the catch)</li>")
  text <- paste0(text, "<li>Ct (catch)</li>")
  text <- paste0(text, "<li>bt (biomass estimates, if available; otherwise input “NA”)</li>")
  text <- paste0(text, "<li>Other columns are identifiers that may be included, but are not necessary to run the model.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of natural mortality (M) for the Optional Parameters section.")
  
  text
})
output$elefanSampleDataset <- renderText({
  session$userData$page('elefan-sample')
  link <- "<a href='https://goo.gl/tsqt64' target='_blank'>Click Here</a>"
  text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>Elefan</b> methods", "</h4></p>")
  text <- paste0(text, "<hr>")
  text <- paste0(text, "<b>If you are creating your own dataset:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Mandatory fields for ELEFAN are")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Mid-lengths : Length classes are inserted as rows in the first column of the dataset.</li>")
  text <- paste0(text, "<li>Catches (in number) per length class (rows) and per sampling time (columns).</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "<li>", "Sampling time column names should be dates in one of the formats:<br/><ul><li>YYYY-MM-DD</li><li>YYYY/MM/DD</li></ul>", "</li>")
  text <- paste0(text, "<li>", "Ensure that your length-frequency data is representative of the full population. (If this is not so, then estimates of fishing mortality will be biased.)", "</li>")
  text <- paste0(text, "<li>", "Ensure that all age groups were sampled.", "</li>")
  text <- paste0(text, "<li>", "Ensure that the sample was from a range of areas where different life histories might live. (e.g., if juveniles occupy nearshore habitat and adults are offshore)", "</li>")
  text <- paste0(text, "<li>", "Ensure that a variety of gears with different selectivities used to collect the samples so that the samples contain multiple age groups.", "</li>")
  text <- paste0(text, "<li>", "Ensure that the “midLength” column name is identical to the sample dataset.", "</li>")
  text <- paste0(text, "<li>", "Ensure your data are in .csv format.", "</li>")
  text <- paste0(text, "<li>", "Use a “.” to separate decimals in the data.", "</li>")
  text <- paste0(text, "<li>", "**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of L∞ and von Bertalanffy K in the Optional Parameters section in the ELEFAN_GA tool.", "</li>")
  text <- paste0(text, "</ul>")
  
  text
})
output$fishMethodsSampleDataset <- renderText({
  session$userData$page('fishmethods-sample')
  link <- "<a href='https://data.d4science.org/shub/E_NnMvMjhHUjB4Q3k4SE1oWjFCamxxMm5zdUxMSEpKbFdlcjVWaHQ1U1ZoTXJJY0dqaWJzRmxHWDVFemFjYVhwcQ==' target='_blank'>Click Here</a>"
  text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>FishMethods</b>", "</h4></p>")
  text <- paste0(text, "<b>If you are creating your own dataset</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Mandatory fields to run YPR/SBPR are:</li>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>age (the age of the fish)</li>")
  text <- paste0(text, "<li>ssbwgt (the spawning stock weights for each age)</li>")
  text <- paste0(text, "<li>partial (the recruitment at each age that is used to determine how much fishing mortality (F) each age group receives)</li>")
  text <- paste0(text, "<li>pmat (the proportion of mature fish at each age (used only for SBPR)</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<li>Ensure that the column names are identical to the sample dataset.</li>")
  text <- paste0(text, "<li>Ensure your data are in .csv format.</li>")
  text <- paste0(text, "<li>Use a “.” to separate decimals in the data.</li>")
  text <- paste0(text, "</ul>")
  text
})

output$vonBertalannfyInfoText <- renderText({
  text <- "<b>The VBGF expresses the length, L, as a function of the age of the fish, t. K is a parameter that controls the curvature</b>"
  text <- paste0(text, "<h5>", withMathJax("\\(L_\\infty\\)"), "is interpreted as 'the mean length of very old (strictly: infinitely old) fish'. It is also called the 'asymptotic length'.", "</h5>")
  text <- paste0(text, "<h5>", "K is a 'curvature parameter' which determines how fast the fish approaches its ", withMathJax("\\(L_\\infty.\\)"), "</h5>")
  text <- paste0(text, "<h5>", "Some species, most of them short-lived, almost reach their ", withMathJax("\\(L_\\infty\\)"),  "in a year or two and have a high value of K.")
  text <- paste0(text, "Other species have a flat growth curve with a low K-value and need many years to reach anything like their ", withMathJax("\\(L_\\infty.\\)"),  "</h5>")
  text <- paste0(text, "<h5>", "Increasing K with the slider bar will result in a growth curve that has more 'bend'.", "</h5>")
  text <- paste0(text, "<h5>", "The third parameter, ", withMathJax("\\(t_0,\\)"),  "sometimes called 'the initial condition parameter', determines the point in time when the fish has zero length.", "</h5>")
  text <- paste0(text, "<h5>",  "Biologically, this has no meaning, because the growth begins at hatching when the larva already has a certain length, which may be called L(0) when we put t = 0 at the day of birth.", "</h5>")
  text <- paste0(text, "<h5>",  "It is easily identified by inserting t = 0 into the equation.", "</h5>")
  text <- paste0(text, "<h5>",  "Growth parameters differ from species to species, but they may also vary from stock to stock within the same species, i.e. growth parameters of a particular species may take different values in different parts of its range. Also successive cohorts may grow differently depending on environmental conditions.", "</h5>")
  text <- paste0(text, "<h5>", "Further growth parameters often take different values for the two sexes. If there are pronounced differences between the sexes in their growth parameters, the input data should be separated by sex and values of K, ", withMathJax("\\(L_\\infty,\\)"), " and ", withMathJax("\\(t_0\\)"), "should be estimated for each sex separately.", "</h5>")
})
output$seasonalVonBertalannfyInfoText <- renderText({
  text <- "<h4>Like the generalized VBGF, the seasonal VBGF expresses the length, L, as a function of the age of the fish, t. K is a parameter that controls the curvature.</h4>"
  text <- paste0(text, "<h4>", "The addition of the term:", withMathJax("\\(\\frac{Ck}{2\\pi}sin2\\pi(t-t_s)\\)"), "produces seasonal oscillations of the growth rate, by changing ", withMathJax("\\(t_0,\\)"), "during the year. The parameter ", withMathJax("\\(t_s,\\)"))
  text <- paste0(text, "is called the 'summer point', and takes values between 0 and 1. At the time of the year when ")
  text <- paste0(text, "the fraction ", withMathJax("\\(t_s,\\)"), "has elapsed, the growth rate is the highest. At time")
  text <- paste0(text, " ", withMathJax("\\(t_w = t_s+0.5,\\)"), "which is the 'winter point', the growth rate is the lowest.")
  text <- paste0(text, "</h4>")
  text <- paste0(text, "<h4>")
  text <- paste0(text, "If C = 0, then the equation reduces to the generalized VBGF. In other words, C = 0 implies that there is no ")
  text <- paste0(text, "seasonality in the growth rate. The higher the value of C the more pronounced are the seasonal oscillations. ")
  text <- paste0(text, "If C = 1, the growth rate becomes zero at the winter point.")
  text <- paste0(text, "</h4>")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "<h5>")
  text <- paste0(text, "As with the generalized VBGF:")
  text <- paste0(text, "</h5>")
  text <- paste0(text, "<h5>")
  text <- paste0(text, withMathJax("\\(L_\\infty\\)"), "is is interpreted as 'the mean length of very old (strictly: infinitely old) fish'. it is also called the 'asymptotic length'.")
  text <- paste0(text, "</h5>")
  text <- paste0(text, "<h5>")
  text <- paste0(text, "K is a 'curvature parameter' which determines how fast the fish approaches its ", withMathJax("\\(L_\\infty.\\)"))
  text <- paste0(text, "</h5>")
  text <- paste0(text, "<h5>")
  text <- paste0(text, "Some species, most of them short-lived, almost reach their ", withMathJax("\\(L_\\infty\\)"), "in a year or two and have a high value of K. Other species have a flat growth curve with a low K-value and need many years to reach anything like their ", withMathJax("\\(L_\\infty.\\)"))
  text <- paste0(text, "</h5>")
  text <- paste0(text, "<h5>")
  text <- paste0(text, "Increasing K with the slider bar will result in a growth curve that has more 'bend'.")
  text <- paste0(text, "</h5>")
  text <- paste0(text, "<h5>")
  text <- paste0(text, "The third parameter, ", withMathJax("\\(t_0,\\)"), "sometimes called 'the initial condition parameter', determines the point in time when the fish has zero length.")
  text <- paste0(text, "</h5>")
  text <- paste0(text, "<h5>")
  text <- paste0(text, "Biologically, this has no meaning, because the growth begins at hatching when the larva already has a certain length, which may be called L(0) when we put t = 0 at the day of birth.")
  text <- paste0(text, "</h5>")
  text <- paste0(text, "<h5>")
  text <- paste0(text, "It is easily identified by inserting t = 0 into the equation.")
  text <- paste0(text, "</h5>")
  text
})
output$naturalMortalityInfoText <- renderText({
  text <- "<h5>This tool employs various empirical estimators of natural mortality.</h5>"
  text <- paste0(text, "<h5>", "When the user enters the scientific name for a fish species, FishBase will be queried for:", "</h5>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>", "(1) Maximum age (Amax)", "</li>")
  text <- paste0(text, "<li>", "(2) Age at maturity (Amat)", "</li>")
  text <- paste0(text, "<li>", "(3) L-infinity (in cm) (Linf)", "</li>")
  text <- paste0(text, "<li>", "(4) Von Bertalanffy Growth Function (VBGF) growth coefficient (k)", "</li>")
  text <- paste0(text, "<li>", "(5) VBGF age at size 0 (t0)", "</li>")
  text <- paste0(text, "<li>", "(6) Body length in cm (Bl)", "</li>")
  text <- paste0(text, "<li>", "(7) Mean water temperature in Celsius (Temp)", "</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<h5>", "Averaging of Von Bertalanffy (VBFG) parameters is done following the specifications of Pauly, D. (1991). Growth performance in fishes: rigorous description of patterns as a basis for understanding causal mechanisms. ICLARM Contribution No. 793.", "</h5>")
  text <- paste0(text, "<h5>", "Estimates will be displayed in the main panel.",  "</h5>")
  text <- paste0(text, "<h5>", "Four methods use Amax, three methods use the VBGF parameters, two methods use the VBGF parameters & Temp, one method uses the VBGF parameters & Amat, and three methods use only Amat. These groupings are indicated by the different colors in the top plot.", "</h5>")
  text <- paste0(text, "<h5>", "<em>The user can also choose to input their own parameters if they have inputs from local studies. These input values will override the FishBase calculations if all the necessary parameters for a particular method are available (e.g., all the VBGF parameters are available for those methods that require them).</em>", "</h5>")
  text <- paste0(text, "<h5>","The individual estimates of M are combined with defined weightings below (that the user can modify) and a single average M is provided. This average M can be used as input in the YPR/SBPR and ELEFAN applications.",  "</h5>")
  text <- paste0(text, "<h5>", "References for each method can be found", " <a href=\"javascript:window.open('References_M.html', '_blank','width=600,height=400')\">here</a>", "</h5>")
  text <- paste0(text, "<h5>",  "</h5>")
  text
})

output$tropFishRLibVersion1 <- renderText({
  text <- paste0("<span class='subTitle'>", "By <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'>TropFishR</a> version ", packageVersion("TropFishR"),"</span>")
  text
})

output$tropFishRLibVersion2 <- renderText({
  text <- paste0("<span class='subTitle'>", "By <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'>TropFishR</a> version ", packageVersion("TropFishR"),"</span>")
  text
})

output$tropFishRLibVersion3 <- renderText({
  text <- paste0("<span class='subTitle'>", "By <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'>TropFishR</a> version ", packageVersion("TropFishR"),"</span>")
  text
})

output$fishMethodsVersion1 <- renderText({
  text <- paste0("<span class='subTitle'>", "By <a href='https://cran.r-project.org/web/packages/fishmethods/index.html' target='_blank'>Fishmethods</a> version ", packageVersion("fishmethods"),"</span>")
  text
})

output$fishMethodsVersion2 <- renderText({
  text <- paste0("<span class='subTitle'>", "By <a href='https://cran.r-project.org/web/packages/fishmethods/index.html' target='_blank'>Fishmethods</a> version ", packageVersion("fishmethods"),"</span>")
  text
})

output$basicShaeferTitle <- renderText({
  session$userData$page('basic-shaefer')
  text <- "<span><h3><b>Run surplus production model</b></h3></span>"
  text
})
output$basicVonBertalannfyTitle <- renderText({
  session$userData$page('basic-von-bertalannfy')
  text <- "<span><h3><b>Generalized Von Bertalanffy Growth Function (VBGF)</b></h3></span>"
  text
})
output$SeasonalVonBertalannfyTitle <- renderText({
  session$userData$page('seasonal-von-bertalannfy')
  text <- "<span><h3><b>Seasonal Von Bertalanffy Growth Function (soVBGF)</b></h3></span>"
  text
})
output$naturalMortalityTitle <- renderText({
  session$userData$page('natural-mortality')
  text <- "<div style='width: 100%;position: relative;height: 100px; margin-bottom:3px;'>"
  text <- paste0(text, "<div style='float: left; width: 70%;'><span><h3><b>Estimating Natural Mortality (M) from FishBase life history parameters</b></h3><br>This application is a modified version of the Barefoot Ecologist tool developed by Jason Cope: <a target='_blank' href='http://barefootecologist.com.au/shiny_m.html'>http://barefootecologist.com.au/shiny_m.html</a></span></div>")
  text <- paste0(text, "</div>")
  text
})


output$rnMax_sa <- renderText({
  if ("results" %in% names(elefan_sa)) {
    title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", round(elefan_sa$results$data$Rn_max, 3))
    title
  } else {  "" }
})
output$rnMax <- renderText({
  if ("results" %in% names(elefan)) {
    title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", round(elefan$results$data$Rn_max, 3))
    title
  } else {  "" }
})

output$titlePlot1_elefan <- renderText({
  if ('results' %in% names(elefan)) {
    txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
    txt
  }
})
output$titlePlot2_elefan <- renderText({
  if ('results' %in% names(elefan)) {
    txt <- "<p class=\"pheader_elefan\">Restructured LFQ data</p>"
    txt
  }
})
output$titlePlot3_elefan <- renderText({
  if ('results' %in% names(elefan)) {
    txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F</p>"
    txt
  }
})
output$titlePlot4_elefan <- renderText({
  if ('results' %in% names(elefan)) {
    txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F and Lc</p>"
    txt
  }
})
output$title_tbl1_e <- renderText({
  if ('results' %in% names(elefan)) {
    txt <- "<p class=\"pheader_elefan\">Biological reference levels:</p>"
    txt
  }
})
output$title_tbl2_e <- renderText({
  if ('results' %in% names(elefan)) {
    txt <- "<p class=\"pheader_elefan\">Current levels:</p>"
    txt
  }
})
output$titleResultsOfTheComputation_elefan <- renderText({
  if ('results' %in% names(elefan)) {
    txt <- "<h2>Results of the ELEFAN computation</h2>"
    txt
  }
})

output$shaefer_ex1 <- renderUI({
  withMathJax(helpText('Classic Schaefer (logistic) form  $$B_t = rB_t\\left(1 +
               \\frac{r}{K}\\right)\\!$$'))
})
output$shaefer_ex2 <- renderUI({
  withMathJax(helpText('Biomass giving maximum sustainable yield:  $$B_{MSY} = \\frac{K}{2}\\!$$'))
})
output$shaefer_ex3 <- renderUI({
  withMathJax(helpText('Maximum sustainable yield:  $$MSY = \\frac{rK}{4}\\!$$'))
})
output$shaefer_ex4 <- renderUI({
  withMathJax(helpText('Fishing mortality rate at MSY:  $$F_{MSY} = \\frac{r}{2}\\!$$'))
})
output$basicShaeferInfoText <- renderText({
  text <- "<p><h5>The surplus production model is an example of a 'holistic model', wherein the stock is considered as one unit of biomass and no attempt is made to model on an age- or length-base. These models deal with the entire stock, the entire fishing effort and the total yield obtained from the stock, without incorporating details such as growth and mortality-at-age or the effect of the gear on the age of fish capture.</h5></p>"
  text <- paste0(text, "<p><h5>", "The objective of the application of 'surplus production models' is to determine the optimum level of effort, that is the effort that produces the maximum yield that can be sustained without affecting the long-term productivity of the stock, the so-called maximum sustainable yield (MSY).", "</h5></p>")
})

output$cmsyLegacyWarning <- renderText({
  text <- "<span style='margin-left: 20px;'>This computation may take several minutes to complete.</span>"
  text
})