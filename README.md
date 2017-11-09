# Stock Monitoring Tools
This is the source code a the **Stock Monitoring Tools** Shiny application.
The application has been developed on an Ubuntu 16.04 operating system and the following requirements are meant for that platform

### System Wide Dependendencies
These dependencies can be installed using the usual *sudo apt install* command

- pandoc
- pandoc-citeproc
- libcurl4-openssl-dev
- libcairo2-dev
- libxt-dev
- libssl-dev
- libssl1.0.0
- libxml2 
- libxml2-dev 
- texlive-latex-base 
- texlive-fonts-recommended 
- texlive-formats-extra 
- libv8-3.14.5 
- libv8-dev

### R libraries
**devtools** must be installed in order to install some packages from GitHub
```
install.packages("devtools")
```
The following libraries have to be installed from GitHub
```
devtools::install_github('AnalytixWare/ShinySky')
devtools::install_github('daattali/shinyjs')
devtools::install_github('jyypma/nloptr')
```
The following libraries can be installed from CRAN
```
install.packages(c('shiny', 'rmarkdown', 'shinythemes', 'shinydashboard', 'TropFishR', 'RCurl', 'V8', 'fishmethods'), repos='https://cloud.r-project.org/')
```