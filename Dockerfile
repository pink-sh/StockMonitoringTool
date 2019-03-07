FROM openanalytics/r-base

MAINTAINER Enrico Anello "enrico.anello@fao.org"


# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-openssl-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    default-jre \
    default-jdk \
    libxml2 \
    libxml2-dev \
    git \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-formats-extra \
    libv8-3.14.5 \
    libv8-dev


RUN apt-get update && apt-get upgrade -y

 # install dependencies of the Stock monitoring tool app
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'shinythemes', 'shinydashboard', 'RCurl', 'devtools', 'ggplot2', 'rfishbase', 'shinyBS'), repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('AnalytixWare/ShinySky')"
RUN R -e "devtools::install_github('daattali/shinyjs')"
RUN R -e "devtools::install_github('jyypma/nloptr')"
RUN R -e "install.packages(c('fishmethods'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('V8'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('XML'), repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_version('TropFishR', version='1.2', repos = 'http://cran.us.r-project.org')"

RUN git -C /root/ clone https://github.com/pink-sh/StockMonitoringTool.git
RUN mkdir -p /srv/shiny/
RUN ln -s /root/StockMonitoringTool /srv/shiny/stockMonitoringTools
 
###EXPOSE 3838

###CMD ["R", "-e shiny::runApp('/srv/shiny/stockMonitoringTools',port=3838,host='0.0.0.0')"]
CMD ["R", "-e shiny::runApp('/srv/shiny/stockMonitoringTools')"]
