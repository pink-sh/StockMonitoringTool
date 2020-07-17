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

#Install XML package from archive
#Issue is that XML package from 2020-07 is referenced as depending on R >= 4.0
#To temporarily solve that we use the previous XML package version from archive
RUN wget https://cran.r-project.org/src/contrib/Archive/XML/XML_3.99-0.3.tar.gz
RUN R -e "install.packages('XML_3.99-0.3.tar.gz', repos = NULL, type = 'source')"

# install dependencies of the Stock monitoring tool app
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'shinythemes', 'shinydashboard', 'RCurl', 'devtools', 'ggplot2', 'rfishbase', 'shinyBS', 'lubridate', 'waiter', 'pracma', 'googleVis', 'stringr','R.utils'), repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('AnalytixWare/ShinySky')"
RUN R -e "devtools::install_github('daattali/shinyjs')"
RUN R -e "devtools::install_github('jyypma/nloptr')"
RUN R -e "install.packages(c('fishmethods'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('V8'), repos='https://cloud.r-project.org/')"
#RUN R -e "install.packages(c('XML'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('DT'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('futile.logger', repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_version('TropFishR', version='1.6.1', repos = 'http://cran.r-project.org')"

RUN git -C /root/ clone https://github.com/abennici/StockMonitoringTool.git && echo "OK!"
RUN mkdir -p /srv/shiny/
RUN ln -s /root/StockMonitoringTool /srv/shiny/stockMonitoringTools
 
EXPOSE 3838

ENV SMT_LOG=session.log

RUN apt-get install -y curl
CMD ["R", "-e shiny::runApp('/srv/shiny/stockMonitoringTools',port=3838,host='0.0.0.0')"]
#CMD ["R", "-e shiny::runApp('/srv/shiny/stockMonitoringTools')"]
