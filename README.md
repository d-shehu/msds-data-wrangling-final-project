# msds-data-wrangling-final-project
Repo for Rutger's, MSDS Program's Data Wrangling class

Please see the final_project_report.rmd and final_project_report.pdf for the
detailed information. What's included here:

* app: the complete shiny app which loads the corpus of news and has visualizations
for text mining, sentiment analysis, topic modeling, etc.

* data: a copy of the data files which reside in S3 as well. The app loads Data
from s3 and these are here only as a frame of reference. There 2 sets, small
and large. Small is a sample of 1000 articles. All data files in Parquet format.

* final project report: pdf and rmd (R markdown) with detailed view of data and
analysis

* build.sh and run.sh script for building running processing and data collection
scripts in a containerized R environment

* scripts: the actual logic for the aforementioned data collection and parsing

URL: https://github.com/d-shehu/msds-data-wrangling-final-project
News API Key: <redacted>
