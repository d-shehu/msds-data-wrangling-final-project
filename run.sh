#!/bin/bash

containerName=$1
if [ -z "$containerName" ]
then
     containerName="data_wrangling_news_scraper"
fi

docker stop $containerName
docker rm $containerName

scriptDir=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd )
echo "Current script dir: ${scriptDir}"

docker run -it --rm --name=$containerName \
        --entrypoint=/bin/bash \
        --volume $HOME/runtime/data/raw/DW_Final_Project:/app/data \
        --volume $HOME/runtime/data/projects/msds-data-wrangling-final-project:/app/src \
	data_wrangling/scraper:latest
