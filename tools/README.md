## dhtcrawler-bin

This git branch maintain pre-compiled erlang files to start dhtcrawler directly. 

## Usage

* start mongodb first

    mongod --dbpath your-database-path --setParameter textSearchEnabled=true

* start dhtcrawler crawler part, on Windows, just click `win_start.bat`
* start dhtcrawler httpd part, on Windows, just click `win_start_http.bat`
* checkout `localhost:8000`



