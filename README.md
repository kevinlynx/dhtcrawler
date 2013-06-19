## dhtcrawler

dhtcrawler is a DHT crawler written in erlang. It can join a DHT network and crawl many P2P torrents. The program save all torrent info into database and provide an http interface to search a torrent by a keyword.

![screenshot](https://raw.github.com/kevinlynx/dhtcrawler/master/screenshot.png)

## Usage

* Download mongodb and start it with text search,  i.e:

        mongod --dbpath db --setParameter textSearchEnabled=true

* Download dhtcrawler source code
* Use `rebar` to download and install all dependent libraries

        rebar get-deps

* compile 

        rebar compile

* start erl and add all dependent libraries path, i.e:

        code:add_path("deps/kdht/ebin").
        code:add_path("deps/bson-erlang/ebin").
        code:add_path("deps/mongodb-erlang/ebin").

* start all dependent application specified in app meta file, i.e:

        Apps = [crypto, public_key, ssl, inets, bson, mongodb],	
        [application:start(App) || App <- Apps].
        
* start dhtcrawler

        application:start(dhtcrawler).

* start the http front-end

        crawler_http:start().

* Open a web browser and point to `localhost:8000/index.html`

## Config

see priv/dhtcrawler.config.

