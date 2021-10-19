
<!-- README.md is generated from README.Rmd. Please edit that file -->

# YFAR

<!-- badges: start -->

<!-- badges: end -->

The goal of YFAR is to provide a collection of functions for easy
retrieval of fantasy hockey league data from the Yahoo Fantasy API.
Please note any fantasy sports other than hockey are outside the scope
of this package.

The functions in this package can retrieve data from public leagues and
the private leagues participated in by the current user logged into
Yahoo.  
You will notice this in some of the uri’s as `use_login=1`.

## Installation

You can fork YFAR from github repo
[YFAR](https://github.com/macraesdirtysocks/YFAR) with:

The Yahoo Fantasy API uses Oauth2.0 for authentication. This means to
use the functions in this package you will need a client id and client
secret obtained by registering an app with the Yahoo Developer Network.

To register an app with the YDN log into your Yahoo account and go here:
[Yahoo Developer Network](https://developer.yahoo.com/apps/create/).

After creating and registering your app you should receive a client id
and client secret.

If further help regarding YDN is required please check out this
[repository](https://github.com/edwarddistel/yahoo-fantasy-baseball-reader)
which covers this topic and the Yahoo API in great detail.

Pass the client id and secret as arguments to `YFAR::y_create_token()`
which are passed to [httr](https://httr.r-lib.org/) to create an access
token.

The first time `y_create_token()` is run the console will ask for a code
and a web browser will open to display the code. Copy and paste this
code into the console to create your access token. If this step is being
a nuisance and your default browser is Chrome try Firefox.

By default caching is enabled and httr will create a .httr-oauth file
and you can refresh your token with `my_token$refresh()`.

### Get access token

Returns an Token2.0 environment

`library("YFAR")`

`my_token <- y_create_token(client_id, client_secret)`

After getting an access token you will need a league key to supply to
the functions. Each league in Yahoo\! Fantasy has a unique league key.
You can get your league keys by running `y_games(my_token)`.

### Get league id

Returns a tibble with meta data from the leagues played in by the logged
in user.

`my_leagues <- y_games(my_token)`

`league_id <- my_leagues$league_key[1]`

## Usage

Once you have selected a league key you supply it to a function with
your token to get the data:

### Get team info

Returns a tibble with the data about the teams in the league.

`teams_in_my_league <- y_teams(league_id, my_token)`

## Known limitations, issues and future work

1.  Incompatible data types when binding rows. Errors in the api result
    in incompatible data types for binding rows. Right now the easiest
    was to solve this problem is to convert all columns to data type
    character.  
    It would be nice to keep proper data types.
    
    `Error: "Can't combine <integer> and <character>"`
