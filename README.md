
<!-- README.md is generated from README.Rmd. Please edit that file -->

# YFAR

*A collection of functions for retrieving fantasy hockey league data
from the Yahoo Fantasy API.*

<!-- badges: start -->

<!-- badges: end -->

## Intro

At this time any fantasy sports other than hockey are outside the scope
of this package for the time being.

The functions in this package can retrieve data from public leagues and
the private leagues participated in by the current user logged into
Yahoo. You will notice this in some of the uri’s as `use_login=1`.

After creating a token and retrieving some simple input data you can use
the functions to return data from your league and teams.

## Installation

devtools::install\_github(“<https://github.com/macraesdirtysocks/YFAR>”)

The Yahoo Fantasy API uses Oauth2.0 for authentication. To use the
functions in this package you will need a client id and client secret
obtained by registering an app with the Yahoo Developer Network.

To register an app with the YDN log into your Yahoo account and go here:
[Yahoo Developer Network](https://developer.yahoo.com/apps/create/).

  - If further help regarding YDN is required please check out the
    following resources:
      - Edward Distel’s
        [repository](https://github.com/edwarddistel/yahoo-fantasy-baseball-reader)
      - [Yahoo Oauth
        workflow](%22https://web.archive.org/web/20130630133353/http://developer.yahoo.com/oauth/guide/oauth-guide.html)
      - [Yahoo API
        walkthrough](%22https://web.archive.org/web/20130822065813/http://developer.yahoo.com/fantasysports/guide/index.html)

After creating and registering your app you should receive a client id
and client secret.

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

`teams_in_my_league <- y_teams(league_id, my_token)`

### Get league settings

`my_league_settings <- y_league_settings(league_id, my_token)`

### Get draft results

Some functions accept a team\_id or league\_id. Any function with an
`id` argument as opposed to the more explicit league\_id or team\_id
will accept either or.

For example here are the arguments for `y_draft_results`:

    y_draft_results <- function(id = NULL, token_name = NULL, debug = FALSE)

So you could do:

`draft_results <- y_draft_results(league_id, my_token)`

OR

`draft_results <- y_draft_results(team__id, my_token)`

And much more. See the vignette for more info.

## Other notes

If you have suggestions or want to collab shoot me an email.

In order to keep the scope of this project manageable and myself
motivated by interest I’ve restricted the scope to hockey. However I
have tried to write the functions as broadly as possible and some of
them undoubtedly will work.

I hope to get to the point where all leagues are supported.
