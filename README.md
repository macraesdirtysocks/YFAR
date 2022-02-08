
<!-- README.md is generated from README.Rmd. Please edit that file -->

# YFAR

A collection of functions to get data from the Yahoo\! Fantasy API.

<!-- badges: start -->

<!-- badges: end -->

## Introduction

The functions in this package can retrieve data from public leagues and
the private leagues participated in by the current user logged into
Yahoo.

The Yahoo Fantasy API uses Oauth2.0 for authentication. To use the
functions in this package you will need a client id and client secret
obtained by registering an app with the [Yahoo Developer
Network](https://developer.yahoo.com/apps/create/).

After creating a token and retrieving some simple input data you can use
the functions to return data from games, leagues and teams.

  - There are no extensive instructions in this package for registering
    an app with the Yahoo Developer Network but here are some resources
    :
    
      - Edward Distel’s Github [Yahoo Fantasy Baseball API
        Reader](https://github.com/edwarddistel/yahoo-fantasy-baseball-reader)
      - Maëlle Salmon’s Blog [How to deal with OAuth2.0 in R
        packages?](https://blog.r-hub.io/2021/01/25/oauth-2.0/)
      - YDN [Yahoo Fanatsy Developer
        Website](https://developer.yahoo.com/fantasysports/guide/)

  - Package highlights:
    
      - 18 functions to get data from various resource of the API.
      - Notable functions include `y_rosters()`, `y_players()`,
        `y_draft_results()`, `y_draft_adp()`.
      - Return data from both current and past leagues.
      - Functions by default return tibbles with an option to return a
        list.
      - Functions are written using the tidyverse.

  - Function mechanics in a nutshell:
    
      - Takes the keys and other provided arguments to generate a uri.
      - Using the internal function `.y_get_request()` sends a get
        request to the yahoo api.
      - The api returns a response which containts fantasy content in
        JSON format.
      - JSON fantasy content is parsed using internal functions heavily
        reliant on the [purrr package](https://purrr.tidyverse.org/).
      - Return a tibble.
      - Option to return a list which includes uri, response, content
        and the api resource.

## Installation

    # install.packages("devtools")
    devtools::install_github("https://github.com/macraesdirtysocks/YFAR")

## The Basics

  - At its core YFAR has 3 types of functions:
      - **Authentication**
      - **Accessory Functions**
          - Uninteresting but useful. Mostly for acquiring data to
            supply as arguments to other functions i.e. weeks, dates,
            team and league keys, etc.
      - **The Beef**
          - Probably why you are here. Rosters, stats, standings,
            match-ups etc.

### Authentication

After creating and registering your app you should receive a client id
and client secret.

Pass the client id and secret as arguments to `YFAR::y_create_token()`
which are passed to [httr](https://httr.r-lib.org/) to create an access
token.

This token is an argument to all functions in this package.

    my_token <- y_create_token(my_key, my_secret, app_name)

### Auxiliary data functions

There a few of these but the most noteworthy is `y_games()`. This
function will return a tibble containing information on all your fantasy
leagues which can be used as arguments for the more interesting
functions.

    y_games(my_token)

### Mains

> Some functions accept a team\_key or league\_key. Any function with an
> key argument as opposed to the more explicit league\_key or team\_key
> can accept a varitey of keys.

  - A few examples
    
      - Get League Rosters
        
        `rosters <- y_rosters(key, my_token)`
    
      - Get Draft Results
        
        `y_draft_results <- function(key, token_name)`

And much more. See the vignette for more info.

## Other notes

If you have suggestions or want to colab shoot me an email.

In order to keep the scope of this project manageable and myself
motivated by interest I’ve restricted the scope to hockey. However I
have tried to write the functions as broadly as possible and some of
them undoubtedly will work.

I hope to get to the point where all type of leagues are supported.
