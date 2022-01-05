
<!-- README.md is generated from README.Rmd. Please edit that file -->

# YFAR

A collection of functions for retrieving fantasy hockey league data from
the Yahoo Fantasy API.

<!-- badges: start -->

<!-- badges: end -->

## Intro

The functions in this package can retrieve data from public leagues and
the private leagues participated in by the current user logged into
Yahoo. You will notice this in some of the uri’s as `use_login=1`.

After creating a token and retrieving some simple input data you can use
the functions to return data from your league and teams.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/macraesdirtysocks/YFAR")
```

## The Basics

  - At its core YFAR has 3 types of functions:
      - **Authentication**
      - **Auxiliary data functions**
          - Uninteresting but useful. Mostly for acquiring data to
            supply as arguments to other functions i.e. weeks, dates,
            team ids, league ids, etc.
      - **Mains**
          - Probably why you are here. Rosters, stats, standings,
            matchups etc.

### Authentication

The Yahoo Fantasy API uses Oauth2.0 for authentication. To use the
functions in this package you will need a client id and client secret
obtained by registering an app with the [Yahoo Developer
Network](https://developer.yahoo.com/apps/create/).

After creating and registering your app you should receive a client id
and client secret.

Pass the client id and secret as arguments to `YFAR::y_create_token()`
which are passed to [httr](https://httr.r-lib.org/) to create an access
token.

This token is an argument to all functions in this package.

`my_token <- y_create_token(my_key, my_secret)`

### Auxiliary data functions

There a few of these but the most noteworthy is `y_games()`. This
function will return a tibble containing information on all your fantasy
leagues which can be used as arguments for the more interesting
functions.

`my_games <- y_games(my_token)`

### Mains

> Some functions accept a team\_id or league\_id. Any function with an
> `id` argument as opposed to the more explicit league\_id or team\_id
> will accept either or.

A few examples

Get League Rosters

    rosters <- y_rosters(id, my_token)

Get Draft Results

    y_draft_results <- function(id, token_name)

And much more. See the vignette for more info.

## Other notes

If you have suggestions or want to colab shoot me an email.

In order to keep the scope of this project manageable and myself
motivated by interest I’ve restricted the scope to hockey. However I
have tried to write the functions as broadly as possible and some of
them undoubtedly will work.

I hope to get to the point where all type of leagues are supported.
