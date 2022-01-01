httptest::set_requester(
    function(response){
        httptest::gsub_request(response, "https\\://fantasysports.yahooapis.com/fantasy/v2/", "api/")
    })
