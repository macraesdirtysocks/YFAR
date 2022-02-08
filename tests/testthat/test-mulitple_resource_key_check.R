library(YFAR)

test_that("key argument is checked properly and returns good keys",{

    expect_identical(
        .multiple_resource_key_check(c("411.l.1245", "411.l.1239", "411"), e_key_types = "leagues"),
         list(
         resource = "leagues",
         key = c("411.l.1245", "411.l.1239"),
         invalid_key = c("411")
         )

    )

    expect_identical(
        .multiple_resource_key_check(c("411.l.1245", "411.l.1239", "411", "406", "407"), e_key_types = "games"),
        list(
            resource = "games",
            key = c("411", "406", "407"),
            invalid_key = c("411.l.1245", "411.l.1239")
        )

    )

    expect_identical(
        .multiple_resource_key_check(c("411.l.1239", "411.p.7109", "411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122"), e_key_types = "players"),
        list(
            resource= "players",
            key = c("411.p.7109", "411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122"),
            invalid_key = c("411.l.1239")
        )
    )

    expect_identical(
        .multiple_resource_key_check(c("411.l.1239", "411p.7109", "411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122"), e_key_types = "players"),
        list(
            resource= "players",
            key = c("411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122"),
            invalid_key = c("411.l.1239", "411p.7109")
        )
    )

    expect_identical(
        .multiple_resource_key_check(c("411.l.1239", "411p.7109", "411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122", "411p.7109"), e_key_types = "players"),
        list(
            resource= "players",
            key = c("411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122"),
            invalid_key = c("411.l.1239", "411p.7109")
        )
    )

    expect_identical(
        .multiple_resource_key_check(c("411.l.1239", "411p.7109", "411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122", "411p.7109", "411.p.7122"), e_key_types = "players"),
        list(
            resource= "players",
            key = c("411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122"),
            invalid_key = c("411.l.1239", "411p.7109")
        )
    )

    # Test that even though player keys are numerous leagues keys win out because e_key_types is set to "leagues".
    expect_identical(
        .multiple_resource_key_check(c("411.l.1239", "411p.7109", "411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122", "411p.7109", "411.p.7122"), e_key_types = "leagues"),
        list(
            resource= "leagues",
            key = c("411.l.1239"),
            invalid_key = c("411p.7109", "411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122")
        )
    )

    # Test that even though player keys are numerous leagues keys win out because e_key_types is set to "leagues".
    expect_identical(
        .multiple_resource_key_check(c("411.l.1239", "411p.7109", "411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122", "411p.7109", "411.p.7122"), e_key_types = c("leagues", "players")),
        list(
            resource= "players",
            key = c("411.p.7534", "411.p.4351", "411.p.7518", "411.p.7122"),
            invalid_key = c("411.l.1239", "411p.7109")
        )
    )


})
