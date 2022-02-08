library(YFAR)

test_that("key argument is checked properly and returns good keys",{

    expect_identical(
        .single_resource_key_check(c("411.l.1245", "411.l.1239", "411"), .league_key_check),
        c("411.l.1245", "411.l.1239")

    )

    expect_identical(
        .single_resource_key_check(c("411.p.6369", "411.p.3472", "411"), .player_key_check),
        c("411.p.6369", "411.p.3472")

    )

    expect_identical(
        .single_resource_key_check(c("411.p.6369", "411.p.3472", "411"), .game_key_check),
        c("411")
    )

    expect_identical(
        .single_resource_key_check(c("411.p.6369", "411.p.3472", "411", "411.l.1239.t.8", "411.p.3472", "411.l.1239.t.1"), .team_key_check),
        c("411.l.1239.t.8", "411.l.1239.t.1")
    )
})

