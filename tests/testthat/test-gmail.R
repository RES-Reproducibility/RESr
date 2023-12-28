test_that("creating draft works", {
    x = RESr:::ej_randr("florian","Oswald",
                         "florian.oswald@gmail.com","1234",
                         "cool apper","dropboxlink",1)
    expect_true(typeof(x$id) == "character")
    expect_true(nchar(x$id) > 2)
})


test_that("sending test message works",{
    x = RESr:::ej_testmail()
    r = gmailr::gm_message(x$id)
    expect_true(x$id == r$id)
})
