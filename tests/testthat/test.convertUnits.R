

context('Test convertUnits')

test_that('We get errors for invalid inputs', {

    expect_error(convertUnits("line", 0.5, "data"))
    expect_error(convertUnits("line", 0.5, "data", NULL))
    expect_error(convertUnits("line", 0.5, "data", axis = 'x'))
    expect_error(convertUnits("line", NULL, "data"))
    
    expect_error(convertUnits("line", 0.5, "proportion"))
    expect_error(convertUnits("line", 0.5, "proportion", NULL))
    expect_error(convertUnits("line", 0.5, "proportion", 0))
    expect_error(convertUnits("line", 0.5, "proportion", 1:2))
    expect_error(convertUnits("line", 0.5, "proportion", axis = 'x'))
    expect_error(convertUnits("line", 0.5, "proportion", side = 1, region = 'asdf'))
    expect_error(convertUnits("line", NULL, "proportion", side = 1))
    
    expect_error(convertUnits("data", 0.5, "line"))
    expect_error(convertUnits("data", 0.5, "line", NULL))
    expect_error(convertUnits("data", 0.5, "line", 0))
    expect_error(convertUnits("data", 0.5, "line", 1:2))
    expect_error(convertUnits("data", 0.5, "line", axis = 'x'))
    expect_error(convertUnits("data", NULL, "line", 1))
    
    expect_error(convertUnits("data", 0.5, "proportion"))
    expect_error(convertUnits("data", 0.5, "proportion", axis = NULL))
    expect_error(convertUnits("data", 0.5, "proportion", axis = 'asdf'))
    expect_error(convertUnits("data", 0.5, "proportion", axis = c('x', 'y')))
    expect_error(convertUnits("data", 0.5, "proportion", axis = 'x', region = 'asdf'))
    expect_error(convertUnits("data", NULL, "proportion", axis = 'x'))
    
    expect_error(convertUnits("proportion", 0.5, "line"))
    expect_error(convertUnits("proportion", 0.5, "line", NULL))
    expect_error(convertUnits("proportion", 0.5, "line", 0))
    expect_error(convertUnits("proportion", 0.5, "line", 1:2))
    expect_error(convertUnits("proportion", 0.5, "line", axis = 'x'))
    expect_error(convertUnits("proportion", 0.5, "line", side = 1, region = 'asdf'))
    expect_error(convertUnits("proportion", NULL, "line", side = 1))
    
    expect_error(convertUnits("proportion", 0.5, "data"))
    expect_error(convertUnits("proportion", 0.5, "data", NULL))
    expect_error(convertUnits("proportion", 0.5, "data", 0))
    expect_error(convertUnits("proportion", 0.5, "data", 1:2))
    expect_error(convertUnits("proportion", 0.5, "data", side = 1))
    expect_error(convertUnits("proportion", 0.5, "data", side = 1, region = 'asdf'))
    expect_error(convertUnits("proportion", NULL, "data", side = 1))
    
})
