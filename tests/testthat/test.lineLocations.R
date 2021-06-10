

context('Test lineLocations')

test_that('We get errors for invalid inputs', {
    
    expect_error(lineLocations())
    expect_error(lineLocations(NULL))
    expect_error(lineLocations(NULL, NULL))
    expect_error(lineLocations(1, NULL))
    expect_error(lineLocations(NULL, 1))
    expect_error(lineLocations(0, 1))
    expect_error(lineLocations(1, numeric(0)))
    expect_error(lineLocations(1:2, 1))
    
})
