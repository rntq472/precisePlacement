

context('Test getBoundaries')

test_that('We get errors for invalid inputs', {
    
    expect_error(getBoundaries())
    expect_error(getBoundaries('asdf'))
    
})
