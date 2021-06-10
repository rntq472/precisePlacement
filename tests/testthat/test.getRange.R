

context('Test getRange')

test_that('We get errors for invalid inputs', {
    
    expect_error(getRange())
    expect_error(getRange('asdf'))
    expect_error(getRange('data'))
    expect_error(getRange('data', 'asdf'))
    
})
