## Create and cache an input matrix and its inverse, if inverse already calc.

## makeCacheMatrix is based on the assignment example function makeVector.

makeCacheMatrix <- function(input_matrix) {
        
        inverse_matrix <- NULL ## acts as flag that inverse needs calculating
        
        set_input <- function(y) {
                input_matrix <<- y ## cache to parent input_matrix
                inverse_matrix <<- NULL ## cache to parent calc. inverse flag
        }
        
        ## store in current environment input_matrix
        get_input <- function() input_matrix
        
        ## cache to parent the solved inverse
        set_inverse <- function(inverse_sol) inverse_matrix <<- inverse_sol
        
        ## store in current environment the solved inverse
        get_inverse <- function() inverse_matrix
        
        ## create a 'special matrix' list of the above gets and sets
        list (set_input = set_input, get_input = get_input,
              set_inverse = set_inverse,
              get_inverse = get_inverse)
}

## Calculate and cache the inverse of some input matrix, where 
## input %*% inverse = identity. This is done using solve(input), which returns
## the solution to the preceding equation.

## However, if the inverse is already in cache, no calculation is performed and
## the inverse is returned.

## cacheSolve is based on the assignment example cachemean

cacheSolve <- function(input_matrix_list, ...) {
        
        # grabs either the inverse solution or NULL, NULL flagging calc. needed
        inverse_matrix <- input_matrix_list$get_inverse()
        
        # if inverse_matrix not NULL, no calc. needed
        if(!is.null(inverse_matrix)) {
                return(inverse_matrix)
        }
        
        # grab the input matrix
        matrix_data <- input_matrix_list$get_input()
        
        # solve for the inverse
        inverse_matrix <- solve(matrix_data)
        
        # cache the inverse
        input_matrix_list$set_inverse(inverse_matrix)
        
        # return the inverse
        inverse_matrix
}