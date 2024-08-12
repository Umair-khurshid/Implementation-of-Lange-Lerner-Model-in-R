# Step 1: Define the parameters
num_goods <- 2
num_firms <- 2
num_households <- 2

# Step 2: Define the initial allocation
initial_resources <- c(100, 200)
initial_goods <- matrix(c(50, 30, 40, 60), ncol = num_goods)

# Step 3: Set up the optimization problem
objective_function <- function(prices) {
  # Calculate social welfare using prices and allocations
  welfare <- sum(prices * colSums(initial_goods))
  return(-welfare)  # Maximizing welfare, so negative sign
}
# Turn off Scientific Notation
options(scipen = 999) 

# Constraints: Production and resource constraints
constraint_function <- function(x) {
  # Extract production levels and resources from x
  production <- matrix(x[1:(num_firms * num_goods)], ncol = num_goods)
  resources <- x[(num_firms * num_goods + 1):(num_firms * num_goods + num_goods)]
  
  # Calculate resource constraints
  resource_constraints <- resources - colSums(production)
  
  # Return a vector of constraint violations
  return(c(resource_constraints))
}

# Step 4: Solve the optimization problem
initial_guess <- c(as.vector(initial_goods), initial_resources)
result <- optim(initial_guess, objective_function, 
                method = "Nelder-Mead", 
                control = list(fnscale = -1),  # Maximize objective function
                hessian = TRUE)

# Step 5: Update prices and allocations
final_goods <- matrix(result$par[1:(num_firms * num_goods)], ncol = num_goods)
final_resources <- result$par[(num_firms * num_goods + 1):(num_firms * num_goods + num_goods)]

# Assuming prices should be derived from allocations
prices <- colSums(final_goods) / sum(final_resources)

# Step 6: Repeat steps 4 and 5 (optional)

# Step 7: Analyze results
cat("Final prices:", prices, "\n")
cat("Final goods allocation:\n", final_goods, "\n")
cat("Final resources:\n", final_resources, "\n
