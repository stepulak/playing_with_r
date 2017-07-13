# Useful functions which I've used frequently or which were worth to write down
# Doxygen doc, sorry for mistakes in english grammar

# @brief Create sequence of random numbers according to the gaussian distribution with default sd and mean
# @param len length of the sequence
# @return normalized rnorm sequence
normalized_rnorm <- function(len) {
	rand_norm <- rnorm(len)
	return(rand_norm / max(rand_norm))
}

# @brief Create sine-like fuzzy sequence with specific hum and range
# @param from interval beginning
# @param to interval end
# @param hum_q hum quotient of our "fuzziness"
# @return Sine fuzzy sequence
sine_fuzzy_seq <- function(from = 0, to = 4*pi, step = 0.01, hum_q = 1) {
	t <- seq(from, to, step)
	seq <- sin(t + hum_q * normalized_rnorm(length(t)))
	return(seq - min(seq))
}

# @brief Create generic fuzzy sequence with specific hum and range
# @param angle_from minimum (ascending/descending) angle between previous and new point and the horizontal axis
# @param angle_to maximum (ascending/descending) angle between previous and new point and the horizontal axis
# @param hum_q hum quotient of our "fuzziness"
# @param num_sections number of sections of our sequence, each section represents one straight line (before fuzzing)
# @param section_length length of each section
# @return Generic fuzzy sequence
generic_fuzzy_seq <- function(angle_from = -pi/2 + 0.1, angle_to = pi/2 + 0.1, hum_q = 5, num_sections = 10, section_length = 20) {
	# constants
	possible_angles = runif(100, angle_from, angle_to)
	
	# first, generate generic "trend" sequence
	curr_angle = sample(possible_angles, 1)
	inity <- 0
	values <- double(0) # here we are going to store our final values
	values_index <- 1
	
	for(curr_section in seq(1, num_sections)) {
		for (i in seq(0, section_length, 0.1)) {
			values[values_index] <- inity + i * sin(curr_angle)
			values_index <- values_index + 1
		}
		inity = values[length(values)]
		curr_angle = sample(possible_angles, 1)
	}
	
	# make it a little bit fuzzy
	seq <- values + hum_q * normalized_rnorm(length(values))
	
	return(seq - min(seq))
}

# @brief Linear filter which is based on moving average method	(used formula: 1/(2*a+1) * sum(X_t, from=-a, to=a))
# @param vec vector (sequence) of numbers to filter
# @param a interval constant
# @return filtered sequence
linear_filter <- function(vec, a) {
	values <- double(0) # return values
	values_index <- 1
	vec_len <- length(vec)
	i <- 1
	
	while (i <= vec_len) {
		ai <- 0
		s <- 0
		
		# sum of area from index: i to i + a
		while(ai < a && i <= vec_len) {
			s <- s + vec[i]
			ai <- ai + 1
			i <- i + 1
		}
		
		# we cannot use 1/(2*a+1) expression as I've stated before,
		# because we operate in range from zero to a-1, not from -a to a (including zero index)
		values[values_index] = s/a
		values_index <- values_index + 1
	}
	
	return(scale_vec(values, vec_len))
}

# @brief Ugly function which remaps (or scale) an old vector of values to new one with different length
# 	Values between points are approximated during the extension process
# @param vec vector (sequence) of numbers to scale
# @param new_len new length of your desired vector
# @return vector after scaling
scale_vec <- function(vec, new_len) {
	vec_len <- length(vec)
	vec_scaled <- double(0)
	scale <- new_len / vec_len
	
	# recopy the data
	for(i in seq(1, vec_len)) {
		vec_scaled[floor(i * scale)] = vec[i]
	}
	
	left_val <- vec_scaled[1]
	left_index <- 1
	
	# get rid of NA values 
	# if x is NA, then take closest left and right valid value and determine a new value for x
	# x = (distance_from_left * left_value + distance_from_right * right_value) / (distance_from_left + distance_from_right)
	for(i in seq(2, new_len)) {
		if (!is.na(vec_scaled[i])) {
			right_val <- vec_scaled[i]
			seq_len <- i - left_index
			
			for(j in seq(left_index, i-1)) {
				vec_scaled[j] = ((seq_len - (j - left_index)) * left_val + (j - left_index) * right_val) / seq_len 
			}
			
			left_val <- right_val
			left_index <- i
		}
	}
	
	return(vec_scaled)
}

# @brief Return positive or negative value from given range (remember, given range interval must be positive)
# @param from lower boundary (from N+)
# @param to upper boundary (from N+)
# @return random value
even_negative_random <- function(from = 0, to = 1) {
	return (runif(1, from, to) * (if(runif(1) < 0.5) -1 else 1))
}

# @brief Generate data for linear regression analysis (x and y values)
# @param num_samples number of data samples
# @param from X lower limit
# @param to X upper limit
# @param var variation between X and Y value
# @return Data for linear regression 
gen_data_linear_reg <- function(num_samples = 20, from = 100, to = 200, var = 30) {
	samples <- c(from:to)
	x <- sample(samples, num_samples, replace = TRUE)
	y <- integer(0)
	
	for (i in seq(1:num_samples)) {
		y[i] = x[i] + even_negative_random(-var, var)
	}
	
	return(data.frame(x = x, y = y))
}

# @brief Get linear function from given sequences of x and y prepared for regression analysis
# @param x sequence of x values
# @param y sequence of y values
# @return Function info (vertical offset and coefficient) for linear regression
linear_regression <- function(x, y) {
	avg_x <- mean(x)
	avg_y <- mean(y)
	b1 <- cov(x, y) / var(x)
	b0 <- avg_y - b1 * avg_x
	return(data.frame(b1 = b1, b0 = b0, yoffset = b0, coefficient = b1))
}

# @brief Generate random data using gen_data_linear_reg(), analyse them using linear_regression() and create a plot
plot_random_data_lin_reg <- function() {
	data <- gen_data_linear_reg(num_samples = 50, var = 50)
	func <- linear_regression(data$x, data$y)
	
	plot(data)
	
	xmin <- min(data$x)
	xmax <- max(data$x)
	ymin <- func$yoffset + func$coefficient * xmin
	ymax <- func$yoffset + func$coefficient * xmax
	
	lines(x = c(xmin, xmax), y = c(ymin, ymax))
}

# @brief My own covariance
# @param x sequence of x values (same length as y)
# @param y sequence of y values (same length as x)
# @return covariance between these values
my_cov <- function(x, y) {
	if(length(x) != length(y)) {
		print("different length x, y")
		return(Inf);
	}
	
	avg_x <- mean(x)
	avg_y <- mean(y)
	
	index <- 1
	max_index <- length(x)
	sum <- 0
	
	while(index <= max_index) {
		sum <- sum + (x[index] - avg_x) * (y[index] - avg_y)
		index <- index + 1
	}
	
	return (sum / (max_index - 1))
}

# @brief My own variance
# @param x sequence of x values
# @return variance between these values
my_var <- function(x) {
	avg_x <- mean(x)
	sum <- 0
	
	for(ax in x) {
		sum <- sum + (ax - avg_x)^2
	}
	
	return(sum / (length(x) - 1))
}