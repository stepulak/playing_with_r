normalized_rnorm <- function(len) {
	rand_norm <- rnorm(len)
	return(rand_norm / max(rand_norm))
}

# sine-like function with hum
sine_fuzzy_seq <- function(from = 0, to = 4*pi, step = 0.01, hum_q = 1) {
	t <- seq(from, to, step)
	seq <- sin(t + hum_q * normalized_rnorm(length(t)))
	return(seq - min(seq))
}

# generic function with hum
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

# ugly linear filter (moving average filter)
linear_filter <- function(vec, a) {
	# my own implementation of linear filter
	# filter(vec, rep(1/(0.5*a), a))
	# in other words: 1/(2*a+1) * sum(X_t, from=-a, to=a)
	
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
		# because we are operating in range from zero to a-1, not from -a to a (including zero index)
		values[values_index] = s/a
		values_index <- values_index + 1
	}
	
	return(scale_vec(values, vec_len))
}

# ugly scale function
# this function remaps vector (or scale, extend) to new one with different vector length
# data values are preserved and approximated during the scaling process
# FYI something similar (but not quite the same) you can achieve with ts() function
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
	# -> if x is NA, then take closest left and right valid value
	# -> new value for x = (distance_from_left * left_value + distance_from_right * right_value) / (distance_from_left + distance_from_right)
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

