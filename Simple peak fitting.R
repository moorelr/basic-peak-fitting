# ---------------------------------------
# Initialization
# ---------------------------------------

# Function to make a line
line <- function(x, m, b){
  # Note: 
  #   x = a vector of x-axis values
  #   m = the slope
  #   b = the intercept
  #   the function returns the y-values so if you plot the input x values against
  #      the y values, you get a line.  This is used in the peak-fitting part
  #      below to calculate the background.
  
  return((m*x)+b)
}

# Function to make a peak
peak <- function(x, center, imax, beta.sq){
  # Note:
  #   x = the x values
  #   center = the center peak position (i.e. somewhere located on the x-axis
  #      presumably, but not necessarily, between the lowest and highest x value)
  #   imax = the peak height
  #   beta.sq = basically the peak width
  
  imax*exp(-pi*(x-center)^2/beta.sq)
}


# ---------------------------------------
# Synthetic data generation
# ---------------------------------------

# Generate a random synthetic spectrum
xs <- 1:100
ys <- line(x = xs, m = 0.12, b = 4)

# here's what it looks like before adding a peak...
plot(xs, ys, ylim = c(0, 100))

# ...and here's what it looks like after adding a peak.
peak_center = 43 # assigning this one to a variable for reasons that will make sense later
ys <- ys + peak(x = xs, center = peak_center, imax = 78, beta.sq = 50)
plot(xs, ys, ylim = c(0, 100))

# However, data are usually noisy, and we're used to looking at spectra plotted
#   as a bunch of line segments:
ys <- ys + rnorm(n = 100, mean = 0, sd = 5)
plot(xs, ys, type = "line")



# ---------------------------------------
# peak fitting
# ---------------------------------------

# Peak fitting can be done using the function nls(), which requires an initial guess
#   for the parameters of the model being fit to the data.  In this case, the model
#   is the sum of a peak and a line.  In this case, we know what the correct values
#   are, but for this example, I will pretend that we don't know.

# Here is an initial guess for the parameters comprising the line:
m.i <- 0.1 # It looks like the background is positively sloped, going from the two
           #   points (0, 0) to (100, 10)
b.i <- 0   # Looks like the Y intercept is about 0

# Here is an initial guess for the parameters comprising the peak
center.i <- 40  # Looks like the peak is centered at about x = 40
imax.i <- 80    # Looks like the peak has a height of about 80 above the baseline
beta.sq.i <- 10 # I have no idea what is appropriate here, so I will guess 10

# Note that we don't need to guess the xs because that is the independent variable.

# the next step is to plot the initial guess and see if it looks close to the data:
ys_guess <- line(x = xs, m = m.i, b = b.i) + peak(x = xs, center = center.i, imax = imax.i, beta.sq = beta.sq.i)
lines(xs, ys_guess, col = "blue")

# That looks like a pretty good guess, so the next step is to fit the model:
peak.fit <- nls(ys ~ line(xs, m.fit, b.fit)
                + peak(xs, center.fit, imax.fit, beta.sq.fit)
                , start = c(
                  m.fit = m.i, b.fit = b.i
                  , center.fit = center.i, imax.fit = imax.i, beta.sq.fit = beta.sq.i 
                )
)

# the summary() function returns the fit background and peak parameters as well as their
#   respective uncertainties and significance levels
summary(peak.fit)


# ---------------------------------------
# Checking that the fit seems ok
# ---------------------------------------

# Since we fit the model to a synthetic spectrum that we generated ourselves, we know what
#  these values are supposed to be.  For example...
print(paste("Fit peak position =", summary(peak.fit)$coefficients[3, 1]
            , "; correct peak position =", peak_center)) # (That's why I made this variable earlier)

# However, if we didn't know the correct value, we can also check the results graphically:
# First, assign the model fit parameters to some variables...
m.fit <- summary(peak.fit)$coefficients[1, 1]
b.fit <- summary(peak.fit)$coefficients[2, 1]
center.fit <- summary(peak.fit)$coefficients[3, 1]
imax.fit <- summary(peak.fit)$coefficients[4, 1]
beta.sq.fit <- summary(peak.fit)$coefficients[5, 1]

# Then, calculate the y-values using those variables
ys_fit <- line(x = xs, m = m.fit, b = b.fit) + peak(x = xs, center = center.fit, imax = imax.fit, beta.sq = beta.sq.fit)

# Finally, plot the line
lines(xs, ys_fit, col = "red")

# Looks pretty good to me!