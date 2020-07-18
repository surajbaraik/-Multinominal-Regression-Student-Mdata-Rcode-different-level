#### MULTINOMINAL REGRESSION ASSIGNMENT



###load in the data
mdata <- read.csv(file.choose())

### Format categorical variables
### The first, prog is a categorical variable indicating what type of program a student is in: "General" (1), "Academic" (2), or "Vocational" (3).
### The ses variable is a categorical variable indicating someone's socioeconomic class: "Low" (1), "Middle" (2), and "High" (3)
mdata$prog <- factor(mdata$prog)
mdata$ses <- factor(mdata$ses)


### So first,we have to set base group. 
## do this by using the relevel() function and the ref argument, which stands for "reference group."

# Set the reference group for prog to be 1
mdata$prog <- relevel(mdata$prog, ref=1)



### Now, we can go ahead and run our model using the multinom() command, which is in the nnet package.

# Load the package
library(nnet)
# Run the model
model <- multinom(prog ~ ses + write, data=mdata)

summary(model)

coefs <- coef(model)

summary(model)$standard.errors

# Calculate z-values
zvalues <- summary(model)$coefficients / summary(model)$standard.errors
# Show z-values
zvalues


#### we use the pnorm() function instead of the pt() function because the z-value is related to a normal distribution, not a t distribution.

pnorm(abs(zvalues), lower.tail=FALSE)*2
