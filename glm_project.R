# Reading data 
Gender <- rep(c(1,0) , each = 16)
Severe <- rep(c(1,0), each = 8, times = 2)
Information <- rep(c(1,0), each = 4, times = 4)
Age <- rep(1:4, times = 8)
n <- c(18, 23, 22, 17, 19, 35, 30, 22, 24, 37, 29, 24, 
       28, 42, 37, 30, 11, 25, 12, 8, 14, 34, 22, 21, 
       18, 28, 24, 8, 28, 47, 45, 30)
Behaviour <- c(11, 14, 11, 5, 4, 15, 8, 8, 10, 13, 8, 6,
               11, 14, 15, 9, 6, 13, 7, 8, 7, 15, 8, 5, 
               12, 15, 7, 1, 13, 21, 11, 6)

# Preliminary Analyses
Prop <- Behaviour/n
par(mfrow = c(2,2))
plot(Gender, Prop)
# Females have more changed behaviour related to hygiene 
# than males
plot(Severe, Prop)
# People who believe that the consequences of contracting swine flu 
# were severe have higher change in behaviour related to hygiene
plot(Information, Prop)
# People who believe that the information available about swine flu 
# was adequate have higher change in behaviour related to hygiene
plot(Age, Prop)
# Change in behaviour related to hygiene decreases as the age increases

# Logistic Regression Model