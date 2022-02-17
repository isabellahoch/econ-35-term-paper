# Term Paper

set.seed(012345678) # set seed
N <- 1000 # number of elections; may vary geographically
beta <-0.4 # sensitivity of preferences to changes in public opinion regarding the Big Lie

standardize <- function(x){(x-min(x))/(max(x)-min(x))} # helper function to constrain values between 0 and 1

v_norm <- 0 + rnorm(N) # assumes each candidate's platform has about the same quality but with slight differences. median voter value is modeled by drawing from a standard normal distribution (mean 0, SD 1)

v <- standardize(v_norm) # makes all values of v be between 0 (denying Big Lie) and 1 (affirming Big Lie)

hist(v,
     main="Distribution of Median GOP Voter Stances on the Big Lie", 
     xlab="Median Voter Stance", 
     border="#7ab2bf", 
     col="#baebf7")

# We equate "marginal costs" with the existing political heavy-lifting of devising concrete stances on issues ranging from abortion rights to Build Back Better; we will assume for this model's sake that the two candidates have parallel "cost structures" around 0 because they both have the same public beliefs and campaign promises, but perhaps with slight differences accounted for by drawing from a normal distribution.
c_A <- 0 + rnorm(N)
c_B <- 0 + rnorm(N)

# Nash equilibrium
# calculates where public actions relating to Big Lie (whether it comes from answering questions in interviews, releasing documents, etc.) lies on a spectrum from -1 (Denying Big Lie) to 1 (Affirming Big Lie)
p_A <- function(v, c_A, c_B) {(v + 3 + 2*beta*c_A + beta*c_B)/(3*beta)}
p_B <- function(v, c_A, c_B) {(-v + 3 + 2*beta*c_B + beta*c_A)/(3*beta)}

data <- cbind(p_A(v, c_A, c_B), p_B(v, c_A, c_B),)
colnames(data) <- c("Candidate A", "Candidate B")

print(head(data,10))
summary(data)

# Point where a voter would be indifferent between Candidates A and B
x_A <- standardize((v - beta*p_A(v, c_A, c_B) + beta*p_B(v, c_A, c_B))/2)

plot(x_A, 
     (p_A(v, c_A, c_B) - p_B(v, c_A, c_B)),
     main="Differences of Outward Political Stances Regarding Big Lie",
     xlab="Point of Voter Indifference",
     ylab="Difference between Public Affirmation of Big Lie",
)

# as you can see, the differences tend to 0, with a few anomalies

# plots voter demand against relative social cost of voting for a particular candidate

# look into Hotelling's original paper discussing this very topic