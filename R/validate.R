## Code for checking data integrity

all.equal(sum(table(sexpref)), 2099)
all.equal(sum(table(gender)), 2099)

## Check if any values are code 53 (do not reside in US) or 40 (Puerto Rico)
any(pregint$Q110 %in% c(53, 40))
