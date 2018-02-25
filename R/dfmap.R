dfmap <- rbind.data.frame(
data.frame(
    recodeName = rep("pregPlan", 6),
    response = c("can be planned in advance",
    "can be planned in discussion with your partner",
    "can be planned to happen after one's ideal criteria are fulfilled",
    "'just happens'",
    "can be left to 'fate' or a higher power like God",
    "is a natural process that happens when it’s meant to be"),
    recodeValue = c(rep("Yes", 4), rep("No", 2)), stringsAsFactors = FALSE),

data.frame(
    recodeName = rep("becomeControl", 2),
    response = c("no control", "a little control",
        "complete control", "a lot of control"),
    recodeValue = c(rep("Low control", 2), rep("High control", 2)),
    stringsAsFactors = FALSE),

data.frame(
    recodeName = rep("avoidControl", 2),
    response = c("no control", "a little control",
        "complete control", "a lot of control"),
    recodeValue = c(rep("Low control", 2), rep("High control", 2)),
    stringsAsFactors = FALSE),

data.frame(recodeName = rep("regionOrg", 53),
response = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "District of Columbia",
    "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
    "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
    "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island",
    "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
    "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin",
    "Wyoming", "I do not reside in the United States"),
recodeValue = c("South",  "West", "West", "South", "West", "West", "North East",
    "South", "South", "South", "South", "West", "West", "MidWest", "MidWest",
    "MidWest", "MidWest", "South", "South", "North East", "South",
    "North East", "MidWest", "MidWest", "South", "MidWest", "West",
    "MidWest", "West", "North East", "North East", "West", "North East",
    "South", "MidWest", "MidWest", "South", "West", "North East",
    NA, "North East", "South", "MidWest", "South", "South", "West",
    "North East", "South", "West", "South", "MidWest", "West", NA),
stringsAsFactors = FALSE)
)
