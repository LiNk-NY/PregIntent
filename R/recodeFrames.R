rbind.data.frame(
data.frame(
    recodename = rep("pregPlan", 6),
    category = c("can be planned in advance",
    "can be planned in discussion with your partner",
    "can be planned to happen after one's ideal criteria are fulfilled",
    "'just happens'",
    "can be left to 'fate' or a higher power like God",
    "is a natural process that happens when it’s meant to be"),
    value = c(rep("Yes", 4), rep("No", 2)), stringsAsFactors = FALSE),

data.frame(
    recodename = rep("becomeControl", 2),
    category = c("no control", "a little control",
        "complete control", "a lot of control"),
    value = c(rep("Low control", 2), rep("High control", 2)),
    stringsAsFactors = FALSE),

data.frame(
    recodename = rep("avoidControl", 2),
    category = c("no control", "a little control",
        "complete control", "a lot of control"),
    value = c(rep("Low control", 2), rep("High control", 2)),
    stringsAsFactors = FALSE)
)
