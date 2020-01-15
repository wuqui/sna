# packages
library(stringr)

# variables
lemma <- "climate crisis"
start <- "2000-01-01"
end <- "2019-12-31"

cmd_twint <- str_c("twint -s '", lemma, "' --since ", start, " --until ", end, " -o", " '", lemma, "_", start, "_", end, ".csv'", " --csv --hashtags --count -l en")

cmd_notif <- " ; echo '...' | mail -s 'twint finished' q.wuerschinger@gmail.com"


# copy to clipboard ----
cmd <- str_c(cmd_twint, cmd_notif)
pb <- pipe("pbcopy", "w")
write(cmd, file=pb)
close(pb)
