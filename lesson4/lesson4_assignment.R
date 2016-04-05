pf <- read.csv("pseudo_facebook.tsv", sep="\t")

ggplot(aes(x=age, y=friendships_initiated), data=pf) + geom_point()

ggplot(aes(x=age, y=friendships_initiated), data=pf) +
  geom_point(alpha=1/20)

ggplot(aes(x=age, y=friendships_initiated), data=pf) +
geom_point(alpha=1/20) + xlim(13,90)

ggplot(aes(x=age, y=friendships_initiated), data=pf) +
geom_jitter(alpha=1/20) + 
  xlim(13,90)

by(pf$friendships_initiated, pf$age, summary)