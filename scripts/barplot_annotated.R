library(ggplot2)

lbl <- count(diamonds, cut)

ggplot(data = diamonds) + 
  geom_bar(aes(x = cut)) + 
  geom_text(data = lbl,
            aes(x = cut, y = n),
            label = lbl$n,
            nudge_y = 800)

cor(select(diamonds, -color, -cut, -clarity))

cor(diamonds$price, diamonds$carat)

summarize(diamonds, cor_pc = cor(price, carat))

ggplot(diamonds, aes(x = price, y = carat)) +
  geom_point() + 
  facet_wrap(~cut)
