vote.df <- data.frame(
  orig = mz.rf$y,
  mz.rf$votes
)
sort.order <- do.call(order, lapply(c(1:ncol(vote.df)), function(i) vote.df[, i]))
vote.df <- vote.df[sort.order, ]
vote.df$id <- 1:nrow(vote.df)
vote.df <- melt(vote.df, id.vars = c("id", "orig"), variable.name = "pred")


ggplot(to.plot, aes(x = id, y = value)) + geom_area(aes(fill = Species)) +
  scale_fill_brewer(palette = "Accent") +
  facet_wrap(~ orig.spp, scales = "free_x") + scale_x_continuous("Individual") +
  scale_y_continuous("Membership Probability") +
  ggtitle("Unknown - Multiple Events") +
  theme(
    text = element_text(size = 16), legend.text = element_text(face = "italic"),
    axis.text.x = element_blank(), axis.ticks.x = element_blank()
  )

ggplot(singles.df, aes(x = orig.spp, y = value)) +
  geom_bar(aes(fill = Species), stat = "identity") + coord_flip() +
  scale_fill_brewer(palette = "Accent") + scale_x_discrete("Species Code") +
  scale_y_continuous("Membership Probability") +
  ggtitle("Unknown - Single Event") +
  theme(
    text = element_text(size = 16), legend.text = element_text(face = "italic"),
    axis.text.x = element_blank(), axis.ticks.x = element_blank()
  )
