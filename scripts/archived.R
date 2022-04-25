#### MAKING PLOT LIST ####
make_plot_list <- function(df,vars,plot_formula) {
  #' create a list of plots from a df, using specified columns and plot function
  #' @param df the input dataframe
  #' @param vars vector of column names
  #' @param plot_formula the plotting formulate constructed using ggplot
  require(purrr)
  require(dplyr)
  cols <- df %>% select(all_of(vars)) %>% colnames()
  out <- cols %>% purrr::map(plot_formula)
  names(out) <- cols
  return(out)
}

plot_hist <- ~ggplot(df) +
  aes(.data[[.x]]) +
  geom_histogram(bins=50) +
  ylab("")+
  theme_linedraw()

ls_hist <- make_plot_list(df,setdiff(cols_nume,"response"),plot_hist)

make_plot_grid <- function(plots,title){
  plots <- append(plots,list(plot_spacer(),plot_spacer()),length(plots)-3)
  wrap_plots(plots,nrow=8,ncol=6,byrow=F) + plot_annotation(title=title)
}

make_plot_grid(ls_hist,"Histogram of input variables")


```{r out.width=500, fig.width=5, fig.asp=0.65, warning=FALSE}
df %>% select(region, customer, binary_outcome) %>%
  group_by(region, customer) %>%
  summarise(.groups="drop", event_prob=mean(binary_outcome)) %>%
  ggplot(aes(x=customer, y=region)) +
  geom_count(aes(size=event_prob, color=event_prob),shape="square") +
  geom_text(aes(label=round(event_prob,2)), nudge_y=0.1) +
  guides(color = "legend")+
  theme_linedraw()
```


