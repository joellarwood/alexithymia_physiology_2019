apa_f <- function(model, caption = NULL){
  anova(model, 3) %>% 
    broom::tidy() %>% 
    mutate(
      statistic = glue::glue("F({round(NumDF,2)}, {round(DenDF, 2)}) = {round(statistic, 2)}, p = {round(p.value, 3)}")
    ) %>% 
    relocate(term, statistic, everything()) %>% 
    kableExtra::kable(caption = caption) %>% 
    kableExtra::kable_styling()
}


apa_post_hoc <- function(emmeans, caption = NULL){
  tmp.df <- data.frame(summary(
    pairs(
      emmeans,
      infer = TRUE
    )
  )
  )
  tmp.df %>% 
    mutate(
      effect = glue::glue(
        "est = {round(estimate, 3)}, p = {round(p.value, 2)}, 95% CI [{round(asymp.LCL, 2)}, {round(asymp.UCL, 2)}]")
    ) %>% 
    relocate(contrast, effect, everything()) %>% 
    kableExtra::kable(caption = caption) %>% 
    kableExtra::kable_styling()
}

