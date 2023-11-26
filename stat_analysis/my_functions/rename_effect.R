library(dplyr)

rename_effect <- function(effect) {
  dplyr::case_when(
    effect == "subsMemory1" ~ "main effect memory",
    effect == "emotionnegative" ~ "main effect emotion",
    effect == "run_centered" ~ "main effect run",
    effect == "emotionnegative:run_centered" | 
      effect == "run_centered:emotionnegative" ~ 
      "run × emotion",
    effect == "subsMemory1:emotionnegative" ~ 
      "memory × emotion",
    effect == "subsMemory1:run_centered" | 
      effect == "run_centered:subsMemory1" ~ 
      "memory × run",
    effect == "subsMemory1:emotionnegative:run_centered" | 
      effect == "run_centered:emotionnegative:subsMemory1" ~ 
      "memory × run × emotion",
    
    effect == "subsMemory" ~ "main effect memory",
    effect == "emotion" ~ "main effect emotion",
    effect == "emotion:run_centered" ~ "run × emotion",
    
    effect == "subsMemory:emotion" | effect == "emotionnegative:subsMemory1" ~ "memory × emotion",
    effect == "subsMemory:run_centered" ~ "memory × run",
    effect == "subsMemory:emotion:run_centered" ~ "memory × run × emotion",
    effect == "subsMemory:emotion" ~  "memory × emotion",
    effect == "emotion_centered1" ~ "emotion",

    TRUE ~ effect # Keeps other values the same
  )
}