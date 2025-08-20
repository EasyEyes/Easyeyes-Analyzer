calculate_anova <- function(df_list){
  crowding <- df_list$crowding %>%
    mutate(measure = log_crowding_distance_deg) %>%  
    group_by(participant, font) %>% 
    summarize(measure = mean(measure, rm.na=T),.groups = "drop")
  
  rsvp <- df_list$rsvp %>%
    mutate(measure = block_avg_log_WPM) %>%  
    group_by(participant, font) %>% 
    summarize(measure = mean(measure, rm.na=T),.groups = "drop")
  
  reading <- df_list$reading %>%
    filter(is.finite(log_WPM)) %>% 
    mutate(measure = log_WPM) %>%  
    group_by(participant, font) %>% 
    summarize(measure = mean(measure, rm.na=T),.groups = "drop")
  print('inside calculate_anova')
  print(reading)
  comfort <- df_list$QA %>%
    filter(grepl('CMFRT', questionAndAnswerNickname)) %>%
    mutate(measure = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.otf",
                            TRUE ~ questionAndAnswerNickname  # fallback for any unmatched cases
           )) %>%
    filter(!is.na(measure)) %>%
    group_by(participant, font) %>% 
    summarize(measure = mean(measure), .groups = "drop")
  
  beauty <- df_list$QA %>%
    filter(grepl('bty', tolower(questionAndAnswerNickname))) %>%
    mutate(measure = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.otf",
                            TRUE ~ conditionName  # fallback for any unmatched cases
           )) %>% 
    filter(!is.na(measure)) %>%
    group_by(participant, font) %>% 
    summarize(measure = mean(measure), .groups = "drop")
  
  # Define font order for consistent factor levels
  font_order <- c(
    "Al-Awwal-Regular.ttf",
    "majalla.ttf", 
    "Saudi-Regular.ttf",
    "SaudiTextv1-Regular.otf",
    "SaudiTextv2-Regular.otf", 
    "SaudiTextv3-Regular.otf"
  )
  
  # Function to perform ANOVA safely
  perform_anova <- function(data) {
    if (nrow(data) == 0) {
      return(list(
        anova_result = NULL,
        summary = "No data available"
      ))
    }
  
    data$font <- factor(data$font, levels = font_order)
    
    data <- data %>% filter(!is.na(font))
    
    # Check if we have enough data for ANOVA
    font_counts <- table(data$font)
    fonts_with_data <- sum(font_counts > 0)
    
    if (fonts_with_data < 2) {
      return(list(
        anova_result = NULL,
        summary = paste("Insufficient data: only", fonts_with_data, "font(s) with data")
      ))
    }
    
    # Perform ANOVA
    tryCatch({
      anova_result <- aov(measure ~ font, data = data)
      anova_summary <- summary(anova_result)
      
      # Perform pairwise t-tests with Bonferroni correction
      pairwise_result <- pairwise.t.test(data$measure, data$font, p.adjust.method = "bonferroni")
      
      return(list(
        anova_result = anova_result,
        summary = anova_summary,
        pairwise = pairwise_result
      ))
    }, error = function(e) {
      return(list(
        anova_result = NULL,
        summary = paste("ANOVA error:", e$message),
        pairwise = NULL
      ))
    })
  }
  
  # Function to print results nicely
  print_results <- function(result, measure_name) {
    cat("\n=== ", toupper(measure_name), " ANALYSIS ===\n")
    
    if (!is.null(result$anova_result)) {
      cat("\n--- ANOVA ---\n")
      print(result$summary)
      
      cat("\n--- PAIRWISE COMPARISONS (Bonferroni corrected p-values) ---\n")
      print(result$pairwise)
      cat("\n")
    } else {
      cat("Error:", result$summary, "\n")
    }
  }
  
  # Perform ANOVA for each measure
  results <- list(
    reading = perform_anova(reading),
    crowding = perform_anova(crowding),
    rsvp = perform_anova(rsvp),
    beauty = perform_anova(beauty),
    comfort = perform_anova(comfort)
  )
  
  # Print all results
  print_results(results$reading, "reading")
  print_results(results$crowding, "crowding") 
  print_results(results$rsvp, "rsvp")
  print_results(results$beauty, "beauty")
  print_results(results$comfort, "comfort")
  
  return(results)
}