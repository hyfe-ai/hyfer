hyfe_performance <- function(detections,
                             labels,
                             prediction_threshold = 0.7,
                             offset_cutoff = 5,
                             toplot = TRUE,
                             verbose=TRUE){

  #=============================================================================
  # For debugging only -- not run!

  if(FALSE){
    library(dplyr)

    # run the data read / cleaning code in analysis_2 in navarra_clinica / validation

    head(labels)
    head(detections)

    toplot = TRUE
    verbose = TRUE
    prediction_threshold <- 0.7
    offset_cutoff = 5

  }

  #=============================================================================

  # Apply prediction threshold
  detections$prediction <- FALSE
  detections$prediction[detections$prediction_score >= prediction_threshold] <- TRUE

  # Apply cough-second binning in labels!

  #=============================================================================

  aliases <- unique(detections$alias)
  for(alias_i in 1:length(aliases)){
    #alias_i <- 1
    aliasi <- aliases[alias_i] ; aliasi
    if(verbose){message('Alias  ',aliasi,' ... ')}

    # Filter data to this alias
    soundi <- detections[detections$alias == aliasi,]
    head(soundi)

    # Loop through each test
    tests <- labels$test %>% unique  ; tests
    for(tests_i in 1:length(tests)){
      #tests_i <- 1
      testi <- tests[tests_i] ; testi

      if(verbose){message('--- processing test ',testi,' ... ')}

      labeli <- labels[labels$test == testi,]

      # Synchronize times
      sound_test <- soundi
      offset_seconds <- synchronize(reference_times = labels$timestamp,
                                    reference_labels = labels$label,
                                    hyfe_times = sound_test$timestamp,
                                    hyfe_predictions = sound_test$prediction,
                                    verbose=FALSE)
      offset_seconds
      sound_test$timestamp <- sound_test$timestamp + offset_seconds[1]

      # Restrict sounds data to this test window
      sound_test <- sound_test %>% filter(timestamp >= (min(labeli$timestamp) - 60),
                                      timestamp <= (max(labeli$timestamp) + 60))

      # Add detection identifier
      sound_test$id <- 1:nrow(sound_test)

      # Loop through each confirmed sound in the labels table
      pr <- data.frame()
      j=10
      for(j in 1:nrow(labeli)){
        labelj <- labeli[j,]
        labelj

        # Stage results
        labelj$n_detections <- 0
        labelj$min_sep <- NA
        labelj$max_score <- NA
        labelj$match_ids <- NA

        # Determine time difference between this event and all Hyfe detections
        diffs <- labelj$timestamp - sound_test$timestamp

        # Ask whether any Hyfe detections occur within `offset_cutoff` seconds of the event
        possible_matches <- which(abs(diffs) <= offset_cutoff)

        # If at least one does, save results
        if(length(possible_matches)>0){
          labelj$n_detections <- length(possible_matches)
          labelj$min_sep <- min(abs(diffs))
          labelj$max_score <- max(sound_test$prediction_score[possible_matches])
          labelj$match_ids <- paste(sound_test$id[possible_matches],collapse="-")
        }
        labelj

        # Add to results
        pr <- rbind(pr, labelj)
      }
      pr

      # Add alias column
      pr$alias <- aliasi

      # Get list of sounds that were close to a confirmed sound
      ids <- paste(pr$match_ids,collapse="-") ; ids
      ids <- strsplit(ids,'-')[[1]]
      ids <- ids[ids != 'NA']
      ids <- as.numeric(ids)
      ids <- sort(ids)
      ids

      # Now check for false positives
      cough_ids <- sound_test$id[sound_test$prediction_score >= prediction_threshold]
      cough_ids
      fp <- cough_ids [ which(! cough_ids %in% ids) ]
      fp
      results_i <- pr
      if(length(fp)>0){
        for(fp_i in 1:length(fp)){
          #fp_i = 1
          fpi <- fp[fp_i]

          fpi_sound <- sound_test[fpi,] ; fpi_sound
          fpi_sound$timestamp
          fpi_diffs <- fpi_sound$timestamp - labeli$timestamp
          fpi_diffs
          labeli[which.min(abs(fpi_diffs)),]
          pr[which.min(abs(fpi_diffs)),]

          # Create a result row for this false positive
          fp_df_i <- pr[1,] ; fp_df_i
          fp_df_i$id <- NA
          fp_df_i$timestamp <- sound_test$timestamp[fpi]
          fp_df_i$label <- 4
          fp_df_i$n_detections <- 1
          fp_df_i$max_score <- sound_test$prediction_score[fpi]
          fp_df_i$match_ids <- fpi

          # add to results
          results_i <- rbind(results_i, fp_df_i)
        }
      }

      results_i %>% tail

      # Add predicted columns
      results_i$cough_prediction <- FALSE
      results_i$peak_prediction <- FALSE
      for(ri in 1:nrow(results_i)){
        # peak prediction
        if(!is.na(results_i$max_score[ri])){
          results_i$peak_prediction[ri] <- TRUE
        }
        # cough prediction
        if(!is.na(results_i$max_score[ri])){
          if(results_i$max_score[ri] >= prediction_threshold){
            results_i$cough_prediction[ri] <- TRUE
          }
        }
      }

      # Sort by timestamp
      results_i <- results_i %>% arrange(timestamp)
      results_i

      # Add to growing results df
      results <- rbind(results, results_i)

    } # end of loop through each test
  } # end of loop through each alias

  results %>% head
  results %>% nrow

  ################################################################################
  # Analyze results

  # Save a copy
  df <- results
  head(df)

  # Group by alias-test
  dfsum <- df %>%
    #filter(keep == TRUE) %>%
    group_by(test,alias) %>%
    dplyr::summarize(start_time = timestamp[1],
                     duration = max(timestamp) - min(timestamp),
                     events = length(which(label != 4)),

                     label_0 = length(which(label == 0)),
                     label_1 = length(which(label == 1)),
                     label_2 = length(which(label == 2)),
                     label_3 = length(which(label == 3)),
                     label_4 = length(which(label == 4)),

                     peak_detections = length(which(peak_prediction == TRUE)),
                     cough_detections = length(which(cough_prediction == TRUE)),

                     # true positives
                     tp_3 = length(which(label %in% c(3) & cough_prediction == TRUE)),

                     # false negatives
                     fn_3 = length(which(label %in% c(3) & cough_prediction == FALSE)),

                     # false negatives due to misclassified peak
                     fn_3_cough = length(which(label %in% c(3) & peak_prediction == TRUE & cough_prediction == FALSE)),

                     # false negatives due to missed peaks
                     fn_3_peak = length(which(label %in% c(3) & peak_prediction == FALSE)),

                     # false positives
                     fp_3 = length(which(label == 4)) + length(which(label %in% c(0,1,2) & cough_prediction == TRUE))) %>%

           # false negative/positives rates
    mutate(fnr_3 = fn_3 / label_3,
           fpr_3 = fp_3 / events) %>%

           # sensitivity / specificity
    mutate(sens_3 = 1 - fnr_3,
           spec_3 = 1 - fpr_3)

  glimpse(dfsum)

  # Summary table
  dfsum_3 <- dfsum %>%
    group_by(test) %>%
    dplyr::summarize(events_mean = mean(events, na.rm=TRUE),
                     events_sd = sd(events, na.rm=TRUE),
                     coughs_mean = mean(label_3, na.rm=TRUE),
                     coughs_sd = sd(label_3, na.rm=TRUE),
                     sensitivity_mean = mean(sens_3, na.rm=TRUE),
                     sensitivity_sd = sd(sens_3, na.rm=TRUE),
                     specificity_mean = mean(spec_3, na.rm=TRUE),
                     specificity_sd = sd(spec_3, na.rm=TRUE))

  data.frame(dfsum_3)
  dfsum_3 %>% nrow

  return_list <- list()
  return_list$results <- dfsum
  return_list$summary <- dfsum_3

  return(return_list)
}
