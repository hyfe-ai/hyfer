#' Evaluate Hyfe's performance based on a set of labelled sounds.
#'
#' @param detections A `dataframe` of Hyfe detections. This dataframe requires these columns:
#' `alias` (research/device ID); `timestamp` (with a numeric timestamp); `n_peaks` (the number of peaks occurring at
#' the time represented by the timestamp; this can just be 1 if you don't know); and `prediction_score` (0-1).
#'
#' @param labels A `dataframe` with analyst labels. Required columns: `test` (numeric ID for each test labeled);
#' `timestamp` (numeric timestamp for the cough-second); `label` (best label for this cough-second, 1 - 3, using the Hyfe 4-tier system:
#' 1 = a disputable cough, possibly fake; 2 = definitley a cough but barely audible; 3 = a clear authentic cough);
#' `n1` (if this cough second was labeled category 1 by any analysts, this value is 1; if not it is 0);
#' same for `n2` (category 2) and `n3` (category 3);
#' `n_labels` provides the number of labels that occurred within this cough-second;
#' `n_analysts` provides the number of analysts who have reviewed this test;
#' `valid` indicates whether this cough-second should be included in the analysis (`TRUE` or `FALSE`).
#'
#' @param offsets A `dataframe` of time offsets, in seconds, for the Hyfe detections in each test represented in the `labels` dataset.
#' Each row is a test. Required fields are: `alias` (research/device ID); `test` (numeric ID for the test); `offset` (seconds
#' by which to adjust Hyfe timestamp; if postivie, Hyfe timestamps will increase; if negative, they will decrease).
#'
#' @param offset_cutoff A number indicating the allowable difference, in seconds, between a Hyfe detection and a label timestamp.
#' @param prediction_threshold Cough prediction score threshold.
#' @param n3_cutoff Minimum number of category 3 coughs that must be labeled for a test in order for that test to be included in the calculation of performance metrics.
#' @param quality_cutoff Minimum quality of a test in order for that test to be included in the estimation of performance metrics. Quality is defined as the fraction of cough-seconds that are category 3.
#' @param remove_tests Numeric vector providing the IDs for any tests you want to remove from the performance analysis.
#' @param toplot Plot diagnostic plots?
#' @param verbose Plot status updates?
#'
#' @return A list with various summaries of the results.
#' @export
#'
hyfe_performance <- function(detections,
                             labels,
                             offsets,
                             offset_cutoff = 3,
                             prediction_threshold = 0.7,
                             n3_cutoff = 10,
                             quality_cutoff = 0.95,
                             remove_tests = NULL,
                             toplot = TRUE,
                             verbose=TRUE){

  #=============================================================================
  # For debugging only -- not run!

  if(FALSE){
    library(dplyr)

    # run the data read / cleaning code in analysis_2 in navarra_clinica / validation

    head(labels)
    detections <- detecti
    head(detections)
    head(offsets)

    toplot = TRUE
    verbose = TRUE
    prediction_threshold <- 0.7
    offset_cutoff = 3

    remove_tests = NULL
  }

  #=============================================================================

  # Apply prediction threshold
  detections$prediction <- FALSE
  detections$prediction[detections$prediction_score >= prediction_threshold] <- TRUE

  # Remove tests?
  if(!is.null(remove_tests)){
    labels <- labels %>% filter(! test %in% remove_tests)
  }

  #=============================================================================

  # Stage results dataframe
  results <- data.frame()

  aliases <- unique(detections$alias)
  for(alias_i in 1:length(aliases)){
    #alias_i <- 1
    aliasi <- aliases[alias_i] ; aliasi
    if(verbose){message('Alias  ',aliasi,' ... ')}

    # Filter data to this alias
    soundi <- detections[detections$alias == aliasi,]
    head(soundi)

    par(mfrow=c(5,2))

    # Loop through each test
    tests <- labels$test %>% unique  ; tests
    for(tests_i in 1:length(tests)){
      #tests_i <- 1
      testi <- tests[tests_i] ; testi

      if(verbose){message('--- processing test ',testi,' ... ')}

      labeli <- labels[labels$test == testi,]
      labeli %>% head

      sound_test <- soundi

      # Apply time offsets
      offseti <- offsets %>% filter(test==testi, alias == aliasi)
      offseti
      valid_check <- all(nrow(offseti)==1, !is.na(offseti$offset))

      # If timestamps are valid after applying offset, continue:
      if(valid_check){
        sound_test$timestamp <- sound_test$timestamp + offseti$offset

        reference_times <- labeli$timestamp
        reference_labels <- labeli$label
        hyfe_times <- sound_test$timestamp
        hyfe_predictions <- sound_test$prediction

        # Restrict sounds data to this test window
        sound_test <- sound_test %>% filter(timestamp >= (min(labeli$timestamp) - 60),
                                            timestamp <= (max(labeli$timestamp) + 60))
        #sound_test$timestamp %>% as_datetime %>% head

        # Diagnostic plot
        if(toplot){
          plot(1,type='n',xlim=range(labeli$timestamp),ylim=c(-1,4),axes=FALSE,ann=FALSE)
          axis(1) ; axis(2,at=c(0,1,2,3),las=2) ; title(ylab = 'Label',main=paste0('Test ',testi))
          points(x=labeli$timestamp, y=labeli$label)
          abline(v=sound_test$timestamp[which(sound_test$prediction==FALSE)], col='grey')
          abline(v=sound_test$timestamp[which(sound_test$prediction==TRUE)])
        }

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
        results_i <- as.data.frame(pr)
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
            #fp_df_i$id <- NA
            fp_df_i$timestamp <- sound_test$timestamp[fpi]
            fp_df_i$label <- 4
            fp_df_i$n_detections <- 1
            fp_df_i$max_score <- sound_test$prediction_score[fpi]
            fp_df_i$match_ids <- fpi

            # add to results
            results_i <- rbind(results_i, as.data.frame(fp_df_i))
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

      } # end of offset validity check
    } # end of loop through each test
  } # end of loop through each alias

  results %>% head
  results %>% nrow

  nrow(results)
  results$label %>% table
  results$cough_prediction %>% table

  ################################################################################
  # Analyze results

  # Save a copy
  df <- results
  head(df)

  df %>%
    filter(alias == 'navarrac+002@hyfe.ai') %>%
    group_by(test) %>% summarize(events = length(which(label < 4)),
                                      n3 = length(which(label == 3)))

  # Group by alias-test and summarize each test
  dfsum <- df %>%
    filter(valid == TRUE) %>%
    group_by(alias, test) %>%
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
           spec_3 = 1 - fpr_3) %>%

    mutate(quality = label_3 / events) %>%

    filter(label_3 > n3_cutoff,
           quality > quality_cutoff) #%>% # only use tests in which at least 10 label-3 coughs occurred.

  #filter(! test %in% c(2, 17, 35, 38, 41, 45))

  #dfsum$label_1 %>% table
  #dfsum$quality %>% sort

  #dfsum %>% select(test, quality)

  #glimpse(dfsum)
  #dfsum %>% select(test, label_3) %>% data.frame
  #dfsum$events %>% table
  #dfsum$label_3 %>% table

  #dfsum$quality %>% sort
  #df[df$n1>0, 1:10]
  #df$test[which(as.numeric(as.character(df$label)) == 1)] %>%  table
  #df[df$valid == TRUE &
  #   df$n1 > 0,
  #   1:10]

  #dfi <- df %>% filter(valid == TRUE, alias == 'navarrac+003@hyfe.ai', test == 1) ; dfi
  #df$n1 %>% table

  #df[which(df$n1 >0) %>% head,]
  #df[df$test==1,1:10]


  if(FALSE){
    # don't run this code when called as a function
    # made available here for debugging / exploration purposes only

    # Plots ======================================================================
    # Summary of each test
    p_sens <- ggplot(dfsum,aes(y=factor(test),x=sens_3)) + geom_col() + ylab('Test') + xlab('Sensitivity') + facet_wrap(~alias)
    p_spec <- ggplot(dfsum,aes(y=factor(test),x=spec_3)) + geom_col() + ylab('Test') + xlab('Specificity') + facet_wrap(~alias)
    p_tests <- ggarrange(p_sens,p_spec,nrow=2)
    #p_tests

    p_viol_sens <-
      ggplot(dfsum,aes(y=sens_3, x=alias)) +
      geom_violin(fill='dodgerblue4', alpha=.5, draw_quantiles = c(0.25,.5,.75)) +
      scale_y_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
      ylab('Sensitivity') + xlab(NULL)
    p_viol_spec <-
      ggplot(dfsum,aes(y=spec_3, x=alias)) +
      geom_violin(fill='dodgerblue4', alpha=.5, draw_quantiles = c(0.25,.5,.75)) +
      scale_y_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
      ylab('Specificity') + xlab(NULL)
    p_overall <- ggarrange(p_viol_sens, p_viol_spec,nrow=2)
  }

  # Summary table - one row for each test (avearaging devices together)
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


  # Summary table - one row for each device (avearaging tests together)
  dfsum_4 <- dfsum %>%
    group_by(alias) %>%
    dplyr::summarize(events_mean = mean(events, na.rm=TRUE),
                     events_sd = sd(events, na.rm=TRUE),
                     coughs_mean = mean(label_3, na.rm=TRUE),
                     coughs_sd = sd(label_3, na.rm=TRUE),
                     sensitivity_mean = mean(sens_3, na.rm=TRUE),
                     sensitivity_sd = sd(sens_3, na.rm=TRUE),
                     specificity_mean = mean(spec_3, na.rm=TRUE),
                     specificity_sd = sd(spec_3, na.rm=TRUE))
  data.frame(dfsum_4)
  #dfsum_3 %>% nrow


  dfpunch_3 <- data.frame(n_devices = length(unique(df$alias)),
                          n_tests = nrow(dfsum_3),
                          sensitivity_mean = dfsum_3$sensitivity_mean %>% mean(na.rm=TRUE),
                          sensitivity_sd = dfsum_3$sensitivity_mean %>% sd(na.rm=TRUE),
                          specificity_mean = dfsum_3$specificity_mean %>% mean(na.rm=TRUE),
                          specificity_sd = dfsum_3$specificity_mean %>% sd(na.rm=TRUE))
  dfpunch_3

  # Prepare list for return
  return_list <- list()
  return_list$punchline <- dfpunch_3
  return_list$summary <- dfsum_3
  return_list$tests <- dfsum
  return_list$details <- df

  #print(p_tests)

  return(return_list)
}
