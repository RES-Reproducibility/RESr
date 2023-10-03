
# All related sheets

sheet_url   <- function() {"https://docs.google.com/spreadsheets/d/1D7nhTs8ao9yIW-PQ_z4zjYNB3pGMWcLWABtaA4I_WL0"}
EctJsheet_url   <- function() {"https://docs.google.com/spreadsheets/d/1fCz0ZyK7Fsw2E9jbgKgedtoDgmBePk2V_0cuyHijfv0"}
billing_url <- function(){"https://docs.google.com/spreadsheets/d/1dJijWplmvxlqgFgzYHy7gFs4FCFsLAv7LifX4g-4GIo"}


#' Read EctJ sheet
#'
#'@export
read_ectj <- function(refresh = FALSE){
    if (refresh){
        x = data.table(
            googlesheets4::read_sheet(
                EctJsheet_url(),sheet = "List",skip = 1, range = "List!A2:AD1000000") %>%
                janitor::clean_names()
        )
        x = x[!is.na(ms)]
        saveRDS(x, file = "~/Dropbox/EctJ/EctJ-1-key-documents/Report/current-sheet.Rds")
    } else {
        x = readRDS(file = "~/Dropbox/EctJ/EctJ-1-key-documents/Report/current-sheet.Rds")
    }
    x
}

#' Read EJ Google Sheet with Replications
#'
#' @export
read_list <- function(refresh = FALSE){
    if (refresh){
        x = data.table(
            googlesheets4::read_sheet(
                sheet_url(),sheet = "List",skip = 1, range = "List!A2:AD2000") %>%
                janitor::clean_names()
        )
        x = x[!is.na(ms)]
        saveRDS(x, file = "~/Dropbox/EJ/EJ-1-key-documents/Report/current-sheet.Rds")
    } else {
        x = readRDS(file = "~/Dropbox/EJ/EJ-1-key-documents/Report/current-sheet.Rds")
    }
    x
}

clean_list <- function(refresh_sheet = FALSE){

    x = read_list(refresh = refresh_sheet)

    x[, completed_quarter := zoo::as.yearqtr(date_completed)]
    # create some variables
    x[, completed := status %in% c("AP","NT", "P", "p"), by = ms]
    x[, hours_paper := sum(hours_spent), by = ms]
    x[, iterations_paper := max(round), by = ms]

    # timing measures on each iteration
    x[ , time_assign          := lubridate::make_difftime(date_assigned - arrival_date, units = "days")]
    x[ , time_replication     := lubridate::make_difftime(date_completed - date_assigned, units = "days")]
    x[ , time_decision        := lubridate::make_difftime(date_processed - date_completed, units = "days")]
    x[ , time_resubmission    := lubridate::make_difftime(date_resub - date_processed, units = "days")]
    # compute cumulative times
    x[ , cumtime_replication  := time_assign + time_replication]
    x[ , cumtime_decision     := cumtime_replication + time_decision]
    x[ , cumtime_resubmission := cumtime_decision + time_resubmission]

    # melt for plotting
    # mr = melt(r, id.vars = c("arrival_date","ms","round"),
    #           measure.vars = c("time_assign","cumtime_replication","cumtime_decision"))
    # ggplot(mr, aes(x = arrival_date,y= value, color = variable)) + geom_point()


    # moving averages at iteration level
    setkey(x, arrival_date, ms)
    # share of rejections by iteration (1,2,3)
    x[ , sround := round]
    x[sround > 2, sround := 3]
    hazard = x[, .(hazard = mean(status == "R")), by = .(arrival_date, sround)]
    hazard[, hazard_smooth := data.table::frollmean(hazard,n = 11,na.rm = TRUE), by = .(sround)]

    # add total timing at completed paper level
    xp <- x[ , list(
              hours_spent = sum(hours_spent),
              iterations_paper = .N,
              time_assign_paper = sum(time_assign),
              time_replication_paper =sum(cumtime_replication),
              time_decision_paper = sum(cumtime_decision),
              time_resubmission_paper = sum(cumtime_resubmission),
              arrival_date = .SD[round == 1, as.Date(arrival_date)]
          ),
      by = ms]
    # # add cumulatives
    # xp[, cumtime_replication_paper := time_assign_paper + time_replication_paper]
    # xp[, cumtime_decision_paper := cumtime_replication_paper + time_decision_paper]
    # xp[, cumtime_resubmission_paper := cumtime_decision_paper + time_resubmission_paper]


    x[, arrival_date := as.Date(arrival_date)]

    list(iterations = x, papers = xp, iteration_hazard = hazard)
}
read_replicators <- function(refresh = FALSE){
    if (refresh){
        x = data.table(
            googlesheets4::read_sheet(
                sheet_url(),sheet = "Replicator-Availability")
        )
        saveRDS(x, file = "~/Dropbox/EJ/EJ-1-key-documents/Report/current-replicators.Rds")
    } else {
        x = readRDS(file = "~/Dropbox/EJ/EJ-1-key-documents/Report/current-replicators.Rds")
    }
    x
}

#' compute quarterly stats for each replicator
#'
#' @export
billing <- function(rate = 25, write = FALSE,refresh = FALSE){
    xectj = read_ectj(refresh = refresh)
    xej = read_list(refresh = refresh)

    reps = read_replicators(refresh = refresh)
    r0 = xectj[checker %in% reps$replicator][,
                                                           .(checker,date_assigned,
                                                             ms,
                                                             date_completed,
                                                             hours_spent,
                                                             days_replic,
                                                             decision,
                                                             journal = "EctJ"
                                                             )]
    r0 = merge(r0, reps[,.(replicator,name,surname)] , by.x = "checker", by.y = "replicator")


    r1 = xej[checker %in% read_replicators()$replicator][,
                                                        .(checker,date_assigned,
                                                          ms,
                                                          date_completed,
                                                          hours_spent,
                                                          days_replic,
                                                          decision,
                                                          journal = "EJ"
                                                        )]

    r1 = merge(r1, reps[,.(replicator,name,surname)] , by.x = "checker", by.y = "replicator")


    r = rbind(r0,r1)
    r[, assigned_quarter := zoo::as.yearqtr(date_assigned)]

    # r = r[!is.na(completed_quarter)]
    # compute total payments by quarter
    qtr = r[, .(completed_jobs = sum(!is.na(date_completed)),
                completed_papers = .SD[!is.na(date_completed), length(unique(ms))],
                qtr_payment_euros = round(sum(hours_spent,na.rm = TRUE) * rate,2),
                ongoing_jobs = sum(is.na(date_completed)),
                Avg_hours = round(mean(hours_spent,na.rm = TRUE),2),
                Tot_hours = round(sum(hours_spent,na.rm = TRUE),2),
                Avg_days = round(mean(days_replic,na.rm = TRUE),2),
                share_reject = round(mean(decision == "R",na.rm = TRUE) , 2)), by = .(checker,name, surname,assigned_quarter)][
                    order(assigned_quarter)
                ]

    # compute total payments by quarter for EJ
    qtr_ej = r[journal == "EJ", .(completed_jobs = sum(!is.na(date_completed)),
                completed_papers = .SD[!is.na(date_completed), length(unique(ms))],
                qtr_payment_euros = round(sum(hours_spent,na.rm = TRUE) * rate,2),

                ongoing_jobs = sum(is.na(date_completed)),
                Avg_hours = round(mean(hours_spent,na.rm = TRUE),2),
                Tot_hours = round(sum(hours_spent,na.rm = TRUE),2),
                Avg_days = round(mean(days_replic,na.rm = TRUE),2),
                share_reject = round(mean(decision == "R",na.rm = TRUE) , 2)), by = .(checker,name, surname,assigned_quarter)][
                    order(assigned_quarter)
                ]
    # compute total payments by quarter for EctJ
    qtr_ectj = r[journal == "EctJ", .(completed_jobs = sum(!is.na(date_completed)),
                                  completed_papers = .SD[!is.na(date_completed), length(unique(ms))],
                                  qtr_payment_euros = round(sum(hours_spent,na.rm = TRUE) * rate,2),
                                  ongoing_jobs = sum(is.na(date_completed)),
                                  Avg_hours = round(mean(hours_spent,na.rm = TRUE),2),
                                  Tot_hours = round(sum(hours_spent,na.rm = TRUE),2),
                                  Avg_days = round(mean(days_replic,na.rm = TRUE),2),
                                  share_reject = round(mean(decision == "R",na.rm = TRUE) , 2)), by = .(checker,name, surname,assigned_quarter)][
                                      order(assigned_quarter)
                                  ]

    # compute stats all times
    ov =r[, .(completed_jobs = sum(!is.na(date_completed)),
              completed_papers = .SD[!is.na(date_completed), length(unique(ms))],
              qtr_payment_euros = round(sum(hours_spent,na.rm = TRUE) * rate,2),
              ongoing_jobs = sum(is.na(date_completed)),
              Avg_hours = round(mean(hours_spent,na.rm = TRUE),2),
              Tot_hours = round(sum(hours_spent,na.rm = TRUE),2),
              Avg_days = round(mean(days_replic,na.rm = TRUE),2),
              share_reject = round(mean(decision == "R",na.rm = TRUE) , 2) ), by = .(checker,name, surname)]


    if (write){
        qtr[, assigned_quarter := as.character(assigned_quarter)]
        qtr_ej[, assigned_quarter := as.character(assigned_quarter)]
        qtr_ectj[, assigned_quarter := as.character(assigned_quarter)]
        googlesheets4::write_sheet(qtr, billing_url(), sheet = "quarterly-payments-overall")
        googlesheets4::write_sheet(qtr_ej, billing_url(), sheet = "quarterly-payments-EJ")
        googlesheets4::write_sheet(qtr_ectj, billing_url(), sheet = "quarterly-payments-EctJ")
    }
    list(qtr,ov,qtr_ectj,qtr_ej)

}



