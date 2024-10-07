
# All related sheets

sheet_url   <- function() {"https://docs.google.com/spreadsheets/d/1D7nhTs8ao9yIW-PQ_z4zjYNB3pGMWcLWABtaA4I_WL0"}
EctJsheet_url   <- function() {"https://docs.google.com/spreadsheets/d/1fCz0ZyK7Fsw2E9jbgKgedtoDgmBePk2V_0cuyHijfv0"}
billing_url <- function(){"https://docs.google.com/spreadsheets/d/1dJijWplmvxlqgFgzYHy7gFs4FCFsLAv7LifX4g-4GIo"}

view_bill <- function(){
    googlesheets4::gs4_browse(billing_url())
}

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
            # TODO https://googlesheets4.tidyverse.org/reference/cell-specification.html
            googlesheets4::read_sheet(
                sheet_url(),sheet = "List",skip = 1, range = "List!A2:AE2000",
                col_types = "ciccccccccDDcccicccDDddccccDccD") %>%
                janitor::clean_names()
        )
        x = x[!is.na(ms)]
        saveRDS(x, file = "~/Dropbox/EJ/EJ-1-key-documents/Report/current-sheet.Rds")
    } else {
        x = readRDS(file = "~/Dropbox/EJ/EJ-1-key-documents/Report/current-sheet.Rds")
    }
    # fix missing arrival date field
    x[, arrival_date := as.Date(max(arrival_date_ee, arrival_date_package,na.rm = TRUE)), by = .(ms, round)]
    x[(!is.finite(arrival_date)) | is.na(arrival_date) , arrival_date := date_assigned]

    # coerce hours to numeric
    x[ , hours_checker2 := as.numeric(hours_checker2)]
    # drop if waiting for submission
    x = x[is.na(de_comments)]
    #
    # resubmitted date
    data.table::setorder(x,ms,round)
    x[, date_resub := data.table::shift(arrival_date, type = "lead"), by = ms ]
    x
}

nazerosum <- function(x,y){
    if (length(x)>1){
        print(x)
    }
    if(is.na(x)){
        x = 0
    } else if (is.na(y)){
        y = 0
    }
    x + y
}

clean_list <- function(refresh_sheet = FALSE){

    x = read_list(refresh = refresh_sheet)


    x[, completed_quarter := zoo::as.yearqtr(date_completed)]
    # create some variables
    x[, completed := any(status %in% c("AP","NT", "nt", "P", "p")), by = ms]
    x[, hours_spent := nazerosum(hours_checker1, hours_checker2), by = .(ms,round)]
    x[, hours_paper := sum(hours_spent,na.rm = TRUE), by = ms]
    x[, iterations_paper := max(round), by = ms]

    # timing measures on each iteration
    x[ , time_assign          := date_assigned - arrival_date]
    x[ , time_replication     := date_completed - date_assigned]
    x[ , time_decision        := date_processed - date_completed]
    x[ , time_resubmission    := date_resub - date_processed]
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
    xp <- x[(completed) , list(
              hours_spent = sum(hours_spent,na.rm = TRUE),
              iterations_paper,
              time_assign_paper = sum(time_assign,na.rm = TRUE),
              time_replication_paper =sum(cumtime_replication,na.rm = TRUE),
              time_decision_paper = sum(cumtime_decision,na.rm = TRUE),
              time_resubmission_paper = sum(cumtime_resubmission,na.rm = TRUE),
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

    xej[  , days_replic := difftime(date_completed, date_assigned, units = "days")]
    xectj[, days_replic := difftime(date_completed, date_assigned, units = "days")]

    reps = read_replicators(refresh = refresh)
    # 1. filter out rows of my replicators
    r0 = xectj[checker1 %in% reps$replicator][,.(checker1, checker2,date_assigned,
                                                             ms,
                                                             date_completed,
                                                             hours_checker1,
                                                             hours_checker2,
                                                             days_replic,
                                                             decision,
                                                             journal = "EctJ"
                                                             )]
    # 2. get checker 2 hours as well
    r02 = r0[!(is.na(checker2)),  list(checker = checker2,hours_spent = hours_checker2,date_assigned,
                                       ms,
                                       date_completed,
                                       hours_checker1,
                                       hours_checker2,
                                       days_replic,
                                       decision,
                                       journal = "EctJ")]
    setnames(r0, c("checker1","hours_checker1"), c("checker","hours_spent"))
    r0[,c("checker2","hours_checker2") := NULL]

    r0 = rbind(r0[,.(checker,hours_spent,date_assigned,
                     ms,
                     date_completed,
                     days_replic,
                     decision,
                     journal)],
               r02[,.(checker,hours_spent,date_assigned,
                      ms,
                      date_completed,
                      days_replic,
                      decision,
                      journal)])
    r0 = merge(r0, reps[,.(replicator,name,surname)] , by.x = "checker", by.y = "replicator")
    r0[, date_assigned := as.Date(date_assigned)]
    r0[, date_completed := as.Date(date_completed)]


    r1 = xej[checker1 %in% read_replicators()$replicator][,
                                                        .(checker1, checker2,date_assigned,
                                                          ms,
                                                          date_completed,
                                                          hours_checker1,
                                                          hours_checker2,
                                                          days_replic,
                                                          decision,
                                                          journal = "EJ"
                                                        )]
    # 2. get checker 2 hours as well
    r12 = r1[!(is.na(checker2)),  list(checker = checker2,hours_spent = hours_checker2,date_assigned,
                                       ms,
                                       date_completed,
                                       hours_checker1,
                                       hours_checker2,
                                       days_replic,
                                       decision,
                                       journal = "EJ")]
    setnames(r1, c("checker1","hours_checker1"), c("checker","hours_spent"))
    r1[,c("checker2","hours_checker2") := NULL]

    r1 = rbind(r1[,.(checker,hours_spent,date_assigned,
                     ms,
                     date_completed,
                     days_replic,
                     decision,
                     journal)],
               r12[,.(checker,hours_spent,date_assigned,
                      ms,
                      date_completed,
                      days_replic,
                      decision,
                      journal)])

    r1 = merge(r1, reps[,.(replicator,name,surname)] , by.x = "checker", by.y = "replicator")


    r = rbind(r0,r1)
    r[, assigned_quarter := zoo::as.yearqtr(date_assigned)]
    r[, completed_quarter := zoo::as.yearqtr(date_completed)]



    r = r[!is.na(completed_quarter)]
    # compute total payments by quarter
    qtr = r[, .(completed_jobs = sum(!is.na(date_completed)),
                completed_papers = .SD[!is.na(date_completed), length(unique(ms))],
                qtr_payment_euros = round(sum(hours_spent,na.rm = TRUE) * rate,2),
                ongoing_jobs = sum(is.na(date_completed)),
                Avg_hours = round(mean(hours_spent,na.rm = TRUE),2),
                Tot_hours = round(sum(hours_spent,na.rm = TRUE),2),
                Avg_days = round(mean(as.numeric(days_replic),na.rm = TRUE),2),
                share_reject = round(mean(decision == "R",na.rm = TRUE) , 2)), by = .(checker,name, surname,completed_quarter)][
                    order(c(completed_quarter))
                ]

    # compute total payments by quarter for EJ
    qtr_ej = r[journal == "EJ", .(completed_jobs = sum(!is.na(date_completed)),
                completed_papers = .SD[!is.na(date_completed), length(unique(ms))],
                qtr_payment_euros = round(sum(hours_spent,na.rm = TRUE) * rate,2),

                ongoing_jobs = sum(is.na(date_completed)),
                Avg_hours = round(mean(hours_spent,na.rm = TRUE),2),
                Tot_hours = round(sum(hours_spent,na.rm = TRUE),2),
                Avg_days = round(mean(as.numeric(days_replic),na.rm = TRUE),2),
                share_reject = round(mean(decision == "R",na.rm = TRUE) , 2)), by = .(checker,name, surname,completed_quarter)][
                    order(completed_quarter)
                ]
    # compute total payments by quarter for EctJ
    qtr_ectj = r[journal == "EctJ", .(completed_jobs = sum(!is.na(date_completed)),
                                  completed_papers = .SD[!is.na(date_completed), length(unique(ms))],
                                  qtr_payment_euros = round(sum(hours_spent,na.rm = TRUE) * rate,2),
                                  ongoing_jobs = sum(is.na(date_completed)),
                                  Avg_hours = round(mean(hours_spent,na.rm = TRUE),2),
                                  Tot_hours = round(sum(hours_spent,na.rm = TRUE),2),
                                  Avg_days = round(mean(as.numeric(days_replic),na.rm = TRUE),2),
                                  share_reject = round(mean(decision == "R",na.rm = TRUE) , 2)), by = .(checker,name, surname,completed_quarter)][
                                      order(completed_quarter)
                                  ]

    # compute stats all times
    ov =r[, .(completed_jobs = sum(!is.na(date_completed)),
              completed_papers = .SD[!is.na(date_completed), length(unique(ms))],
              qtr_payment_euros = round(sum(hours_spent,na.rm = TRUE) * rate,2),
              ongoing_jobs = sum(is.na(date_completed)),
              Avg_hours = round(mean(hours_spent,na.rm = TRUE),2),
              Tot_hours = round(sum(hours_spent,na.rm = TRUE),2),
              Avg_days = round(mean(as.numeric(days_replic),na.rm = TRUE),2),
              share_reject = round(mean(decision == "R",na.rm = TRUE) , 2) ), by = .(checker,name, surname)]


    if (write){
        qtr[, completed_quarter := as.character(completed_quarter)]
        qtr_ej[, completed_quarter := as.character(completed_quarter)]
        qtr_ectj[, completed_quarter := as.character(completed_quarter)]
        googlesheets4::write_sheet(qtr, billing_url(), sheet = "quarterly-payments-overall")
        googlesheets4::write_sheet(qtr_ej, billing_url(), sheet = "quarterly-payments-EJ")
        googlesheets4::write_sheet(qtr_ectj, billing_url(), sheet = "quarterly-payments-EctJ")
    }
    list(qtr = qtr,alltimes = ov,qtr_ectj = qtr_ectj,qtr_ej = qtr_ej, r = r)

}


billing_table_ej <- function(){
    b = billing()
    t = b$qtr_ej[, .(
        Hours = sum(Tot_hours,na.rm = TRUE),
        Euros = sum(qtr_payment_euros),
        Replicators = .N,
        `Hours/Replicator` = mean(Tot_hours,na.rm = TRUE),
        mean_completed_jobs = mean(completed_jobs),
        mean_pay = mean(qtr_payment_euros)), by = list(Quarter = completed_quarter)]
    t

}


