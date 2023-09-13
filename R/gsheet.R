
# All EJ related sheets

sheet_url <- function() {"https://docs.google.com/spreadsheets/d/1D7nhTs8ao9yIW-PQ_z4zjYNB3pGMWcLWABtaA4I_WL0"}

read_list <- function(refresh = FALSE){
    if (refresh){
        x = data.table(
            googlesheets4::read_sheet(
                sheet_url(),sheet = "List",skip = 1, range = "List!A2:AD1000000") %>%
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
    x[, completed := any(status %in% c("NT", "P", "p")), by = ms]
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
    mr = melt(r, id.vars = c("arrival_date","ms","round"),
              measure.vars = c("time_assign","cumtime_replication","cumtime_decision"))
    ggplot(mr, aes(x = arrival_date,y= value, color = variable)) + geom_point()



    # moving averages at iteration level
    setkey(x, arrival_date, ms)
    # share of rejections by iteration (1,2,3)
    x[ , sround := round]
    x[sround > 2, sround := 3]
    hazard = x[, .(hazard = mean(status == "R")), by = .(arrival_date, sround)]
    x[, hazard := mean(status == "R"), by = .(arrival_date, round)]
    x[round == 1, fail1 := status == "R"]
    x[round == 2, fail2 := status == "R"]
    x[round > 2, fail3 := status == "R"]


    x[, ma_time_assign := frollmean(time_assign,n = 11,na.rm = TRUE)]
    x[, ma_time_replication := frollmean(cumtime_replication,n = 11,na.rm = TRUE)]
    x[, ma_time_decision := frollmean(cumtime_decision,n = 11,na.rm = TRUE)]

    # add total timing at paper level
    xp = x[, c("time_assign_paper",
          "time_replication_paper",
          "time_decision_paper",
          "time_resubmission_paper") := list(
              sum(time_assign),
              sum(time_replication),
              sum(time_decision),
              sum(time_resubmission)
          ),
      by = ms]
    # add cumulatives
    xp[, cumtime_replication_paper := time_assign_paper + time_replication_paper]
    xp[, cumtime_decision_paper := cumtime_replication_paper + time_decision_paper]
    xp[, cumtime_resubmission_paper := cumtime_decision_paper + time_resubmission_paper]


    x
}
read_replicators <- function(){
    x = googlesheets4::read_sheet(
        sheet_url(),sheet = "Replicator-Availability")
    x$replicator
}

#' compute quarterly stats for each replicator
billing <- function(x){
    r = x[Checker %in% read_replicators()]
    # throw out unfinished jobs
    r = r[!is.na(completed_quarter)]
    # compute stats by quarter
    qtr = r[, .(N_packages = .N,
                Avg_hours = mean(hours_spent,na.rm = TRUE),
                Tot_hours = sum(hours_spent,na.rm = TRUE),
                Avg_days = mean(`Days replic.`,na.rm = TRUE),
                share_reject = mean(Decision == "R",na.rm = TRUE) ), by = .(Checker,completed_quarter)]

    # compute stats overall
    ov =r[, .(N_packages = .N,
            Avg_hours = mean(`Hours spent`,na.rm = TRUE),
            Tot_hours = sum(`Hours spent`,na.rm = TRUE),
            Avg_days = mean(`Days replic.`,na.rm = TRUE),
            share_reject = mean(Decision == "R",na.rm = TRUE) ), by = Checker]

    list(qtr,ov)
}



