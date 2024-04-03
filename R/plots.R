


plot_all <- function(refresh = FALSE,write = FALSE){
    # width and height
    w1 = 8
    h1 = 6

    # width and height narrower
    w2 = 5
    h2 = 5

    path = file.path("~","git","EJData","reports",paste0("Report"),"images")

    li = read_list(refresh = refresh)
    cl = clean_list(refresh = refresh)

    out = list()
    out$proctime_iters = plot_proctime(cl$iterations)
    out$rejection_hazard = plot_hazard(cl$iteration_hazard)

    tab = cl$iterations[, .(iterations_paper = iterations_paper[1]), by = ms][, .N,  by = iterations_paper]
    out$iter_paper_bar = ggplot(tab, aes(x = iterations_paper, y = N)) +
        ggplot2::geom_bar(stat="identity", fill = "black") + ggplot2::theme_bw()


    out$proctime_papers = plot_proctime(cl$papers,by = "Paper")

    hpi = plot_hours_iters_paper()
    out$hours_iteration = hpi$hours_i
    out$hours_paper = hpi$hours_p
    out$iters_paper = hpi$iters_p

    out$data_statements = plot_DS(li)

    out$software_ts = plot_software()

    if (write){
        ggplot2::ggsave(file.path(path,"proctime_iters.png"),
                        plot = out$proctime_iters, width = w1, height=h1)
        ggplot2::ggsave(file.path(path,"hazard.png"),
                        plot = out$rejection_hazard, width = w1, height=h1)
        ggplot2::ggsave(file.path(path,"iters_paper_bar.png"),
                        plot = out$iter_paper_bar, width = w1, height=h1)

        ggplot2::ggsave(file.path(path,"proctime_papers.png"),
                        plot = out$proctime_papers, width = w1, height=h1)
        ggplot2::ggsave(file.path(path,"hours_iter.png"),
                        plot = out$hours_iteration, width = 7, height=h1)
        ggplot2::ggsave(file.path(path,"hours_paper.png"),
                        plot = out$hours_paper, width = 7, height=h1)
        ggplot2::ggsave(file.path(path,"iters_paper.png"),
                        plot = out$iters_paper, width = 7, height=h1)
        ggplot2::ggsave(file.path(path,"datastatements.png"),
                        plot = out$data_statements, width = w2, height=h2)
        ggplot2::ggsave(file.path(path,"software.png"),
                        plot = out$software_ts, width = w1, height=h1)

    }

    out
}

plot_trends <- function(x){
    n = x[, .(Papers = length(unique(ms)),Iterations = .N),
          by = .(quarter = zoo::as.yearqtr(arrival_date))]

    l = list()
    l$p1 = ggplot2::ggplot(melt(n, id.vars = "quarter",value.name = "Count"), aes(x = quarter , y = Count )) +
        ggplot2::geom_point() + ggplot2::geom_line() +
        ggplot2::facet_wrap(~variable) + ggplot2::theme_bw()

    n[ , Iterations_Paper := Iterations / Papers]
    l$p2 = ggplot2::ggplot(n, aes(x = quarter , y = Iterations_Paper )) +
        ggplot2::geom_point() + ggplot2::geom_line() + ggplot2::theme_bw()

}

date_labeller <- function(start = "2020-01-01", stop = "2024-01-01"){
    seq(as.Date(start), as.Date(stop), by = "6 months")

}

#' give clean_list
plot_hours_iters_paper <-function(){
    x = clean_list()
    stopifnot(is.list(x))
    l = list()
    ii = x$iterations[hours_spent < 6, .(hours_spent = median(hours_spent, na.rm= TRUE)), by = arrival_date]
    ii[, hours_smooth := data.table::frollmean(hours_spent,n = 11,na.rm = TRUE)]

    l$hours_i = ggplot2::ggplot(x$iterations[hours_spent < 6], aes(x = arrival_date, y = hours_spent)) + geom_point(size = 0.5) +
        ggplot2::geom_line(data = ii, aes(x = arrival_date,y = hours_smooth), size = 1, color = "red") + theme_bw() +
        ggplot2::scale_x_date("Arrival Date",breaks = date_labeller() ,date_labels = "%Y-%m",
                     limits = c(as.Date("2020-01-01"), NA)) +
        ggplot2::geom_vline(xintercept = as.Date("2023-07-01")) + ggplot2::ggtitle("Hours Per Iteration", subtitle = glue::glue("Dropping {nrow(x$iterations[hours_spent>=6])} Outliers"))

    pp = x$papers[ hours_spent < 10,.(hours_spent = median(hours_spent, na.rm= TRUE)), by = arrival_date]
    pp[, hours_spent := data.table::frollmean(hours_spent,n = 11,na.rm = TRUE)]

    ip = x$papers[,.(iters_paper = mean(iterations_paper, na.rm= TRUE)), by = arrival_date]
    ip[, iterations_paper := data.table::frollmean(iters_paper,n = 11,na.rm = TRUE)]


    l$hours_p = ggplot2::ggplot(x$papers[ hours_spent < 10], aes(x = arrival_date, y = hours_spent)) + geom_point(size = 0.5) +
        ggplot2::geom_line(data = pp, aes(x = arrival_date, y = hours_spent), color = "red",size = 1) + theme_bw() +
        ggplot2::scale_x_date("Arrival Date",limits = c(as.Date("2020-01-01"), NA), breaks = date_labeller()  ,date_labels = "%Y-%m") +
        ggplot2::geom_vline(xintercept = as.Date("2023-07-01")) + ggplot2::ggtitle("Hours Per Paper", subtitle = glue::glue("Dropping {nrow(x$papers[hours_spent >= 10])} Outliers"))

    l$iters_p = ggplot2::ggplot(x$papers, aes(x = arrival_date, y = iterations_paper)) + geom_point(size = 0.5) +
        ggplot2::geom_line(data = ip, aes(x = arrival_date, y = iterations_paper)  , color = "red",size = 1) + ggplot2::theme_bw() +
        ggplot2::scale_x_date("Arrival Date",limits = c(as.Date("2020-01-01"), NA),breaks = date_labeller()  ,date_labels = "%Y-%m") +
        ggplot2::ggtitle("Iterations Per Paper")


    l
}


plot_proctime <- function(x, by = "Iteration"){

    if (by == "Iteration"){
        labs = c("Assignment","Replication","Decision", "Resubmission")
        mr = data.table::melt(x, id.vars = c("arrival_date","ms","round"),
                              measure.vars = c("time_assign",
                                               "cumtime_replication",
                                               "cumtime_decision",
                                               "cumtime_resubmission"),
                              value.name =  "Days")
        mr = mr[Days < 50 & arrival_date > "2020-01-01" & Days >= 0]

        # mean hours by arrival date
        h = x[, .(hours = mean(hours_spent, na.rm = TRUE)), by = arrival_date]
        h[, smooth := data.table::frollmean(hours,n = 30,na.rm = TRUE)]

        hline_v = 15



    } else {
        # by paper
        labs = c("Assignment","Replication","Decision", "Resubmission")
        mr = data.table::melt(x, id.vars = c("arrival_date","ms"),
                              measure.vars = c("time_assign_paper",
                                               "time_replication_paper",
                                               "time_decision_paper",
                                               "time_resubmission_paper"),
                              value.name =  "Days")
        mr = mr[Days < 60 & arrival_date > "2020-01-01" & Days >= 0]

        hline_v = 25

    }
    mr[, arrival_date := as.Date(arrival_date)]
    mr = mr[!is.na(arrival_date)]
    setkey(mr, arrival_date,ms)

    # rejection
    haz = mr[, .(hazard = mean(Days,na.rm = TRUE)), by = .(arrival_date, variable)]
    haz[, smooth := data.table::frollmean(hazard,n = 40,na.rm = TRUE), by = .(variable)]

    # mean hours spent by arrival date
    # h = x[, .(hours = mean(hours_spent, na.rm = TRUE)), by = arrival_date]
    # h[, smooth := data.table::frollmean(hours,n = 30,na.rm = TRUE)]

    # base plot
    p = ggplot2::ggplot(mr, aes(x = arrival_date,y= Days, color = variable)) + ggplot2::geom_point(size = 0.5) +
        ggplot2::geom_line(data = haz, aes(x = arrival_date, y = smooth, color = variable),size = 1)



    #days
    days = p + ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_color_discrete(name = "Time Until:",
                           labels = labs) +
        ggplot2::scale_x_date("Arrival Date",limits = c(as.Date("2020-01-01"), NA),breaks = date_labeller()  ,date_labels = "%Y-%m") +
        ggplot2::geom_vline(xintercept = as.Date("2023-07-01")) +
        ggplot2::geom_hline(yintercept = hline_v, linetype = "dashed") +
        ggplot2::scale_y_continuous(breaks = c(0,15,20,25,40,60)) +
        ggplot2::ggtitle(glue::glue("Processing Time By {by}"))

    # #hours
    # l$hours = p + ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom") +
    #     scale_color_discrete(name = "Time Until:",
    #                          labels = labs) +
    #     scale_x_date("Arrival Date",breaks = seq(as.Date("2020-01-01"), as.Date("2023-10-01"), by = "6 months") ,date_labels = "%Y-%m") +
    #     geom_vline(xintercept = as.Date("2023-07-01")) +
    #     geom_hline(yintercept = 15, linetype = "dashed") +
    #     ggtitle(glue::glue("Processing Time By {by}"))
    days
}

plot_software <- function(){
    l = data.table::melt(list_software()$time_series, id.vars = "completed_quarter")
    ls = subset(l,
                variable %in% c("stata","R","python","matlab","ArcGIS_QGIS","fortran","mathematica","julia") &
                completed_quarter > "2020-01-01")

    ggplot(ls, aes(x = completed_quarter, y = value, color = variable)) +
        geom_line(linewidth = 1) + theme_bw() +
        ggplot2::scale_y_log10(name = "proportion [log scale]")


}

#' share of software used
#'
list_software <- function( ){
    y = clean_list()$iterations

    x = y[(completed), .(date_completed, software,ms,round)]
    x[, final := round == max(round), by = ms]
    x = x[(final)]

    x[ , stata := grepl("STATA", software, ignore.case = TRUE)]
    x[ , R := grepl("\\bR\\b|RSTUDIO|RSTAN", software, ignore.case = TRUE)]
    x[ , python := grepl("PYTHON", software, ignore.case = TRUE)]
    x[ , matlab := grepl("MATLAB", software, ignore.case = TRUE)]
    x[ , GAMS := grepl("GAMS", software, ignore.case = TRUE)]
    x[ , ArcGIS_QGIS := grepl("ARCGIS|QGIS", software, ignore.case = TRUE)]
    x[ , fortran := grepl("FORTRAN", software, ignore.case = TRUE)]
    x[ , cpp := grepl("C\\+\\+", software, ignore.case = TRUE)]
    x[ , mathematica := grepl("MATHEMATICA|WOLFRAM", software, ignore.case = TRUE)]
    x[ , julia := grepl("JULIA", software, ignore.case = TRUE)]
    x[ , excel := grepl("EXCEL", software, ignore.case = TRUE)]
    x[ , SAS := grepl("SAS", software, ignore.case = TRUE)]
    x[ , dynare := grepl("DYNARE", software, ignore.case = TRUE)]
    x[ , ztree := grepl("ZTREE|Z\\-TREE", software, ignore.case = TRUE)]
    x[ , GAUSS := grepl("GAUSS", software, ignore.case = TRUE)]

    x[, completed_quarter := zoo::as.yearqtr(date_completed)]
    x[ , software :=NULL]
    x[ , date_completed :=NULL]
    x[ , ms :=NULL]
    x[ , round :=NULL]

    # overall proportions
    d = x[, lapply(.SD, mean,na.rm = TRUE), .SDcols = stata:ztree]
    dd = data.table(Proportion = t(d), Software = names(d))
    data.table::setnames(dd, "Proportion.V1", "Proportion")
    dd = rbind(dd, data.table(Proportion = dd[Proportion < 0.006, sum(Proportion)], Software = "Other"))
    dd = dd[Proportion >= 0.006]
    dd = dd[order(Proportion, decreasing = TRUE)]
    dd = rbind(dd[-which(Software == "Other")], dd[which(Software == "Other")])
    dd[, Proportion := round(Proportion, 3)]

    # time series of software
    ts = x[, lapply(.SD, mean,na.rm = TRUE), by = completed_quarter]

    data.table::setcolorder(dd, neworder = c("Software","Proportion"))
    list(table = dd, time_series = ts)
}

#' Plot Data Statements
#'
plot_DS <- function(x){

    # group sub cases of DS together somehow:
    x[, ds_A := grepl("^[^(T|S|P)]*[A][^(T|S|P)]*$", data_statement)]
    x[, ds_AS := grepl("^[^(P|T)]*A[^(P|T)]*$", data_statement) * grepl("^[^(P|T)]*S[^(P|T)]*$", data_statement)]
    x[, ds_AT := grepl("^[^(P|S)]*A[^(P|S)]*$", data_statement) * grepl("^[^(P|S)]*T[^(P|S)]*$", data_statement)]
    x[, ds_T := grepl("^[^(A|S|P)]*[T][^(A|S|P)]*$", data_statement)]
    x[, ds_P := grepl("P", data_statement)]  # any P
    # x[, ds_AP := grepl("^[^(P|T)]*A[^(P|T)]*$", data_statement) * grepl("^[^(P|T)]*S[^(P|T)]*$", data_statement)]
    # x[, ds_ATS := grepl("A", data_statement) * grepl("S", data_statement) * grepl("T", data_statement)]
    # x[, ds_ATP := grepl("A", data_statement) * grepl("P", data_statement) * grepl("T", data_statement)]

    x[, completed := status %in% c("AP","NT", "P", "p"), by = ms]
    x[, arrival_quarter := zoo::as.yearqtr(arrival_date)]

    pd = x[(completed), lapply(.SD, mean,na.rm = TRUE), by = arrival_quarter,.SDcols = ds_A:ds_P]
    m = data.table::melt(pd, id.vars = "arrival_quarter", value.name = "Share")

    facet_names = c(
        ds_A = "Data 100% Available",
        ds_AS = "Some Data, Some Simulation",
        ds_AT = "Some Data, Some Temp. Access",
        ds_T = "Temp. Access Only",
        ds_P = "Partial Replication Only"

    )
    ggplot2::ggplot(m, aes(x = arrival_quarter, y = Share)) + ggplot2::geom_line() +
        ggplot2::facet_wrap(~variable,ncol = 1, labeller = ggplot2::as_labeller(facet_names)) + ggplot2::theme_bw()
}



plot_hazard <- function(x){
    x[, round := factor(sround)]
    x[, arrival_date := as.Date(arrival_date)]

    m = x[, .(mh = mean(hazard_smooth, na.rm = TRUE)),by = round]

    # data.table::setnames(x, "hazard_smooth", "hazard")
    ggplot2::ggplot(x, aes(x = arrival_date, y = hazard_smooth, color = round)) + ggplot2::geom_line() +
        ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom") +
        ggplot2::geom_vline(xintercept = as.Date("2023-07-01"),size = 1) +
        ggplot2::geom_line(aes(y = ave(hazard_smooth, round, FUN = function(x){mean(x,na.rm = TRUE)})),size = 1) +
        ggplot2::ggtitle("Rejection Hazard by Round of Iteration")

}
