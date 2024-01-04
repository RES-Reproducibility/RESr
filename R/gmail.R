

# gmailr::gm_auth_configure()
# gmailr::gm_oauth_client()
# gmailr::gm_auth()

# do auth once per session
ej_auth <- function() {gmailr::gm_auth(cache = TRUE)}  # authenticate with gmail


ej_db_processed <- function(){
    file.path(Sys.getenv("R_DB_EJ"),"EJ-3-replication-reports","DE-processed")
}

ej_paper_number <- function(author,ms,rev){
    paste(author,ms,paste0("R",rev),sep = "-")
}

ej_db_submitted_path <- function(author,ms,rev, full = FALSE){
    p = file.path("EJ-2-submitted-replication-packages",ej_paper_number(author,ms,rev ))

    if (full) {
        return(file.path(Sys.getenv("R_DB_EJ"),p))
    } else {
        return(p)
    }
}

ej_testmail <- function(){
    email <- gmailr::gm_mime() |>
        gmailr::gm_to("'Flo' <florian.oswald@gmail.com>") |>
        gmailr::gm_from("'EJ Data Editor' <ejdataeditor@gmail.com>") |>
        gmailr::gm_subject("Gmailr is a very handy package!") |>
        gmailr::gm_html_body(paste("<b>Gmailr</b> is a <i>very</i> handy package!",ej_signature()))

    gmailr::gm_send_message(email)
}

ej_filerequest <- function(first,address,ms,url,draft = TRUE){
    email <- gmailr::gm_mime() |>
        gmailr::gm_to(address) |>
        gmailr::gm_from("'EJ Data Editor' <ejdataeditor@gmail.com>") |>
        gmailr::gm_subject("EJ Replication Package Upload Request") |>
        gmailr::gm_html_body(
            glue::glue("Dear {first},
            <br>
            <br>
            I am the Data Editor of EJ.
            I would like to invite you to submit your replication package as a single zip
            file via this dropbox file request link:
            <br>
            <br>
           {url}
            <br>
            <br>

Please notice the required contents of your package:
<br>

https://ejdataeditor.github.io/package.html#preliminaries
           <br>
            <br>

I'm looking forward to receiving your package.
           <br>
            <br>

Best wishes,<br>
Florian",
                  ej_signature()))
    if (draft) {
        gmailr::gm_create_draft(email)
    } else {
        gmailr::gm_send_message(email)

    }
}

#' create draft email to author about R&R
#'
#' craete email, attach report
#' then create subdir `old` in their uplaod folder
#' so that they can upload the new package using the same url.
#'
ej_randr <- function(firstname,lastname,address,ms,title,url,revision){

    report_path = file.path(ej_db_processed(),
                            paste0(paste(lastname,ms,paste0("R",revision),sep = "-"),".pdf"))
    if (!(file.exists(report_path))){
        stop(paste(report_path, " does not exist."))
    }

    # gmailr::gm_auth()  # authenticate with gmail

    email <- gmailr::gm_mime() |>
        gmailr::gm_to(address) |>
        gmailr::gm_from("'EJ Data Editor' <ejdataeditor@gmail.com>") |>
        gmailr::gm_subject("EJ Reproducibility Checks Results") |>
        gmailr::gm_html_body(
            glue::glue("Dear {firstname},
            <br>
            <br>
            Thank you for providing us with your final files and replication
            package for your paper {ms}, titled '{title}'.
            Please find attached the report with the outcome of our checks.
            As you will see in the report, the reproducibility team has identified
            a few issues that need to be fixed.
            <br>
            <br>

<ol>
  <li>Item 1</li>
  <li>Item 2.</li>
</ol>
<br>
Could you please address these issues and resubmit your final files (including your paper, appendix and replication package) like before? Please use this link:<br><br>

                       {url}

<br>
<br>

We need you to submit the entire package again because updating
the replication package ourselves increases the potential risk
that the files you intend to submit for possible publication may be mishandled.

<br>
<br>

Please also submit a letter addressed to me explaining how you
dealt with each of the issues raised in the report.
Once your final files are received, these will be returned
to the reproducibility team for another check before being
sent to the editor in charge.
<br>
<br>

I would like to thank you for your cooperation and I
look forward to receiving your revised package.
<br>
<br>

Kind regards,<br>

Florian",
                       ej_signature()))

    # build email and create draft
    email |>
        gmailr::gm_attach_file(report_path) |>
        gmailr::gm_create_draft()
}


ej_replicator_assignment <- function(firstname,address,authorlast,ms,revision, back = FALSE, draft = FALSE){

    pnumber = ej_paper_number(authorlast,ms,revision)
    fullurl = ej_db_submitted_path(authorlast,ms,revision,full = TRUE)
    if (!(file.exists(fullurl))){
        stop(paste(fullurl, " does not exist."))
    }

    # shortened url to include in email.
    url = ej_db_submitted_path(authorlast,ms,revision)

    if (back) {
        message_body <- glue::glue(
            "Hi {firstname},
    <br>
    <br>
    <b>Caution: New reporting template! Instructions below!</b>
    <br>
    The <i>{authorlast}</i> paper is back.
    <br>
    <br>
    You can download the package from the EJ dropbox at
    <br>
    <br>
    <code>{url}</code>
    <br>
    <br>

    <b>Important:</b> For your report,
    please follow the naming convention <code>{pnumber}.docx</code> (or <code>{pnumber}.qmd</code> - I need to be able to edit your report, so don't return a <code>.pdf</code> document.)
    <br>
    <br>

    <b>Reminders:</b>
    <br>


    <ol>
    <li><a href=\"https://docs.google.com/spreadsheets/d/1C5Wck6rw9KlnRfck5JyJ2T1Y5kbzoW1pqX-TPWPpOIE/edit#gid=0\">here</a> is the google sheet with all relevant access links.</li>
    <li><a href=\"https://res-reproducibility.github.io/EJ-Workflow/\">here</a> is the current workflow (look at the Replicator section).</li>
    <li>You should get the latest version of our reporting template by downloading the zip archive from <a href=\"https://github.com/RES-Reproducibility/EJ-report-template/releases/latest\">this repository</a>.
    You will find different versions of the same document (docx, qmd, odt) - pick your favourite and return an editable version to me (not pdf!). Please return via email to this address. </li>
    </ol>
    <br>
    <br>
    Thanks,
    <br>
    Florian",
            ej_signature()
        )
    } else {

    message_body <- glue::glue(
    "Hi {firstname},
    <br>
    <br>
    <b>Caution: New reporting template! Instructions below!</b>
    <br>
    <br>
    I assigned you the <i>{authorlast}</i> paper for <i>The Economic Journal (EJ)</i>.
    <br>
    <br>
    You can download the package from the EJ dropbox at
    <br>
    <br>
    <code>{url}</code>
    <br>
    <br>

    Please reply on this email thread for related communications,
    in particular, to return your report. <b>Important:</b> For your report,
    please follow the naming convention <code>{pnumber}.docx</code> (or <code>{pnumber}.qmd</code> - I need to be able to edit your report, so don't return a <code>.pdf</code> document.)
    <br>
    <br>

    <b>Reminders:</b>
    <br>


    <ol>
    <li><a href=\"https://docs.google.com/spreadsheets/d/1C5Wck6rw9KlnRfck5JyJ2T1Y5kbzoW1pqX-TPWPpOIE/edit#gid=0\">here</a> is the google sheet with all relevant access links.</li>
    <li><a href=\"https://res-reproducibility.github.io/EJ-Workflow/\">here</a> is the current workflow (look at the Replicator section).</li>
    <li>You should get the latest version of our reporting template by downloading the zip archive from <a href=\"https://github.com/RES-Reproducibility/EJ-report-template/releases/latest\">this repository</a>.
    You will find different versions of the same document (docx, qmd, odt) - pick your favourite and return an editable version to me (not pdf!). Please return via email to this address. </li>
    </ol>

 <br>

    Don't hesitate to reach out on slack for any ongoing issues with the replication. Let's talk about computational requirements there etc.

    <br>
    <br>
    Thanks,
    <br>
    Florian",
    ej_signature()
    )
    }

    if (!back) {
        subject = glue::glue("I assigned you the {authorlast} paper (EJ)")
    } else {
        subject = glue::glue("The {authorlast} paper (EJ) is back")

    }

    email <- gmailr::gm_mime() |>
        gmailr::gm_to(address) |>
        gmailr::gm_from("'EJ Data Editor' <ejdataeditor@gmail.com>") |>
        gmailr::gm_subject(subject) |>
        gmailr::gm_html_body(body = message_body)
    if (draft){
        gmailr::gm_create_draft(email)
    } else {
        gmailr::gm_send_message(email)
    }

}



ej_signature <- function(){
    "
    <br>
    <br>
    --<br>
    Florian Oswald<br>
    Data Editor<br>
    The Economic Journal<br>
    email: ejdataeditor@gmail.com<br>
    web (EJ) : https://ejdataeditor.github.io/<br>
    web (personal) : https://floswald.github.io/
    "
}
