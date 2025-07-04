

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

ej_filerequest <- function(first,address,address2,ms,url,draft = TRUE){
    email <- gmailr::gm_mime() |>
        gmailr::gm_to(make_email_addresses(address,address2)) |>
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
ej_randr <- function(firstname,lastname,address,address2,ms,title,url,revision, attachment = TRUE){

    report_path = file.path(ej_db_processed(),
                            paste0(paste(lastname,ms,paste0("R",revision),sep = "-"),".pdf"))
    if ((!(file.exists(report_path))) & attachment){
        stop(paste(report_path, " does not exist - you need to first save the pdf of the report in the dropbox"))
    }

    # gmailr::gm_auth()  # authenticate with gmail

    if (attachment){

        email <- gmailr::gm_mime() |>
            gmailr::gm_to(make_email_addresses(address,address2,eof = FALSE)) |>
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

<ol>
  <li>Item 1</li>
  <li>Item 2.</li>
</ol>

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

    } else {

        email <- gmailr::gm_mime() |>
            gmailr::gm_to(make_email_addresses(address,address2,eof = FALSE)) |>
            gmailr::gm_from("'EJ Data Editor' <ejdataeditor@gmail.com>") |>
            gmailr::gm_subject("EJ Reproducibility Checks Results") |>
            gmailr::gm_html_body(
                glue::glue("Dear {firstname},
            <br>
            <br>
            Thank you for providing us with your final files and replication
            package for your paper {ms}, titled '{title}'.
            I am asking you for a quick resubmission of your package based the following points - we don't provide
            our usual full report in this instance.
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
Please reply to this email in response to my points above
<br>
<br>


I would like to thank you for your cooperation and I
look forward to receiving your revised package.
<br>
<br>


Kind regards,<br>

Florian",
                           ej_signature()))


    }



    # build email and create draft
    if (attachment){
        email |>
            gmailr::gm_attach_file(report_path) |>
            gmailr::gm_create_draft()

    } else {
        email |>
            gmailr::gm_create_draft()
    }

}

make_email_addresses <- function(address, address2, eof = TRUE){
    if (address2 != ""){
        addresses <- c(address,address2)
    } else {
            addresses <- c(address)
    }
    if (eof){
        addresses <- c(addresses,"ej@editorialoffice.co.uk")
    }
    addresses
}

#' EJ package on zenodo email
#'
#' create email that package is good to go.
#'
ej_zg2g <- function(firstname,lastname,address,address2 = NULL,ms,revision){

    pnumber = ej_paper_number(lastname,ms,revision)


    email <- gmailr::gm_mime() |>
        gmailr::gm_to(make_email_addresses(address,address2)) |>
        gmailr::gm_from("'EJ Data Editor' <ejdataeditor@gmail.com>") |>
        gmailr::gm_subject(glue::glue("EJ Data Editor: {pnumber} is on zenodo!")) |>
        gmailr::gm_html_body(
            glue::glue("Dear {firstname},
            <br>
            <br>
            I have successfully verified that your package on <code>zenodo.org</code>
            is identical to the lastest version in our system, therefore I have
            accepted it into our zenodo community.
            I have also logged the corresponding DOI, which means:
<br>
<br>
<b>You have successfully completed all reproducibility checks at EJ!</b> 🎉
<br>
<br>

The Editorial Office will handle the few remaining steps from here on.
 <br>
 <br>
Congratulations!
<br>
<br>

Best wishes,<br>

Florian",
                       ej_signature()))

    # build email and create draft
    email |>
        gmailr::gm_send_message()
}


#' EJ Good to Go
#'
#' create email that package is good to go.
#'
ej_g2g <- function(firstname,lastname,address,address2,ms,revision, draft = FALSE){

    pnumber = ej_paper_number(lastname,ms,revision)

    email <- gmailr::gm_mime() |>
        gmailr::gm_to(make_email_addresses(address,address2)) |>
        gmailr::gm_from("'EJ Data Editor' <ejdataeditor@gmail.com>") |>
        gmailr::gm_subject(glue::glue("EJ Data Editor: {pnumber} is good to go!")) |>
        gmailr::gm_html_body(
            glue::glue("Dear {firstname},
            <br>
            <br>
            I am happy to tell you that your package {pnumber} is good to go from my end! 🚀
            <br>
            Two things are going to happen concurrently now:
            <br>

<ol>
  <li>The Editorial Office will perform some quick anti-plagiarism checks on the paper.</li>
  <li>You are requested to upload your package at the repository of the EJ Community
  on https://zenodo.org/. Please follow the step by step instructions on the
  EJ Data Editor Website (https://ejdataeditor.github.io/package.html#after-the-reproducibility-checks-are-completed-publish-your-package).</li>
</ol>
<br>
It is of great importance that you do not modify the files in your submitted package anymore. We will check the final version of the package you sent us against what you will publish on Zenodo in a very strict (and automated) fashion.
Unless the files on Zenodo are <i>exactly identical</i> to ours, this check will fail.
Please remove the letter to the data editor before you submit - and do <b>not</b> include any confidential data.
<br>

After these steps are completed, your files will be sent back to your editor for final acceptance.
All this should happen before too long.
In the meantime, if you have any queries regarding the publication process, please contact ej@res.org.uk.

<br>
<br>

Thank you again for your cooperation, and congratulations on a very fine replication package indeed.

<br>
<br>

Kind regards,<br>

Florian",
                       ej_signature()))

    if (draft){
        gmailr::gm_create_draft(email)
    } else {
        gmailr::gm_send_message(email)
    }
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
