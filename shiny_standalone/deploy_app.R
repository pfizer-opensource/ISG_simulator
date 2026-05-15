app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

account <- Sys.getenv("RSCONNECT_ACCOUNT")
app_name <- Sys.getenv("RSCONNECT_APPNAME", "safety-pharm-design-simulater")
token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")

if (!nzchar(account)) {
  stop("Set RSCONNECT_ACCOUNT before deploying.")
}

if (!nzchar(token) || !nzchar(secret)) {
  stop("Set SHINYAPPS_TOKEN and SHINYAPPS_SECRET before deploying.")
}

rsconnect::setAccountInfo(
  name = account,
  token = token,
  secret = secret
)

rsconnect::deployApp(
  appDir = app_dir,
  appPrimaryDoc = "app.R",
  account = account,
  server = "shinyapps.io",
  appName = app_name,
  forceUpdate = TRUE
)