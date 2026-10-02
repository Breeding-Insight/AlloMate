#' ORCID sign-in with a Firestore approved list (Cloud Run only)
#'
#' Follows HapApp's design:
#' - `/auth/login` clears any existing AlloMate session, sets a random `state`
#'   cookie and redirects to ORCID.
#' - `/auth/callback` checks `state`, exchanges the code for the ORCID iD, and
#'   signs the person in only if Firestore `users/{orcid_id}` has
#'   `is_active: true`.
#' - The session is a signed (HMAC-SHA256) cookie; nothing is stored on the
#'   server, so any instance can check it.
#'
#' @noRd
NULL

AUTH_SESSION_COOKIE <- "allomate_session"
AUTH_STATE_COOKIE   <- "allomate_oauth_state"
AUTH_CONTACT_EMAIL  <- "bi-science-team@ufl.edu"

.cloud_cache <- new.env(parent = emptyenv())

#' Local development only: skip ORCID when ALLOMATE_AUTH_BYPASS is set.
#' Never honoured on Cloud Run.
#' @noRd
auth_bypass_enabled <- function() {
  tolower(Sys.getenv("ALLOMATE_AUTH_BYPASS")) %in% c("1", "true", "yes") && !on_cloud_run()
}

auth_session_seconds <- function() {
  as.numeric(cloud_setting("session_hours", 12)) * 3600
}

# ── Cookies ──────────────────────────────────────────────────────────────────

#' Parse a Cookie request header into a named list
#' @noRd
parse_cookies <- function(header) {
  if (is.null(header) || !nzchar(header)) return(list())
  parts <- trimws(strsplit(header, ";", fixed = TRUE)[[1]])
  parts <- parts[grepl("=", parts, fixed = TRUE)]
  keys <- sub("=.*$", "", parts)
  values <- sub("^[^=]*=", "", parts)
  stats::setNames(as.list(values), keys)
}

#' Build a Set-Cookie header value
#' @noRd
set_cookie <- function(name, value, max_age, path = "/", secure = TRUE) {
  paste0(
    name, "=", value,
    "; Path=", path,
    "; Max-Age=", as.integer(max_age),
    "; HttpOnly; SameSite=Lax",
    if (secure) "; Secure" else ""
  )
}

clear_cookie <- function(name, path = "/", secure = TRUE) {
  set_cookie(name, "", 0, path = path, secure = secure)
}

constant_time_equal <- function(a, b) {
  a <- charToRaw(a)
  b <- charToRaw(b)
  if (length(a) != length(b)) return(FALSE)
  all(xor(a, b) == as.raw(0))
}

auth_sign <- function(payload, key) {
  as.character(openssl::sha256(payload, key = key))
}

#' Create the signed session cookie value
#' @noRd
encode_session <- function(orcid_id, name, key, now = Sys.time()) {
  body <- jsonlite::toJSON(
    list(orcid = orcid_id, name = name, exp = as.numeric(now) + auth_session_seconds()),
    auto_unbox = TRUE
  )
  payload <- jsonlite::base64url_enc(as.character(body))
  paste0(payload, ".", auth_sign(payload, key))
}

#' Check a session cookie value; returns list(orcid, name) or NULL
#' @noRd
decode_session <- function(value, key, now = Sys.time()) {
  if (is.null(value) || !nzchar(value) || !nzchar(key)) return(NULL)
  parts <- strsplit(value, ".", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(NULL)
  if (!constant_time_equal(auth_sign(parts[1], key), parts[2])) return(NULL)
  body <- tryCatch(
    jsonlite::fromJSON(rawToChar(jsonlite::base64url_dec(parts[1]))),
    error = function(e) NULL
  )
  if (is.null(body) || is.null(body$exp) || as.numeric(now) > body$exp) return(NULL)
  list(orcid = body$orcid, name = body$name %||% "")
}

#' The signed-in person for an HTTP request or Shiny session, or NULL
#' @noRd
auth_user_from_cookies <- function(cookie_header) {
  if (auth_bypass_enabled()) {
    return(list(orcid = "0000-0000-0000-0000", name = "Local development"))
  }
  cookies <- parse_cookies(cookie_header)
  decode_session(cookies[[AUTH_SESSION_COOKIE]], Sys.getenv("SECRET_KEY"))
}

auth_session_user <- function(session) {
  auth_user_from_cookies(session$request$HTTP_COOKIE)
}

#' Signed-in name and a Sign out link for the navbar
#' @noRd
cloud_navbar_items <- function(request) {
  user <- auth_user_from_cookies(request$HTTP_COOKIE)
  label <- if (!is.null(user) && nzchar(user$name %||% "")) user$name else user$orcid %||% ""
  shiny::tags$li(
    class = "nav-item dropdown",
    style = "display: flex; align-items: center;",
    shiny::span(class = "nav-link", style = "color: #6c757d;", label),
    shiny::tags$a(class = "nav-link", href = "/auth/logout", "Sign out")
  )
}

# ── URLs and responses ───────────────────────────────────────────────────────

#' Public origin of the service, e.g. https://allomate-xyz.run.app
#'
#' Uses ALLOMATE_PUBLIC_URL when set; otherwise the Host and
#' X-Forwarded-Proto headers that Cloud Run sends.
#' @noRd
auth_public_url <- function(req) {
  configured <- cloud_env("ALLOMATE_PUBLIC_URL")
  if (nzchar(configured)) return(sub("/+$", "", configured))
  proto <- req$HTTP_X_FORWARDED_PROTO %||% "http"
  proto <- trimws(strsplit(proto, ",", fixed = TRUE)[[1]][1])
  paste0(proto, "://", req$HTTP_HOST %||% "localhost")
}

auth_callback_url <- function(req) paste0(auth_public_url(req), "/auth/callback")

auth_secure_cookies <- function(req) startsWith(auth_public_url(req), "https://")

orcid_base_url <- function() sub("/+$", "", cloud_env("ORCID_BASE_URL", "https://orcid.org"))

#' A redirect response; `cookies` are Set-Cookie header values
#' @noRd
http_redirect <- function(location, cookies = character()) {
  headers <- list(Location = location, `Cache-Control` = "no-store")
  for (cookie in cookies) headers <- c(headers, list(`Set-Cookie` = cookie))
  shiny::httpResponse(302, content_type = "text/plain", content = "", headers = headers)
}

#' A small, plain HTML page for sign-in problems
#' @noRd
auth_message_page <- function(status, heading, message, action_href = "/auth/login",
                              action_label = "Try signing in again", cookies = character()) {
  esc <- htmltools::htmlEscape
  contact <- sprintf('<a href="mailto:%1$s">%1$s</a>', AUTH_CONTACT_EMAIL)
  html <- sprintf(
    '<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="robots" content="noindex">
<title>AlloMate sign-in</title>
<style>
body{font-family:system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;background:#f5f7f8;color:#1f2a30;margin:0;padding:48px 16px}
main{max-width:560px;margin:0 auto;background:#fff;border:1px solid #dde3e6;border-radius:10px;padding:28px}
h1{font-size:1.4rem;margin:0 0 12px}p{line-height:1.55}
a{color:#2A6576}.btn{display:inline-block;margin-top:8px;padding:10px 16px;border-radius:6px;background:#2A6576;color:#fff;text-decoration:none}
.btn:focus-visible,a:focus-visible{outline:3px solid #EFB526;outline-offset:2px}
</style></head><body><main>
<h1>%s</h1><p>%s</p><p>If you need help, contact %s.</p>
<a class="btn" href="%s">%s</a>
</main></body></html>',
    esc(heading), esc(message), contact, esc(action_href), esc(action_label)
  )
  headers <- list(`Cache-Control` = "no-store")
  for (cookie in cookies) headers <- c(headers, list(`Set-Cookie` = cookie))
  shiny::httpResponse(status, content_type = "text/html; charset=UTF-8", content = html, headers = headers)
}

# ── Routes ───────────────────────────────────────────────────────────────────

#' GET /auth/login
#' @noRd
auth_login <- function(req) {
  if (auth_bypass_enabled()) return(http_redirect("/"))
  secure <- auth_secure_cookies(req)
  state <- paste(as.character(openssl::rand_bytes(32)), collapse = "")
  query <- paste0(
    "client_id=", utils::URLencode(Sys.getenv("ORCID_CLIENT_ID"), reserved = TRUE),
    "&response_type=code",
    "&scope=", utils::URLencode("/authenticate", reserved = TRUE),
    "&redirect_uri=", utils::URLencode(auth_callback_url(req), reserved = TRUE),
    "&state=", state
  )
  http_redirect(
    paste0(orcid_base_url(), "/oauth/authorize?", query),
    cookies = c(
      # Start every sign-in without an identity, so a rejected or failed
      # callback cannot leave an earlier approved account signed in.
      clear_cookie(AUTH_SESSION_COOKIE, secure = secure),
      set_cookie(AUTH_STATE_COOKIE, state, 600, path = "/auth", secure = secure)
    )
  )
}

#' GET /auth/callback
#' @noRd
auth_callback <- function(req) {
  secure <- auth_secure_cookies(req)
  clear_state <- clear_cookie(AUTH_STATE_COOKIE, path = "/auth", secure = secure)
  query <- shiny::parseQueryString(req$QUERY_STRING %||% "")
  expected <- parse_cookies(req$HTTP_COOKIE)[[AUTH_STATE_COOKIE]]

  if (is.null(expected) || is.null(query$state) || !constant_time_equal(expected, query$state)) {
    return(auth_message_page(400, "Sign-in expired",
      "Your sign-in could not be completed. Please start again.", cookies = clear_state))
  }
  if (is.null(query$code) || !nzchar(query$code)) {
    return(auth_message_page(400, "Sign-in cancelled",
      query$error_description %||% "ORCID did not authorize the sign-in.", cookies = clear_state))
  }

  token <- tryCatch(orcid_exchange_code(query$code, auth_callback_url(req)), error = function(e) e)
  if (inherits(token, "error") || is.null(token$orcid)) {
    message("AlloMate sign-in: ORCID token exchange failed: ",
            if (inherits(token, "error")) conditionMessage(token) else "no ORCID iD returned")
    return(auth_message_page(502, "Sign-in failed",
      "AlloMate could not confirm your ORCID iD. Please try again.", cookies = clear_state))
  }

  approved <- tryCatch(firestore_user_is_active(token$orcid), error = function(e) e)
  if (inherits(approved, "error")) {
    message("AlloMate sign-in: Firestore lookup failed: ", conditionMessage(approved))
    return(auth_message_page(503, "Sign-in unavailable",
      "AlloMate could not check your access right now. Please try again in a few minutes.",
      cookies = clear_state))
  }
  if (!isTRUE(approved)) {
    return(auth_message_page(403, "Access not approved yet",
      sprintf("The ORCID iD %s is not approved to use AlloMate. To request access, email %s with your ORCID iD.",
              token$orcid, AUTH_CONTACT_EMAIL),
      action_href = cloud_env("ALLOMATE_LANDING_URL", "/auth/login"),
      action_label = "Back to AlloMate", cookies = clear_state))
  }

  session_value <- encode_session(token$orcid, token$name %||% "", Sys.getenv("SECRET_KEY"))
  http_redirect("/", cookies = c(
    clear_state,
    set_cookie(AUTH_SESSION_COOKIE, session_value, auth_session_seconds(), secure = secure)
  ))
}

#' GET /auth/logout
#' @noRd
auth_logout <- function(req) {
  http_redirect(
    cloud_env("ALLOMATE_LANDING_URL", "/auth/login"),
    cookies = clear_cookie(AUTH_SESSION_COOKIE, secure = auth_secure_cookies(req))
  )
}

# ── ORCID and Firestore ──────────────────────────────────────────────────────

#' Exchange an ORCID authorization code; returns list(orcid, name)
#' @noRd
orcid_exchange_code <- function(code, redirect_uri) {
  req <- httr2::request(paste0(orcid_base_url(), "/oauth/token"))
  req <- httr2::req_body_form(
    req,
    client_id     = Sys.getenv("ORCID_CLIENT_ID"),
    client_secret = Sys.getenv("ORCID_CLIENT_SECRET"),
    grant_type    = "authorization_code",
    code          = code,
    redirect_uri  = redirect_uri
  )
  req <- httr2::req_headers(req, Accept = "application/json")
  req <- httr2::req_timeout(req, 15)
  body <- httr2::resp_body_json(httr2::req_perform(req))
  list(orcid = body$orcid, name = body$name)
}

#' Google access token from the Cloud Run metadata server (cached until expiry)
#'
#' For local testing against a real Firestore database, set
#' GOOGLE_ACCESS_TOKEN (e.g. from `gcloud auth print-access-token`).
#' @noRd
gcp_access_token <- function() {
  local_token <- cloud_env("GOOGLE_ACCESS_TOKEN")
  if (nzchar(local_token)) return(local_token)
  cached <- .cloud_cache$token
  if (!is.null(cached) && Sys.time() < cached$expires) return(cached$value)
  req <- httr2::request(
    "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token"
  )
  req <- httr2::req_headers(req, `Metadata-Flavor` = "Google")
  req <- httr2::req_timeout(req, 5)
  body <- httr2::resp_body_json(httr2::req_perform(req))
  .cloud_cache$token <- list(
    value = body$access_token,
    expires = Sys.time() + max(0, body$expires_in - 60)
  )
  body$access_token
}

gcp_project_id <- function() {
  project <- cloud_env("GOOGLE_CLOUD_PROJECT")
  if (nzchar(project)) return(project)
  if (is.null(.cloud_cache$project)) {
    req <- httr2::request("http://metadata.google.internal/computeMetadata/v1/project/project-id")
    req <- httr2::req_headers(req, `Metadata-Flavor` = "Google")
    req <- httr2::req_timeout(req, 5)
    .cloud_cache$project <- httr2::resp_body_string(httr2::req_perform(req))
  }
  .cloud_cache$project
}

#' TRUE when Firestore users/{orcid_id} exists with is_active: true
#' @noRd
firestore_user_is_active <- function(orcid_id) {
  if (!grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", orcid_id)) return(FALSE)
  url <- sprintf(
    "https://firestore.googleapis.com/v1/projects/%s/databases/%s/documents/users/%s",
    gcp_project_id(), Sys.getenv("FIRESTORE_DATABASE"), orcid_id
  )
  req <- httr2::request(url)
  req <- httr2::req_auth_bearer_token(req, gcp_access_token())
  req <- httr2::req_timeout(req, 10)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status == 404) return(FALSE)
  if (status != 200) stop("Firestore returned HTTP ", status)
  fields <- httr2::resp_body_json(resp)$fields
  isTRUE(fields$is_active$booleanValue)
}
