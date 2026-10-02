skip_if_not_installed("openssl")
skip_if_not_installed("jsonlite")

with_cloud_env <- function(code, landing = "https://example.org/AlloMate/") {
  old <- Sys.getenv(c("R_CONFIG_ACTIVE", "SECRET_KEY", "ORCID_CLIENT_ID",
                      "ALLOMATE_LANDING_URL", "ALLOMATE_PUBLIC_URL", "ALLOMATE_AUTH_BYPASS", "K_SERVICE"),
                    unset = NA)
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, as.list(old[name]))
    }
  }, add = TRUE)
  Sys.setenv(
    R_CONFIG_ACTIVE = "cloudrun",
    SECRET_KEY = strrep("k", 40),
    ORCID_CLIENT_ID = "APP-TEST",
    ALLOMATE_LANDING_URL = landing,
    ALLOMATE_PUBLIC_URL = "https://allomate.example.run.app"
  )
  Sys.unsetenv(c("ALLOMATE_AUTH_BYPASS", "K_SERVICE"))
  force(code)
}

fake_request <- function(path, cookie = NULL, query = "") {
  list(PATH_INFO = path, HTTP_COOKIE = cookie, QUERY_STRING = query,
       HTTP_HOST = "allomate.example.run.app", HTTP_X_FORWARDED_PROTO = "https")
}

header_values <- function(resp, name) unname(unlist(resp$headers[names(resp$headers) == name]))

test_that("cloud mode is off by default", {
  old <- Sys.getenv("R_CONFIG_ACTIVE", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("R_CONFIG_ACTIVE") else Sys.setenv(R_CONFIG_ACTIVE = old))
  Sys.unsetenv("R_CONFIG_ACTIVE")
  expect_false(AlloMate:::cloud_mode())
})

test_that("session cookies round-trip and reject tampering and expiry", {
  with_cloud_env({
    key <- Sys.getenv("SECRET_KEY")
    value <- AlloMate:::encode_session("0000-0002-1825-0097", "Ada", key)
    user <- AlloMate:::decode_session(value, key)
    expect_equal(user$orcid, "0000-0002-1825-0097")
    expect_equal(user$name, "Ada")

    expect_null(AlloMate:::decode_session(value, strrep("x", 40)))
    tampered <- paste0(substr(value, 1, nchar(value) - 1),
                       if (endsWith(value, "a")) "b" else "a")
    expect_null(AlloMate:::decode_session(tampered, key))
    expect_null(AlloMate:::decode_session(value, key, now = Sys.time() + 13 * 3600))
    expect_null(AlloMate:::decode_session("not-a-cookie", key))
  })
})

test_that("session cookies for long names have no line breaks", {
  with_cloud_env({
    value <- AlloMate:::encode_session("0000-0002-4762-3518", strrep("Long Display Name ", 6),
                                       Sys.getenv("SECRET_KEY"))
    # Cloud Run rejects a Set-Cookie header containing a line break or space.
    expect_match(value, "^[A-Za-z0-9_-]+\\.[0-9a-f]{64}$")
    expect_equal(AlloMate:::decode_session(value, Sys.getenv("SECRET_KEY"))$orcid, "0000-0002-4762-3518")
  })
})

test_that("health and robots.txt answer without sign-in", {
  with_cloud_env({
    health <- AlloMate:::cloud_route_request(fake_request("/health"))
    expect_equal(health$status, 200L)
    robots <- AlloMate:::cloud_route_request(fake_request("/robots.txt"))
    expect_equal(robots$status, 200L)
    expect_match(robots$content, "Disallow: /", fixed = TRUE)
  })
})

test_that("signed-out visitors go to the landing page; signed-in visitors get the app", {
  with_cloud_env({
    resp <- AlloMate:::cloud_route_request(fake_request("/"))
    expect_equal(resp$status, 302L)
    expect_equal(header_values(resp, "Location"), "https://example.org/AlloMate/")

    value <- AlloMate:::encode_session("0000-0002-1825-0097", "Ada", Sys.getenv("SECRET_KEY"))
    expect_null(AlloMate:::cloud_route_request(
      fake_request("/", cookie = paste0("other=1; allomate_session=", value))
    ))
  })
  with_cloud_env({
    resp <- AlloMate:::cloud_route_request(fake_request("/"))
    expect_equal(header_values(resp, "Location"), "/auth/login")
  }, landing = "")
})

test_that("sign-in starts by clearing the session and setting a state cookie", {
  with_cloud_env({
    resp <- AlloMate:::cloud_route_request(fake_request("/auth/login"))
    expect_equal(resp$status, 302L)
    location <- header_values(resp, "Location")
    expect_match(location, "^https://orcid.org/oauth/authorize\\?")
    expect_match(location, "redirect_uri=https%3A%2F%2Fallomate.example.run.app%2Fauth%2Fcallback")
    cookies <- header_values(resp, "Set-Cookie")
    expect_true(any(grepl("^allomate_session=; .*Max-Age=0", cookies)))
    expect_true(any(grepl("^allomate_oauth_state=[0-9a-f]{64}; Path=/auth", cookies)))
    expect_true(all(grepl("HttpOnly; SameSite=Lax; Secure", cookies, fixed = TRUE)))
  })
})

test_that("a callback without the matching state is refused", {
  with_cloud_env({
    resp <- AlloMate:::cloud_route_request(
      fake_request("/auth/callback", cookie = "allomate_oauth_state=abc", query = "?code=x&state=def")
    )
    expect_equal(resp$status, 400L)
    resp <- AlloMate:::cloud_route_request(fake_request("/auth/callback", query = "?code=x&state=def"))
    expect_equal(resp$status, 400L)
  })
})

test_that("sign-out clears the session and returns to the landing page", {
  with_cloud_env({
    resp <- AlloMate:::cloud_route_request(fake_request("/auth/logout"))
    expect_equal(header_values(resp, "Location"), "https://example.org/AlloMate/")
    expect_match(header_values(resp, "Set-Cookie"), "^allomate_session=; .*Max-Age=0")
  })
})

test_that("the sign-in bypass is ignored on Cloud Run", {
  with_cloud_env({
    Sys.setenv(ALLOMATE_AUTH_BYPASS = "true")
    expect_true(AlloMate:::auth_bypass_enabled())
    Sys.setenv(K_SERVICE = "allomate")
    expect_false(AlloMate:::auth_bypass_enabled())
  })
})

test_that("only well-formed ORCID iDs are looked up", {
  expect_false(AlloMate:::firestore_user_is_active("../users/x"))
  expect_false(AlloMate:::firestore_user_is_active("0000-0002-1825-009"))
})
