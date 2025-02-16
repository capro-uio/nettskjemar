test_that("test user information", {
  vcr::use_cassette("me", {
    me <- nskj_get_me()
  })

  expect_is(me, "list")
  expect_length(me, 10)
  expect_equal(
    names(me),
    c("isPersonalDataResponsible", "displayName", "logoutLink", "isSuperUser", 
    "isAuthenticated", "userType", "hasAcceptedTos", "isSupportUser", 
    "isAdministrativeUser", "isInLdapGroupUioTils")
  )
  expect_is(me$isPersonalDataResponsible, "logical")
  expect_is(me$isAuthenticated, "logical")
  expect_is(me$isAdministrativeUser, "logical")
  expect_is(me$displayName, "character")
  expect_match(
    me$displayName, 
    "^[[:alnum:].-_]+@[[:alnum:].-]+$"
  )
})