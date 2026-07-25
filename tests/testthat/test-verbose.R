test_that("API functions are verbose only in interactive sessions by default", {
  api_functions <- list(
    ecotopia_login,
    ecotopia_device_page,
    ecotopia_devlist,
    ecotopia_data,
    kineis_login,
    kineis_devlist,
    kineis_data
  )

  for (api_function in api_functions) {
    expect_identical(
      formals(api_function)$verbose,
      quote(interactive())
    )
  }
})
