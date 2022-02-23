test_that("Can instantiate resampling with id and with task", {
  oml_task = OMLTask$new(59)
  oml_resampling1 = OMLResampling$new(oml_task)
  oml_resampling2 = OMLResampling$new(task_id = 59)
  expect_equal(oml_resampling1$id, oml_resampling2$id)
  expect_equal(oml_resampling1$task$id, oml_resampling2$task$id)
  resampling1 = oml_resampling1$convert()
  resampling2 = oml_resampling2$convert()
  expect_equal(resampling1, resampling2)
})

test_that("Can use the converted resampling", {
  # TODO: This should be better tested with more resamplings
  with_public_server()
  oml_task = OMLTask$new(59)
  oml_resampling = OMLResampling$new(task_id = 59)

  oml_resampling = oml_task$resampling
  expect_r6(oml_resampling, "OMLResampling")
  expect_integer(oml_resampling$id)
  expect_r6(oml_resampling$task, "OMLTask")
  resampling = oml_resampling$convert()
  expect_r6(resampling, "ResamplingCustom")
})
