test_that("sanitise_name works", {
  expect_equal(sanitise_name('hypericum adenotrichum var. myriotrichum'), 'Hypericum adenotrichum var. myriotrichum')
  expect_equal(sanitise_name('TRIGONELLA smyrnaea'), 'Trigonella smyrnaea')
  expect_equal(sanitise_name('× orchiserapias bevilacquae'), '× Orchiserapias bevilacquae')
  expect_equal(sanitise_name('× ORCHiserapias bevilacquae'), '× Orchiserapias bevilacquae')
  expect_equal(sanitise_name('+ ORCHiserapias bevilacquae'), '+ Orchiserapias bevilacquae')
  expect_equal(sanitise_name('x ORCHiserapias bevilacquae'), 'x Orchiserapias bevilacquae')


})
