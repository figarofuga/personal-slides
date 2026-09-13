test_that("Rectangular named braid_rows()", {
  expect_equal(
    braid_rows(
      lapply(1:3, function(x) {
        list(
          A = data.frame(first = x, second = x + 1:2),
          B = data.frame(info = 1, other = 3)
        )
      })
    ),
    list(
      A = data.frame(first = c(1, 1, 2, 2, 3, 3), second = c(2, 3, 3, 4, 4, 5)),
      B = data.frame(info = c(1, 1, 1), other = c(3, 3, 3))
    )
  )
})

test_that("Rectangular unnamed braid_rows()", {
  expect_equal(
    braid_rows(
      lapply(1:3, function(x) {
        list(
          data.frame(first = x, second = x + 1:2),
          data.frame(info = 1, other = 3)
        )
      })
    ),
    list(
      data.frame(first = c(1, 1, 2, 2, 3, 3), second = c(2, 3, 3, 4, 4, 5)),
      data.frame(info = c(1, 1, 1), other = c(3, 3, 3))
    )
  )
})

test_that("Jagged named braid_rows()", {
  expect_equal(
    braid_rows(
      lapply(1:3, function(x) {
        if (x == 2) {
          list(
            A = data.frame(first = x, second = x + 1:2),
            B = data.frame(info = 1, other = 3),
            C = data.frame(more = 3)
          )
        } else {
          list(
            A = data.frame(first = x, second = x + 1:2),
            B = data.frame(info = 1, other = 3)
          )
        }
      })
    ),
    list(
      A = data.frame(first = c(1, 1, 2, 2, 3, 3), second = c(2, 3, 3, 4, 4, 5)),
      B = data.frame(info = c(1, 1, 1), other = c(3, 3, 3)),
      C = data.frame(more = 3)
    )
  )
})

test_that("Jagged unnamed braid_rows()", {
  expect_equal(
    braid_rows(
      lapply(1:3, function(x) {
        if (x == 2) {
          list(
            data.frame(first = x, second = x + 1:2),
            data.frame(info = 1, other = 3),
            data.frame(more = 3)
          )
        } else {
          list(
            data.frame(first = x, second = x + 1:2),
            data.frame(info = 1, other = 3)
          )
        }
      })
    ),
    list(
      data.frame(first = c(1, 1, 2, 2, 3, 3), second = c(2, 3, 3, 4, 4, 5)),
      data.frame(info = c(1, 1, 1), other = c(3, 3, 3)),
      data.frame(more = 3)
    )
  )
})
