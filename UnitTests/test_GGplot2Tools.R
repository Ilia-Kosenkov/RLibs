context("GGplot2 helper functions")
test_that("find.gtable provides output", {
          
          data = data.frame(x = 1:10, y = log(1:10))
          plot = ggplot(data, aes(x, y)) + geom_line() + geom_point()
          grob = ggplotGrob(plot)
          result = lookup(grob, "panel", "axis-l")

          expect_true((c("panel") %in% result$name) &&
                      (c("axis-l") %in% result$name))
          })


