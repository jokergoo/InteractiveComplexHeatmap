
set.seed(123)
m = matrix(rnorm(100), 10)
ht = Heatmap(m, cluster_rows = FALSE, cluster_columns = FALSE)


f = tempfile()
pdf(f)
ht = draw(ht)
pos = selectPosition(ht, pos = unit(c(3, 3), "cm"))
dev.off()
file.remove(f)

test_that("test selectPosition()", {
	expect_equal(pos[1, "row_index"], 9)
	expect_equal(pos[1, "column_index"], 2)
})


f = tempfile()
pdf(f)
ht = draw(ht)
pos = selectArea(ht, pos1 = unit(c(3, 3), "cm"), pos2 = unit(c(5, 5), "cm"))
dev.off()
file.remove(f)



test_that("test selectPosition()", {
	expect_equal(pos[1, "row_index"][[1]], c(8, 9))
	expect_equal(pos[1, "column_index"][[1]], c(2, 3, 4))
})
