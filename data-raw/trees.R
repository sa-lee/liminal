if (require("phateR")) {
  fake_trees <- tree.data[["data"]]
  colnames(fake_trees) <- paste0("dim", seq_len(ncol(fake_trees)))
  tree_branch <- tree.data[["branches"]]
  fake_trees <- as.data.frame(fake_trees)
  fake_trees$branches <- tree_branch
}

usethis::use_data(fake_trees, overwrite = "TRUE")
