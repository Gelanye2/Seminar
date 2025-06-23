# 类别数
n_installer <- n_distinct(x$installer)
n_funder    <- n_distinct(x$funder)

# 出现次数小于10的比例
installer_freq <- table(x$installer)
funder_freq    <- table(x$funder)

prop_installer_rare <- mean(installer_freq < 10)
prop_funder_rare    <- mean(funder_freq < 10)

cat("Installer 类别数:", n_installer, "\n",
    "稀有 Installer 比例 (<10):", round(prop_installer_rare, 3), "\n\n")

cat("Funder 类别数:", n_funder, "\n",
    "稀有 Funder 比例 (<10):", round(prop_funder_rare, 3), "\n")

