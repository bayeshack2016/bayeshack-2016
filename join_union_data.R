library(dplyr)

data=read.csv("stateLevelOccSal.csv")
data$OCC_CODE_PREFIX=as.numeric(sapply(data$OCC_CODE, substring, 0, 2))

membership=read.csv("union-occupation-membership.csv")
salary=read.csv("union-occupation-wk-salary.csv")

#membership$OCC_CODE = seq.int(nrow(membership))
#salary$OCC_CODE = seq.int(nrow(salary))
#write.csv(membership, "union-occupation-membership.csv")
#write.csv(salary, "union-occupation-wk-salary.csv")

union_occ_lookup=read.csv("union-occ-lookup.csv")

data_with_union = data %>%
  inner_join(union_occ_lookup, by=c("OCC_CODE_PREFIX" = "REG_OCC_CODE_PREFIX")) %>%
  inner_join(salary, by=c("UNION_OCC_CODE" = "OCC_CODE"))
