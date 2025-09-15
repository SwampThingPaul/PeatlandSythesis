
## Synthesis Literature Search
## Breaking the list into four groups (one for each author) 
## with a randomly assigned 10% overlap.
set.seed(123)  # for reproducibility

n <- 110
g <- 4
x <- 1:n
group_size <- floor(n / g)
overlap_prop <- 0.12

# Shuffle the data to avoid ordered bias
x_shuffled <- sample(x)

groups <- vector("list", g)
used_indices <- integer(0)

for (i in 1:g) {
  # Get remaining (non-core) items
  remaining <- setdiff(x_shuffled, used_indices)
  
  # Base group without overlap
  group_core <- head(remaining, group_size)
  
  # Add random overlap from previous group
  if (i > 1) {
    overlap_n <- ceiling(overlap_prop * group_size)
    overlap_from_prev <- sample(groups[[i - 1]], size = overlap_n)
    group_i <- unique(c(group_core, overlap_from_prev))
  } else {
    group_i <- group_core
  }
  
  groups[[i]] <- group_i
  used_indices <- unique(c(used_indices, group_core))  # only count core toward usage
}

# Find the max group length (they may differ slightly)
max_len <- max(sapply(groups, length))

# Pad groups with NA to make equal-length columns
group_df <- as.data.frame(
  lapply(groups, function(g) {
    length(g) <- max_len  # pad with NA
    return(g)
  })
)
# Name the columns
names(group_df) <- paste0("Group", seq_len(g))



# Build binary membership table: rec_num 1:110 × Group1..Group4
rec_num <- 1:n
final_table <- data.frame(rec_num = rec_num)

# For each group, mark membership
for (i in seq_along(groups)) {
  group_name <- paste0("Group", i)
  final_table[[group_name]] <- as.integer(rec_num %in% groups[[i]])
}

# View first few rows
head(final_table)

row_sum <- apply(final_table[,2:5],1,sum)
which(row_sum==0)
sum(row_sum==2)/n

apply(final_table[,2:5],2,sum)
# two rows were not assigned a group. 
# This fills in the group with the least number of records
final_table[which(row_sum==0),"Group1"] <- 1


# Write to clipboard to paste into spreadsheet
write.table(final_table, "clipboard", sep = "\t", row.names = FALSE)
