---
  title: "CFANS reorg analysis"
author: "Sam Safran & Rob Blair"
date: "08-September-2023"
output:
  html_document:
  df_print: kable
theme: cosmo
toc: true
toc_depth: 4
toc_float: true
---
  <style>
  
  pre {
    font-size: 12px;
  }
</style>
  ---
  
  
# load packages
library(tidyverse)
library(vegan)
library(here)
library(goeveg) # needed for dimcheckMDS to create scree plot.

set.seed(845)

# load data
dat <- read.csv(here("Data", "Students Data Set.csv")) %>%
  rename("course" = "Course1",
         "student" = "Name",
         "major" = "Major1")

# consolidate courses with modifiers--e.g., make "WRIT 3562W" and "WRIT 3562V" both just "WRIT 3562". This reduces the number of courses by 24 (from 1944 courses to 1920 courses)
dat <- dat %>%
  mutate(course = sub("[A-Za-z]+$", "", course))

# number of students in each course
enrollment <- dat %>%
  group_by(course) %>%
  summarize(n_students = n()) %>%
  arrange(desc(n_students))

# plot histogram counting number of courses by enrollment size
enrollment %>%
  ggplot() +
  geom_histogram(mapping = aes(x = n_students), binwidth = 1) +
  labs(title = "Number of courses by enrollment size",
       x = "CFANS enrollment",
       y = "Number of courses")

require(ggQC)

# pareto chart--starts to level off at courses with ~5 students  
enrollment %>%
  ggplot(aes(x = course, y = n_students)) +
  stat_pareto(point.color = "red",
              point.size = 1,
              line.color = "black",
              bars.fill = c("blue", "orange"),)

# Number of courses with <= X CFANS students-- 701 of 1920 total courses are only taken by 1 student
courses_to_drop <- enrollment %>% 
  filter(n_students <= 10) %>%
  pull(course)

# remove students who didn't take at least 4 courses (196 of 1991)
students_to_drop <- dat %>% group_by(student) %>%
  summarize(n_courses = n()) %>%
  filter(n_courses <=3) %>%
  pull(student)

# Remove courses and students not meeting minimums
df <- dat %>%
  filter(!(course %in% courses_to_drop)) %>%
  filter(!(student %in% students_to_drop))

# generate student-course binary matrix
matrix <- df %>%
  pivot_wider(student, 
              names_from = course, 
              values_from = course, 
              values_fn = list(course = length), 
              values_fill = list(course = 0)) %>%
  column_to_rownames(var = "student") 


# double majors have their courses duplicated, yielding counts >1 in the matrix. fix that.
matrix[matrix>1] <- 1

# pull student data for this matrix
student_majors <- dat %>%
  group_by(student) %>%
  summarize(major_count = length(unique(major)),
            majors_all = paste(unique(major), collapse = ", "),
            major = sample(unique(major), 1)) # for double-majors, randomly pick a single major to assign for anlaysis 

student_dat <- row.names(matrix) %>%
  as.data.frame() %>%
  rename("student" = ".") %>%
  left_join(sudent_majors)

# Generate distance matrix
dist_mat <- vegdist(matrix, "jaccard", binary = TRUE)

# Generate mean dissimilarity matrix, based on major
dissim <- meandist(dist_mat, grouping = student_dat$major) 

#"Function meandist calculates a matrix of mean within-cluster dissimilarities (diagonal) and between-cluster dissimilarities (off-diagonal elements), and an attribute n of grouping counts. Function summary finds the within-class, between-class and overall means of these dissimilarities, and the MRPP statistics with all weight.type options and the Classification Strength, CS (Van Sickle and Hughes, 2000)."
summary(dissim) # I don't actually know how to interpret this...

# Plot resulting dendrogram
plot(dissim, cex = .6)









# Create a binary matrix where rows represent students and columns represent courses
binary_matrix <- table(df$student, df$course)

binary_matrix[binary_matrix >=2] <- 1

# Perform PCA
pca_result <- prcomp(binary_matrix, center = TRUE, scale. = TRUE)

# Get the course loadings (coefficients of each course on the principal components)
course_loadings <- pca_result$rotation

# Cluster courses based on the loadings using hierarchical clustering
dist_matrix <- dist(course_loadings)
clusters <- hclust(dist_matrix, method = "complete")

hc <- clusters
hcd <- as.dendrogram(clusters)


plot(hc)
plot(hcd)

# install.packages("ape")
library("ape")
# Default plot
plot(as.phylo(clusters))

plot(as.phylo(hc), type = "unrooted", cex = 0.6,
     no.margin = TRUE)



plot(clusters)
# Cut the tree into 3 clusters
course_clusters <- cutree(clusters, k = 3)

# Cut the tree into 4 clusters
course_clusters <- cutree(clusters, k = 4)

# View the Results
result_df <- data.frame(CourseID = colnames(binary_matrix), Cluster = course_clusters)
print(result_df)

# add column with dept
result_df <- result_df %>%
  separate(CourseID, into = c('subject', 'course_num'), sep = '\\s', remove = FALSE)

test <- result_df %>%
  group_by(subject, Cluster) %>%
  summarize(n_courses = n(),
            courses = paste(CourseID, collapse = ", "))


### Run NMDS

matrix <- df %>%
  pivot_wider(student, 
              names_from = course, 
              values_from = course, 
              values_fn = list(course = length), 
              values_fill = list(course = 0)) %>%
  column_to_rownames(var = "student") 


# double majors have their courses duplicated, yielding counts >1 in the matrix. fix that.
matrix[matrix>1] <- 1

# pull student data for this matrix
student_majors <- dat %>%
  group_by(student) %>%
  summarize(major_count = length(unique(major)),
            majors_all = paste(unique(major), collapse = ", "),
            major = sample(unique(major), 1))

student_dat <- row.names(matrix) %>%
  as.data.frame() %>%
  rename("student" = ".") %>%
  left_join(sudent_majors)

# Generate distance matrix
dist_mat <- vegdist(matrix, "jaccard", binary = TRUE)

dissim <- meandist(dist_mat, grouping = student_dat$major) 

plot(dissim)

# Determine best dimensionailty by comparing stress levels with scree plot
screeplot <- dimcheckMDS(matrix,
                         distance = "bray", # distance measure used for metaMDS analysis
                         #trymax = 200,
                         k = 6) # maximum number of dimensions, default is 6. could in theory test up to max number of species.

screeplot

# stress from just 2 dimensions is >0.2, which is unusable according to some. going to go with 3 dimensions.
set.seed(1)
nmds3 <- metaMDS(comm = matrix, # Define the data to use 
                 distance = "bray", # Specify distance
                 k = 3,
                 trace = FALSE) # allow default for community data

nmds3

### Evaluate NDMS

nmds3$ndim                        # how many nMDS axes were created?
nmds3$converged                 # did it find a convergent solution?
nmds3$points                 # scores for each location (as samples)
nmds3$species          # scores for each animal order (as variables)

#A Shepard diagram is a scatterplot of distances between points in an nMDS final configuration (observed dissimilarity) against the original dissimilarities (ordination distance). For a nMDS, the line of best fit produced is monotonic, in that it can only step up or remain constant. The points in a good Shepard diagram should be a clean curve or straight line. Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions. If your plot resembles a step-wise or inverse L-shaped function, it would be wise to not interpret this data and instead try to solve for a degenerate solution (not covered in this course). However, please note if you have a small data set (not many observations) your data may look step-wise in a Shepard diagram. Note, you will need to produce a new Shepard diagram when altering the number of dimensions being plotted.Confusingly, the Shepard diagram is called stressplot in R!

stressplot(nmds3)

