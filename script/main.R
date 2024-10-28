
library(parallel)

script_paths <- c('script/1_incidence_all.R',
                  'script/2_incidence_group.R',
                  'script/3_death_all.R',
                  'script/4_death_group.R',
                  'script/5_cluster.R')


cl <- makeCluster(4)

results <- parLapply(cl, script_paths, function(script) {
     source(script)
})

stopCluster(cl)

print(results)
