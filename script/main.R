
script_paths <- c('script/1_incidence_all.R',
                  'script/2_incidence_group.R',
                  'script/3_death_all.R',
                  'script/4_death_group.R',
                  'script/5_cfr.R',
                  'script/6_cluster.R')


for (p in script_paths) {
     source(p)
     remove(list = ls())
}
